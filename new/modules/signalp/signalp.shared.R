signalp_method_label <- function(method) {
  switch(
    tolower(as.character(method)),
    all = "All Methods",
    signalp = "SignalP",
    tmhmm = "TMHMM",
    phobius = "Phobius",
    as.character(method)
  )
}

signalp_status_levels <- function() {
  c("Up", "Non", "Down")
}

signalp_species_key <- function(upload_context) {
  species_meta <- upload_context$species_meta

  if (is.list(species_meta) && !is.null(species_meta$acronym) && nzchar(as.character(species_meta$acronym))) {
    return(as.character(species_meta$acronym))
  }

  if (!is.null(upload_context$species) && nzchar(as.character(upload_context$species))) {
    species_meta <- ribote_species_meta(upload_context$species)
    if (!is.null(species_meta$acronym) && nzchar(as.character(species_meta$acronym))) {
      return(as.character(species_meta$acronym))
    }
  }

  NULL
}

signalp_resource_root <- function() {
  app_data_path("signal")
}

signalp_resource_pattern <- function(species_key, method) {
  normalized_method <- tolower(as.character(method))

  file_token <- switch(
    normalized_method,
    signalp = "signalP",
    tmhmm = "tmhmm",
    phobius = "phobius",
    NULL
  )

  if (is.null(file_token) || is.null(species_key) || !nzchar(as.character(species_key))) {
    return(NULL)
  }

  sprintf("^%s(?:\\.gencode_v[0-9]+)?\\.pep\\.%s\\.txt$", gsub("\\.", "\\\\.", as.character(species_key)), file_token)
}

signalp_resource_path <- function(species_key, method) {
  resource_dir <- signalp_resource_root()
  pattern <- signalp_resource_pattern(species_key, method)

  if (!dir.exists(resource_dir) || is.null(pattern)) {
    return(NULL)
  }

  candidates <- list.files(resource_dir, pattern = pattern, full.names = TRUE)

  if (length(candidates) == 0L) {
    return(NULL)
  }

  preferred_name <- switch(
    tolower(as.character(method)),
    signalp = sprintf("%s.pep.signalP.txt", species_key),
    tmhmm = sprintf("%s.pep.tmhmm.txt", species_key),
    phobius = sprintf("%s.pep.phobius.txt", species_key),
    basename(candidates[[1]])
  )

  preferred_match <- candidates[basename(candidates) == preferred_name]
  chosen <- if (length(preferred_match)) preferred_match[[1]] else candidates[[1]]
  normalizePath(chosen, winslash = "/", mustWork = TRUE)
}

signalp_available_methods <- function(upload_context) {
  species_key <- signalp_species_key(upload_context)

  if (is.null(species_key)) {
    return(character())
  }

  Filter(
    function(method) !is.null(signalp_resource_path(species_key, method)),
    c("signalp", "tmhmm", "phobius")
  )
}

signalp_supported_for_upload_context <- function(upload_context) {
  length(signalp_available_methods(upload_context)) > 0L
}

signalp_method_options <- function(upload_context = NULL) {
  available_methods <- if (is.null(upload_context)) {
    c("signalp", "tmhmm", "phobius")
  } else {
    signalp_available_methods(upload_context)
  }

  options <- list()

  if (length(available_methods) > 1L) {
    options[[length(options) + 1L]] <- ribote_choice("All", "all")
  }

  for (method in available_methods) {
    options[[length(options) + 1L]] <- ribote_choice(signalp_method_label(method), method)
  }

  if (!length(options)) {
    options <- list(
      ribote_choice("All", "all"),
      ribote_choice("SignalP", "signalp"),
      ribote_choice("TMHMM", "tmhmm"),
      ribote_choice("Phobius", "phobius")
    )
  }

  options
}

signalp_default_method <- function(upload_context = NULL) {
  options <- signalp_method_options(upload_context)
  if (!length(options)) {
    return("all")
  }

  as.character(options[[1]]$value)
}

signalp_normalize_parameters <- function(method = NULL, available_methods = NULL) {
  normalized_method <- if (is.null(method) || !nzchar(as.character(method))) {
    "all"
  } else {
    tolower(as.character(method))
  }

  allowed_methods <- c("all", "signalp", "tmhmm", "phobius")
  if (!normalized_method %in% allowed_methods) {
    normalized_method <- "all"
  }

  if (!is.null(available_methods) && length(available_methods)) {
    selectable_methods <- c(if (length(available_methods) > 1L) "all" else character(), available_methods)
    if (!normalized_method %in% selectable_methods) {
      normalized_method <- selectable_methods[[1]]
    }
  }

  list(method = normalized_method)
}

signalp_parameters_key <- function(parameters) {
  as.character(parameters$method)
}

signalp_source_signature <- function(te_context, preprocess_context, upload_context) {
  paste(
    pca_source_signature(te_context, preprocess_context),
    signalp_species_key(upload_context),
    sep = "::"
  )
}

signalp_cache_version <- function() {
  "signalp_v1_local_annotations"
}

signalp_export_defaults <- function() {
  list(
    format = "png",
    width = 2600,
    height = 1600,
    dpi = 300,
    data_format = "csv"
  )
}

signalp_export_config <- function(id) {
  ns <- NS(id)
  defaults <- signalp_export_defaults()

  list(
    ids = list(
      format = ns("export_format"),
      width = ns("export_width"),
      height = ns("export_height"),
      dpi = ns("export_dpi"),
      trigger = ns("export_plot"),
      dataFormat = ns("data_export_format"),
      dataTrigger = ns("data_export")
    ),
    defaults = defaults,
    choices = list(
      format = list(
        list(label = "PNG", value = "png"),
        list(label = "PDF", value = "pdf")
      ),
      dataFormat = list(
        list(label = "CSV", value = "csv"),
        list(label = "TXT (tab-delimited)", value = "txt")
      )
    )
  )
}

signalp_export_settings <- function(input) {
  defaults <- signalp_export_defaults()
  width <- suppressWarnings(as.numeric(input$export_width))
  height <- suppressWarnings(as.numeric(input$export_height))
  dpi <- suppressWarnings(as.numeric(input$export_dpi))
  format <- tolower(if (is.null(input$export_format) || identical(input$export_format, "")) defaults$format else input$export_format)
  data_format <- tolower(if (is.null(input$data_export_format) || identical(input$data_export_format, "")) defaults$data_format else input$data_export_format)

  list(
    format = if (format %in% c("png", "pdf")) format else defaults$format,
    width = if (is.finite(width) && width > 0) width else defaults$width,
    height = if (is.finite(height) && height > 0) height else defaults$height,
    dpi = if (is.finite(dpi) && dpi > 0) dpi else defaults$dpi,
    data_format = if (data_format %in% c("csv", "txt")) data_format else defaults$data_format
  )
}

signalp_safe_filename_token <- function(value, fallback = "signalp") {
  normalized <- tolower(as.character(value))
  normalized <- gsub("[^a-z0-9]+", "_", normalized)
  normalized <- gsub("^_+|_+$", "", normalized)

  if (!nzchar(normalized)) {
    fallback
  } else {
    normalized
  }
}

signalp_figure_filename <- function(method_label, extension = "png") {
  sprintf(
    "signalp_%s_plot_%s.%s",
    signalp_safe_filename_token(method_label, fallback = "all_methods"),
    format(Sys.Date(), "%Y%m%d"),
    extension
  )
}

signalp_archive_filename <- function(kind = "data") {
  sprintf(
    "signalp_%s_%s.zip",
    signalp_safe_filename_token(kind, fallback = "data"),
    format(Sys.Date(), "%Y%m%d")
  )
}

signalp_data_filename <- function(kind = "summary", method_label = "all_methods", extension = "csv") {
  sprintf(
    "signalp_%s_%s_%s.%s",
    signalp_safe_filename_token(kind, fallback = "summary"),
    signalp_safe_filename_token(method_label, fallback = "all_methods"),
    format(Sys.Date(), "%Y%m%d"),
    extension
  )
}

signalp_table_export_content <- function(data_frame, format = "csv") {
  stopifnot(is.data.frame(data_frame))

  connection <- textConnection("export_lines", "w", local = TRUE)
  on.exit(close(connection), add = TRUE)

  utils::write.table(
    data_frame,
    file = connection,
    sep = if (identical(format, "txt")) "\t" else ",",
    row.names = FALSE,
    col.names = TRUE,
    quote = TRUE
  )

  paste(export_lines, collapse = "\n")
}

signalp_normalize_gene_key <- function(values) {
  normalized <- trimws(as.character(values))
  normalized <- toupper(normalized)
  sub("\\.\\d+$", "", normalized)
}

signalp_read_resource <- function(resource_path) {
  resource_table <- utils::read.delim(
    resource_path,
    header = FALSE,
    sep = "\t",
    stringsAsFactors = FALSE,
    quote = "",
    comment.char = ""
  )

  if (!nrow(resource_table)) {
    return(character())
  }

  unique(signalp_normalize_gene_key(resource_table[[1]]))
}

signalp_extract_gene_table <- function(te_context) {
  result_table <- translation_efficiency_result_table_labels(te_context$result_table)
  stopifnot(is.data.frame(result_table))
  stopifnot("GeneID" %in% colnames(result_table))

  te_status <- if ("TE_Status" %in% colnames(result_table)) {
    as.character(result_table$TE_Status)
  } else if ("diffTE" %in% colnames(result_table)) {
    as.character(result_table$diffTE)
  } else {
    rep("Non", nrow(result_table))
  }

  te_status[!te_status %in% signalp_status_levels()] <- "Non"
  gene_name <- if ("gene_name" %in% colnames(result_table)) {
    as.character(result_table$gene_name)
  } else {
    rep("unknown", nrow(result_table))
  }
  gene_name[is.na(gene_name) | !nzchar(trimws(gene_name))] <- "unknown"

  data.frame(
    GeneID = as.character(result_table$GeneID),
    GeneKey = signalp_normalize_gene_key(result_table$GeneID),
    gene_name = gene_name,
    TE_Status = te_status,
    stringsAsFactors = FALSE
  )
}

signalp_fisher_pvalue <- function(yes_case, no_case, yes_non, no_non) {
  matrix_values <- matrix(
    c(yes_case, no_case, yes_non, no_non),
    nrow = 2,
    byrow = TRUE
  )

  if (any(rowSums(matrix_values) == 0) || any(colSums(matrix_values) == 0)) {
    return(NA_real_)
  }

  stats::fisher.test(matrix_values)$p.value
}

signalp_method_context <- function(gene_table, method, annotation_keys) {
  annotated_mask <- gene_table$GeneKey %in% annotation_keys
  status_levels <- signalp_status_levels()
  total_counts <- table(factor(gene_table$TE_Status, levels = status_levels))
  yes_counts <- table(factor(gene_table$TE_Status[annotated_mask], levels = status_levels))
  no_counts <- total_counts - yes_counts
  total_values <- as.integer(total_counts[status_levels])
  yes_values <- as.integer(yes_counts[status_levels])
  no_values <- as.integer(no_counts[status_levels])

  up_pvalue <- signalp_fisher_pvalue(
    yes_case = yes_values[[1]],
    no_case = no_values[[1]],
    yes_non = yes_values[[2]],
    no_non = no_values[[2]]
  )
  down_pvalue <- signalp_fisher_pvalue(
    yes_case = yes_values[[3]],
    no_case = no_values[[3]],
    yes_non = yes_values[[2]],
    no_non = no_values[[2]]
  )

  summary_rows <- data.frame(
    method = as.character(method),
    methodLabel = signalp_method_label(method),
    teGroup = status_levels,
    annotatedCount = yes_values,
    nonAnnotatedCount = no_values,
    totalCount = total_values,
    percent = ifelse(total_values > 0, yes_values / total_values, 0),
    upVsNonPValue = up_pvalue,
    downVsNonPValue = down_pvalue,
    stringsAsFactors = FALSE
  )

  test_rows <- data.frame(
    method = c(as.character(method), as.character(method)),
    methodLabel = c(signalp_method_label(method), signalp_method_label(method)),
    comparison = c("Up vs Non", "Down vs Non"),
    rawPValue = c(up_pvalue, down_pvalue),
    annotatedInTestGroup = c(yes_values[[1]], yes_values[[3]]),
    nonAnnotatedInTestGroup = c(no_values[[1]], no_values[[3]]),
    annotatedInNonGroup = c(yes_values[[2]], yes_values[[2]]),
    nonAnnotatedInNonGroup = c(no_values[[2]], no_values[[2]]),
    stringsAsFactors = FALSE
  )

  list(
    summary = summary_rows,
    tests = test_rows
  )
}

signalp_group_summary_table <- function(group_summary) {
  export_table <- group_summary
  export_table$percent <- export_table$percent * 100
  colnames(export_table) <- c(
    "Method",
    "Method_Label",
    "TE_Group",
    "Annotated_Genes",
    "Non_Annotated_Genes",
    "Total_Genes",
    "Annotated_Percentage",
    "Up_vs_Non_Fisher_PValue",
    "Down_vs_Non_Fisher_PValue"
  )
  export_table
}

signalp_test_summary_table <- function(test_summary) {
  export_table <- test_summary
  colnames(export_table) <- c(
    "Method",
    "Method_Label",
    "Comparison",
    "Raw_Fisher_PValue",
    "Annotated_In_Test_Group",
    "Non_Annotated_In_Test_Group",
    "Annotated_In_Non_Group",
    "Non_Annotated_In_Non_Group"
  )
  export_table
}

signalp_export_entries <- function(context, format = "csv") {
  extension <- if (identical(format, "txt")) "txt" else "csv"

  list(
    list(
      filename = signalp_data_filename("group_summary", context$method_label, extension = extension),
      content = signalp_table_export_content(
        signalp_group_summary_table(context$group_summary),
        format = format
      )
    ),
    list(
      filename = signalp_data_filename("fisher_tests", context$method_label, extension = extension),
      content = signalp_table_export_content(
        signalp_test_summary_table(context$test_summary),
        format = format
      )
    )
  )
}

signalp_rows_payload <- function(data_frame) {
  if (is.null(data_frame) || !is.data.frame(data_frame) || nrow(data_frame) == 0L) {
    return(list())
  }

  lapply(seq_len(nrow(data_frame)), function(index) {
    row_values <- data_frame[index, , drop = FALSE]
    payload <- as.list(row_values)

    for (name in names(payload)) {
      value <- payload[[name]]
      if (is.numeric(value)) {
        payload[[name]] <- unname(value)
      } else {
        payload[[name]] <- as.character(value)
      }
    }

    payload
  })
}

signalp_compute_context <- function(te_context, preprocess_context, upload_context, resources, parameters) {
  gene_table <- signalp_extract_gene_table(te_context)
  available_methods <- names(resources)
  requested_methods <- if (identical(parameters$method, "all")) {
    c("signalp", "tmhmm", "phobius")
  } else {
    parameters$method
  }
  selected_methods <- if (identical(parameters$method, "all")) {
    available_methods
  } else {
    intersect(parameters$method, available_methods)
  }

  if (!length(selected_methods)) {
    stop("Local SignalP annotation resources are not installed for the current species.", call. = FALSE)
  }

  method_contexts <- lapply(selected_methods, function(method) {
    signalp_method_context(gene_table, method, resources[[method]])
  })

  group_summary <- do.call(rbind, lapply(method_contexts, `[[`, "summary"))
  rownames(group_summary) <- NULL
  test_summary <- do.call(rbind, lapply(method_contexts, `[[`, "tests"))
  rownames(test_summary) <- NULL
  plot_rows <- group_summary

  union_annotation_keys <- unique(unlist(resources[selected_methods], use.names = FALSE))
  annotated_in_workspace <- sum(gene_table$GeneKey %in% union_annotation_keys, na.rm = TRUE)
  missing_methods <- setdiff(requested_methods, available_methods)
  note <- if (length(missing_methods)) {
    sprintf(
      "Unavailable for the current species: %s.",
      paste(vapply(missing_methods, signalp_method_label, character(1)), collapse = ", ")
    )
  } else {
    NULL
  }

  list(
    source_signature = signalp_source_signature(te_context, preprocess_context, upload_context),
    parameters = parameters,
    species_key = signalp_species_key(upload_context),
    method_label = if (identical(parameters$method, "all")) "All Methods" else signalp_method_label(parameters$method),
    selected_method_labels = vapply(selected_methods, signalp_method_label, character(1)),
    group_summary = group_summary,
    test_summary = test_summary,
    plot_rows = plot_rows,
    note = note,
    metrics = list(
      list(label = "Genes Assessed", value = format(nrow(gene_table), big.mark = ",")),
      list(label = "Annotated Genes", value = format(annotated_in_workspace, big.mark = ",")),
      list(label = "Methods Compared", value = length(selected_methods)),
      list(label = "Raw p < 0.05", value = sum(test_summary$rawPValue < 0.05, na.rm = TRUE))
    )
  )
}

signalp_results_payload <- function(context) {
  list(
    note = context$note,
    methodLabel = context$method_label,
    selectedMethodLabels = as.list(context$selected_method_labels),
    plot = list(
      rows = signalp_rows_payload(context$plot_rows)
    ),
    table = list(
      rows = signalp_rows_payload(context$group_summary),
      totalRows = nrow(context$group_summary),
      comparisonCount = nrow(context$test_summary)
    )
  )
}

signalp_module_config <- function(upload_context = NULL) {
  ribote_build_module_config(
    key = "signalp",
    title = "SignalP",
    eyebrow = "Secretory / Membrane Features",
    description = "Compare TE-defined gene groups against signal peptide and membrane-related annotations from local SignalP, TMHMM, and Phobius resources.",
    requires = "te",
    run_label = "Run SignalP",
    sections = list(
      list(
        title = "Annotation Source",
        fields = list(
          ribote_field(
            "signal_method",
            "Method",
            "select",
            signalp_default_method(upload_context),
            options = signalp_method_options(upload_context)
          )
        )
      )
    ),
    wide_sidebar = TRUE,
    show_export_panel = FALSE,
    blank_analysis_panel = TRUE
  )
}
