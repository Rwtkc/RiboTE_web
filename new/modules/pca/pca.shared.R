pca_export_defaults <- function() {
  list(
    format = "png",
    width = 2800,
    height = 1800,
    dpi = 300,
    figure_scope = "current",
    data_format = "csv",
    data_scope = "current"
  )
}

pca_export_scope_options <- function() {
  list(
    list(label = "Current Figure", value = "current"),
    list(label = "Current Data Space, All Methods", value = "current_space_all_methods"),
    list(label = "Current Method, All Data Spaces", value = "current_method_all_spaces"),
    list(label = "All Data Spaces x All Methods", value = "all")
  )
}

pca_export_config <- function(id) {
  ns <- NS(id)
  defaults <- pca_export_defaults()

  list(
    ids = list(
      figureScope = ns("export_scope"),
      format = ns("export_format"),
      width = ns("export_width"),
      height = ns("export_height"),
      dpi = ns("export_dpi"),
      trigger = ns("export_plot"),
      dataScope = ns("data_export_scope"),
      dataFormat = ns("data_export_format"),
      dataTrigger = ns("data_export")
    ),
    defaults = defaults,
    choices = list(
      figureScope = pca_export_scope_options(),
      format = list(
        list(label = "PNG", value = "png"),
        list(label = "PDF", value = "pdf")
      ),
      dataScope = pca_export_scope_options(),
      dataFormat = list(
        list(label = "CSV", value = "csv"),
        list(label = "TXT (tab-delimited)", value = "txt")
      )
    ),
    sectioned = TRUE
  )
}

pca_export_settings <- function(input) {
  defaults <- pca_export_defaults()

  width <- suppressWarnings(as.numeric(input$export_width))
  height <- suppressWarnings(as.numeric(input$export_height))
  dpi <- suppressWarnings(as.numeric(input$export_dpi))
  figure_scope <- if (is.null(input$export_scope) || identical(input$export_scope, "")) defaults$figure_scope else input$export_scope
  format <- tolower(if (is.null(input$export_format) || identical(input$export_format, "")) defaults$format else input$export_format)
  data_scope <- if (is.null(input$data_export_scope) || identical(input$data_export_scope, "")) defaults$data_scope else input$data_export_scope
  data_format <- tolower(if (is.null(input$data_export_format) || identical(input$data_export_format, "")) defaults$data_format else input$data_export_format)

  list(
    figure_scope = pca_normalize_export_scope(figure_scope),
    format = if (format %in% c("png", "pdf")) format else defaults$format,
    width = if (is.finite(width) && width > 0) width else defaults$width,
    height = if (is.finite(height) && height > 0) height else defaults$height,
    dpi = if (is.finite(dpi) && dpi > 0) dpi else defaults$dpi,
    data_scope = pca_normalize_export_scope(data_scope),
    data_format = if (data_format %in% c("csv", "txt")) data_format else defaults$data_format
  )
}

pca_normalize_export_scope <- function(scope = NULL) {
  normalized_scope <- if (is.null(scope) || !nzchar(as.character(scope))) "current" else as.character(scope)
  allowed_scopes <- c("current", "current_space_all_methods", "current_method_all_spaces", "all")

  if (!normalized_scope %in% allowed_scopes) {
    normalized_scope <- "current"
  }

  normalized_scope
}

pca_archive_filename <- function(kind = "export") {
  safe_kind <- gsub("[^a-z0-9]+", "_", tolower(as.character(kind)))
  sprintf("pca_%s_%s.zip", safe_kind, format(Sys.Date(), "%Y%m%d"))
}

pca_projection_filename <- function(data_space, method, extension) {
  safe_space <- gsub("[^a-z0-9]+", "_", tolower(as.character(data_space)))
  safe_method <- gsub("[^a-z0-9]+", "_", tolower(as.character(method)))
  sprintf("pca_%s_%s_%s.%s", safe_space, safe_method, format(Sys.Date(), "%Y%m%d"), extension)
}

pca_rows_payload <- function(data_frame) {
  if (is.null(data_frame) || !is.data.frame(data_frame) || nrow(data_frame) == 0) {
    return(list())
  }

  lapply(seq_len(nrow(data_frame)), function(index) {
    row_values <- data_frame[index, , drop = FALSE]

    lapply(row_values, function(value) {
      if (is.numeric(value)) {
        return(unname(value))
      }

      as.character(value)
    })
  })
}

pca_plot_points_payload <- function(data_frame) {
  if (is.null(data_frame) || !is.data.frame(data_frame) || nrow(data_frame) == 0) {
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

pca_search_index <- function(data_frame) {
  if (is.null(data_frame) || !is.data.frame(data_frame) || nrow(data_frame) == 0) {
    return(character())
  }

  normalized_columns <- lapply(data_frame, function(column) {
    tolower(as.character(column))
  })

  do.call(paste, c(normalized_columns, sep = " "))
}

pca_table_export_content <- function(data_frame, format = "csv") {
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

pca_round_result_table <- function(data_frame, digits = 4L) {
  rounded <- data_frame

  for (column_name in colnames(rounded)) {
    if (is.numeric(rounded[[column_name]])) {
      rounded[[column_name]] <- round(rounded[[column_name]], digits = digits)
    }
  }

  rounded
}

pca_data_export_content <- function(pca_context, format = "csv") {
  stopifnot(is.data.frame(pca_context$result_table))
  pca_table_export_content(pca_round_result_table(pca_context$result_table, digits = 4L), format = format)
}

pca_data_space_options <- function() {
  list(
    ribote_choice("TE Ratio", "TE"),
    ribote_choice("RNA Abundance", "RNA"),
    ribote_choice("Ribo Abundance", "Ribo")
  )
}

pca_data_space_label <- function(data_space) {
  switch(
    as.character(data_space),
    TE = "TE Ratio",
    RNA = "RNA Abundance",
    Ribo = "Ribo Abundance",
    as.character(data_space)
  )
}

pca_method_options <- function() {
  list(
    ribote_choice("PCA"),
    ribote_choice("MDS"),
    ribote_choice("T-SNE")
  )
}

pca_normalize_parameters <- function(data_space = NULL, method = NULL) {
  normalized_data_space <- if (is.null(data_space) || !nzchar(as.character(data_space))) "TE" else as.character(data_space)
  normalized_method <- if (is.null(method) || !nzchar(as.character(method))) "PCA" else as.character(method)

  if (!normalized_data_space %in% c("TE", "RNA", "Ribo")) {
    normalized_data_space <- "TE"
  }

  if (!normalized_method %in% c("PCA", "MDS", "T-SNE")) {
    normalized_method <- "PCA"
  }

  list(
    data_space = normalized_data_space,
    method = normalized_method
  )
}

pca_export_targets <- function(current_parameters, scope = "current") {
  scope <- pca_normalize_export_scope(scope)
  current_parameters <- pca_normalize_parameters(
    data_space = current_parameters$data_space,
    method = current_parameters$method
  )

  target_grid <- switch(
    scope,
    current = expand.grid(
      data_space = current_parameters$data_space,
      method = current_parameters$method,
      stringsAsFactors = FALSE
    ),
    current_space_all_methods = expand.grid(
      data_space = current_parameters$data_space,
      method = c("PCA", "MDS", "T-SNE"),
      stringsAsFactors = FALSE
    ),
    current_method_all_spaces = expand.grid(
      data_space = c("TE", "RNA", "Ribo"),
      method = current_parameters$method,
      stringsAsFactors = FALSE
    ),
    all = expand.grid(
      data_space = c("TE", "RNA", "Ribo"),
      method = c("PCA", "MDS", "T-SNE"),
      stringsAsFactors = FALSE
    )
  )

  split(target_grid, seq_len(nrow(target_grid)))
}

pca_base_signature <- function(te_context, preprocess_context) {
  paste(
    translation_efficiency_preprocess_signature(preprocess_context),
    tolower(as.character(te_context$parameters$te_tool)),
    sep = "::"
  )
}

pca_source_signature <- function(te_context, preprocess_context) {
  parameters <- te_context$parameters

  paste(
    pca_base_signature(te_context, preprocess_context),
    tolower(as.character(parameters$te_tool)),
    as.character(parameters$fvalue),
    as.character(parameters$p_cutoff),
    tolower(as.character(parameters$p_type)),
    sep = "::"
  )
}

pca_build_short_names <- function(pair_manifest) {
  counters <- c(Control = 0L, Treatment = 0L)
  entries <- list(rna = list(), ribo = list(), te = list())

  for (index in seq_len(nrow(pair_manifest))) {
    pair_row <- pair_manifest[index, , drop = FALSE]
    role <- as.character(pair_row$group_role[[1]])
    counters[[role]] <- counters[[role]] + 1L
    suffix <- if (identical(role, "Control")) {
      paste0("C", counters[[role]])
    } else {
      paste0("T", counters[[role]])
    }

    entries$rna[[length(entries$rna) + 1L]] <- data.frame(
      display_sample = paste0("RNA.", suffix),
      actual_sample = as.character(pair_row$rna_sample[[1]]),
      actual_rna = as.character(pair_row$rna_sample[[1]]),
      actual_ribo = "",
      group = role,
      stringsAsFactors = FALSE
    )

    entries$ribo[[length(entries$ribo) + 1L]] <- data.frame(
      display_sample = paste0("RPF.", suffix),
      actual_sample = as.character(pair_row$ribo_sample[[1]]),
      actual_rna = "",
      actual_ribo = as.character(pair_row$ribo_sample[[1]]),
      group = role,
      stringsAsFactors = FALSE
    )

    entries$te[[length(entries$te) + 1L]] <- data.frame(
      display_sample = paste0("TE.", suffix),
      actual_sample = paste0(as.character(pair_row$ribo_sample[[1]]), " / ", as.character(pair_row$rna_sample[[1]])),
      actual_rna = as.character(pair_row$rna_sample[[1]]),
      actual_ribo = as.character(pair_row$ribo_sample[[1]]),
      group = role,
      stringsAsFactors = FALSE
    )
  }

  lapply(entries, function(rows) {
    do.call(rbind, rows)
  })
}

pca_extract_numeric_matrix <- function(data_frame, columns) {
  stopifnot(is.data.frame(data_frame))
  stopifnot(all(columns %in% colnames(data_frame)))

  matrix_values <- data_frame[, columns, drop = FALSE]
  matrix_values <- as.matrix(data.frame(lapply(matrix_values, as.numeric), check.names = FALSE))
  rownames(matrix_values) <- if ("GeneID" %in% colnames(data_frame)) {
    as.character(data_frame$GeneID)
  } else {
    seq_len(nrow(data_frame))
  }

  matrix_values
}

pca_prepare_feature_matrix <- function(feature_matrix) {
  if (!is.matrix(feature_matrix) || nrow(feature_matrix) == 0 || ncol(feature_matrix) == 0) {
    stop("No feature matrix is available for PCA.", call. = FALSE)
  }

  finite_rows <- apply(feature_matrix, 1, function(row) all(is.finite(row)))
  prepared <- feature_matrix[finite_rows, , drop = FALSE]

  if (nrow(prepared) == 0) {
    stop("No finite features remain after filtering.", call. = FALSE)
  }

  variable_rows <- apply(prepared, 1, function(row) {
    stats::var(row, na.rm = TRUE) > 0
  })
  prepared <- prepared[variable_rows, , drop = FALSE]

  if (nrow(prepared) == 0) {
    stop("No variable features remain after filtering.", call. = FALSE)
  }

  prepared
}

pca_build_base_context <- function(te_context, preprocess_context, upload_context) {
  stopifnot(is.data.frame(te_context$result_table))
  stopifnot(is.data.frame(upload_context$pair_manifest))

  pair_manifest <- upload_context$pair_manifest
  sample_map <- pca_build_short_names(pair_manifest)
  result_table <- te_context$result_table

  rna_matrix <- pca_extract_numeric_matrix(result_table, sample_map$rna$actual_sample)
  ribo_matrix <- pca_extract_numeric_matrix(result_table, sample_map$ribo$actual_sample)
  te_matrix <- sapply(seq_len(nrow(sample_map$te)), function(index) {
    log2((as.numeric(result_table[[sample_map$te$actual_ribo[[index]]]]) + 1) / (as.numeric(result_table[[sample_map$te$actual_rna[[index]]]]) + 1))
  })

  if (is.null(dim(te_matrix))) {
    te_matrix <- matrix(te_matrix, ncol = 1L)
  }

  rownames(te_matrix) <- if ("GeneID" %in% colnames(result_table)) {
    as.character(result_table$GeneID)
  } else {
    seq_len(nrow(result_table))
  }
  colnames(te_matrix) <- sample_map$te$display_sample

  log_rna_matrix <- log2(rna_matrix + 1)
  colnames(log_rna_matrix) <- sample_map$rna$display_sample
  log_ribo_matrix <- log2(ribo_matrix + 1)
  colnames(log_ribo_matrix) <- sample_map$ribo$display_sample

  list(
    base_signature = pca_base_signature(te_context, preprocess_context),
    source_signature = pca_source_signature(te_context, preprocess_context),
    te_tool = as.character(te_context$parameters$te_tool),
    spaces = list(
      TE = list(
        label = pca_data_space_label("TE"),
        feature_matrix = pca_prepare_feature_matrix(te_matrix),
        sample_map = sample_map$te
      ),
      RNA = list(
        label = pca_data_space_label("RNA"),
        feature_matrix = pca_prepare_feature_matrix(log_rna_matrix),
        sample_map = sample_map$rna
      ),
      Ribo = list(
        label = pca_data_space_label("Ribo"),
        feature_matrix = pca_prepare_feature_matrix(log_ribo_matrix),
        sample_map = sample_map$ribo
      )
    )
  )
}

pca_projection_axis_labels <- function(method, fit = NULL) {
  if (identical(method, "PCA") && !is.null(fit$sdev)) {
    explained <- round(100 * (fit$sdev^2) / sum(fit$sdev^2), 1)
    return(list(
      x = sprintf("PC1 (%s%%)", explained[[1]]),
      y = sprintf("PC2 (%s%%)", explained[[2]])
    ))
  }

  list(
    x = "Dimension 1",
    y = "Dimension 2"
  )
}

pca_projection_coordinates <- function(feature_matrix, method) {
  if (ncol(feature_matrix) < 2L) {
    stop("At least two samples are required for PCA projection.", call. = FALSE)
  }

  if (identical(method, "PCA")) {
    fit <- stats::prcomp(t(feature_matrix), center = TRUE, scale. = FALSE)
    if (is.null(fit$x) || ncol(fit$x) < 2L) {
      stop("PCA could not derive two projection components.", call. = FALSE)
    }

    return(list(
      coordinates = fit$x[, 1:2, drop = FALSE],
      axis = pca_projection_axis_labels(method, fit = fit)
    ))
  }

  if (identical(method, "MDS")) {
    correlation <- suppressWarnings(stats::cor(feature_matrix, use = "pairwise.complete.obs", method = "pearson"))
    correlation[!is.finite(correlation)] <- 0
    diag(correlation) <- 1
    distance_matrix <- 1 - correlation
    distance_matrix[distance_matrix < 0] <- 0
    distance <- stats::as.dist(distance_matrix)
    fit <- stats::cmdscale(distance, eig = TRUE, k = 2)

    if (is.null(fit$points) || ncol(fit$points) < 2L) {
      stop("MDS could not derive two projection dimensions.", call. = FALSE)
    }

    return(list(
      coordinates = fit$points[, 1:2, drop = FALSE],
      axis = pca_projection_axis_labels(method)
    ))
  }

  if (identical(method, "T-SNE")) {
    if (!requireNamespace("Rtsne", quietly = TRUE)) {
      stop("Rtsne is required to run T-SNE projections.", call. = FALSE)
    }

    sample_count <- ncol(feature_matrix)
    if (sample_count < 3L) {
      stop("T-SNE requires at least three samples.", call. = FALSE)
    }

    perplexity <- max(1, min(5, floor((sample_count - 1) / 3)))
    perplexity <- min(perplexity, sample_count - 1L)
    set.seed(1)
    fit <- Rtsne::Rtsne(
      t(feature_matrix),
      dims = 2,
      perplexity = perplexity,
      verbose = FALSE,
      max_iter = 400,
      check_duplicates = FALSE
    )

    return(list(
      coordinates = fit$Y[, 1:2, drop = FALSE],
      axis = pca_projection_axis_labels(method)
    ))
  }

  stop(sprintf("Unsupported PCA method: %s", method), call. = FALSE)
}

pca_projection_context <- function(base_context, data_space = "TE", method = "PCA", source_signature = NULL) {
  parameters <- pca_normalize_parameters(data_space = data_space, method = method)
  space_context <- base_context$spaces[[parameters$data_space]]

  if (is.null(space_context)) {
    stop(sprintf("Unsupported PCA data space: %s", parameters$data_space), call. = FALSE)
  }

  feature_matrix <- space_context$feature_matrix
  sample_map <- space_context$sample_map
  projection <- pca_projection_coordinates(feature_matrix, parameters$method)
  coordinates <- as.data.frame(projection$coordinates, stringsAsFactors = FALSE)
  coordinate_column_names <- c(projection$axis$x, projection$axis$y)
  colnames(coordinates) <- coordinate_column_names

  display_table <- data.frame(
    `Display Sample` = sample_map$display_sample,
    Group = sample_map$group,
    `Data Space` = space_context$label,
    Method = parameters$method,
    stringsAsFactors = FALSE
  )

  if (identical(parameters$data_space, "TE")) {
    display_table$`Actual RNA` <- sample_map$actual_rna
    display_table$`Actual Ribo` <- sample_map$actual_ribo
    display_table$`Actual Pair` <- sample_map$actual_sample
  } else {
    display_table$`Actual Sample` <- sample_map$actual_sample
  }

  result_table <- cbind(display_table, coordinates)

  plot_points <- data.frame(
    display_sample = sample_map$display_sample,
    actual_sample = sample_map$actual_sample,
    actual_rna = sample_map$actual_rna,
    actual_ribo = sample_map$actual_ribo,
    group = sample_map$group,
    x = coordinates[[coordinate_column_names[[1]]]],
    y = coordinates[[coordinate_column_names[[2]]]],
    stringsAsFactors = FALSE
  )

  list(
    base_signature = base_context$base_signature,
    source_signature = if (is.null(source_signature)) base_context$source_signature else source_signature,
    parameters = parameters,
    plot = list(
      title = sprintf("%s on %s Space", parameters$method, space_context$label),
      x_label = projection$axis$x,
      y_label = projection$axis$y,
      data_space_label = space_context$label,
      method_label = parameters$method
    ),
    result_table = result_table,
    plot_points = plot_points,
    metrics = list(
      list(label = "Samples Projected", value = ncol(feature_matrix)),
      list(label = "Genes Used", value = format(nrow(feature_matrix), big.mark = ","))
    )
  )
}

pca_results_payload <- function(pca_context) {
  list(
    plot = list(
      title = pca_context$plot$title,
      xLabel = pca_context$plot$x_label,
      yLabel = pca_context$plot$y_label,
      dataSpaceLabel = pca_context$plot$data_space_label,
      methodLabel = pca_context$plot$method_label,
      points = pca_plot_points_payload(pca_context$plot_points)
    )
  )
}

pca_module_config <- function() {
  ribote_build_module_config(
    key = "pca",
    title = "PCA",
    eyebrow = "Sample Space Projection",
    description = "Compare how Control and Treatment samples separate in TE ratio, RNA abundance, or Ribo abundance space.",
    requires = "te",
    run_label = "Run PCA",
    sections = list(
      list(
        title = "Projection Controls",
        fields = list(
          ribote_field("pca_data_space", "Data Space", "select", "TE", options = pca_data_space_options()),
          ribote_field("pca_method", "Method", "select", "PCA", options = pca_method_options())
        )
      )
    ),
    wide_sidebar = TRUE,
    show_export_panel = TRUE,
    show_analysis_panel = TRUE,
    snapshot_columns = 2
  )
}
