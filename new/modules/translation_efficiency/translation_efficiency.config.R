translation_efficiency_build_status <- function(log2_te_fc, selected_metric, fold_threshold) {
  status <- rep("Non", length(log2_te_fc))
  status[log2_te_fc >= fold_threshold & selected_metric < 1] <- "Up"
  status[log2_te_fc <= -fold_threshold & selected_metric < 1] <- "Down"
  status
}

translation_efficiency_result_table_labels <- function(result_table) {
  if (is.null(result_table) || !is.data.frame(result_table)) {
    return(result_table)
  }

  rename_map <- c(
    input1 = "RNA_Control_Mean",
    input2 = "RNA_Treatment_Mean",
    logInputFC = "RNA_log2FC",
    diffExp = "RNA_Expression_Status",
    rpf1 = "Ribo_Control_Mean",
    rpf2 = "Ribo_Treatment_Mean",
    logRPFfc = "Ribo_log2FC",
    diffRibo = "Ribo_Expression_Status",
    TE_A1 = "TE_Control_Mean",
    TE_A2 = "TE_Treatment_Mean",
    logTEfc = "TE_log2FC",
    diffTE = "TE_Status"
  )

  current_names <- colnames(result_table)
  matched <- current_names %in% names(rename_map)
  current_names[matched] <- unname(rename_map[current_names[matched]])
  colnames(result_table) <- current_names
  result_table
}

translation_efficiency_round_result_table <- function(result_table, digits = 4L) {
  if (is.null(result_table) || !is.data.frame(result_table)) {
    return(result_table)
  }

  rounded <- result_table
  numeric_columns <- vapply(rounded, is.numeric, logical(1))
  rounded[numeric_columns] <- lapply(rounded[numeric_columns], function(column) round(column, digits = digits))
  rounded
}

translation_efficiency_reorder_result_table <- function(result_table) {
  if (is.null(result_table) || !is.data.frame(result_table)) {
    return(result_table)
  }

  current_names <- colnames(result_table)
  priority_order <- c(
    "RNA_log2FC",
    "RNA_Expression_Status",
    "Ribo_log2FC",
    "Ribo_Expression_Status",
    "pvalue",
    "padj",
    "TE_log2FC"
  )

  required_columns <- c("pvalue", "padj", "TE_log2FC")
  if (!all(required_columns %in% current_names)) {
    return(result_table)
  }

  ordered_priority <- priority_order[priority_order %in% current_names]
  remaining_names <- setdiff(current_names, ordered_priority)
  te_mean_anchor <- match("TE_Treatment_Mean", remaining_names)

  if (is.na(te_mean_anchor)) {
    reordered_names <- c(remaining_names, ordered_priority)
  } else {
    trailing_names <- if (te_mean_anchor < length(remaining_names)) {
      remaining_names[(te_mean_anchor + 1L):length(remaining_names)]
    } else {
      character()
    }

    reordered_names <- c(
      remaining_names[seq_len(te_mean_anchor)],
      ordered_priority,
      trailing_names
    )
  }

  result_table[, reordered_names, drop = FALSE]
}

translation_efficiency_normalize_input_matrix <- function(gene_matrix) {
  stopifnot(is.data.frame(gene_matrix))

  normalized <- gene_matrix
  if (!"GeneID" %in% colnames(normalized) && ncol(normalized) >= 1L) {
    colnames(normalized)[1] <- "GeneID"
  }

  stopifnot("GeneID" %in% colnames(normalized))
  normalized
}

translation_efficiency_normalize_parameters <- function(
  te_tool = "Riborex",
  fvalue = 1.5,
  p_cutoff = 0.05,
  p_type = "Fdr"
) {
  tool_value <- if (is.null(te_tool) || !nzchar(as.character(te_tool))) "Riborex" else as.character(te_tool)
  fold_value <- suppressWarnings(as.numeric(fvalue))
  cutoff_value <- suppressWarnings(as.numeric(p_cutoff))
  p_type_value <- if (is.null(p_type) || !nzchar(as.character(p_type))) "Fdr" else as.character(p_type)

  if (!is.finite(fold_value) || fold_value < 1) {
    fold_value <- 1.5
  }

  if (!is.finite(cutoff_value) || cutoff_value <= 0) {
    cutoff_value <- 0.05
  }

  if (!identical(tolower(p_type_value), "rawpvalue")) {
    p_type_value <- "Fdr"
  } else {
    p_type_value <- "RawPvalue"
  }

  list(
    te_tool = tool_value,
    fvalue = fold_value,
    p_cutoff = cutoff_value,
    p_type = p_type_value
  )
}

translation_efficiency_preprocess_signature <- function(preprocess_context) {
  matrix_path <- preprocess_context$matrix_path
  if (is.character(matrix_path) && length(matrix_path) == 1L && nzchar(matrix_path)) {
    return(normalizePath(matrix_path, winslash = "/", mustWork = FALSE))
  }

  preview <- preprocess_context$preview
  if (!is.data.frame(preview)) {
    return("translation-efficiency::missing-preview")
  }

  paste(
    nrow(preview),
    ncol(preview),
    paste(colnames(preview), collapse = "|"),
    sep = "::"
  )
}

translation_efficiency_build_base_token <- function(te_tool) {
  paste0(
    "translation-efficiency-base-",
    tolower(gsub("[^a-z0-9]+", "-", as.character(te_tool))),
    "-",
    gsub("[^0-9]", "", format(Sys.time(), "%Y%m%d%H%M%OS3")),
    "-",
    sample.int(1000000, 1)
  )
}

translation_efficiency_load_gene_annotation <- function(gff_rda_path = NULL) {
  if (is.null(gff_rda_path) || !file.exists(gff_rda_path)) {
    return(NULL)
  }

  annotation_env <- new.env(parent = emptyenv())
  load(gff_rda_path, envir = annotation_env)
  object_names <- ls(annotation_env)

  if (length(object_names) == 0L) {
    return(NULL)
  }

  annotation_object <- get(object_names[[1]], envir = annotation_env)
  annotation_df <- tryCatch(
    as.data.frame(annotation_object),
    error = function(...) NULL
  )

  if (is.null(annotation_df) || !all(c("gene_id", "gene_name") %in% colnames(annotation_df))) {
    return(NULL)
  }

  annotation_df <- annotation_df[, c("gene_id", "gene_name"), drop = FALSE]
  annotation_df$gene_id <- toupper(as.character(annotation_df$gene_id))
  annotation_df$gene_id <- gsub("\\.\\d+$", "", annotation_df$gene_id)
  annotation_df$gene_name <- as.character(annotation_df$gene_name)
  annotation_df <- annotation_df[!is.na(annotation_df$gene_id) & nzchar(annotation_df$gene_id), , drop = FALSE]
  annotation_df <- annotation_df[!duplicated(annotation_df$gene_id), , drop = FALSE]
  colnames(annotation_df) <- c("GeneID", "gene_name")
  rownames(annotation_df) <- NULL
  annotation_df
}

