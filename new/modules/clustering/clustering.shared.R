clustering_export_defaults <- function() {
  list(
    format = "png",
    width = 2800,
    height = 1800,
    dpi = 300,
    scope = "main",
    data_format = "csv"
  )
}

clustering_export_scope_options <- function() {
  list(
    list(label = "Main Heatmap", value = "main"),
    list(label = "Detail Heatmap", value = "detail"),
    list(label = "All Available", value = "all")
  )
}

clustering_export_config <- function(id) {
  ns <- NS(id)
  defaults <- clustering_export_defaults()

  list(
    ids = list(
      scope = ns("export_scope"),
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
      scope = clustering_export_scope_options(),
      format = list(
        list(label = "PNG", value = "png"),
        list(label = "PDF", value = "pdf")
      ),
      dataFormat = list(
        list(label = "CSV", value = "csv"),
        list(label = "TXT (tab-delimited)", value = "txt")
      )
    ),
    sectioned = TRUE
  )
}

clustering_normalize_export_scope <- function(scope = NULL) {
  normalized_scope <- if (is.null(scope) || !nzchar(as.character(scope))) "main" else as.character(scope)

  if (!normalized_scope %in% c("main", "detail", "all")) {
    normalized_scope <- "main"
  }

  normalized_scope
}

clustering_export_settings <- function(input) {
  defaults <- clustering_export_defaults()
  width <- suppressWarnings(as.numeric(input$export_width))
  height <- suppressWarnings(as.numeric(input$export_height))
  dpi <- suppressWarnings(as.numeric(input$export_dpi))
  scope <- if (is.null(input$export_scope) || identical(input$export_scope, "")) defaults$scope else input$export_scope
  format <- tolower(if (is.null(input$export_format) || identical(input$export_format, "")) defaults$format else input$export_format)
  data_format <- tolower(if (is.null(input$data_export_format) || identical(input$data_export_format, "")) defaults$data_format else input$data_export_format)

  list(
    scope = clustering_normalize_export_scope(scope),
    format = if (format %in% c("png", "pdf")) format else defaults$format,
    width = if (is.finite(width) && width > 0) width else defaults$width,
    height = if (is.finite(height) && height > 0) height else defaults$height,
    dpi = if (is.finite(dpi) && dpi > 0) dpi else defaults$dpi,
    data_format = if (data_format %in% c("csv", "txt")) data_format else defaults$data_format
  )
}

clustering_data_space_options <- function() {
  pca_data_space_options()
}

clustering_distance_options <- function() {
  list(
    ribote_choice("Pearson"),
    ribote_choice("Euclidean"),
    ribote_choice("Absolute_Pearson")
  )
}

clustering_linkage_options <- function() {
  list(
    ribote_choice("average"),
    ribote_choice("complete"),
    ribote_choice("single"),
    ribote_choice("median"),
    ribote_choice("centroid"),
    ribote_choice("mcquitty")
  )
}

clustering_detail_mode_options <- function() {
  list(
    ribote_choice("Select Area", "area"),
    ribote_choice("Gene IDs", "genes")
  )
}

clustering_detail_mode_label <- function(detail_mode) {
  switch(
    as.character(detail_mode),
    area = "Select Area",
    genes = "Gene IDs",
    as.character(detail_mode)
  )
}

clustering_normalize_parameters <- function(
  data_space = NULL,
  top_genes = NULL,
  distance_method = NULL,
  linkage = NULL,
  color_series = NULL,
  zscore_max = NULL,
  gene_centricity = NULL,
  detail_mode = NULL,
  detail_gene_ids = NULL
) {
  normalized_data_space <- pca_normalize_parameters(data_space = data_space, method = "PCA")$data_space
  top_genes_value <- suppressWarnings(as.integer(top_genes))
  zscore_value <- suppressWarnings(as.numeric(zscore_max))
  normalized_distance <- if (is.null(distance_method) || !nzchar(as.character(distance_method))) {
    "Pearson"
  } else {
    as.character(distance_method)
  }
  normalized_linkage <- if (is.null(linkage) || !nzchar(as.character(linkage))) {
    "average"
  } else {
    as.character(linkage)
  }
  normalized_color_series <- "Blue-White-Red"
  normalized_detail_mode <- if (is.null(detail_mode) || !nzchar(as.character(detail_mode))) {
    "area"
  } else {
    as.character(detail_mode)
  }

  if (!normalized_distance %in% c("Pearson", "Euclidean", "Absolute_Pearson")) {
    normalized_distance <- "Pearson"
  }

  if (!normalized_linkage %in% c("average", "complete", "single", "median", "centroid", "mcquitty")) {
    normalized_linkage <- "average"
  }

  if (!normalized_detail_mode %in% c("area", "genes")) {
    normalized_detail_mode <- "area"
  }

  list(
    data_space = normalized_data_space,
    top_genes = if (length(top_genes_value) == 1L && is.finite(top_genes_value) && top_genes_value >= 10L) top_genes_value else 2000L,
    distance_method = normalized_distance,
    linkage = normalized_linkage,
    color_series = normalized_color_series,
    zscore_max = if (length(zscore_value) == 1L && is.finite(zscore_value) && zscore_value > 0) zscore_value else 3,
    gene_centricity = isTRUE(gene_centricity),
    detail_mode = normalized_detail_mode,
    detail_gene_ids = if (is.null(detail_gene_ids)) "" else as.character(detail_gene_ids)
  )
}

clustering_parameters_key <- function(parameters) {
  paste(
    parameters$data_space,
    parameters$top_genes,
    parameters$distance_method,
    parameters$linkage,
    parameters$zscore_max,
    if (isTRUE(parameters$gene_centricity)) "centered" else "raw",
    sep = "::"
  )
}

clustering_palette_values <- function(color_series) {
  switch(
    as.character(color_series),
    "Green-Black-Red" = c("#2f6f58", "#101010", "#c23b35"),
    "Red-Black-Green" = c("#c23b35", "#101010", "#2f6f58"),
    "Blue-White-Red" = c("#4b74b6", "#ffffff", "#c23b35"),
    "Green-Black-Magenta" = c("#2f6f58", "#101010", "#af4f9a"),
    "Blue-Yellow-Red" = c("#4b74b6", "#f1d76a", "#c23b35"),
    "Blue-White-Brown" = c("#4b74b6", "#ffffff", "#8a6246"),
    "Orange-White-Blue" = c("#d89043", "#ffffff", "#4b74b6"),
    c("#4b74b6", "#ffffff", "#c23b35")
  )
}

clustering_space_label <- function(data_space) {
  pca_data_space_label(data_space)
}

clustering_base_signature <- function(te_context, preprocess_context) {
  pca_base_signature(te_context, preprocess_context)
}

clustering_source_signature <- function(te_context, preprocess_context) {
  pca_source_signature(te_context, preprocess_context)
}

clustering_select_top_rows <- function(feature_matrix, top_genes) {
  if (!is.matrix(feature_matrix) || nrow(feature_matrix) == 0 || ncol(feature_matrix) == 0) {
    stop("No clustering feature matrix is available.", call. = FALSE)
  }

  gene_count <- min(max(10L, as.integer(top_genes)), nrow(feature_matrix))
  row_sd <- apply(feature_matrix, 1, stats::sd, na.rm = TRUE)
  row_sd[!is.finite(row_sd)] <- -Inf
  ranked_rows <- order(row_sd, decreasing = TRUE)
  feature_matrix[ranked_rows[seq_len(gene_count)], , drop = FALSE]
}

clustering_center_genes <- function(feature_matrix) {
  feature_matrix - rowMeans(feature_matrix, na.rm = TRUE)
}

clustering_clip_matrix <- function(feature_matrix, zscore_max) {
  clipped <- feature_matrix
  finite_values <- as.numeric(clipped[is.finite(clipped)])

  if (!length(finite_values)) {
    return(clipped)
  }

  global_median <- stats::median(finite_values)
  global_sd <- stats::sd(finite_values)

  if (!is.finite(global_sd) || global_sd <= 0) {
    return(clipped)
  }

  upper_cutoff <- global_median + zscore_max * global_sd
  lower_cutoff <- global_median - zscore_max * global_sd
  clipped[clipped > upper_cutoff] <- upper_cutoff
  clipped[clipped < lower_cutoff] <- lower_cutoff
  clipped
}

clustering_distance_object <- function(feature_matrix, distance_method = "Pearson", axis = c("rows", "columns")) {
  axis <- match.arg(axis)
  matrix_data <- if (identical(axis, "columns")) t(feature_matrix) else feature_matrix

  if (!is.matrix(matrix_data) || nrow(matrix_data) < 2L) {
    return(NULL)
  }

  if (identical(distance_method, "Euclidean")) {
    return(stats::dist(matrix_data))
  }

  correlation <- suppressWarnings(stats::cor(t(matrix_data), method = "pearson", use = "pairwise.complete.obs"))
  correlation[!is.finite(correlation)] <- 0
  diag(correlation) <- 1
  distance_matrix <- if (identical(distance_method, "Absolute_Pearson")) {
    1 - abs(correlation)
  } else {
    1 - correlation
  }
  distance_matrix[!is.finite(distance_matrix)] <- 1
  distance_matrix[distance_matrix < 0] <- 0
  stats::as.dist(distance_matrix)
}

clustering_hclust_order <- function(feature_matrix, distance_method, linkage, axis = c("rows", "columns")) {
  axis <- match.arg(axis)
  entity_count <- if (identical(axis, "columns")) ncol(feature_matrix) else nrow(feature_matrix)

  if (!is.numeric(entity_count) || entity_count <= 1L) {
    return(seq_len(max(1L, entity_count)))
  }

  distance_object <- clustering_distance_object(feature_matrix, distance_method = distance_method, axis = axis)

  if (is.null(distance_object)) {
    return(seq_len(entity_count))
  }

  stats::hclust(distance_object, method = linkage)$order
}

clustering_build_analysis_context <- function(base_context, parameters, source_signature = NULL) {
  space_context <- base_context$spaces[[parameters$data_space]]

  if (is.null(space_context)) {
    stop(sprintf("Unsupported clustering data space: %s", parameters$data_space), call. = FALSE)
  }

  feature_matrix <- clustering_select_top_rows(space_context$feature_matrix, parameters$top_genes)

  if (isTRUE(parameters$gene_centricity)) {
    feature_matrix <- clustering_center_genes(feature_matrix)
  }

  clipped_matrix <- clustering_clip_matrix(feature_matrix, parameters$zscore_max)
  row_order <- clustering_hclust_order(clipped_matrix, parameters$distance_method, parameters$linkage, axis = "rows")
  column_order <- clustering_hclust_order(clipped_matrix, parameters$distance_method, parameters$linkage, axis = "columns")

  list(
    base_signature = base_context$base_signature,
    source_signature = if (is.null(source_signature)) base_context$source_signature else source_signature,
    parameters = parameters,
    space_label = clustering_space_label(parameters$data_space),
    ordered_matrix = clipped_matrix[row_order, column_order, drop = FALSE],
    sample_map = space_context$sample_map[column_order, , drop = FALSE],
    palette = clustering_palette_values(parameters$color_series)
  )
}

clustering_parse_gene_ids <- function(gene_ids) {
  if (is.null(gene_ids) || !nzchar(as.character(gene_ids))) {
    return(character())
  }

  parsed <- unlist(strsplit(as.character(gene_ids), "[,，\\s]+", perl = TRUE), use.names = FALSE)
  unique(stats::na.omit(trimws(parsed[nzchar(trimws(parsed))])))
}

clustering_normalize_selection <- function(selection, ordered_matrix) {
  if (is.null(selection) || !length(selection) || !is.matrix(ordered_matrix)) {
    return(NULL)
  }

  row_start <- suppressWarnings(as.integer(selection$rowStart))
  row_end <- suppressWarnings(as.integer(selection$rowEnd))
  col_start <- suppressWarnings(as.integer(selection$colStart))
  col_end <- suppressWarnings(as.integer(selection$colEnd))

  if (!all(is.finite(c(row_start, row_end, col_start, col_end)))) {
    return(NULL)
  }

  row_start <- max(1L, min(nrow(ordered_matrix), row_start))
  row_end <- max(1L, min(nrow(ordered_matrix), row_end))
  col_start <- max(1L, min(ncol(ordered_matrix), col_start))
  col_end <- max(1L, min(ncol(ordered_matrix), col_end))

  list(
    row_start = min(row_start, row_end),
    row_end = max(row_start, row_end),
    col_start = min(col_start, col_end),
    col_end = max(col_start, col_end)
  )
}

clustering_empty_detail_context <- function(detail_mode, empty_message) {
  list(
    mode = detail_mode,
    mode_label = clustering_detail_mode_label(detail_mode),
    matrix = NULL,
    sample_map = NULL,
    summary = NULL,
    empty_message = empty_message,
    show_row_labels = FALSE
  )
}

clustering_extract_detail_context <- function(clustering_context, detail_mode = "area", detail_gene_ids = NULL, selection = NULL) {
  ordered_matrix <- clustering_context$ordered_matrix
  sample_map <- clustering_context$sample_map

  if (identical(detail_mode, "genes")) {
    requested_genes <- clustering_parse_gene_ids(detail_gene_ids)

    if (!length(requested_genes)) {
      return(clustering_empty_detail_context("genes", "Enter one or more Gene IDs to build the detail heatmap."))
    }

    matched_rows <- which(rownames(ordered_matrix) %in% requested_genes)

    if (!length(matched_rows)) {
      return(clustering_empty_detail_context("genes", "None of the requested Gene IDs are present in the clustered matrix."))
    }

    detail_matrix <- ordered_matrix[matched_rows, , drop = FALSE]
    return(list(
      mode = "genes",
      mode_label = clustering_detail_mode_label("genes"),
      matrix = detail_matrix,
      sample_map = sample_map,
      summary = sprintf("%s matched genes x %s samples", format(nrow(detail_matrix), big.mark = ","), ncol(detail_matrix)),
      empty_message = NULL,
      show_row_labels = nrow(detail_matrix) <= 80L
    ))
  }

  normalized_selection <- clustering_normalize_selection(selection, ordered_matrix)

  if (is.null(normalized_selection)) {
    return(clustering_empty_detail_context("area", "Drag across the main heatmap to stage a detail view."))
  }

  row_index <- seq.int(normalized_selection$row_start, normalized_selection$row_end)
  column_index <- seq.int(normalized_selection$col_start, normalized_selection$col_end)
  detail_matrix <- ordered_matrix[row_index, column_index, drop = FALSE]

  if (!nrow(detail_matrix) || !ncol(detail_matrix)) {
    return(clustering_empty_detail_context("area", "The selected heatmap area did not contain any visible cells."))
  }

  list(
    mode = "area",
    mode_label = clustering_detail_mode_label("area"),
    matrix = detail_matrix,
    sample_map = sample_map[column_index, , drop = FALSE],
    summary = sprintf("%s selected genes x %s samples", format(nrow(detail_matrix), big.mark = ","), ncol(detail_matrix)),
    empty_message = NULL,
    show_row_labels = nrow(detail_matrix) <= 80L
  )
}

clustering_heatmap_columns_payload <- function(sample_map) {
  if (is.null(sample_map) || !nrow(sample_map)) {
    return(list())
  }

  lapply(seq_len(nrow(sample_map)), function(index) {
    row_values <- sample_map[index, , drop = FALSE]
    list(
      displaySample = as.character(row_values$display_sample[[1]]),
      actualSample = as.character(row_values$actual_sample[[1]]),
      actualRna = as.character(row_values$actual_rna[[1]]),
      actualRibo = as.character(row_values$actual_ribo[[1]]),
      group = as.character(row_values$group[[1]])
    )
  })
}

clustering_heatmap_matrix_payload <- function(matrix_data) {
  if (!is.matrix(matrix_data) || !nrow(matrix_data) || !ncol(matrix_data)) {
    return(list())
  }

  lapply(seq_len(nrow(matrix_data)), function(index) {
    as.list(unname(as.numeric(matrix_data[index, ])))
  })
}

clustering_heatmap_signature <- function(signature_seed, matrix_data = NULL, sample_map = NULL, empty_message = NULL) {
  if (is.null(matrix_data) || !is.matrix(matrix_data) || !nrow(matrix_data) || !ncol(matrix_data)) {
    return(paste(signature_seed, "empty", as.character(empty_message), sep = "::"))
  }

  row_labels <- rownames(matrix_data)
  column_labels <- colnames(matrix_data)
  row_anchor <- paste(row_labels[c(1L, max(1L, floor(length(row_labels) / 2)), length(row_labels))], collapse = "|")
  column_anchor <- paste(column_labels, collapse = "|")
  value_anchor <- paste(round(c(
    matrix_data[[1, 1]],
    matrix_data[[nrow(matrix_data), ncol(matrix_data)]]
  ), digits = 4L), collapse = "|")

  paste(
    signature_seed,
    nrow(matrix_data),
    ncol(matrix_data),
    row_anchor,
    column_anchor,
    value_anchor,
    sep = "::"
  )
}

clustering_heatmap_payload <- function(matrix_data, sample_map, title, palette, show_row_labels = FALSE, brush_enabled = FALSE, signature = NULL) {
  list(
    title = title,
    subtitle = sprintf("%s genes x %s samples", format(nrow(matrix_data), big.mark = ","), ncol(matrix_data)),
    signature = if (is.null(signature)) "" else as.character(signature),
    palette = as.list(palette),
    rowLabels = as.list(rownames(matrix_data)),
    columns = clustering_heatmap_columns_payload(sample_map),
    matrix = clustering_heatmap_matrix_payload(matrix_data),
    showRowLabels = isTRUE(show_row_labels),
    brushEnabled = isTRUE(brush_enabled)
  )
}

clustering_results_payload <- function(clustering_context, detail_context, selection_input_id) {
  main_signature_seed <- paste(
    clustering_context$source_signature,
    clustering_parameters_key(clustering_context$parameters),
    clustering_context$parameters$color_series,
    sep = "::"
  )

  payload <- list(
    ids = list(selectionInputId = selection_input_id),
    main = clustering_heatmap_payload(
      matrix_data = clustering_context$ordered_matrix,
      sample_map = clustering_context$sample_map,
      title = sprintf("Clustered %s Heatmap", clustering_context$space_label),
      palette = clustering_context$palette,
      show_row_labels = FALSE,
      brush_enabled = identical(detail_context$mode, "area"),
      signature = clustering_heatmap_signature(
        signature_seed = main_signature_seed,
        matrix_data = clustering_context$ordered_matrix,
        sample_map = clustering_context$sample_map
      )
    ),
    detailMode = detail_context$mode,
    detailModeLabel = detail_context$mode_label,
    detailSummary = detail_context$summary,
    detailEmptyMessage = detail_context$empty_message
  )

  if (!is.null(detail_context$matrix) && is.matrix(detail_context$matrix) && nrow(detail_context$matrix) && ncol(detail_context$matrix)) {
    payload$detail <- clustering_heatmap_payload(
      matrix_data = detail_context$matrix,
      sample_map = detail_context$sample_map,
      title = "Detail Heatmap",
      palette = clustering_context$palette,
      show_row_labels = detail_context$show_row_labels,
      brush_enabled = FALSE,
      signature = clustering_heatmap_signature(
        signature_seed = paste(main_signature_seed, detail_context$mode, detail_context$summary, sep = "::"),
        matrix_data = detail_context$matrix,
        sample_map = detail_context$sample_map
      )
    )
  } else {
    payload$detail <- list(
      title = "Detail Heatmap",
      subtitle = "",
      signature = clustering_heatmap_signature(
        signature_seed = paste(main_signature_seed, detail_context$mode, "empty", sep = "::"),
        matrix_data = NULL,
        sample_map = NULL,
        empty_message = detail_context$empty_message
      ),
      palette = as.list(clustering_context$palette),
      rowLabels = list(),
      columns = list(),
      matrix = list(),
      showRowLabels = FALSE,
      brushEnabled = FALSE
    )
  }

  payload
}

clustering_export_table <- function(kind = c("main", "detail"), clustering_context, detail_context = NULL) {
  kind <- match.arg(kind)
  matrix_data <- if (identical(kind, "detail")) detail_context$matrix else clustering_context$ordered_matrix
  sample_map <- if (identical(kind, "detail")) detail_context$sample_map else clustering_context$sample_map

  if (is.null(matrix_data) || !is.matrix(matrix_data) || !nrow(matrix_data) || !ncol(matrix_data)) {
    return(data.frame())
  }

  rows <- lapply(seq_len(ncol(matrix_data)), function(index) {
    data.frame(
      GeneID = rownames(matrix_data),
      `Display Sample` = sample_map$display_sample[[index]],
      `Actual Sample` = sample_map$actual_sample[[index]],
      `Actual RNA` = sample_map$actual_rna[[index]],
      `Actual Ribo` = sample_map$actual_ribo[[index]],
      Group = sample_map$group[[index]],
      Value = as.numeric(matrix_data[, index]),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}

clustering_table_export_content <- function(data_frame, format = "csv") {
  pca_table_export_content(data_frame, format = format)
}

clustering_round_export_table <- function(data_frame, digits = 4L) {
  rounded <- data_frame

  if ("Value" %in% colnames(rounded) && is.numeric(rounded$Value)) {
    rounded$Value <- round(rounded$Value, digits = digits)
  }

  rounded
}

clustering_export_content <- function(kind = c("main", "detail"), clustering_context, detail_context = NULL, format = "csv") {
  kind <- match.arg(kind)
  export_table <- clustering_export_table(kind = kind, clustering_context = clustering_context, detail_context = detail_context)
  clustering_table_export_content(clustering_round_export_table(export_table), format = format)
}

clustering_figure_filename <- function(data_space, kind = c("main", "detail"), extension = "png") {
  kind <- match.arg(kind)
  safe_space <- gsub("[^a-z0-9]+", "_", tolower(as.character(data_space)))
  safe_kind <- gsub("[^a-z0-9]+", "_", tolower(as.character(kind)))
  sprintf("clustering_%s_%s_%s.%s", safe_space, safe_kind, format(Sys.Date(), "%Y%m%d"), extension)
}

clustering_data_filename <- function(data_space, kind = c("main", "detail"), extension = "csv") {
  kind <- match.arg(kind)
  safe_space <- gsub("[^a-z0-9]+", "_", tolower(as.character(data_space)))
  safe_kind <- gsub("[^a-z0-9]+", "_", tolower(as.character(kind)))
  sprintf("clustering_%s_%s_data_%s.%s", safe_space, safe_kind, format(Sys.Date(), "%Y%m%d"), extension)
}

clustering_archive_filename <- function(kind = "figure") {
  safe_kind <- gsub("[^a-z0-9]+", "_", tolower(as.character(kind)))
  sprintf("clustering_%s_%s.zip", safe_kind, format(Sys.Date(), "%Y%m%d"))
}

clustering_result_metrics <- function(clustering_context, detail_context) {
  list(
    list(label = "Genes Clustered", value = format(nrow(clustering_context$ordered_matrix), big.mark = ",")),
    list(label = "Samples Clustered", value = ncol(clustering_context$ordered_matrix)),
    list(label = "Data Space", value = clustering_context$space_label),
    list(
      label = "Detail Genes",
      value = if (!is.null(detail_context$matrix) && is.matrix(detail_context$matrix)) {
        format(nrow(detail_context$matrix), big.mark = ",")
      } else {
        "0"
      }
    )
  )
}

clustering_module_config <- function() {
  ribote_build_module_config(
    key = "clustering",
    title = "Clustering",
    eyebrow = "Clustered Heatmap",
    description = "Group genes with similar patterns in TE ratio, RNA abundance, or Ribo abundance, and inspect selected regions in more detail.",
    requires = "te",
    run_label = "Run Clustering",
    sections = list(
      list(
        title = "Detail Heatmap",
        collapsible = TRUE,
        default_expanded = TRUE,
        fields = list(
          ribote_field("clustering_detail_mode", "Source", "select", "area", options = clustering_detail_mode_options()),
          ribote_field("clustering_detail_gene_ids", "Gene IDs", "text", "", placeholder = "Enter Gene IDs when Source = Gene IDs")
        )
      ),
      list(
        title = "Heatmap Data",
        collapsible = TRUE,
        default_expanded = FALSE,
        fields = list(
          ribote_field("clustering_data_space", "Data Space", "select", "TE", options = clustering_data_space_options()),
          ribote_field("clustering_top_genes", "Top Genes", "number", 2000, min = 10, step = 100),
          ribote_field("clustering_zscore_max", "Max Z Score", "number", 3, min = 1, step = 1),
          ribote_field("clustering_gene_centricity", "Gene Centricity (subtract mean)", "checkbox", TRUE)
        )
      ),
      list(
        title = "Clustering Settings",
        collapsible = TRUE,
        default_expanded = FALSE,
        fields = list(
          ribote_field("clustering_distance", "Distance", "select", "Pearson", options = clustering_distance_options()),
          ribote_field("clustering_linkage", "Linkage", "select", "average", options = clustering_linkage_options())
        )
      )
    ),
    wide_sidebar = TRUE,
    show_export_panel = TRUE,
    show_analysis_panel = TRUE,
    snapshot_columns = 2
  )
}
