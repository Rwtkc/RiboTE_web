translation_efficiency_compute_context <- function(
  preprocess_context,
  upload_context,
  te_tool = "Riborex",
  fvalue = 1.5,
  p_cutoff = 0.05,
  p_type = "Fdr"
) {
  base_timing <- system.time({
    base_context <- translation_efficiency_build_base_context(
      preprocess_context = preprocess_context,
      upload_context = upload_context,
      te_tool = te_tool
    )
  })

  apply_timing <- system.time({
    te_context <- translation_efficiency_apply_parameters(
      base_context = base_context,
      fvalue = fvalue,
      p_cutoff = p_cutoff,
      p_type = p_type
    )
  })

  translation_efficiency_attach_performance(
    te_context = te_context,
    base_context = base_context,
    cache_hit = FALSE,
    base_elapsed_sec = base_timing[["elapsed"]],
    apply_elapsed_sec = apply_timing[["elapsed"]]
  )
}

translation_efficiency_results_payload <- function(
  te_context,
  page = 1L,
  page_size = 10L,
  page_input_id = NULL,
  search_query = "",
  search_input_id = NULL,
  active_view = "data"
) {
  preview <- te_context$result_table
  stopifnot(is.data.frame(preview))

  filtered_preview <- preview
  search_index <- te_context$search_index
  if (is.null(search_index) || length(search_index) != nrow(preview)) {
    search_index <- translation_efficiency_search_index(preview)
  }

  if (is.null(search_query)) {
    search_query <- ""
  }

  normalized_query <- trimws(as.character(search_query))
  normalized_query_lower <- tolower(normalized_query)

  if (nzchar(normalized_query_lower)) {
    row_matches <- grepl(normalized_query_lower, search_index, fixed = TRUE)
    filtered_preview <- preview[row_matches, , drop = FALSE]
  }

  total_rows <- nrow(filtered_preview)
  page_size <- max(as.integer(page_size), 1L)
  page_count <- max(ceiling(total_rows / page_size), 1L)
  page <- min(max(as.integer(page), 1L), page_count)

  if (total_rows > 0) {
    start_row <- ((page - 1L) * page_size) + 1L
    end_row <- min(start_row + page_size - 1L, total_rows)
    page_slice <- filtered_preview[start_row:end_row, , drop = FALSE]
  } else {
    page_slice <- filtered_preview[0, , drop = FALSE]
    page <- 1L
  }

  charts <- list(
    volcano = list(),
    status = list(),
    scatterInput = list(),
    scatter = list(),
    scatterTeExpression = list(),
    scatterTe = list(),
    displayMeta = list()
  )

  if (identical(active_view, "volcano")) {
    volcano_payload <- translation_efficiency_chart_display_payload(
      data_frame = te_context$volcano_points,
      x_column = "log2FoldChange",
      y_column = "significance",
      status_column = "diffTE"
    )
    charts$volcano <- volcano_payload$rows
    charts$displayMeta$volcano <- volcano_payload$meta
  } else if (identical(active_view, "scatter")) {
    scatter_input_payload <- translation_efficiency_chart_display_payload(
      data_frame = te_context$scatter_input,
      x_column = "input1",
      y_column = "input2",
      status_column = "diffExp",
      scale_type = "log2"
    )
    scatter_rpf_payload <- translation_efficiency_chart_display_payload(
      data_frame = te_context$scatter_points,
      x_column = "log2InputFC",
      y_column = "log2RPFFC",
      status_column = "diffTE"
    )
    scatter_te_expression_payload <- translation_efficiency_chart_display_payload(
      data_frame = te_context$scatter_te_expression,
      x_column = "TE_A1",
      y_column = "TE_A2",
      status_column = "diffTE",
      scale_type = "log2"
    )
    scatter_te_payload <- translation_efficiency_chart_display_payload(
      data_frame = te_context$scatter_te,
      x_column = "log2InputFC",
      y_column = "log2FoldChange",
      status_column = "diffTE"
    )

    charts$scatterInput <- scatter_input_payload$rows
    charts$scatter <- scatter_rpf_payload$rows
    charts$scatterTeExpression <- scatter_te_expression_payload$rows
    charts$scatterTe <- scatter_te_payload$rows
    charts$displayMeta$scatterInput <- scatter_input_payload$meta
    charts$displayMeta$scatter <- scatter_rpf_payload$meta
    charts$displayMeta$scatterTeExpression <- scatter_te_expression_payload$meta
    charts$displayMeta$scatterTe <- scatter_te_payload$meta
  }

  fvalue <- suppressWarnings(as.numeric(te_context$parameters$fvalue))
  p_cutoff <- suppressWarnings(as.numeric(te_context$parameters$p_cutoff))
  if (!is.finite(fvalue) || fvalue < 1) {
    fvalue <- 1
  }
  if (!is.finite(p_cutoff) || p_cutoff <= 0) {
    p_cutoff <- 0.05
  }

  list(
    table = list(
      columns = as.list(colnames(preview)),
      rows = translation_efficiency_rows_payload(page_slice),
      page = page,
      pageCount = page_count,
      pageSize = page_size,
      totalRows = total_rows,
      pageInputId = page_input_id,
      searchQuery = normalized_query,
      searchInputId = search_input_id
    ),
    thresholds = list(
      foldChange = log2(fvalue),
      significance = -log10(max(p_cutoff, 1e-300))
    ),
    charts = charts
  )
}

translation_efficiency_module_config <- function() {
  ribote_build_module_config(
    key = "translation_efficiency",
    title = "Translation Efficiency",
    eyebrow = "Differential TE Estimation",
    description = "Estimate Translation Efficiency differences between Control and Treatment samples, then review the result table, volcano plot, and expression comparisons.",
    requires = "upload",
    run_label = "Run TE Analysis",
    sections = list(
      list(
        title = "TE Controls",
        fields = list(
          ribote_field("te_tool", "TE Tool", "select", "Riborex", options = list(ribote_choice("Riborex"), ribote_choice("Xtail"))),
          ribote_field("fvalue", "Fold Change", "number", 1.5, min = 1, step = 0.1),
          ribote_field("p_cutoff", "P-value", "number", 0.05, min = 0, step = 0.01),
          ribote_field("p_type", "P-value Type", "select", "Fdr", options = list(ribote_choice("padj", "Fdr"), ribote_choice("pvalue", "RawPvalue")))
        )
      )
    ),
    views = list(
      list(id = "data", title = "Data"),
      list(id = "volcano", title = "TE Volcano Plot"),
      list(id = "scatter", title = "TE Scatter Plots")
    ),
    result_metrics = list(
      list(label = "Methods", value = "Riborex / Xtail"),
      list(label = "Result Views", value = "3"),
      list(label = "Downstream Analyses", value = "Available after TE")
    ),
    empty_message = "",
    success_message = "Translation Efficiency results are ready, and downstream analyses are now available.",
    marks_te_ready = TRUE,
    wide_sidebar = TRUE,
    show_export_panel = TRUE,
    show_analysis_panel = TRUE,
    snapshot_columns = 4
  )
}
