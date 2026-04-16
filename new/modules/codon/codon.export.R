codon_table_to_delimited_text <- function(data_frame, format = "csv") {
  table_frame <- if (is.data.frame(data_frame)) data_frame else data.frame()

  if (!nrow(table_frame)) {
    return(paste(colnames(table_frame), collapse = if (identical(format, "txt")) "\t" else ","))
  }

  con <- textConnection("output", "w", local = TRUE)
  on.exit(close(con), add = TRUE)
  utils::write.table(
    table_frame,
    file = con,
    sep = if (identical(format, "txt")) "\t" else ",",
    row.names = FALSE,
    col.names = TRUE,
    quote = TRUE,
    na = ""
  )
  paste(output, collapse = "\n")
}

codon_export_rows_to_table <- function(rows, fallback = data.frame()) {
  if (!length(rows)) {
    return(fallback)
  }

  as.data.frame(
    do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors = FALSE)),
    stringsAsFactors = FALSE
  )
}

codon_collect_export_columns <- function(rows) {
  columns <- character()

  for (row in rows) {
    row_names <- names(row)
    new_columns <- setdiff(row_names, columns)
    if (length(new_columns)) {
      columns <- c(columns, new_columns)
    }
  }

  columns
}

codon_export_rows_to_delimited_text <- function(rows, format = "csv", preferred_columns = NULL) {
  row_list <- if (is.list(rows)) rows else list()
  columns <- if (!is.null(preferred_columns) && length(preferred_columns)) preferred_columns else codon_collect_export_columns(row_list)

  if (!length(columns)) {
    return("")
  }

  table_rows <- lapply(row_list, function(row) {
    complete_row <- setNames(as.list(rep("", length(columns))), columns)
    row_names <- names(row)
    for (column_name in intersect(columns, row_names)) {
      value <- row[[column_name]]
      if (is.null(value) || length(value) == 0L || (length(value) == 1L && is.na(value))) {
        complete_row[[column_name]] <- ""
      } else {
        complete_row[[column_name]] <- value[[1]]
      }
    }
    complete_row
  })

  codon_table_to_delimited_text(
    as.data.frame(do.call(rbind, lapply(table_rows, as.data.frame, stringsAsFactors = FALSE)), stringsAsFactors = FALSE),
    format = format
  )
}

codon_empty_plot_export <- function(view_config, view_id, format = "csv") {
  note <- if (!is.null(view_config$note) && nzchar(as.character(view_config$note))) {
    as.character(view_config$note)
  } else {
    "No plotted data are available for the selected Codon view."
  }

  codon_export_rows_to_delimited_text(
    list(list(view = view_id, note = note)),
    format = format,
    preferred_columns = c("view", "note")
  )
}

codon_export_view_config <- function(context, active_view) {
  switch(
    active_view,
    selected_codon_vs_rna = context$results$selectedCodonVsRna,
    cbi_associations = context$results$cbiAssociations,
    selected_codon_burden = context$results$selectedCodonBurden,
    permutation_support = context$results$permutationSupport,
    te_bias_selected_load = context$results$teBiasSelectedLoad,
    selected_load_effect = context$results$selectedLoadEffect,
    codon_clustering = context$results$codonClustering,
    codon_usage_heatmap = context$results$codonUsageHeatmap,
    codon_run_zscore = context$results$codonRunZscore,
    NULL
  )
}

codon_export_selected_codon_vs_rna_plot_data <- function(view_config, view_id, format = "csv") {
  panels <- if (is.list(view_config$panels)) view_config$panels else list()
  plot_rows <- list()

  for (panel in panels) {
    comparisons <- if (is.list(panel$comparisons)) panel$comparisons else list()

    for (comparison in comparisons) {
      points <- if (is.list(comparison$points)) comparison$points else list()

      for (point_index in seq_along(points)) {
        point <- points[[point_index]]
        plot_rows[[length(plot_rows) + 1L]] <- list(
          view = view_id,
          codon = panel$codon,
          condition = comparison$condition,
          pointIndex = point_index,
          geneId = point$geneId,
          geneName = point$geneName,
          teGroup = point$teGroup,
          codonUsagePercent = point$x,
          rnaAbundanceLog2MeanPlus1 = point$y,
          rawRnaMean = point$rawRna,
          genesMeasured = comparison$geneCount,
          displayedGenes = comparison$displayedGeneCount,
          pearsonR = comparison$correlation,
          pValue = comparison$pValue,
          slope = comparison$slope,
          intercept = comparison$intercept
        )
      }
    }
  }

  if (!length(plot_rows)) {
    return(codon_empty_plot_export(view_config, view_id, format = format))
  }

  codon_export_rows_to_delimited_text(
    plot_rows,
    format = format,
    preferred_columns = c(
      "view", "codon", "condition", "pointIndex", "geneId", "geneName", "teGroup",
      "codonUsagePercent", "rnaAbundanceLog2MeanPlus1", "rawRnaMean", "genesMeasured",
      "displayedGenes", "pearsonR", "pValue", "slope", "intercept"
    )
  )
}

codon_export_point_panel_plot_data <- function(view_config, view_id, format = "csv") {
  panels <- if (is.list(view_config$panels)) view_config$panels else list()
  plot_rows <- list()

  for (panel in panels) {
    points <- if (is.list(panel$points)) panel$points else list()

    for (point_index in seq_along(points)) {
      point <- points[[point_index]]
      plot_rows[[length(plot_rows) + 1L]] <- list(
        view = view_id,
        panelId = panel$panelId,
        association = panel$associationLabel,
        condition = panel$conditionLabel,
        pointIndex = point_index,
        geneId = point$geneId,
        geneName = point$geneName,
        teGroup = point$teGroup,
        xLabel = panel$xLabel,
        x = point$x,
        yLabel = panel$yLabel,
        y = point$y,
        rawYLabel = panel$rawYLabel,
        rawY = point$rawY,
        genesMeasured = panel$geneCount,
        displayedGenes = panel$displayedGeneCount,
        pearsonR = panel$correlation,
        pValue = panel$pValue,
        slope = panel$slope,
        intercept = panel$intercept
      )
    }
  }

  if (!length(plot_rows)) {
    return(codon_empty_plot_export(view_config, view_id, format = format))
  }

  codon_export_rows_to_delimited_text(
    plot_rows,
    format = format,
    preferred_columns = c(
      "view", "panelId", "association", "condition", "pointIndex", "geneId", "geneName",
      "teGroup", "xLabel", "x", "yLabel", "y", "rawYLabel", "rawY", "genesMeasured",
      "displayedGenes", "pearsonR", "pValue", "slope", "intercept"
    )
  )
}

codon_export_histogram_plot_data <- function(view_config, view_id, format = "csv") {
  panels <- if (is.list(view_config$panels)) view_config$panels else list()
  plot_rows <- list()

  for (panel in panels) {
    bins <- if (is.list(panel$bins)) panel$bins else list()

    for (bin_index in seq_along(bins)) {
      bin <- bins[[bin_index]]
      plot_rows[[length(plot_rows) + 1L]] <- list(
        view = view_id,
        panelId = panel$panelId,
        title = panel$title,
        xLabel = panel$xLabel,
        observedValue = panel$observedValue,
        binIndex = bin_index,
        x0 = bin$x0,
        x1 = bin$x1,
        count = bin$count
      )
    }
  }

  if (!length(plot_rows)) {
    return(codon_empty_plot_export(view_config, view_id, format = format))
  }

  codon_export_rows_to_delimited_text(
    plot_rows,
    format = format,
    preferred_columns = c("view", "panelId", "title", "xLabel", "observedValue", "binIndex", "x0", "x1", "count")
  )
}

codon_export_load_trend_plot_data <- function(view_config, view_id, format = "csv") {
  panels <- if (is.list(view_config$panels)) view_config$panels else list()
  plot_rows <- list()

  for (panel in panels) {
    bins <- if (is.list(panel$bins)) panel$bins else list()

    for (bin_index in seq_along(bins)) {
      bin <- bins[[bin_index]]
      fractions <- if (is.list(bin$fractions)) bin$fractions else list()

      for (fraction in fractions) {
        plot_rows[[length(plot_rows) + 1L]] <- list(
          view = view_id,
          panelId = panel$panelId,
          title = panel$title,
          xLabel = panel$xLabel,
          yLabel = panel$yLabel,
          binIndex = bin_index,
          label = bin$label,
          loadBin = bin$loadBin,
          loadRange = bin$loadRange,
          genes = bin$genes,
          medianLoad = bin$medianLoad,
          meanTeLog2Fc = bin$meanTeLog2Fc,
          group = fraction$group,
          fraction = fraction$value
        )
      }
    }
  }

  if (!length(plot_rows)) {
    return(codon_empty_plot_export(view_config, view_id, format = format))
  }

  codon_export_rows_to_delimited_text(
    plot_rows,
    format = format,
    preferred_columns = c(
      "view", "panelId", "title", "xLabel", "yLabel", "binIndex", "label", "loadBin",
      "loadRange", "genes", "medianLoad", "meanTeLog2Fc", "group", "fraction"
    )
  )
}

codon_export_heatmap_plot_data <- function(view_config, view_id, format = "csv") {
  panels <- if (is.list(view_config$panels)) view_config$panels else list()
  plot_rows <- list()

  for (panel in panels) {
    row_labels <- if (is.list(panel$rows)) panel$rows else list()
    columns <- if (is.list(panel$columns)) panel$columns else list()
    values <- if (is.list(panel$values)) panel$values else list()

    for (row_index in seq_along(row_labels)) {
      row_label <- row_labels[[row_index]]
      row_values <- if (length(values) >= row_index && is.list(values[[row_index]])) values[[row_index]] else list()

      for (column_index in seq_along(columns)) {
        column <- columns[[column_index]]
        cell_value <- if (length(row_values) >= column_index) row_values[[column_index]] else NULL
        plot_rows[[length(plot_rows) + 1L]] <- list(
          view = view_id,
          panelId = panel$panelId,
          title = panel$title,
          rowIndex = row_index,
          row = row_label,
          columnIndex = column_index,
          column = column$label,
          columnSelected = column$selected,
          value = cell_value,
          colorMax = panel$colorMax
        )
      }
    }
  }

  if (!length(plot_rows)) {
    return(codon_empty_plot_export(view_config, view_id, format = format))
  }

  codon_export_rows_to_delimited_text(
    plot_rows,
    format = format,
    preferred_columns = c(
      "view", "panelId", "title", "rowIndex", "row", "columnIndex",
      "column", "columnSelected", "value", "colorMax"
    )
  )
}

codon_plot_data_export_content <- function(context, active_view, format = "csv") {
  view_config <- codon_export_view_config(context, active_view)

  switch(
    active_view,
    selected_codon_vs_rna = codon_export_selected_codon_vs_rna_plot_data(view_config, active_view, format = format),
    cbi_associations = codon_export_point_panel_plot_data(view_config, active_view, format = format),
    selected_codon_burden = codon_export_point_panel_plot_data(view_config, active_view, format = format),
    permutation_support = codon_export_histogram_plot_data(view_config, active_view, format = format),
    te_bias_selected_load = codon_export_load_trend_plot_data(view_config, active_view, format = format),
    selected_load_effect = codon_export_point_panel_plot_data(view_config, active_view, format = format),
    codon_clustering = codon_export_heatmap_plot_data(view_config, active_view, format = format),
    codon_usage_heatmap = codon_export_heatmap_plot_data(view_config, active_view, format = format),
    codon_run_zscore = codon_export_point_panel_plot_data(view_config, active_view, format = format),
    codon_empty_plot_export(view_config, active_view, format = format)
  )
}

codon_export_tables <- function(context) {
  stopifnot(is.list(context))

  empty_input_summary <- data.frame(
    GeneID = character(),
    Gene_Name = character(),
    TE_Group = character(),
    RNA_Control_Mean = numeric(),
    RNA_Treatment_Mean = numeric(),
    Total_Codons = numeric(),
    Selected_Codon_Count = numeric(),
    Selected_Codon_per_1k = numeric(),
    Selected_Codon_Frequency = numeric(),
    stringsAsFactors = FALSE
  )

  list(
    input_summary = if (!is.null(context$scope_context)) codon_input_summary_table(context$scope_context) else empty_input_summary,
    selected_codon_usage = codon_export_rows_to_table(
      context$results$selectedCodonUsage$rows,
      fallback = data.frame(
        Codon = character(),
        Genes_Measured = numeric(),
        Up_Median_Percent = numeric(),
        Non_Median_Percent = numeric(),
        Down_Median_Percent = numeric(),
        Up_vs_Non_PValue = numeric(),
        Down_vs_Non_PValue = numeric(),
        stringsAsFactors = FALSE
      )
    ),
    cbi_tai_by_group = codon_export_rows_to_table(
      context$results$cbiTaiByGroup$rows,
      fallback = data.frame(
        Metric = character(),
        Genes_Measured = numeric(),
        Up_Median = numeric(),
        Non_Median = numeric(),
        Down_Median = numeric(),
        Up_vs_Non_PValue = numeric(),
        Down_vs_Non_PValue = numeric(),
        stringsAsFactors = FALSE
      )
    ),
    codon_enrichment_shifted = codon_export_rows_to_table(
      context$results$codonEnrichmentShifted$rows,
      fallback = data.frame(
        codon = character(),
        Up_Genes = numeric(),
        Non_Genes = numeric(),
        Down_Genes = numeric(),
        Shifted_Genes = numeric(),
        Genes_With_Hits = numeric(),
        Log2_Up_vs_Down = numeric(),
        Shifted_Fraction = numeric(),
        Selected = logical(),
        stringsAsFactors = FALSE
      )
    ),
    selected_codon_across_groups = codon_export_rows_to_table(
      context$results$selectedCodonAcrossGroups$rows,
      fallback = data.frame(
        Metric = character(),
        Genes_Measured = numeric(),
        Up_Median = numeric(),
        Non_Median = numeric(),
        Down_Median = numeric(),
        Up_vs_Non_PValue = numeric(),
        Down_vs_Non_PValue = numeric(),
        stringsAsFactors = FALSE
      )
    ),
    codon_run_enrichment = codon_export_rows_to_table(
      context$results$codonRunEnrichment$rows,
      fallback = data.frame(
        Metric = character(),
        Genes_Measured = numeric(),
        Up_Median = numeric(),
        Non_Median = numeric(),
        Down_Median = numeric(),
        Up_vs_Non_PValue = numeric(),
        Down_vs_Non_PValue = numeric(),
        stringsAsFactors = FALSE
      )
    )
  )
}

codon_data_export_entries <- function(context, format = "csv", view = "input_summary") {
  extension <- if (identical(format, "txt")) "txt" else "csv"
  tables <- codon_export_tables(context)
  active_view <- codon_normalize_result_view(view)

  if (identical(active_view, "input_summary")) {
    return(list(list(
      filename = codon_data_filename("input_summary", extension = extension),
      content = codon_table_to_delimited_text(tables$input_summary, format = format)
    )))
  }

  if (identical(active_view, "selected_codon_usage")) {
    return(list(list(
      filename = codon_data_filename("selected_codon_usage_summary", extension = extension),
      content = codon_table_to_delimited_text(tables$selected_codon_usage, format = format)
    )))
  }

  if (identical(active_view, "selected_codon_vs_rna")) {
    return(list(list(
      filename = codon_data_filename("selected_codon_vs_rna_plot_data", extension = extension),
      content = codon_plot_data_export_content(context, active_view, format = format)
    )))
  }

  if (identical(active_view, "cbi_tai_by_group")) {
    return(list(list(
      filename = codon_data_filename("codon_bias_and_adaptation_by_te_group", extension = extension),
      content = codon_table_to_delimited_text(tables$cbi_tai_by_group, format = format)
    )))
  }

  if (identical(active_view, "cbi_associations")) {
    return(list(list(
      filename = codon_data_filename("cbi_associations_plot_data", extension = extension),
      content = codon_plot_data_export_content(context, active_view, format = format)
    )))
  }

  if (identical(active_view, "selected_codon_burden")) {
    return(list(list(
      filename = codon_data_filename("selected_codon_burden_plot_data", extension = extension),
      content = codon_plot_data_export_content(context, active_view, format = format)
    )))
  }

  if (identical(active_view, "codon_enrichment_shifted")) {
    return(list(list(
      filename = codon_data_filename("codon_enrichment_shifted", extension = extension),
      content = codon_table_to_delimited_text(tables$codon_enrichment_shifted, format = format)
    )))
  }

  if (identical(active_view, "selected_codon_across_groups")) {
    return(list(list(
      filename = codon_data_filename("selected_codon_across_groups", extension = extension),
      content = codon_table_to_delimited_text(tables$selected_codon_across_groups, format = format)
    )))
  }

  if (identical(active_view, "permutation_support")) {
    return(list(list(
      filename = codon_data_filename("permutation_support_plot_data", extension = extension),
      content = codon_plot_data_export_content(context, active_view, format = format)
    )))
  }

  if (identical(active_view, "te_bias_selected_load")) {
    return(list(list(
      filename = codon_data_filename("te_bias_selected_load_plot_data", extension = extension),
      content = codon_plot_data_export_content(context, active_view, format = format)
    )))
  }

  if (identical(active_view, "selected_load_effect")) {
    return(list(list(
      filename = codon_data_filename("selected_load_effect_plot_data", extension = extension),
      content = codon_plot_data_export_content(context, active_view, format = format)
    )))
  }

  if (identical(active_view, "codon_clustering")) {
    return(list(list(
      filename = codon_data_filename("codon_clustering_plot_data", extension = extension),
      content = codon_plot_data_export_content(context, active_view, format = format)
    )))
  }

  if (identical(active_view, "codon_usage_heatmap")) {
    return(list(list(
      filename = codon_data_filename("codon_usage_heatmap_plot_data", extension = extension),
      content = codon_plot_data_export_content(context, active_view, format = format)
    )))
  }

  if (identical(active_view, "codon_run_zscore")) {
    return(list(list(
      filename = codon_data_filename("codon_run_zscore_plot_data", extension = extension),
      content = codon_plot_data_export_content(context, active_view, format = format)
    )))
  }

  if (identical(active_view, "codon_run_enrichment")) {
    return(list(list(
      filename = codon_data_filename("codon_run_enrichment", extension = extension),
      content = codon_table_to_delimited_text(tables$codon_run_enrichment, format = format)
    )))
  }

  list(list(
    filename = codon_data_filename(active_view, extension = extension),
    content = ""
  ))
}


codon_supported_data_export_formats <- function() {
  c("csv", "txt")
}

codon_normalize_data_export_format <- function(format = "csv") {
  normalized <- tolower(trimws(as.character(format)))

  if (identical(normalized, "txt")) {
    return("txt")
  }

  "csv"
}

codon_data_export_cache_key <- function(view = "input_summary", format = "csv") {
  paste(
    codon_normalize_result_view(view),
    codon_normalize_data_export_format(format),
    sep = "::"
  )
}

codon_data_export_cache_ready <- function(cache, view = "input_summary", formats = codon_supported_data_export_formats()) {
  cache_entries <- if (is.list(cache)) cache else list()
  normalized_formats <- unique(vapply(formats, codon_normalize_data_export_format, character(1)))

  all(vapply(normalized_formats, function(format) {
    key <- codon_data_export_cache_key(view, format)
    !is.null(cache_entries[[key]])
  }, logical(1)))
}

codon_build_data_export_cache <- function(context, views = codon_supported_result_views(), formats = codon_supported_data_export_formats()) {
  if (is.null(context) || !is.list(context)) {
    return(list())
  }

  normalized_views <- unique(vapply(views, codon_normalize_result_view, character(1)))
  normalized_formats <- unique(vapply(formats, codon_normalize_data_export_format, character(1)))
  cache_entries <- list()

  for (view_id in normalized_views) {
    if (!codon_view_has_data(context, view_id)) {
      next
    }

    for (format in normalized_formats) {
      entries <- codon_data_export_entries(context, format = format, view = view_id)

      if (!length(entries)) {
        next
      }

      cache_entries[[codon_data_export_cache_key(view_id, format)]] <- entries[[1]]
    }
  }

  cache_entries
}
