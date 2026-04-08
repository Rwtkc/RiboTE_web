enrichment_result_metrics <- function(context) {
  list(
    list(label = "Terms Tested", value = format(nrow(context$tested_results), big.mark = ",")),
    list(label = "Significant Terms", value = format(nrow(context$significant_results), big.mark = ",")),
    list(label = "Collection", value = context$collection_label),
    list(label = "Background", value = context$background_label)
  )
}

enrichment_export_table <- function(context, scope = "displayed") {
  scope <- enrichment_normalize_export_data_scope(scope)

  source_table <- switch(
    scope,
    displayed = context$displayed_results,
    significant = context$significant_results,
    tested = context$tested_results
  )

  if (is.null(source_table) || !is.data.frame(source_table) || nrow(source_table) == 0L) {
    return(data.frame())
  }

  link_values <- if ("url" %in% colnames(source_table)) {
    as.character(source_table$url)
  } else {
    rep("", nrow(source_table))
  }

  export_table <- data.frame(
    Group = as.character(source_table$group),
    Pathway = as.character(source_table$pathway),
    Pathway_ID = as.character(source_table$pathway_id),
    FDR = as.numeric(source_table$padj),
    PValue = as.numeric(source_table$pval),
    Fold_Enriched = as.numeric(source_table$fold),
    Overlap = as.integer(source_table$overlap),
    Query_Size = as.integer(source_table$query_size),
    Pathway_Size = as.integer(source_table$pathway_size),
    Background_Size = as.integer(source_table$background_size),
    Overlap_Genes = vapply(source_table$genes, function(genes) paste(as.character(genes), collapse = ", "), character(1)),
    Link = link_values,
    stringsAsFactors = FALSE
  )

  numeric_columns <- vapply(export_table, is.numeric, logical(1))
  export_table[numeric_columns] <- lapply(export_table[numeric_columns], function(column) round(column, 4L))
  export_table
}

enrichment_data_export_content <- function(context, scope = "displayed", format = "csv") {
  export_table <- enrichment_export_table(context, scope = scope)
  pca_table_export_content(export_table, format = format)
}

enrichment_rows_payload <- function(data_frame, show_pathway_id = FALSE) {
  if (is.null(data_frame) || !is.data.frame(data_frame) || nrow(data_frame) == 0L) {
    return(list())
  }

  lapply(seq_len(nrow(data_frame)), function(index) {
    row_values <- data_frame[index, , drop = FALSE]
    url <- if ("url" %in% colnames(row_values)) as.character(row_values$url[[1]]) else ""

    list(
      group = as.character(row_values$group[[1]]),
      pathway = as.character(row_values$pathway[[1]]),
      rawPathway = if (isTRUE(show_pathway_id)) as.character(row_values$pathway_id[[1]]) else "",
      pathwayId = as.character(row_values$pathway_id[[1]]),
      url = url,
      padj = unname(as.numeric(row_values$padj[[1]])),
      pvalue = unname(as.numeric(row_values$pval[[1]])),
      fold = unname(as.numeric(row_values$fold[[1]])),
      overlap = unname(as.integer(row_values$overlap[[1]])),
      pathwaySize = unname(as.integer(row_values$pathway_size[[1]])),
      querySize = unname(as.integer(row_values$query_size[[1]]))
    )
  })
}

enrichment_results_payload <- function(context) {
  list(
    note = context$note,
    collectionLabel = context$collection_label,
    plot = list(
      rows = if (is.list(context$displayed_plot_rows_payload)) context$displayed_plot_rows_payload else enrichment_rows_payload(
        context$displayed_results,
        show_pathway_id = isTRUE(context$parameters$show_pathway_id)
      ),
      backgroundLabel = context$background_label
    ),
    table = list(
      rows = if (is.list(context$displayed_table_rows_payload)) context$displayed_table_rows_payload else enrichment_rows_payload(
        context$displayed_results,
        show_pathway_id = isTRUE(context$parameters$show_pathway_id)
      ),
      totalTested = nrow(context$tested_results),
      significantCount = nrow(context$significant_results),
      displayedCount = nrow(context$displayed_results)
    )
  )
}

