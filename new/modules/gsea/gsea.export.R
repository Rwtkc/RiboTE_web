gsea_results_table <- function(fgsea_results, parameters) {
  ordered <- fgsea_results[order(fgsea_results$padj, -abs(fgsea_results$NES), fgsea_results$pval), , drop = FALSE]
  significant <- ordered[is.finite(ordered$padj) & ordered$padj <= parameters$fdr_cutoff, , drop = FALSE]
  note <- NULL

  if (nrow(significant) == 0L && nrow(ordered) > 0L) {
    note <- sprintf(
      "No pathways met the FDR cutoff of %.2f; showing the top ranked pathways instead.",
      parameters$fdr_cutoff
    )
    display_rows <- utils::head(ordered, parameters$show_n)
  } else {
    display_rows <- utils::head(significant, parameters$show_n)
  }

  list(
    ordered = ordered,
    significant = significant,
    displayed = display_rows,
    note = note
  )
}

gsea_export_table <- function(context, scope = "displayed") {
  scope <- gsea_normalize_export_data_scope(scope)

  source_table <- switch(
    scope,
    displayed = context$displayed_results,
    significant = context$significant_results,
    tested = context$ordered_results
  )

  if (is.null(source_table) || !is.data.frame(source_table) || nrow(source_table) == 0L) {
    return(data.frame())
  }

  data.frame(
    Pathway = as.character(source_table$pathway),
    Pathway_ID = as.character(source_table$pathway_id),
    Direction = as.character(source_table$direction),
    NES = as.numeric(source_table$NES),
    PValue = as.numeric(source_table$pval),
    FDR = as.numeric(source_table$padj),
    Size = as.integer(source_table$size),
    Leading_Edge_Size = as.integer(source_table$leading_edge_size),
    stringsAsFactors = FALSE
  )
}

gsea_round_export_table <- function(data_frame, digits = 4L) {
  rounded <- data_frame

  for (column_name in colnames(rounded)) {
    if (is.numeric(rounded[[column_name]])) {
      rounded[[column_name]] <- round(rounded[[column_name]], digits = digits)
    }
  }

  rounded
}

gsea_data_export_content <- function(context, scope = "displayed", format = "csv") {
  export_table <- gsea_export_table(context, scope = scope)
  pca_table_export_content(gsea_round_export_table(export_table, digits = 4L), format = format)
}

gsea_rows_payload <- function(data_frame) {
  if (is.null(data_frame) || !is.data.frame(data_frame) || nrow(data_frame) == 0L) {
    return(list())
  }

  lapply(seq_len(nrow(data_frame)), function(index) {
    row_values <- data_frame[index, , drop = FALSE]

    list(
      pathwayId = as.character(row_values$pathway_id[[1]]),
      pathway = as.character(row_values$pathway[[1]]),
      rawPathway = as.character(row_values$pathway_raw[[1]]),
      direction = as.character(row_values$direction[[1]]),
      nes = unname(as.numeric(row_values$NES[[1]])),
      padj = unname(as.numeric(row_values$padj[[1]])),
      pvalue = unname(as.numeric(row_values$pval[[1]])),
      size = unname(as.integer(row_values$size[[1]])),
      leadingEdgeSize = unname(as.integer(row_values$leading_edge_size[[1]])),
      significant = isTRUE(row_values$significant[[1]])
    )
  })
}

gsea_result_metrics <- function(context) {
  list(
    list(label = "Pathways Tested", value = format(nrow(context$ordered_results), big.mark = ",")),
    list(label = "Significant Pathways", value = format(nrow(context$significant_results), big.mark = ",")),
    list(label = "Collection", value = context$collection_label),
    list(label = "Ranked Genes", value = format(length(context$ranked_stats), big.mark = ","))
  )
}

