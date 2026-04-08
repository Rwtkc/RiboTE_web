gsea_compute_context <- function(te_context, preprocess_context, upload_context, pathways, parameters) {
  species_key <- gsea_species_key(upload_context)
  source_signature <- gsea_source_signature(te_context, preprocess_context, upload_context)
  ranked_stats <- gsea_prepare_ranked_stats(te_context$result_table, species_key = species_key)
  fgsea_result <- gsea_with_seed(
    gsea_fixed_seed(),
    fgsea::fgseaMultilevel(
      pathways = pathways,
      stats = ranked_stats,
      minSize = parameters$geneset_min,
      maxSize = parameters$geneset_max,
      eps = 0
    )
  )

  fgsea_df <- as.data.frame(fgsea_result, stringsAsFactors = FALSE)

  if (nrow(fgsea_df) == 0L) {
    stop("No gene sets remained after applying the selected GSEA size filters.", call. = FALSE)
  }

  fgsea_df$leading_edge_size <- lengths(fgsea_result$leadingEdge)
  fgsea_df$direction <- ifelse(fgsea_df$NES >= 0, "Up", "Down")
  fgsea_df$pathway_raw <- fgsea_df$pathway
  fgsea_df$pathway <- vapply(
    fgsea_df$pathway_raw,
    gsea_pretty_pathway_name,
    character(1),
    collection = parameters$collection
  )
  fgsea_df$pathway_id <- fgsea_df$pathway_raw
  fgsea_df$significant <- is.finite(fgsea_df$padj) & fgsea_df$padj <= parameters$fdr_cutoff

  table_context <- gsea_results_table(fgsea_df, parameters)
  displayed_rows_payload <- gsea_rows_payload(table_context$displayed)

  plot_payloads <- if (is.data.frame(table_context$displayed) && nrow(table_context$displayed) > 0L) {
    stats::setNames(
      lapply(seq_len(nrow(table_context$displayed)), function(index) {
        pathway_row <- table_context$displayed[index, , drop = FALSE]
        gsea_build_plot_payload(
          stats = ranked_stats,
          pathway_row = pathway_row,
          pathway_genes = pathways[[as.character(pathway_row$pathway_id[[1]])]],
          collection_label = gsea_collection_label(parameters$collection),
          species_key = species_key
        )
      }),
      as.character(table_context$displayed$pathway_id)
    )
  } else {
    list()
  }

  plot_catalog <- list(
    maxRank = as.integer(length(ranked_stats)),
    metricPoints = gsea_metric_points_payload(ranked_stats),
    pathways = lapply(names(plot_payloads), function(pathway_id) {
      plot_payload <- plot_payloads[[pathway_id]]
      list(
        pathwayId = as.character(pathway_id),
        hits = if (is.list(plot_payload$hits)) plot_payload$hits else list()
      )
    })
  )

  list(
    source_signature = source_signature,
    parameters = parameters,
    collection_label = gsea_collection_label(parameters$collection),
    resource_species_key = species_key,
    ranked_stats = ranked_stats,
    pathways = pathways,
    ordered_results = table_context$ordered,
    significant_results = table_context$significant,
    displayed_results = table_context$displayed,
    displayed_rows_payload = displayed_rows_payload,
    plot_catalog = plot_catalog,
    plot_payloads = plot_payloads,
    note = table_context$note,
    metrics = gsea_result_metrics(list(
      ordered_results = table_context$ordered,
      significant_results = table_context$significant,
      collection_label = gsea_collection_label(parameters$collection),
      ranked_stats = ranked_stats
    ))
  )
}

gsea_selected_pathway <- function(context, pathway_id = NULL) {
  displayed <- context$displayed_results

  if (!is.data.frame(displayed) || nrow(displayed) == 0L) {
    return(NULL)
  }

  if (!is.null(pathway_id) && nzchar(as.character(pathway_id))) {
    matched <- displayed[displayed$pathway_id %in% as.character(pathway_id), , drop = FALSE]
    if (nrow(matched) > 0L) {
      return(matched[1, , drop = FALSE])
    }
  }

  displayed[1, , drop = FALSE]
}

gsea_results_payload <- function(context, selected_pathway_id = NULL, selected_pathway_input_id = NULL) {
  selected_pathway <- gsea_selected_pathway(context, selected_pathway_id)
  selected_plot_id <- if (is.null(selected_pathway)) "" else as.character(selected_pathway$pathway_id[[1]])

  list(
    ids = list(
      selectedPathwayInputId = selected_pathway_input_id
    ),
    note = context$note,
    collectionLabel = context$collection_label,
    plotCatalog = if (is.list(context$plot_catalog)) context$plot_catalog else gsea_plot_catalog_payload(context),
    table = list(
      rows = if (is.list(context$displayed_rows_payload)) context$displayed_rows_payload else gsea_rows_payload(context$displayed_results),
      selectedPathway = selected_plot_id,
      totalTested = nrow(context$ordered_results),
      significantCount = nrow(context$significant_results),
      displayedCount = nrow(context$displayed_results),
      fdrCutoff = unname(as.numeric(context$parameters$fdr_cutoff))
    ),
    plot = list()
  )
}

gsea_module_config <- function() {
  ribote_build_module_config(
    key = "gsea",
    title = "GSEA",
    eyebrow = "Ranked Gene Set Enrichment",
    description = "Identify pathways and biological processes that are enriched across the ranked Translation Efficiency gene list.",
    requires = "te",
    run_label = "Run GSEA",
    sections = list(
      list(
        title = "Gene Set Database",
        collapsible = TRUE,
        default_expanded = TRUE,
        fields = list(
          ribote_field("gsea_collection", "Collection", "select", "hallmark", options = gsea_collection_options("hg38"))
        )
      ),
      list(
        title = "Analysis Filters",
        collapsible = TRUE,
        default_expanded = TRUE,
        fields = list(
          ribote_field("gsea_geneset_min", "Geneset Size Min", "number", 5, min = 5, step = 1),
          ribote_field("gsea_geneset_max", "Geneset Size Max", "number", 500, min = 10, step = 10),
          ribote_field("gsea_fdr_cutoff", "FDR Cutoff", "number", 0.05, min = 0.001, max = 1, step = 0.01),
          ribote_field("gsea_show_n", "Pathways to Show", "number", 20, min = 5, step = 1)
        )
      )
    ),
    wide_sidebar = TRUE,
    show_export_panel = TRUE,
    show_analysis_panel = TRUE,
    snapshot_columns = 2
  )
}
