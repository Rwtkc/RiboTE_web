enrichment_compute_context <- function(te_context, preprocess_context, upload_context, resource, parameters) {
  species_key <- enrichment_species_key(upload_context)
  gene_table <- enrichment_prepare_gene_table(te_context$result_table, species_key = species_key)
  gene_lists <- enrichment_gene_lists(gene_table)
  filtered_background <- enrichment_filtered_background(gene_table)
  full_background <- enrichment_full_background(resource)
  use_filtered_background <- isTRUE(parameters$filtered_background)

  background_genes <- if (use_filtered_background) filtered_background else full_background
  grouped_rows <- lapply(gene_lists, function(query_genes) {
    enrichment_overlap_rows(
      query_genes = query_genes,
      background_genes = background_genes,
      resource = resource,
      collection = parameters$collection,
      species_key = species_key,
      sort_by = parameters$sort_by,
      remove_redundant = parameters$remove_redundant,
      top_pathways = parameters$top_pathways
    )
  })

  displayed_results <- enrichment_combined_table(grouped_rows, scope = "displayed")
  significant_results <- enrichment_combined_table(grouped_rows, scope = "significant")
  tested_results <- enrichment_combined_table(grouped_rows, scope = "tested")

  if (nrow(tested_results) == 0L) {
    stop("No overlapping pathways were found for the current Enrichment query genes.", call. = FALSE)
  }

  note <- NULL
  if (nrow(significant_results) == 0L) {
    note <- "No pathways met the default enrichment FDR threshold of 0.10 for the current Up/Down TE gene sets."
  }

  if (!is.data.frame(displayed_results) || nrow(displayed_results) == 0L) {
    displayed_results <- significant_results
  }

  context <- list(
    source_signature = enrichment_source_signature(te_context, preprocess_context, upload_context),
    parameters = parameters,
    collection_label = enrichment_collection_label(parameters$collection),
    background_label = if (use_filtered_background) "Filtered Genes" else "Full Collection",
    resource_species_key = species_key,
    tested_results = tested_results,
    significant_results = significant_results,
    displayed_results = displayed_results,
    displayed_plot_rows_payload = enrichment_rows_payload(
      displayed_results,
      show_pathway_id = isTRUE(parameters$show_pathway_id)
    ),
    displayed_table_rows_payload = enrichment_rows_payload(
      displayed_results,
      show_pathway_id = isTRUE(parameters$show_pathway_id)
    ),
    note = note
  )

  context$metrics <- enrichment_result_metrics(context)
  context
}

enrichment_module_config <- function() {
  ribote_build_module_config(
    key = "enrichment",
    title = "Enrichment",
    eyebrow = "Functional Enrichment",
    description = "Summarize biological functions and pathways enriched among TE-associated Up and Down gene sets.",
    requires = "te",
    run_label = "Run Enrichment",
    sections = list(
      list(
        title = "Gene Set Database",
        collapsible = TRUE,
        default_expanded = TRUE,
        fields = list(
          ribote_field("enrichment_collection", "Collection", "select", "go_bp", options = enrichment_collection_options("hg38"))
        )
      ),
      list(
        title = "Analysis Filters",
        collapsible = TRUE,
        default_expanded = TRUE,
        fields = list(
          ribote_field("enrichment_top_pathways", "Top Pathways", "number", 10, min = 1, max = 30, step = 1),
          ribote_field(
            "enrichment_sort_by",
            "Sort By",
            "select",
            "FDR",
            options = list(ribote_choice("FDR"), ribote_choice("Fold"))
          ),
          ribote_field("enrichment_filtered_background", "Use Filtered Genes as Background", "checkbox", TRUE),
          ribote_field("enrichment_remove_redundant", "Remove Redundant Gene Sets", "checkbox", FALSE),
          ribote_field("enrichment_show_pathway_id", "Show Pathway IDs", "checkbox", FALSE)
        )
      )
    ),
    views = list(
      list(id = "table", title = "Enrichment Table", description = "Term-level enrichment results for Up and Down gene sets.")
    ),
    result_metrics = list(
      list(label = "Output Surface", value = "Table"),
      list(label = "Gene Sets", value = "Up / Down"),
      list(label = "Background", value = "Selectable")
    ),
    empty_message = "",
    success_message = "Enrichment results are ready.",
    wide_sidebar = TRUE,
    show_export_panel = TRUE,
    show_analysis_panel = TRUE,
    snapshot_columns = 2
  )
}
