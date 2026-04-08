source("new/global.R", local = TRUE, encoding = "UTF-8")
source("new/modules/load_data/load_data.shared.R", local = TRUE, encoding = "UTF-8")
source("new/modules/pca/pca.shared.R", local = TRUE, encoding = "UTF-8")
source("new/modules/translation_efficiency/translation_efficiency.shared.R", local = TRUE, encoding = "UTF-8")
source("new/modules/gsea/gsea.shared.R", local = TRUE, encoding = "UTF-8")
source("new/modules/enrichment/enrichment.shared.R", local = TRUE, encoding = "UTF-8")

rice_matrix_path <- "TEShinyData/96_99Y.txt"
stopifnot(file.exists(rice_matrix_path))

rice_matrix <- read.table(rice_matrix_path, header = TRUE, sep = "\t", check.names = FALSE)
species_label <- "Oryza sativa (IRGSP 1.0)"
species_meta <- ribote_species_meta(species_label)
species_key <- species_meta$acronym

upload_context <- list(
  species = species_label,
  species_meta = species_meta,
  pair_manifest = data.frame(
    rna_sample = c("RNA_1196Y_1", "RNA_1196Y_2", "RNA_1199Y_1", "RNA_1199Y_2"),
    ribo_sample = c("1196Y_1", "1196Y_2", "1199Y_1", "1199Y_2"),
    group_role = c("Control", "Control", "Treatment", "Treatment"),
    stringsAsFactors = FALSE
  )
)

preprocess_context <- list(
  preview = rice_matrix,
  parameters = list(min_cpm = 0.5, min_libraries = 1L)
)

te_context <- translation_efficiency_compute_context(
  preprocess_context = preprocess_context,
  upload_context = upload_context,
  te_tool = "Riborex",
  fvalue = 1.5,
  p_cutoff = 0.05,
  p_type = "Fdr"
)

stopifnot(identical(species_key, "osa_IRGSP_1"))
stopifnot("GeneID" %in% colnames(te_context$result_table))
stopifnot(grepl("^Os\\d{2}g\\d+", te_context$result_table$GeneID[[1]]))

rice_scatter_payload <- translation_efficiency_results_payload(te_context, active_view = "scatter")
stopifnot(length(rice_scatter_payload$charts$scatterInput) <= 5000L)
stopifnot(length(rice_scatter_payload$charts$scatterTeExpression) <= 5000L)
stopifnot(length(rice_scatter_payload$charts$scatterTe) <= 5000L)
stopifnot(isTRUE(rice_scatter_payload$charts$displayMeta$scatterInput$isSubset))
stopifnot(isTRUE(rice_scatter_payload$charts$displayMeta$scatterTeExpression$isSubset))
stopifnot(isTRUE(rice_scatter_payload$charts$displayMeta$scatterTe$isSubset))

for (collection in c("go_bp", "kegg")) {
  gsea_parameters <- gsea_normalize_parameters(
    collection = collection,
    geneset_min = 5L,
    geneset_max = 500L,
    fdr_cutoff = 0.05,
    show_n = 20L,
    species_key = species_key
  )
  gsea_pathways <- gsea_read_pathways(
    gsea_resource_path(species_key, gsea_parameters$collection),
    species_key = species_key
  )
  gsea_context <- gsea_compute_context(
    te_context = te_context,
    preprocess_context = preprocess_context,
    upload_context = upload_context,
    pathways = gsea_pathways,
    parameters = gsea_parameters
  )

  stopifnot(nrow(gsea_context$ordered_results) > 0L)
  stopifnot(nrow(gsea_context$displayed_results) > 0L)
  stopifnot(length(gsea_context$ranked_stats) > 0L)

  enrichment_parameters <- enrichment_normalize_parameters(
    collection = collection,
    top_pathways = 10L,
    sort_by = "FDR",
    filtered_background = TRUE,
    remove_redundant = FALSE,
    show_pathway_id = FALSE,
    species_key = species_key
  )
  enrichment_resource <- enrichment_read_gmt_resource(
    enrichment_resource_path(species_key, enrichment_parameters$collection),
    species_key = species_key
  )
  enrichment_context <- enrichment_compute_context(
    te_context = te_context,
    preprocess_context = preprocess_context,
    upload_context = upload_context,
    resource = enrichment_resource,
    parameters = enrichment_parameters
  )

  stopifnot(nrow(enrichment_context$tested_results) > 0L)
  stopifnot(nrow(enrichment_context$displayed_results) > 0L)
  stopifnot(length(intersect(
    enrichment_filtered_background(enrichment_prepare_gene_table(te_context$result_table, species_key = species_key)),
    enrichment_full_background(enrichment_resource)
  )) > 0L)
}

cat("ribote rice gsea enrichment overlap test passed\n")
