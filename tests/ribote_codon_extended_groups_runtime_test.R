source("new/global.R", local = TRUE, encoding = "UTF-8")
source("new/shared/module_shell.R", local = TRUE, encoding = "UTF-8")
source("new/modules/load_data/load_data.shared.R", local = TRUE, encoding = "UTF-8")
source("new/modules/pca/pca.shared.R", local = TRUE, encoding = "UTF-8")
source("new/modules/translation_efficiency/translation_efficiency.shared.R", local = TRUE, encoding = "UTF-8")
source("new/modules/codon/codon.shared.R", local = TRUE, encoding = "UTF-8")

build_demo_codon_workspace <- function() {
  gene_matrix <- read.table("TEShinyData/all.count.txt", header = TRUE, sep = "\t", check.names = FALSE)
  species_label <- "Homo sapiens (hg38)"
  species_meta <- ribote_species_meta(species_label)

  upload_context <- list(
    species = species_label,
    species_meta = species_meta,
    pair_manifest = data.frame(
      rna_sample = c("RNA.WT1", "RNA.WT2", "RNA.KO1", "RNA.KO2"),
      ribo_sample = c("RPF.WT1", "RPF.WT2", "RPF.KO1", "RPF.KO2"),
      group_role = c("Control", "Control", "Treatment", "Treatment"),
      stringsAsFactors = FALSE
    ),
    resource_paths = list(
      gff_rda_path = app_data_path("gff", species_meta$gff_rda_name),
      txlens_path = app_data_path("txlens", species_meta$txlens_name),
      fasta_path = app_data_path("fa", species_meta$fasta_name),
      tai_path = app_data_path("cds", species_meta$tai_name),
      cbi_path = app_data_path("cds", species_meta$cbi_name)
    )
  )

  preprocess_context <- list(
    preview = gene_matrix,
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

  resource_context <- codon_build_resource_context(upload_context)
  base_context <- codon_build_base_context(
    te_context = te_context,
    preprocess_context = preprocess_context,
    upload_context = upload_context,
    resource_context = resource_context
  )

  list(
    upload_context = upload_context,
    preprocess_context = preprocess_context,
    te_context = te_context,
    base_context = base_context
  )
}

workspace <- build_demo_codon_workspace()
parameters <- codon_normalize_parameters(
  codon_select = "AAA",
  codon_direction = "Up",
  codon_display = "Obj"
)

example_sequence <- "AAA AAA AAA TTT AAA AAA"
expected_run_counts <- c(
  codon_count_selected_runs(example_sequence, parameters$codon_select, run_length = 1L),
  codon_count_selected_runs(example_sequence, parameters$codon_select, run_length = 2L),
  codon_count_selected_runs(example_sequence, parameters$codon_select, run_length = 3L)
)
stopifnot(identical(
  as.integer(codon_count_selected_runs_all_lengths(example_sequence, parameters$codon_select, max_run_length = 3L)),
  as.integer(expected_run_counts)
))

usage_context <- codon_compute_context(workspace$base_context, parameters)
bias_context <- codon_compute_bias_context(workspace$base_context, workspace$upload_context)
shift_context <- codon_compute_shift_context(workspace$base_context, parameters)
pattern_context <- codon_compute_pattern_context(workspace$base_context, parameters)
run_context <- codon_compute_run_context(workspace$base_context, parameters)

stopifnot(length(shift_context$results$selectedCodonBurden$panels) == 1L)
stopifnot(length(shift_context$results$selectedCodonBurden$rows) > 0L)
stopifnot(length(shift_context$results$codonEnrichmentShifted$panels) == 1L)
stopifnot(length(shift_context$results$selectedCodonAcrossGroups$panels) == 1L)
stopifnot(length(shift_context$results$permutationSupport$panels) == 2L)
stopifnot(length(shift_context$results$teBiasSelectedLoad$panels) == 1L)
stopifnot(length(shift_context$results$selectedLoadEffect$panels) == 1L)

stopifnot(length(pattern_context$results$codonClustering$panels) == 1L)
stopifnot(length(pattern_context$results$codonUsageHeatmap$panels) == 1L)

stopifnot(length(run_context$results$codonRunZscore$panels) == 3L)
stopifnot(length(run_context$results$codonRunZscore$rows) == 3L)
stopifnot(length(run_context$results$codonRunEnrichment$panels) == 3L)
stopifnot(length(run_context$results$codonRunEnrichment$rows) == 3L)

merged_context <- codon_merge_workspace_contexts(
  usage_context = usage_context,
  bias_context = bias_context,
  shift_context = shift_context,
  pattern_context = pattern_context,
  run_context = run_context
)

for (view_id in c(
  "selected_codon_burden",
  "codon_enrichment_shifted",
  "selected_codon_across_groups",
  "permutation_support",
  "te_bias_selected_load",
  "selected_load_effect",
  "codon_clustering",
  "codon_usage_heatmap",
  "codon_run_zscore",
  "codon_run_enrichment"
)) {
  stopifnot(codon_view_has_data(merged_context, view_id))
  export_entries <- codon_data_export_entries(merged_context, format = "csv", view = view_id)
  stopifnot(length(export_entries) == 1L)
  stopifnot(nzchar(export_entries[[1]]$content))
}

cat("ribote codon extended groups runtime test passed\n")
