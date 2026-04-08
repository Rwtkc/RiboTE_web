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
    base_context = base_context
  )
}

naive_build_run_load_table <- function(scope_context, parameters) {
  scope_table <- as.data.frame(scope_context$gene_table, stringsAsFactors = FALSE)

  build_length_table <- function(run_length) {
    run_counts <- vapply(
      scope_table$codonSequence,
      codon_count_selected_runs,
      numeric(1),
      selected_codons = parameters$codon_select,
      run_length = run_length
    )
    total_codons <- pmax(as.numeric(scope_table$totalCodons), 1)
    run_per1k <- (run_counts * 1000) / total_codons
    mean_run <- mean(run_per1k, na.rm = TRUE)
    sd_run <- stats::sd(run_per1k, na.rm = TRUE)
    run_zscore <- if (is.finite(sd_run) && sd_run > 0) (run_per1k - mean_run) / sd_run else rep(0, length(run_per1k))

    data.frame(
      Run_Length = run_length,
      Run_Label = c("Single", "Double", "Triple")[run_length],
      GeneID = as.character(scope_table$GeneID),
      gene_name = as.character(scope_table$gene_name),
      TE_Status = as.character(scope_table$TE_Status),
      selectedRunCount = run_counts,
      selectedRunPer1k = run_per1k,
      selectedRunZScore = run_zscore,
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, lapply(1:3, build_length_table))
}

workspace <- build_demo_codon_workspace()
parameters <- codon_normalize_parameters(
  codon_select = "AAA",
  codon_direction = "Up",
  codon_display = "Obj"
)
scope_context <- codon_build_scope_table(workspace$base_context, parameters)

naive_start <- Sys.time()
naive_table <- naive_build_run_load_table(scope_context, parameters)
naive_elapsed <- as.numeric(difftime(Sys.time(), naive_start, units = "secs"))

optimized_start <- Sys.time()
optimized_table <- codon_build_run_load_table(scope_context, parameters)
optimized_elapsed <- as.numeric(difftime(Sys.time(), optimized_start, units = "secs"))

count_comparison <- merge(
  naive_table[, c("Run_Length", "GeneID", "selectedRunCount")],
  optimized_table[, c("Run_Length", "GeneID", "selectedRunCount")],
  by = c("Run_Length", "GeneID"),
  suffixes = c("_naive", "_optimized"),
  all = TRUE
)
stopifnot(identical(count_comparison$selectedRunCount_naive, count_comparison$selectedRunCount_optimized))

cat(sprintf("naive_seconds=%.2f\n", naive_elapsed))
cat(sprintf("optimized_seconds=%.2f\n", optimized_elapsed))
cat(sprintf("speedup=%.2fx\n", naive_elapsed / optimized_elapsed))
