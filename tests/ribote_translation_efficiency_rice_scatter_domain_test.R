source("new/global.R", local = TRUE, encoding = "UTF-8")
source("new/modules/load_data/load_data.shared.R", local = TRUE, encoding = "UTF-8")
source("new/modules/translation_efficiency/translation_efficiency.shared.R", local = TRUE, encoding = "UTF-8")

rice_matrix_path <- "TEShinyData/96_99Y.txt"
stopifnot(file.exists(rice_matrix_path))

rice_matrix <- read.table(rice_matrix_path, header = TRUE, sep = "\t", check.names = FALSE)
species_label <- "Oryza sativa (IRGSP 1.0)"
species_meta <- ribote_species_meta(species_label)

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

te_expression_bad_points <- sum(
  te_context$scatter_te_expression$TE_A1 <= 0 |
    te_context$scatter_te_expression$TE_A2 <= 0 |
    !is.finite(te_context$scatter_te_expression$TE_A1) |
    !is.finite(te_context$scatter_te_expression$TE_A2),
  na.rm = TRUE
)
scatter_te_bad_points <- sum(
  !is.finite(te_context$scatter_te$log2InputFC) |
    !is.finite(te_context$scatter_te$log2FoldChange),
  na.rm = TRUE
)

stopifnot(te_expression_bad_points > 0L)
stopifnot(scatter_te_bad_points > 0L)

te_results_code <- paste(
  c(
    readLines("new/frontend/app_shell/src/components/TranslationEfficiencyResults.jsx", warn = FALSE, encoding = "UTF-8"),
    readLines("new/frontend/app_shell/src/components/TranslationEfficiencyCharts.jsx", warn = FALSE, encoding = "UTF-8")
  ),
  collapse = "\n"
)

stopifnot(grepl("function filterScatterDataForScale", te_results_code, fixed = TRUE))
stopifnot(grepl("item.x > 0 && item.y > 0", te_results_code, fixed = TRUE))
stopifnot(grepl(".data(plotData)", te_results_code, fixed = TRUE))

cat("ribote translation efficiency rice scatter domain test passed\n")
