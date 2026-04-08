source("new/global.R", local = TRUE, encoding = "UTF-8")
source("new/modules/translation_efficiency/translation_efficiency.shared.R", local = TRUE, encoding = "UTF-8")

preprocess_data <- read.csv("new/preprocess_data.csv", check.names = FALSE)
colnames(preprocess_data)[1] <- "GeneID"
legacy_normalcount <- read.csv("new/translation_efficiency_normalcount.csv", check.names = FALSE)

upload_context <- list(
  pair_manifest = data.frame(
    rna_sample = c("RNA.WT1", "RNA.WT2", "RNA.KO1", "RNA.KO2"),
    ribo_sample = c("RPF.WT1", "RPF.WT2", "RPF.KO1", "RPF.KO2"),
    group_role = c("Control", "Control", "Treatment", "Treatment"),
    stringsAsFactors = FALSE
  ),
  resource_paths = list(
    gff_rda_path = app_data_path("gff", "hg38.gff.rda")
  )
)

preprocess_context <- list(
  preview = preprocess_data,
  parameters = list(
    min_cpm = 0.5,
    min_libraries = 1L
  )
)

te_context <- translation_efficiency_compute_context(
  preprocess_context = preprocess_context,
  upload_context = upload_context,
  te_tool = "Riborex",
  fvalue = 1.5,
  p_cutoff = 0.05,
  p_type = "Fdr"
)

new_result <- te_context$result_table

stopifnot("gene_name" %in% colnames(new_result))
stopifnot(sum(new_result$diffTE == "Up", na.rm = TRUE) == sum(legacy_normalcount$diffTE == "Up", na.rm = TRUE))
stopifnot(sum(new_result$diffTE == "Down", na.rm = TRUE) == sum(legacy_normalcount$diffTE == "Down", na.rm = TRUE))
stopifnot(sum(new_result$diffTE == "Non", na.rm = TRUE) == sum(legacy_normalcount$diffTE == "Non", na.rm = TRUE))
stopifnot(sum(new_result$gene_name == "unknown", na.rm = TRUE) < nrow(new_result))

new_gene <- new_result[new_result$GeneID == "ENSG00000133636", , drop = FALSE]
old_gene <- legacy_normalcount[legacy_normalcount$GeneID == "ENSG00000133636", , drop = FALSE]
new_gene_named <- new_result[new_result$GeneID == "ENSG00000000419", , drop = FALSE]
old_gene_named <- legacy_normalcount[legacy_normalcount$GeneID == "ENSG00000000419", , drop = FALSE]

stopifnot(nrow(new_gene) == 1L)
stopifnot(nrow(old_gene) == 1L)
stopifnot(identical(as.character(new_gene$diffTE[[1]]), as.character(old_gene$diffTE[[1]])))
stopifnot(nrow(new_gene_named) == 1L)
stopifnot(nrow(old_gene_named) == 1L)
stopifnot(identical(as.character(new_gene_named$gene_name[[1]]), as.character(old_gene_named$gene_name[[1]])))

cat("ribote translation efficiency legacy riborex test passed\n")
