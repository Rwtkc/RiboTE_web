source("new/global.R", local = TRUE, encoding = "UTF-8")
source("new/modules/translation_efficiency/translation_efficiency.shared.R", local = TRUE, encoding = "UTF-8")

data("xtaildata", package = "xtail")

mrna <- xtaildata$mrna
rpf <- xtaildata$rpf

preprocess_context <- list(
  preview = data.frame(
    GeneID = rownames(mrna),
    "RNA.control1" = mrna$control1,
    "RNA.control2" = mrna$control2,
    "RNA.treat1" = mrna$treat1,
    "RNA.treat2" = mrna$treat2,
    "RPF.control1" = rpf$control1,
    "RPF.control2" = rpf$control2,
    "RPF.treat1" = rpf$treat1,
    "RPF.treat2" = rpf$treat2,
    check.names = FALSE,
    stringsAsFactors = FALSE
  ),
  parameters = list(
    min_cpm = 0.5,
    min_libraries = 1L
  )
)

upload_context <- list(
  pair_manifest = data.frame(
    rna_sample = c("RNA.control1", "RNA.control2", "RNA.treat1", "RNA.treat2"),
    ribo_sample = c("RPF.control1", "RPF.control2", "RPF.treat1", "RPF.treat2"),
    group_role = c("Control", "Control", "Treatment", "Treatment"),
    stringsAsFactors = FALSE
  ),
  resource_paths = list()
)

te_context <- translation_efficiency_compute_context(
  preprocess_context = preprocess_context,
  upload_context = upload_context,
  te_tool = "Xtail",
  fvalue = 1.5,
  p_cutoff = 0.05,
  p_type = "Fdr"
)

result_table <- te_context$result_table

stopifnot(is.data.frame(result_table))
stopifnot(nrow(result_table) > 0)
stopifnot("gene_name" %in% colnames(result_table))
stopifnot(all(result_table$gene_name == "unknown"))
stopifnot("TE_Status" %in% colnames(result_table))
stopifnot(sum(result_table$TE_Status == "Up", na.rm = TRUE) > 0)
stopifnot(sum(result_table$TE_Status == "Down", na.rm = TRUE) > 0)

cat("ribote translation efficiency xtail test passed\n")
