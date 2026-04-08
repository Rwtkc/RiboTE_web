te_results_code <- paste(
  readLines("new/frontend/app_shell/src/components/TranslationEfficiencyResults.jsx", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

stopifnot(grepl("function filterScatterDataForScale", te_results_code, fixed = TRUE))
stopifnot(grepl("item.x > 0 && item.y > 0", te_results_code, fixed = TRUE))
stopifnot(grepl("TE_DISPLAY_POINT_LIMIT", te_results_code, fixed = TRUE))
stopifnot(grepl("function buildDisplayDataSubset", te_results_code, fixed = TRUE))
stopifnot(grepl("displaySubset", te_results_code, fixed = TRUE))
stopifnot(grepl("shouldAnimate && !isLargePointSet", te_results_code, fixed = TRUE))
stopifnot(grepl("Only a deterministic display subset is drawn", te_results_code, fixed = TRUE))

source("new/global.R", local = TRUE, encoding = "UTF-8")
source("new/modules/translation_efficiency/translation_efficiency.shared.R", local = TRUE, encoding = "UTF-8")

row_count <- 6001L
te_context <- list(
  result_table = data.frame(
    GeneID = sprintf("Gene%04d", seq_len(row_count)),
    gene_name = "unknown",
    TE_log2FC = seq(-3, 3, length.out = row_count),
    log2FoldChange = seq(-3, 3, length.out = row_count),
    pvalue = rep(0.01, row_count),
    padj = rep(0.02, row_count),
    TE_Status = rep(c("Up", "Non", "Down"), length.out = row_count),
    stringsAsFactors = FALSE
  ),
  search_index = rep("gene", row_count),
  volcano_points = data.frame(
    GeneID = sprintf("Gene%04d", seq_len(row_count)),
    log2FoldChange = seq(-3, 3, length.out = row_count),
    significance = seq(0.1, 10, length.out = row_count),
    pvalue = rep(0.01, row_count),
    padj = rep(0.02, row_count),
    diffTE = rep(c("Up", "Non", "Down"), length.out = row_count),
    stringsAsFactors = FALSE
  ),
  scatter_input = data.frame(
    GeneID = sprintf("Gene%04d", seq_len(row_count)),
    input1 = seq(1, 100, length.out = row_count),
    input2 = seq(2, 200, length.out = row_count),
    diffExp = rep(c("Up", "Non", "Down"), length.out = row_count),
    stringsAsFactors = FALSE
  ),
  scatter_points = data.frame(
    GeneID = sprintf("Gene%04d", seq_len(row_count)),
    log2InputFC = seq(-3, 3, length.out = row_count),
    log2RPFFC = seq(3, -3, length.out = row_count),
    log2FoldChange = seq(-2, 2, length.out = row_count),
    diffTE = rep(c("Up", "Non", "Down"), length.out = row_count),
    stringsAsFactors = FALSE
  ),
  scatter_te_expression = data.frame(
    GeneID = sprintf("Gene%04d", seq_len(row_count)),
    TE_A1 = seq(1, 100, length.out = row_count),
    TE_A2 = seq(2, 200, length.out = row_count),
    diffTE = rep(c("Up", "Non", "Down"), length.out = row_count),
    stringsAsFactors = FALSE
  ),
  scatter_te = data.frame(
    GeneID = sprintf("Gene%04d", seq_len(row_count)),
    log2InputFC = seq(-3, 3, length.out = row_count),
    log2FoldChange = seq(-2, 2, length.out = row_count),
    log2RPFFC = seq(3, -3, length.out = row_count),
    diffTE = rep(c("Up", "Non", "Down"), length.out = row_count),
    stringsAsFactors = FALSE
  ),
  parameters = list(fvalue = 1.5, p_cutoff = 0.05)
)

volcano_payload <- translation_efficiency_results_payload(te_context, active_view = "volcano")
scatter_payload <- translation_efficiency_results_payload(te_context, active_view = "scatter")

stopifnot(length(volcano_payload$charts$volcano) <= 5000L)
stopifnot(length(scatter_payload$charts$scatterInput) <= 5000L)
stopifnot(length(scatter_payload$charts$scatterTeExpression) <= 5000L)
stopifnot(length(scatter_payload$charts$scatterTe) <= 5000L)
stopifnot(identical(volcano_payload$charts$displayMeta$volcano$originalCount, row_count))
stopifnot(isTRUE(volcano_payload$charts$displayMeta$volcano$isSubset))
stopifnot(identical(scatter_payload$charts$displayMeta$scatterTe$originalCount, row_count))
stopifnot(isTRUE(scatter_payload$charts$displayMeta$scatterTe$isSubset))
stopifnot(length(volcano_payload$table$rows) == 10L)

cat("ribote translation efficiency render performance guard test passed\n")
