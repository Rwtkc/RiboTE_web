source("new/global.R", local = TRUE, encoding = "UTF-8")
source("new/modules/translation_efficiency/translation_efficiency.shared.R", local = TRUE, encoding = "UTF-8")

config <- translation_efficiency_export_config("translation_efficiency")
defaults <- translation_efficiency_export_defaults()

stopifnot(identical(config$ids$trigger, NS("translation_efficiency")("export_plot")))
stopifnot(identical(config$ids$dataTrigger, NS("translation_efficiency")("data_export")))
stopifnot(identical(defaults$format, "png"))
stopifnot(identical(defaults$data_format, "csv"))

te_context <- list(
  result_table = data.frame(
    GeneID = c("ENSG1", "ENSG2"),
    RNA_Control_Mean = c(10.123456, 20.987654),
    RNA_Treatment_Mean = c(12.123456, 25.987654),
    Ribo_Control_Mean = c(15.123456, 30.987654),
    Ribo_Treatment_Mean = c(18.123456, 28.987654),
    TE_Control_Mean = c(1.512345, 1.298765),
    TE_Treatment_Mean = c(1.812345, 1.098765),
    RNA_log2FC = c(0.261234, 0.321987),
    RNA_Expression_Status = c("Up", "Up"),
    Ribo_log2FC = c(0.261234, -0.109876),
    Ribo_Expression_Status = c("Up", "Down"),
    log2FoldChange = c(0.101234, -0.201987),
    pvalue = c(0.012345, 0.023456),
    padj = c(0.034567, 0.045678),
    TE_log2FC = c(0.261234, -0.241111),
    TE_Status = c("Up", "Down"),
    check.names = FALSE
  ),
  volcano_points = data.frame(GeneID = c("ENSG1", "ENSG2")),
  scatter_input = data.frame(GeneID = c("ENSG1", "ENSG2")),
  scatter_rpf = data.frame(GeneID = c("ENSG1", "ENSG2")),
  scatter_te_expression = data.frame(GeneID = c("ENSG1", "ENSG2")),
  scatter_te = data.frame(GeneID = c("ENSG1", "ENSG2"))
)

data_export_text <- translation_efficiency_data_export_content(te_context, format = "csv")
header_line <- strsplit(data_export_text, "\n", fixed = TRUE)[[1]][1]
stopifnot(grepl("RNA_Control_Mean", data_export_text, fixed = TRUE))
stopifnot(grepl("RNA_Treatment_Mean", data_export_text, fixed = TRUE))
stopifnot(grepl("RNA_log2FC", data_export_text, fixed = TRUE))
stopifnot(grepl("RNA_Expression_Status", data_export_text, fixed = TRUE))
stopifnot(grepl("Ribo_Control_Mean", data_export_text, fixed = TRUE))
stopifnot(grepl("Ribo_Treatment_Mean", data_export_text, fixed = TRUE))
stopifnot(grepl("Ribo_log2FC", data_export_text, fixed = TRUE))
stopifnot(grepl("Ribo_Expression_Status", data_export_text, fixed = TRUE))
stopifnot(grepl("TE_Control_Mean", data_export_text, fixed = TRUE))
stopifnot(grepl("TE_Treatment_Mean", data_export_text, fixed = TRUE))
stopifnot(grepl("TE_log2FC", data_export_text, fixed = TRUE))
stopifnot(grepl("TE_Status", data_export_text, fixed = TRUE))
stopifnot(!grepl("input1", data_export_text, fixed = TRUE))
stopifnot(!grepl("input2", data_export_text, fixed = TRUE))
stopifnot(!grepl("logInputFC", data_export_text, fixed = TRUE))
stopifnot(!grepl("diffExp", data_export_text, fixed = TRUE))
stopifnot(!grepl("rpf1", data_export_text, fixed = TRUE))
stopifnot(!grepl("rpf2", data_export_text, fixed = TRUE))
stopifnot(!grepl("logRPFfc", data_export_text, fixed = TRUE))
stopifnot(!grepl("diffRibo", data_export_text, fixed = TRUE))
stopifnot(!grepl("RPF_Control_Mean", data_export_text, fixed = TRUE))
stopifnot(!grepl("RPF_Treatment_Mean", data_export_text, fixed = TRUE))
stopifnot(!grepl("RPF_log2FC", data_export_text, fixed = TRUE))
stopifnot(!grepl("TE_A1", data_export_text, fixed = TRUE))
stopifnot(!grepl("TE_A2", data_export_text, fixed = TRUE))
stopifnot(!grepl("logTEfc", data_export_text, fixed = TRUE))
stopifnot(!grepl("diffTE", data_export_text, fixed = TRUE))
stopifnot(grepl("10.1235", data_export_text, fixed = TRUE))
stopifnot(grepl("0.1012", data_export_text, fixed = TRUE))
stopifnot(!grepl("10.123456", data_export_text, fixed = TRUE))
stopifnot(!grepl("0.101234", data_export_text, fixed = TRUE))
stopifnot(grepl("\"RNA_log2FC\",\"RNA_Expression_Status\",\"Ribo_log2FC\",\"Ribo_Expression_Status\",\"pvalue\",\"padj\",\"TE_log2FC\"", header_line, fixed = TRUE))

scatter_entries <- translation_efficiency_chart_export_entries(
  te_context,
  view = "scatter",
  format = "csv"
)

stopifnot(length(scatter_entries) == 3L)
stopifnot(identical(scatter_entries[[1]]$filename, "translation_efficiency_scatter_te_expression.csv"))
stopifnot(identical(scatter_entries[[2]]$filename, "translation_efficiency_scatter_input_expression.csv"))
stopifnot(identical(scatter_entries[[3]]$filename, "translation_efficiency_scatter_rna_vs_te.csv"))
stopifnot(!any(vapply(scatter_entries, function(entry) grepl("rna_vs_rpf", entry$filename, fixed = TRUE), logical(1))))

cat("ribote translation efficiency export test passed\n")
