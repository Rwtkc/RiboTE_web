source("new/global.R", local = TRUE, encoding = "UTF-8")
source("new/modules/data_preprocess/data_preprocess.shared.R", local = TRUE, encoding = "UTF-8")

config <- data_preprocess_export_config("data_preprocess")
defaults <- data_preprocess_export_defaults()

stopifnot(identical(config$ids$trigger, NS("data_preprocess")("export_plot")))
stopifnot(identical(config$ids$dataTrigger, NS("data_preprocess")("data_export")))
stopifnot(identical(defaults$format, "png"))
stopifnot(identical(defaults$data_format, "csv"))

settings <- data_preprocess_export_settings(list(
  export_format = "pdf",
  export_width = "3200",
  export_height = "1800",
  export_dpi = "300",
  data_export_format = "txt"
))

stopifnot(identical(settings$format, "pdf"))
stopifnot(identical(settings$width, 3200))
stopifnot(identical(settings$height, 1800))
stopifnot(identical(settings$dpi, 300))
stopifnot(identical(settings$data_format, "txt"))

preprocess_context <- list(
  preview = data.frame(
    GeneID = c("ENSG1", "ENSG2"),
    RNA.WT1 = c(10, 20),
    RPF.WT1 = c(30, 40),
    check.names = FALSE
  )
)

csv_content <- data_preprocess_data_export_content(preprocess_context, "csv")
txt_content <- data_preprocess_data_export_content(preprocess_context, "txt")
archive_name <- data_preprocess_archive_filename("qc")
barplot_content <- data_preprocess_chart_export_content(
  list(
    barplot_data = data.frame(
      sample = c("RNA.WT1", "RPF.WT1"),
      sample_display = c("RNA.C1", "RPF.C1"),
      total_count = c(10, 20),
      sample_type = c("RNA-seq", "Ribo-seq"),
      stringsAsFactors = FALSE
    )
  ),
  view = "barplot",
  format = "csv"
)
qc_entries <- data_preprocess_chart_export_entries(
  list(
    biotype_summary = data.frame(
      gene_biotype = c("protein_coding"),
      genes_retained = c(100),
      stringsAsFactors = FALSE
    ),
    rrna_summary = data.frame(
      sample = c("RNA.WT1", "RNA.WT1"),
      sample_display = c("RNA.C1", "RNA.C1"),
      category = c("rRNA", "Non-rRNA"),
      total_count = c(1, 9),
      stringsAsFactors = FALSE
    )
  ),
  format = "txt"
)

stopifnot(grepl("\"GeneID\",\"RNA.WT1\",\"RPF.WT1\"", csv_content, fixed = TRUE))
stopifnot(grepl("\"GeneID\"\t\"RNA.WT1\"\t\"RPF.WT1\"", txt_content, fixed = TRUE))
stopifnot(grepl("^data_preprocess_qc_[0-9]{8}\\.zip$", archive_name))
stopifnot(grepl("\"sample\",\"sample_display\",\"total_count\",\"sample_type\"", barplot_content, fixed = TRUE))
stopifnot(length(qc_entries) == 2L)
stopifnot(identical(qc_entries[[1]]$filename, "gene_biotype_composition_data.txt"))
stopifnot(identical(qc_entries[[2]]$filename, "rrna_fraction_by_sample_data.txt"))

main_code <- paste(
  readLines("new/frontend/app_shell/src/main.jsx", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

bridge_code <- paste(
  readLines("new/frontend/app_shell/src/bridge/exportBridge.js", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

motif_export_code <- paste(
  readLines("new/frontend/app_shell/src/bridge/motifExport.js", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

component_code <- paste(
  readLines("new/frontend/app_shell/src/components/PreprocessExport.jsx", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

results_code <- paste(
  readLines("new/frontend/app_shell/src/components/PreprocessResults.jsx", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

server_code <- paste(
  readLines("new/modules/data_preprocess/data_preprocess.server.R", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

stopifnot(grepl("ribote-preprocess-export", main_code, fixed = TRUE))
stopifnot(grepl("initializeRiboteExportBridge", main_code, fixed = TRUE))
stopifnot(grepl("ribote-preprocess-figure-export", bridge_code, fixed = TRUE))
stopifnot(grepl("ribote-preprocess-qc-export", bridge_code, fixed = TRUE))
stopifnot(grepl("ribote-archive-export", bridge_code, fixed = TRUE))
stopifnot(grepl("JSZip", bridge_code, fixed = TRUE))
stopifnot(grepl("ribote-text-export", bridge_code, fixed = TRUE))
stopifnot(grepl("if (!ready)", component_code, fixed = TRUE))
stopifnot(grepl("Accordion", component_code, fixed = TRUE))
stopifnot(grepl("defaultExpanded", component_code, fixed = TRUE))
stopifnot(grepl("expandIcon", component_code, fixed = TRUE))
stopifnot(grepl("currentView !== \"data\"", component_code, fixed = TRUE))
stopifnot(grepl("MenuItem", component_code, fixed = TRUE))
stopifnot(grepl("riboteSelectFieldSx", component_code, fixed = TRUE))
stopifnot(grepl("fontSize: \"1.08rem\"", component_code, fixed = TRUE))
stopifnot(grepl("ribote-control-section--export-data-plain", component_code, fixed = TRUE))
stopifnot(grepl("ribote-control-section--export-figure-plain", component_code, fixed = TRUE))
stopifnot(grepl("data-rnameta-lock-during-analysis", component_code, fixed = TRUE))
stopifnot(grepl("exportPadding", bridge_code, fixed = TRUE))
stopifnot(grepl("rnameta:action-lock", bridge_code, fixed = TRUE))
stopifnot(grepl("rnameta:action-unlock", bridge_code, fixed = TRUE))
stopifnot(grepl("identical\\(active_view, \"barplot\"\\)", server_code))
stopifnot(grepl("identical\\(active_view, \"qc\"\\)", server_code))
stopifnot(grepl("data_preprocess_chart_export_entries", server_code, fixed = TRUE))
stopifnot(grepl("ribote-text-export", server_code, fixed = TRUE))
stopifnot(grepl("library_size_data_", server_code, fixed = TRUE))
stopifnot(!grepl("data_preprocess_archive_filename\\(\"data\"\\)", server_code))
stopifnot(grepl("filename = data_preprocess_data_export_filename\\(extension\\)", server_code))
stopifnot(grepl("ribote-d3-tooltip__key\">Actual:", results_code, fixed = TRUE))
stopifnot(grepl("groupLabel: \"Type\"", results_code, fixed = TRUE))
stopifnot(grepl("showActualLabel: false", results_code, fixed = TRUE))
stopifnot(grepl("groupLabel: \"Class\"", results_code, fixed = TRUE))
stopifnot(grepl("ribote-d3-tooltip__key\">Count:", results_code, fixed = TRUE))
stopifnot(grepl("ribote-d3-tooltip__key\">Fraction:", results_code, fixed = TRUE))
stopifnot(grepl("ribote-d3-tooltip__row", results_code, fixed = TRUE))
stopifnot(grepl("tickSizeOuter\\(0\\)", results_code))
stopifnot(grepl("const EXPORT_BACKGROUND = \"#ffffff\"", bridge_code, fixed = TRUE))
stopifnot(grepl("const EXPORT_BACKGROUND = \"#ffffff\"", motif_export_code, fixed = TRUE))

vector_pdf_code <- paste(
  readLines("new/frontend/app_shell/src/bridge/svgVectorPdf.js", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

stopifnot(grepl("window.getComputedStyle", vector_pdf_code, fixed = TRUE))
stopifnot(grepl("inlineComputedSvgTextStyles", vector_pdf_code, fixed = TRUE))
stopifnot(grepl("computed.textAnchor", vector_pdf_code, fixed = TRUE))
stopifnot(grepl("computed.dominantBaseline", vector_pdf_code, fixed = TRUE))

cat("ribote data preprocess export test passed\n")
