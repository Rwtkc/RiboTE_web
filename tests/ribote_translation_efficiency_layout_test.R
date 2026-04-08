source("new/global.R", local = TRUE, encoding = "UTF-8")
source("new/shared/react_bridge.R", local = TRUE, encoding = "UTF-8")
source("new/shared/module_shell.R", local = TRUE, encoding = "UTF-8")
source("new/modules/translation_efficiency/translation_efficiency.shared.R", local = TRUE, encoding = "UTF-8")
source("new/modules/translation_efficiency/translation_efficiency.ui.R", local = TRUE, encoding = "UTF-8")

te_shared <- paste(
  readLines("new/modules/translation_efficiency/translation_efficiency.shared.R", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

te_ui <- paste(
  readLines("new/modules/translation_efficiency/translation_efficiency.ui.R", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

main_code <- paste(
  readLines("new/frontend/app_shell/src/main.jsx", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

bridge_code <- paste(
  readLines("new/frontend/app_shell/src/bridge/exportBridge.js", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

te_results_code <- paste(
  readLines("new/frontend/app_shell/src/components/TranslationEfficiencyResults.jsx", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

te_export_code <- paste(
  readLines("new/frontend/app_shell/src/components/TranslationEfficiencyExport.jsx", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

te_css_code <- paste(
  readLines("new/www/css/ribote-modules.css", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

te_controls_css_code <- paste(
  readLines("new/frontend/app_shell/src/styles/ribote-controls.css", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

te_markup <- as.character(mod_translation_efficiency_ui("translation_efficiency"))

stopifnot(grepl("show_export_panel = TRUE", te_shared, fixed = TRUE))
stopifnot(grepl("show_analysis_panel = TRUE", te_shared, fixed = TRUE))
stopifnot(grepl("wide_sidebar = TRUE", te_shared, fixed = TRUE))
stopifnot(grepl("translation_efficiency_export_config", te_shared, fixed = TRUE))
stopifnot(grepl("translation_efficiency_results_payload", te_shared, fixed = TRUE))
stopifnot(grepl("Fold Change", te_shared, fixed = TRUE))
stopifnot(grepl("\"P-value\"", te_shared, fixed = TRUE))
stopifnot(grepl("\"padj\"", te_shared, fixed = TRUE))
stopifnot(grepl("\"pvalue\"", te_shared, fixed = TRUE))
stopifnot(grepl("ribote-workspace--wide-sidebar", te_markup, fixed = TRUE))
stopifnot(grepl("ribote-translation-export", te_ui, fixed = TRUE))
stopifnot(grepl("ribote-translation-results", te_ui, fixed = TRUE))
stopifnot(grepl("TranslationEfficiencyResults", main_code, fixed = TRUE))
stopifnot(grepl("TranslationEfficiencyExport", main_code, fixed = TRUE))
stopifnot(grepl("ribote-translation-results", main_code, fixed = TRUE))
stopifnot(grepl("ribote-translation-export", main_code, fixed = TRUE))
stopifnot(grepl("ribote-translation-figure-export", bridge_code, fixed = TRUE))
stopifnot(grepl("ribote-translation-multi-figure-export", bridge_code, fixed = TRUE))
stopifnot(grepl("ribote-translation-results", te_results_code, fixed = TRUE))
stopifnot(grepl("TE Volcano Plot", te_results_code, fixed = TRUE))
stopifnot(grepl("TE Scatter Plots", te_results_code, fixed = TRUE))
stopifnot(grepl("d3", te_results_code, fixed = TRUE))
stopifnot(grepl("renderReadyInputId", te_results_code, fixed = TRUE))
stopifnot(grepl("renderToken", te_results_code, fixed = TRUE))
stopifnot(!grepl("data-rnameta-control=\\\"ribote-translation-results\\\"", te_results_code))
stopifnot(!grepl("TE Status Summary", te_results_code, fixed = TRUE))
stopifnot(!grepl("statusRef", te_results_code, fixed = TRUE))
stopifnot(!grepl("No chart data available.", te_results_code, fixed = TRUE))
stopifnot(grepl("Preparing volcano plot", te_results_code, fixed = TRUE))
stopifnot(grepl("Preparing scatter plots", te_results_code, fixed = TRUE))
stopifnot(grepl("RNA Expression by Group", te_results_code, fixed = TRUE))
stopifnot(grepl("Translation Efficiency by Group", te_results_code, fixed = TRUE))
stopifnot(grepl("RNA vs TE Fold Change", te_results_code, fixed = TRUE))
stopifnot(grepl("RNA_log2FC", te_results_code, fixed = TRUE))
stopifnot(grepl("TE_log2FC", te_results_code, fixed = TRUE))
stopifnot(grepl("\\{\\/\\* Temporary hide RNA vs Ribo Fold Change", te_results_code))
scatter_order_te <- regexpr("ref=\\{teExpressionScatterRef\\}", te_results_code)[[1]]
scatter_order_input <- regexpr("ref=\\{inputScatterRef\\}", te_results_code)[[1]]
scatter_order_te_fc <- regexpr("ref=\\{scatterTeRef\\}", te_results_code)[[1]]
stopifnot(scatter_order_te < scatter_order_input)
stopifnot(scatter_order_input < scatter_order_te_fc)
stopifnot(grepl("scaleLog\\(\\)", te_results_code))
stopifnot(grepl("base\\(2\\)", te_results_code))
stopifnot(grepl("scaleType: \"log2\"", te_results_code, fixed = TRUE))
stopifnot(grepl("drawMarginalDensityScatterChart", te_results_code, fixed = TRUE))
stopifnot(grepl("kernelDensityEstimator", te_results_code, fixed = TRUE))
stopifnot(grepl("kernelEpanechnikov", te_results_code, fixed = TRUE))
stopifnot(grepl("curveBasis", te_results_code, fixed = TRUE))
stopifnot(grepl("clipPath", te_results_code, fixed = TRUE))
stopifnot(grepl("clip-path", te_results_code, fixed = TRUE))
stopifnot(grepl("domain\\(\\[-4, 4\\]\\)", te_results_code))
stopifnot(!grepl("d3.bin\\(", te_results_code))
stopifnot(grepl("thresholds", te_results_code, fixed = TRUE))
stopifnot(grepl("stroke-dasharray", te_results_code, fixed = TRUE))
stopifnot(grepl("chartHeight", te_results_code, fixed = TRUE))
stopifnot(grepl("chartHeight: 600", te_results_code, fixed = TRUE))
stopifnot(grepl(".attr\\(\"y\", 12\\)", te_results_code))
stopifnot(grepl("buildLegendCounts", te_results_code, fixed = TRUE))
stopifnot(grepl("legendCounts", te_results_code, fixed = TRUE))
stopifnot(grepl("text(`${item} (${legendCounts[item] ?? 0})`)", te_results_code, fixed = TRUE))
stopifnot(grepl("marginTop", te_results_code, fixed = TRUE))
stopifnot(grepl("incomingVolcanoData", te_results_code, fixed = TRUE))
stopifnot(grepl("cachedVolcanoData", te_results_code, fixed = TRUE))
stopifnot(grepl("setCachedVolcanoData", te_results_code, fixed = TRUE))
stopifnot(grepl("shallowObjectArrayEqual", te_results_code, fixed = TRUE))
stopifnot(grepl("const volcanoData = cachedVolcanoData.length > 0 \\? cachedVolcanoData : incomingVolcanoData;", te_results_code))
stopifnot(grepl("const scatterRpfData = cachedScatterRpfData.length > 0 \\? cachedScatterRpfData : incomingScatterRpfData;", te_results_code))
stopifnot(grepl("const scatterTeData = cachedScatterTeData.length > 0 \\? cachedScatterTeData : incomingScatterTeData;", te_results_code))
stopifnot(grepl("incomingInputScatterData", te_results_code, fixed = TRUE))
stopifnot(grepl("incomingTeExpressionScatterData", te_results_code, fixed = TRUE))
stopifnot(grepl("cachedInputScatterData", te_results_code, fixed = TRUE))
stopifnot(grepl("cachedTeExpressionScatterData", te_results_code, fixed = TRUE))
stopifnot(grepl("shallowObjectArrayEqual\\(currentData, incomingVolcanoData\\)", te_results_code))
stopifnot(grepl("shallowObjectArrayEqual\\(currentData, incomingScatterTeData\\)", te_results_code))
stopifnot(grepl("shallowObjectArrayEqual\\(currentData, incomingInputScatterData\\)", te_results_code))
stopifnot(grepl("shallowObjectArrayEqual\\(currentData, incomingTeExpressionScatterData\\)", te_results_code))
stopifnot(grepl("if \\(renderToken\\)", te_results_code))
stopifnot(grepl("backgroundColor \\|\\| EXPORT_BACKGROUND", bridge_code))
stopifnot(grepl("fill\", backgroundColor", bridge_code))
stopifnot(grepl("async function renderHostToCanvas\\(", bridge_code))
stopifnot(grepl("backgroundColor = EXPORT_BACKGROUND", bridge_code))
stopifnot(grepl("return renderSvgToCanvas\\(\\{ svgElement, width, height, dpi, exportPadding, backgroundColor \\}\\)", bridge_code))
stopifnot(!grepl("renderSvgToCanvas\\(\\{ svgElement, width, height, dpi, exportPadding, backgroundColor: options\\.backgroundColor \\}\\)", bridge_code))
stopifnot(!grepl("backgroundColor: options\\.backgroundColor \\|\\| EXPORT_BACKGROUND", bridge_code))
stopifnot(grepl(".ribote-translation-results", te_css_code, fixed = TRUE))
stopifnot(grepl(".ribote-translation-panel", te_css_code, fixed = TRUE))
stopifnot(grepl("display: grid;", te_css_code, fixed = TRUE))
stopifnot(grepl("gap: 1rem;", te_css_code, fixed = TRUE))
stopifnot(grepl(".ribote-d3-loading", te_css_code, fixed = TRUE))
stopifnot(grepl("Accordion", te_export_code, fixed = TRUE))
stopifnot(grepl("Export Figure", te_export_code, fixed = TRUE))
stopifnot(grepl("Export Data", te_export_code, fixed = TRUE))
stopifnot(grepl("ribote-control-section--export-data-plain", te_export_code, fixed = TRUE))
stopifnot(grepl("ribote-control-section--export-figure-plain", te_export_code, fixed = TRUE))
stopifnot(grepl(".ribote-control-section--export-figure-plain", te_controls_css_code, fixed = TRUE))

cat("ribote translation efficiency layout test passed\n")
