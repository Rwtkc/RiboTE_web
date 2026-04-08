codon_server_code <- paste(
  readLines("new/modules/codon/codon.server.R", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

codon_results_code <- paste(
  readLines("new/frontend/app_shell/src/components/CodonResults.jsx", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

codon_export_code <- paste(
  readLines("new/frontend/app_shell/src/components/CodonExport.jsx", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

css_code <- paste(
  readLines("new/www/css/ribote-modules.css", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

stopifnot(grepl("summaryHostId = session\\$ns\\(\"analysis_summary_shell\"\\)", codon_server_code))
stopifnot(!grepl("if \\(!identical\\(active_group\\(\\), \"Input and Usage\"\\)\\) \\{[[:space:]]*return\\(NULL\\)[[:space:]]*\\}", codon_server_code))
stopifnot(grepl("activeViewInputId = session\\$ns\\(\"result_view\"\\)", codon_server_code))
stopifnot(grepl("readyByView = ready_by_view", codon_server_code, fixed = TRUE))
stopifnot(grepl("window\\.dispatchEvent\\(new CustomEvent\\(\"ribote:active-view-change\"", codon_results_code))
stopifnot(grepl("const \\[renderConfig, setRenderConfig\\] = useState\\(\\(\\) => \\(config\\?\\.ready \\? config : null\\)\\);", codon_export_code))
stopifnot(grepl("const \\[isVisible, setIsVisible\\] = useState\\(\\(\\) => Boolean\\(config\\?\\.ready\\)\\);", codon_export_code))
stopifnot(grepl("const \\[desiredView, setDesiredView\\] = useState\\(\\(\\) => readPersistedActiveView\\(persistedActiveViewKey\\)\\);", codon_export_code))
stopifnot(grepl("const shouldHideStaleExport = Boolean\\(desiredView && desiredView !== config\\?\\.currentView && desiredViewHasData === false\\);", codon_export_code))
stopifnot(grepl("window\\.addEventListener\\(\"ribote:active-view-change\", handleActiveViewChange\\)", codon_export_code))
stopifnot(grepl("if \\(shouldHideStaleExport\\) \\{[[:space:]]*setRenderConfig\\(null\\);[[:space:]]*setIsVisible\\(false\\);", codon_export_code))
stopifnot(grepl("data-ribote-export-ready=\\{isVisible \\? \"true\" : \"false\"\\}", codon_export_code))
stopifnot(grepl("\\.ribote-export-shell \\{", css_code))
stopifnot(grepl("\\.ribote-export-shell\\[data-ribote-export-ready=\"false\"\\] \\{", css_code))
stopifnot(grepl("\\.ribote-codon-analysis-summary-shell \\{", css_code))
stopifnot(grepl("\\.ribote-codon-analysis-summary-shell\\[data-ribote-codon-active-group\\]:not\\(\\[data-ribote-codon-active-group=\"Input and Usage\"\\]\\) \\{", css_code))

cat("ribote codon summary export shell test passed\n")
