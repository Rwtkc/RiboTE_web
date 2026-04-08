module_css <- paste(
  readLines("new/www/css/ribote-modules.css", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

te_results_code <- paste(
  readLines("new/frontend/app_shell/src/components/TranslationEfficiencyResults.jsx", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

stopifnot(grepl("ribote-table-wrap", te_results_code, fixed = TRUE))
stopifnot(grepl("ribote-table-wrap--te", te_results_code, fixed = TRUE))
stopifnot(grepl("ribote-preview-table--te", te_results_code, fixed = TRUE))
stopifnot(grepl("\\.ribote-table-wrap \\{", module_css))
stopifnot(grepl("max-width: 100%;", module_css, fixed = TRUE))
stopifnot(grepl("overflow-x: auto;", module_css, fixed = TRUE))
stopifnot(grepl("\\.ribote-table-wrap--te \\{", module_css))
stopifnot(grepl("overscroll-behavior-x: contain;", module_css, fixed = TRUE))
stopifnot(grepl("\\.ribote-preview-table--te \\{", module_css))
stopifnot(grepl("width: max-content;", module_css, fixed = TRUE))
stopifnot(grepl("\\.ribote-results-root \\{", module_css))

cat("ribote translation efficiency table overflow test passed\n")
