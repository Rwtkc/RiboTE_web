te_results_code <- paste(
  c(
    readLines("new/frontend/app_shell/src/components/TranslationEfficiencyResults.jsx", warn = FALSE, encoding = "UTF-8"),
    readLines("new/frontend/app_shell/src/components/TranslationEfficiencyCharts.jsx", warn = FALSE, encoding = "UTF-8")
  ),
  collapse = "\n"
)

stopifnot(grepl("function toFiniteNumber", te_results_code, fixed = TRUE))
stopifnot(grepl("function filterScatterDataForScale", te_results_code, fixed = TRUE))
stopifnot(grepl("scaleType === \"log2\"", te_results_code, fixed = TRUE))
stopifnot(grepl("item.x > 0 && item.y > 0", te_results_code, fixed = TRUE))
stopifnot(grepl(".data(plotData)", te_results_code, fixed = TRUE))
stopifnot(!grepl(".data(data)\n    .enter()\n    .append(\"circle\")", te_results_code, fixed = TRUE))
stopifnot(!grepl("Number(item[xKey]) || 0", te_results_code, fixed = TRUE))
stopifnot(!grepl("Number(item[yKey]) || 0", te_results_code, fixed = TRUE))

cat("ribote translation efficiency chart domain guard test passed\n")
