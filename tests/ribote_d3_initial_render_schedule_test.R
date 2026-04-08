te_results_code <- paste(
  readLines("new/frontend/app_shell/src/components/TranslationEfficiencyResults.jsx", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

preprocess_results_code <- paste(
  readLines("new/frontend/app_shell/src/components/PreprocessResults.jsx", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

stopifnot(grepl("scheduleRender\\(\"initial\"\\);", te_results_code))
stopifnot(grepl("scheduleRender\\(\"resize\"\\);", te_results_code))
stopifnot(grepl("animate: reason !== \"resize\"", te_results_code, fixed = TRUE))
stopifnot(!grepl("\\n\\s*renderChart\\(\\);\\n\\s*let observer;", te_results_code))
stopifnot(grepl("scheduleRender\\(\"initial\"\\);", preprocess_results_code))
stopifnot(grepl("scheduleRender\\(\"resize\"\\);", preprocess_results_code))
stopifnot(grepl("animate: reason !== \"resize\"", preprocess_results_code, fixed = TRUE))
stopifnot(!grepl("\\n\\s*renderChart\\(\\);\\n\\s*let observer;", preprocess_results_code))

cat("ribote d3 initial render schedule test passed\n")
