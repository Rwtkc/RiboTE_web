module_css <- paste(
  readLines("new/www/css/ribote-modules.css", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

control_css <- paste(
  readLines("new/frontend/app_shell/src/styles/ribote-controls.css", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

load_controls <- paste(
  readLines("new/frontend/app_shell/src/components/LoadDataControls.jsx", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

module_controls <- paste(
  readLines("new/frontend/app_shell/src/components/ModuleControls.jsx", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

stopifnot(grepl("grid-template-columns: minmax\\(24rem, 30rem\\) minmax\\(0, 1fr\\);", module_css))
stopifnot(grepl("\\.ribote-file-picker__field", control_css))
stopifnot(grepl("\\.ribote-control-card--load-data \\.ribote-control-section \\{", control_css))
stopifnot(grepl("border: 0;", control_css, fixed = TRUE))
stopifnot(grepl("\\.ribote-control-card--load-data \\.ribote-field__label \\{", control_css))
stopifnot(grepl("font-size: 1.4rem;", control_css, fixed = TRUE))
stopifnot(grepl("className=\"ribote-control-card ribote-control-card--load-data\"", load_controls, fixed = TRUE))
stopifnot(grepl("<Select", load_controls, fixed = TRUE))
stopifnot(grepl("className=\"ribote-file-picker__field\"", load_controls, fixed = TRUE))
stopifnot(grepl("<Select", module_controls, fixed = TRUE))

cat("ribote load data polish test passed\n")
