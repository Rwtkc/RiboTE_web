module_css <- paste(
  readLines("new/www/css/ribote-modules.css", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

stopifnot(grepl("\\.ribote-d3-tooltip__title \\{", module_css))
stopifnot(grepl("width: max-content;", module_css, fixed = TRUE))
stopifnot(grepl("max-width: calc(100vw - 2rem);", module_css, fixed = TRUE))
stopifnot(grepl("white-space: nowrap;", module_css, fixed = TRUE))
stopifnot(grepl("overflow: hidden;", module_css, fixed = TRUE))
stopifnot(grepl("text-overflow: ellipsis;", module_css, fixed = TRUE))

cat("ribote qc tooltip overflow test passed\n")
