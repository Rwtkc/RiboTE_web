module_css <- paste(
  readLines("new/www/css/ribote-modules.css", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

control_css <- paste(
  readLines("new/frontend/app_shell/src/styles/ribote-controls.css", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

stopifnot(grepl("font-size: 1.3rem;", module_css, fixed = TRUE))
stopifnot(grepl("font-size: 1.4rem;", control_css, fixed = TRUE))
stopifnot(grepl("font-size: 1.1rem;", control_css, fixed = TRUE))
stopifnot(grepl(".ribote-field__label {\n  color: var(--rm-text);\n  font-size: 1.1rem;", control_css, fixed = TRUE))
stopifnot(grepl(".ribote-input,\n.ribote-select,\n.ribote-textarea {\n  width: 100%;", control_css, fixed = TRUE))
stopifnot(grepl("font-size: 1.1rem;", sub("(?s)^.*?(\\.ribote-input,\\n\\.ribote-select,\\n\\.ribote-textarea \\{.*?\\n\\})[\\s\\S]*$", "\\1", control_css, perl = TRUE), fixed = TRUE))

cat("ribote typography scale test passed\n")
