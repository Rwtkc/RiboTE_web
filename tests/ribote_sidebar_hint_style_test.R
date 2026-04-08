module_shell <- paste(
  readLines("new/shared/module_shell.R", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

module_css <- paste(
  readLines("new/www/css/ribote-modules.css", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

stopifnot(grepl('class = "ribote-hint"', module_shell, fixed = TRUE))
stopifnot(grepl("outputOptions\\(output, \"run_hint\", suspendWhenHidden = FALSE\\)", module_shell))
stopifnot(grepl(".ribote-hint", module_css, fixed = TRUE))
stopifnot(grepl("border", module_css, fixed = TRUE))
stopifnot(grepl("background", module_css, fixed = TRUE))
stopifnot(grepl("#875642", module_css, fixed = TRUE))

cat("ribote sidebar hint style test passed\n")
