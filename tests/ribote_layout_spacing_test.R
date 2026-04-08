css <- paste(
  readLines("new/www/css/ribote-modules.css", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

stopifnot(grepl("padding: 0.5rem 0.75rem 1.5rem;", css, fixed = TRUE))
stopifnot(grepl("gap: 1rem;", css, fixed = TRUE))

cat("ribote layout spacing test passed\n")
