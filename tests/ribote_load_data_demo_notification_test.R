server_code <- paste(
  readLines("new/modules/load_data/load_data.server.R", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

stopifnot(!grepl("Demo matrix loaded with sample pairing\\. Downstream modules are ready\\.", server_code))

cat("ribote load data demo notification test passed\n")
