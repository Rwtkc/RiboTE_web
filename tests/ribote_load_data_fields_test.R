load_controls <- paste(
  readLines("new/frontend/app_shell/src/components/LoadDataControls.jsx", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

load_server <- paste(
  readLines("new/modules/load_data/load_data.server.R", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

stopifnot(!grepl("RNA-seq Samples", load_controls, fixed = TRUE))
stopifnot(!grepl("Ribo-seq Samples", load_controls, fixed = TRUE))
stopifnot(!grepl("inputNames.trim\\(\\) !== \"\"", load_controls))
stopifnot(!grepl("rpfNames.trim\\(\\) !== \"\"", load_controls))
stopifnot(!grepl("RNA-seq Samples", load_server, fixed = TRUE))
stopifnot(!grepl("Ribo-seq Samples", load_server, fixed = TRUE))

cat("ribote load data fields test passed\n")
