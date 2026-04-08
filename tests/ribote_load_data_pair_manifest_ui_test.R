load_server <- paste(
  readLines("new/modules/load_data/load_data.server.R", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

load_controls <- paste(
  readLines("new/frontend/app_shell/src/components/LoadDataControls.jsx", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

stopifnot(grepl("sample_type_manifest", load_server, fixed = TRUE))
stopifnot(grepl("pair_manifest", load_server, fixed = TRUE))
stopifnot(grepl("Configure Samples", load_controls, fixed = TRUE))
stopifnot(grepl("group_role", load_controls, fixed = TRUE))
stopifnot(grepl("Next step: click Configure Samples", load_controls, fixed = TRUE))

cat("ribote load data pair manifest ui test passed\n")
