load_ui <- paste(
  readLines("new/modules/load_data/load_data.ui.R", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

load_server <- paste(
  readLines("new/modules/load_data/load_data.server.R", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

global_r <- paste(
  readLines("new/global.R", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

module_css <- paste(
  readLines("new/www/css/ribote-modules.css", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

stopifnot(grepl("session_summary", load_ui, fixed = TRUE))
stopifnot(grepl("Saved file preview \\(first 10 rows\\)", load_server))
stopifnot(grepl("app_data_path", global_r, fixed = TRUE))
stopifnot(grepl("app_data_path\\(\"all.count.txt\"\\)", load_server))
stopifnot(grepl("head\\(", load_server))
stopifnot(grepl("ribote-preview-table", load_server, fixed = TRUE))
stopifnot(grepl("ribote-preview-table", module_css, fixed = TRUE))

cat("ribote load data demo preview test passed\n")
