load_ui <- paste(
  readLines("new/modules/load_data/load_data.ui.R", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

load_server <- paste(
  readLines("new/modules/load_data/load_data.server.R", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

module_css <- paste(
  readLines("new/www/css/ribote-modules.css", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

stopifnot(!grepl("ribote-panel--canvas", load_ui, fixed = TRUE))
stopifnot(!grepl("workflow_notes", load_ui, fixed = TRUE))
stopifnot(!grepl("detail_panel_eyebrow", load_ui, fixed = TRUE))
stopifnot(grepl("Saved file preview \\(first 10 rows\\)", load_server))
stopifnot(grepl("Saved File", load_server, fixed = TRUE))
stopifnot(grepl("input\\$gene_matrix\\$datapath", load_server))
stopifnot(grepl("\\.ribote-summary-grid--load-data \\{", module_css))
stopifnot(grepl("grid-template-columns: repeat\\(3, minmax\\(0, 1fr\\)\\);", module_css))
stopifnot(grepl("margin-top: 0.5rem;", module_css, fixed = TRUE))
stopifnot(grepl("\\.ribote-summary-item__label \\{", module_css))
stopifnot(grepl("font-size: 1rem;", module_css, fixed = TRUE))
stopifnot(grepl("letter-spacing: 0.04em;", module_css, fixed = TRUE))
stopifnot(grepl("\\.ribote-summary-item__value \\{", module_css))
stopifnot(grepl("font-size: 1.1rem;", module_css, fixed = TRUE))
stopifnot(grepl("\\.ribote-preview__header \\{", module_css))
stopifnot(grepl("align-items: flex-end;", module_css, fixed = TRUE))
stopifnot(grepl("flex-wrap: nowrap;", module_css, fixed = TRUE))
stopifnot(grepl("\\.ribote-preview \\{", module_css))
stopifnot(grepl("margin-top: 1rem;", module_css, fixed = TRUE))
stopifnot(grepl("\\.ribote-preview__title \\{", module_css))
stopifnot(grepl("font-size: 1.15rem;", module_css, fixed = TRUE))
stopifnot(grepl("\\.ribote-preview__file-name \\{", module_css))
stopifnot(grepl("font-size: 1.05rem;", module_css, fixed = TRUE))
stopifnot(grepl("white-space: nowrap;", module_css, fixed = TRUE))
stopifnot(grepl("align-self: flex-end;", module_css, fixed = TRUE))
stopifnot(grepl("\\.ribote-preview-table th,", module_css))
stopifnot(grepl("font-size: 1.3rem;", module_css, fixed = TRUE))
stopifnot(grepl("text-align: center;", module_css, fixed = TRUE))

cat("ribote load data single panel test passed\n")
