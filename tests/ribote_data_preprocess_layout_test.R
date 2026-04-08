module_shell <- paste(
  readLines("new/shared/module_shell.R", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

module_css <- paste(
  readLines("new/www/css/ribote-modules.css", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

preprocess_shared <- paste(
  readLines("new/modules/data_preprocess/data_preprocess.shared.R", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

preprocess_ui <- paste(
  readLines("new/modules/data_preprocess/data_preprocess.ui.R", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

stopifnot(grepl("show_export_panel", module_shell, fixed = TRUE))
stopifnot(grepl("show_analysis_panel", module_shell, fixed = TRUE))
stopifnot(grepl("blank_analysis_panel", module_shell, fixed = TRUE))
stopifnot(grepl("wide_sidebar", module_shell, fixed = TRUE))
stopifnot(grepl("snapshot_columns", module_shell, fixed = TRUE))
stopifnot(grepl("ribote-workspace--wide-sidebar", module_css, fixed = TRUE))
stopifnot(grepl("ribote-summary-grid--cols-3", module_css, fixed = TRUE))
stopifnot(grepl("ribote-analysis-blank", module_css, fixed = TRUE))
stopifnot(grepl("show_analysis_panel = TRUE", preprocess_shared, fixed = TRUE))
stopifnot(grepl("blank_analysis_panel = TRUE", preprocess_shared, fixed = TRUE))
stopifnot(grepl("wide_sidebar = TRUE", preprocess_shared, fixed = TRUE))
stopifnot(grepl("snapshot_columns = 3", preprocess_shared, fixed = TRUE))
stopifnot(grepl("data_preprocess_export_config", preprocess_shared, fixed = TRUE))
stopifnot(grepl("ribote-preprocess-export", preprocess_ui, fixed = TRUE))

cat("ribote data preprocess layout test passed\n")
