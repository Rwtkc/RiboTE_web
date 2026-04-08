load_controls <- paste(
  readLines("new/frontend/app_shell/src/components/LoadDataControls.jsx", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

module_controls <- paste(
  readLines("new/frontend/app_shell/src/components/ModuleControls.jsx", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

theme_file <- paste(
  readLines("new/frontend/app_shell/src/components/shared/riboteSelectTheme.js", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

stopifnot(grepl("@mui/material", load_controls, fixed = TRUE))
stopifnot(grepl("<Select", load_controls, fixed = TRUE))
stopifnot(!grepl("<select", load_controls, fixed = TRUE))
stopifnot(grepl("@mui/material", module_controls, fixed = TRUE))
stopifnot(grepl("<Select", module_controls, fixed = TRUE))
stopifnot(grepl("riboteSelectMenuPaperSx", theme_file, fixed = TRUE))
stopifnot(grepl("MuiMenuItem-root.Mui-selected", theme_file, fixed = TRUE))

cat("ribote select component test passed\n")
