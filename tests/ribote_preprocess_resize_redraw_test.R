component_code <- paste(
  readLines("new/frontend/app_shell/src/components/PreprocessResults.jsx", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

stopifnot(grepl("function useD3Chart\\(", component_code))
stopifnot(grepl("ResizeObserver", component_code, fixed = TRUE))
stopifnot(grepl("requestAnimationFrame", component_code, fixed = TRUE))
stopifnot(grepl("scheduleRender\\(\"resize\"\\);", component_code))
stopifnot(grepl("animate: reason !== \"resize\"", component_code, fixed = TRUE))
stopifnot(grepl("if \\(shouldAnimate\\)", component_code))

cat("ribote preprocess resize redraw test passed\n")
