library(shiny)
library(htmltools)
library(jsonlite)

options(
  shiny.autoreload = TRUE,
  shiny.fullstacktrace = TRUE
)

resolve_app_root <- function() {
  candidates <- c(
    normalizePath(".", winslash = "/", mustWork = TRUE),
    normalizePath("./new", winslash = "/", mustWork = FALSE)
  )

  matches <- candidates[vapply(
    candidates,
    function(path) {
      dir.exists(file.path(path, "modules", "welcome"))
    },
    logical(1)
  )]

  if (length(matches) == 0) {
    stop("Unable to locate the refactored RiboTE app root.")
  }

  matches[[1]]
}

app_root <- resolve_app_root()

app_path <- function(...) {
  file.path(app_root, ...)
}

resolve_data_root <- function() {
  candidates <- c(
    file.path(app_root, "TEShinyData"),
    file.path(dirname(app_root), "TEShinyData")
  )

  matches <- candidates[vapply(candidates, dir.exists, logical(1))]

  if (length(matches) == 0) {
    stop("Unable to locate TEShinyData for the refactored RiboTE app.")
  }

  normalizePath(matches[[1]], winslash = "/", mustWork = TRUE)
}

data_root <- resolve_data_root()

app_data_path <- function(...) {
  file.path(data_root, ...)
}
