translation_efficiency_export_defaults <- function() {
  list(
    format = "png",
    width = 3000,
    height = 1800,
    dpi = 300,
    data_format = "csv"
  )
}

translation_efficiency_export_config <- function(id) {
  ns <- NS(id)
  defaults <- translation_efficiency_export_defaults()

  list(
    ids = list(
      format = ns("export_format"),
      width = ns("export_width"),
      height = ns("export_height"),
      dpi = ns("export_dpi"),
      trigger = ns("export_plot"),
      dataFormat = ns("data_export_format"),
      dataTrigger = ns("data_export")
    ),
    defaults = defaults,
    choices = list(
      format = list(
        list(label = "PNG", value = "png"),
        list(label = "PDF", value = "pdf")
      ),
      dataFormat = list(
        list(label = "CSV", value = "csv"),
        list(label = "TXT (tab-delimited)", value = "txt")
      )
    ),
    sectioned = TRUE
  )
}

translation_efficiency_export_settings <- function(input) {
  defaults <- translation_efficiency_export_defaults()

  width <- suppressWarnings(as.numeric(input$export_width))
  height <- suppressWarnings(as.numeric(input$export_height))
  dpi <- suppressWarnings(as.numeric(input$export_dpi))
  format <- tolower(if (is.null(input$export_format) || identical(input$export_format, "")) defaults$format else input$export_format)
  data_format <- tolower(if (is.null(input$data_export_format) || identical(input$data_export_format, "")) defaults$data_format else input$data_export_format)

  list(
    format = if (format %in% c("png", "pdf")) format else defaults$format,
    width = if (is.finite(width) && width > 0) width else defaults$width,
    height = if (is.finite(height) && height > 0) height else defaults$height,
    dpi = if (is.finite(dpi) && dpi > 0) dpi else defaults$dpi,
    data_format = if (data_format %in% c("csv", "txt")) data_format else defaults$data_format
  )
}

translation_efficiency_data_export_filename <- function(extension) {
  sprintf("translation_efficiency_data_%s.%s", format(Sys.Date(), "%Y%m%d"), extension)
}

translation_efficiency_archive_filename <- function(kind = "export") {
  safe_kind <- gsub("[^a-z0-9]+", "_", tolower(as.character(kind)))
  sprintf("translation_efficiency_%s_%s.zip", safe_kind, format(Sys.Date(), "%Y%m%d"))
}

translation_efficiency_table_export_content <- function(data_frame, format = "csv") {
  stopifnot(is.data.frame(data_frame))

  connection <- textConnection("export_lines", "w", local = TRUE)
  on.exit(close(connection), add = TRUE)

  utils::write.table(
    data_frame,
    file = connection,
    sep = if (identical(format, "txt")) "\t" else ",",
    row.names = FALSE,
    col.names = TRUE,
    quote = TRUE
  )

  paste(export_lines, collapse = "\n")
}

translation_efficiency_data_export_content <- function(te_context, format = "csv") {
  stopifnot(is.data.frame(te_context$result_table))
  result_table <- translation_efficiency_result_table_labels(te_context$result_table)
  result_table <- translation_efficiency_round_result_table(result_table, digits = 4L)
  result_table <- translation_efficiency_reorder_result_table(result_table)
  translation_efficiency_table_export_content(result_table, format = format)
}

translation_efficiency_chart_export_entries <- function(te_context, view = "volcano", format = "csv") {
  extension <- if (identical(format, "txt")) "txt" else "csv"
  view_key <- tolower(as.character(view))

  if (identical(view_key, "volcano")) {
    return(list(
      list(
        filename = sprintf("translation_efficiency_volcano_points.%s", extension),
        content = translation_efficiency_table_export_content(te_context$volcano_points, format = format)
      )
    ))
  }

  if (identical(view_key, "scatter")) {
    return(list(
      list(
        filename = sprintf("translation_efficiency_scatter_te_expression.%s", extension),
        content = translation_efficiency_table_export_content(te_context$scatter_te_expression, format = format)
      ),
      list(
        filename = sprintf("translation_efficiency_scatter_input_expression.%s", extension),
        content = translation_efficiency_table_export_content(te_context$scatter_input, format = format)
      ),
      list(
        filename = sprintf("translation_efficiency_scatter_rna_vs_te.%s", extension),
        content = translation_efficiency_table_export_content(te_context$scatter_te, format = format)
      )
    ))
  }

  stop(sprintf("Unsupported Translation Efficiency export view: %s", view_key), call. = FALSE)
}

