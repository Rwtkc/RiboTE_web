data_preprocess_export_defaults <- function() {
  list(
    format = "png",
    width = 3000,
    height = 1800,
    dpi = 300,
    data_format = "csv"
  )
}

data_preprocess_export_config <- function(id) {
  ns <- NS(id)
  defaults <- data_preprocess_export_defaults()

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

data_preprocess_export_settings <- function(input) {
  defaults <- data_preprocess_export_defaults()

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

data_preprocess_figure_export_filename <- function(view_key, extension) {
  safe_view <- gsub("[^a-z0-9]+", "_", tolower(as.character(view_key)))
  sprintf("data_preprocess_%s_%s.%s", safe_view, format(Sys.Date(), "%Y%m%d"), extension)
}

data_preprocess_data_export_filename <- function(extension) {
  sprintf("data_preprocess_data_%s.%s", format(Sys.Date(), "%Y%m%d"), extension)
}

data_preprocess_archive_filename <- function(kind = "export") {
  safe_kind <- gsub("[^a-z0-9]+", "_", tolower(as.character(kind)))
  sprintf("data_preprocess_%s_%s.zip", safe_kind, format(Sys.Date(), "%Y%m%d"))
}

data_preprocess_data_export_content <- function(preprocess_context, format = "csv") {
  preview <- preprocess_context$preview
  stopifnot(is.data.frame(preview))

  data_preprocess_table_export_content(preview, format = format)
}

data_preprocess_table_export_content <- function(data_frame, format = "csv") {
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

data_preprocess_chart_export_content <- function(preprocess_context, view = "barplot", format = "csv") {
  view_key <- tolower(as.character(view))

  if (identical(view_key, "barplot")) {
    barplot_data <- preprocess_context$barplot_data
    stopifnot(is.data.frame(barplot_data))
    return(data_preprocess_table_export_content(barplot_data, format = format))
  }

  stop(sprintf("Unsupported chart export view: %s", view_key), call. = FALSE)
}

data_preprocess_chart_export_entries <- function(preprocess_context, format = "csv") {
  biotype_summary <- preprocess_context$biotype_summary
  rrna_summary <- preprocess_context$rrna_summary
  stopifnot(is.data.frame(biotype_summary))
  stopifnot(is.data.frame(rrna_summary))

  extension <- if (identical(format, "txt")) "txt" else "csv"

  list(
    list(
      filename = sprintf("gene_biotype_composition_data.%s", extension),
      content = data_preprocess_table_export_content(biotype_summary, format = format)
    ),
    list(
      filename = sprintf("rrna_fraction_by_sample_data.%s", extension),
      content = data_preprocess_table_export_content(rrna_summary, format = format)
    )
  )
}

data_preprocess_module_config <- function() {
  ribote_build_module_config(
    key = "data_preprocess",
    title = "Data Preprocess",
    eyebrow = "Count Matrix Conditioning",
    description = "Prepare the count matrix for downstream Translation Efficiency analysis, including missing-value handling, filtering, and quality control.",
    requires = "upload",
    run_label = "Run Preprocess",
    sections = list(
      list(
        title = "Preprocessing Parameters",
        fields = list(
          ribote_field("na_strategy", "Missing Value Estimation", "select", "Zero Imputation", options = list(ribote_choice("Zero Imputation"), ribote_choice("Median Imputation"))),
          ribote_field("min_cpm", "Min. CPM", "number", 0.5, min = 0, step = 0.1),
          ribote_field("min_libraries", "n Libraries", "number", 1, min = 1, step = 1)
        )
      )
    ),
    views = list(
      list(id = "data", title = "Data", description = "Processed count matrix after preprocessing."),
      list(id = "barplot", title = "Library Size", description = "Sequencing depth summary across samples."),
      list(id = "qc", title = "QC", description = "Gene biotype composition and rRNA fraction summaries.")
    ),
    exports = list(
      list(key = "export_plot", label = "Export Figure"),
      list(key = "data_export", label = "Export Data")
    ),
    result_metrics = list(
      list(label = "Genes Retained", value = "12,480"),
      list(label = "Samples Retained", value = "8")
    ),
    empty_message = "Save the species and count matrix information, then run preprocessing to generate the processed matrix, library size summary, and QC results.",
    wide_sidebar = TRUE,
    show_export_panel = TRUE,
    show_analysis_panel = TRUE,
    blank_analysis_panel = TRUE,
    snapshot_columns = 3
  )
}

data_preprocess_rows_payload <- function(data_frame) {
  if (is.null(data_frame) || !is.data.frame(data_frame) || nrow(data_frame) == 0) {
    return(list())
  }

  lapply(seq_len(nrow(data_frame)), function(index) {
    row_values <- data_frame[index, , drop = FALSE]

    lapply(row_values, function(value) {
      if (is.numeric(value)) {
        return(unname(value))
      }

      as.character(value)
    })
  })
}

data_preprocess_search_index <- function(data_frame) {
  if (is.null(data_frame) || !is.data.frame(data_frame) || nrow(data_frame) == 0) {
    return(character())
  }

  normalized_columns <- lapply(data_frame, function(column) {
    tolower(as.character(column))
  })

  do.call(paste, c(normalized_columns, sep = " "))
}

data_preprocess_results_payload <- function(
  preprocess_context,
  page = 1L,
  page_size = 10L,
  page_input_id = NULL,
  search_query = "",
  search_input_id = NULL
) {
  preview <- preprocess_context$preview
  stopifnot(is.data.frame(preview))

  filtered_preview <- preview
  search_index <- preprocess_context$search_index
  if (is.null(search_index) || length(search_index) != nrow(preview)) {
    search_index <- data_preprocess_search_index(preview)
  }

  if (is.null(search_query)) {
    search_query <- ""
  }

  normalized_query <- trimws(as.character(search_query))
  normalized_query_lower <- tolower(normalized_query)

  if (nzchar(normalized_query_lower)) {
    row_matches <- grepl(normalized_query_lower, search_index, fixed = TRUE)
    filtered_preview <- preview[row_matches, , drop = FALSE]
  }

  total_rows <- nrow(filtered_preview)
  page_size <- max(as.integer(page_size), 1L)
  page_count <- max(ceiling(total_rows / page_size), 1L)
  page <- min(max(as.integer(page), 1L), page_count)
  if (total_rows > 0) {
    start_row <- ((page - 1L) * page_size) + 1L
    end_row <- min(start_row + page_size - 1L, total_rows)
    page_slice <- filtered_preview[start_row:end_row, , drop = FALSE]
  } else {
    page_slice <- filtered_preview[0, , drop = FALSE]
    page <- 1L
  }

  list(
    table = list(
      columns = as.list(colnames(preview)),
      rows = data_preprocess_rows_payload(page_slice),
      page = page,
      pageCount = page_count,
      pageSize = page_size,
      totalRows = total_rows,
      pageInputId = page_input_id,
      searchQuery = normalized_query,
      searchInputId = search_input_id
    ),
    charts = list(
      barplot = data_preprocess_rows_payload(preprocess_context$barplot_data),
      biotype = data_preprocess_rows_payload(preprocess_context$biotype_summary),
      rrna = data_preprocess_rows_payload(preprocess_context$rrna_summary)
    )
  )
}
