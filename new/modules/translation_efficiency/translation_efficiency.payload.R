translation_efficiency_rows_payload <- function(data_frame) {
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

translation_efficiency_display_point_limit <- function() {
  5000L
}

translation_efficiency_finite_chart_rows <- function(data_frame, x_column, y_column, scale_type = "linear") {
  if (is.null(data_frame) || !is.data.frame(data_frame)) {
    return(data.frame())
  }

  if (nrow(data_frame) == 0L) {
    return(data_frame[0, , drop = FALSE])
  }

  if (!all(c(x_column, y_column) %in% colnames(data_frame))) {
    return(data_frame[0, , drop = FALSE])
  }

  x_values <- suppressWarnings(as.numeric(data_frame[[x_column]]))
  y_values <- suppressWarnings(as.numeric(data_frame[[y_column]]))
  keep <- is.finite(x_values) & is.finite(y_values)

  if (identical(scale_type, "log2")) {
    keep <- keep & x_values > 0 & y_values > 0
  }

  data_frame[keep, , drop = FALSE]
}

translation_efficiency_stride_indices <- function(row_count, limit) {
  row_count <- as.integer(row_count)
  limit <- as.integer(limit)

  if (row_count <= limit) {
    return(seq_len(row_count))
  }

  if (limit <= 0L) {
    return(integer())
  }

  unique(pmax(1L, pmin(row_count, round(seq(1, row_count, length.out = limit)))))
}

translation_efficiency_display_subset <- function(data_frame, status_column = NULL, limit = translation_efficiency_display_point_limit()) {
  if (is.null(data_frame) || !is.data.frame(data_frame)) {
    return(data.frame())
  }

  if (nrow(data_frame) == 0L) {
    return(data_frame[0, , drop = FALSE])
  }

  limit <- max(1L, as.integer(limit))
  if (nrow(data_frame) <= limit) {
    return(data_frame)
  }

  if (!is.null(status_column) && status_column %in% colnames(data_frame)) {
    status_values <- as.character(data_frame[[status_column]])
    priority_rows <- data_frame[!is.na(status_values) & nzchar(status_values) & status_values != "Non", , drop = FALSE]
    background_rows <- data_frame[is.na(status_values) | !nzchar(status_values) | status_values == "Non", , drop = FALSE]

    if (nrow(priority_rows) >= limit) {
      return(priority_rows[translation_efficiency_stride_indices(nrow(priority_rows), limit), , drop = FALSE])
    }

    remaining <- limit - nrow(priority_rows)
    background_subset <- background_rows[translation_efficiency_stride_indices(nrow(background_rows), remaining), , drop = FALSE]
    return(rbind(priority_rows, background_subset))
  }

  data_frame[translation_efficiency_stride_indices(nrow(data_frame), limit), , drop = FALSE]
}

translation_efficiency_display_status_counts <- function(data_frame, status_column = NULL) {
  if (is.null(status_column) || !status_column %in% colnames(data_frame)) {
    return(list())
  }

  counts <- table(factor(as.character(data_frame[[status_column]]), levels = c("Up", "Non", "Down")))
  list(
    Up = unname(as.integer(counts[["Up"]])),
    Non = unname(as.integer(counts[["Non"]])),
    Down = unname(as.integer(counts[["Down"]]))
  )
}

translation_efficiency_display_domain <- function(data_frame, column_name) {
  if (is.null(data_frame) || !is.data.frame(data_frame) || !column_name %in% colnames(data_frame)) {
    return(NULL)
  }

  values <- suppressWarnings(as.numeric(data_frame[[column_name]]))
  values <- values[is.finite(values)]

  if (length(values) == 0L) {
    return(NULL)
  }

  as.list(unname(range(values)))
}

translation_efficiency_chart_display_payload <- function(
  data_frame,
  x_column,
  y_column,
  status_column = NULL,
  scale_type = "linear",
  limit = translation_efficiency_display_point_limit()
) {
  valid_rows <- translation_efficiency_finite_chart_rows(
    data_frame = data_frame,
    x_column = x_column,
    y_column = y_column,
    scale_type = scale_type
  )
  display_rows <- translation_efficiency_display_subset(
    data_frame = valid_rows,
    status_column = status_column,
    limit = limit
  )

  list(
    rows = translation_efficiency_rows_payload(display_rows),
    meta = list(
      originalCount = as.integer(nrow(valid_rows)),
      displayedCount = as.integer(nrow(display_rows)),
      isSubset = nrow(display_rows) < nrow(valid_rows),
      xDomain = translation_efficiency_display_domain(valid_rows, x_column),
      yDomain = translation_efficiency_display_domain(valid_rows, y_column),
      statusCounts = translation_efficiency_display_status_counts(valid_rows, status_column)
    )
  )
}

translation_efficiency_search_index <- function(data_frame) {
  if (is.null(data_frame) || !is.data.frame(data_frame) || nrow(data_frame) == 0) {
    return(character())
  }

  normalized_columns <- lapply(data_frame, function(column) {
    tolower(as.character(column))
  })

  do.call(paste, c(normalized_columns, sep = " "))
}

