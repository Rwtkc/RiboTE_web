mod_data_preprocess_server <- function(id, session_state, analysis_lock = NULL) {
  moduleServer(id, function(input, output, session) {
    config <- data_preprocess_module_config()
    analysis_result <- reactiveVal(NULL)
    current_page <- reactiveVal(1L)
    current_search <- reactiveVal("")
    current_view <- reactiveVal("data")
    module_lock_id <- session$ns("analysis_lock")

    current_lock_owner <- reactive({
      if (is.null(analysis_lock)) {
        return(NULL)
      }

      analysis_lock()
    })

    is_analysis_locked <- reactive({
      !is.null(current_lock_owner())
    })

    requirement_met <- reactive({
      ribote_has_upload(session_state)
    })

    detect_delimiter <- function(file_path) {
      preview <- readLines(file_path, n = 5, warn = FALSE)
      delimiters <- c("\t", ",", ";", "|")
      delimiter_count <- sapply(delimiters, function(delim) {
        sum(vapply(preview, function(line) length(strsplit(line, delim, fixed = TRUE)[[1]]) > 1, logical(1)))
      })

      delimiters[[which.max(delimiter_count)]]
    }

    read_count_matrix <- function(file_path) {
      delimiter <- detect_delimiter(file_path)

      utils::read.table(
        file_path,
        header = TRUE,
        sep = delimiter,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    }

    preprocess_gene_matrix <- function(gene_matrix, na_strategy, min_cpm, min_libraries) {
      gene_matrix <- gene_matrix[!duplicated(gene_matrix[, 1]), , drop = FALSE]
      gene_matrix <- gene_matrix[!is.na(gene_matrix[, 1]), , drop = FALSE]
      rownames(gene_matrix) <- toupper(as.character(gene_matrix[, 1]))
      gene_matrix <- gene_matrix[, -1, drop = FALSE]
      gene_matrix[] <- lapply(gene_matrix, function(column) as.numeric(as.character(column)))
      gene_matrix <- gene_matrix[!apply(is.na(gene_matrix), 1, all), , drop = FALSE]

      zero_only_columns <- vapply(gene_matrix, function(column) all(column == 0, na.rm = TRUE), logical(1))
      if (sum(zero_only_columns) > 0) {
        gene_matrix <- gene_matrix[, !zero_only_columns, drop = FALSE]
      }

      gene_matrix <- gene_matrix[order(-apply(gene_matrix, 1, stats::sd, na.rm = TRUE)), , drop = FALSE]

      if (sum(is.na(gene_matrix)) > 0) {
        if (identical(na_strategy, "Median Imputation")) {
          row_medians <- apply(gene_matrix, 1, stats::median, na.rm = TRUE)
          for (column_index in seq_len(ncol(gene_matrix))) {
            missing_rows <- which(is.na(gene_matrix[, column_index]))
            gene_matrix[missing_rows, column_index] <- row_medians[missing_rows]
          }
        } else {
          gene_matrix[is.na(gene_matrix)] <- 0
        }
      }

      gene_matrix <- gene_matrix[rowSums(gene_matrix, na.rm = TRUE) >= 1, , drop = FALSE]

      library_sizes <- colSums(gene_matrix, na.rm = TRUE)
      library_sizes[library_sizes <= 0] <- 1
      cpm_matrix <- sweep(gene_matrix, 2, library_sizes / 1e6, "/")
      keep_rows <- rowSums(cpm_matrix >= min_cpm, na.rm = TRUE) >= min_libraries
      gene_matrix <- gene_matrix[keep_rows, , drop = FALSE]

      processed <- as.data.frame(gene_matrix, check.names = FALSE)
      processed <- data.frame(GeneID = rownames(processed), processed, check.names = FALSE)
      rownames(processed) <- NULL
      processed
    }

    write_matrix_cache <- function(gene_matrix) {
      cache_path <- tempfile(pattern = "ribote_preprocess_matrix_", tmpdir = tempdir(), fileext = ".csv")
      utils::write.csv(gene_matrix, cache_path, row.names = FALSE, quote = TRUE)
      normalizePath(cache_path, winslash = "/", mustWork = TRUE)
    }

    build_sample_display_index <- function(upload_context, sample_names) {
      sample_manifest <- upload_context$sample_type_manifest
      pair_manifest <- upload_context$pair_manifest

      if (is.null(sample_manifest) || is.null(pair_manifest)) {
        fallback <- data.frame(
          sample_name = sample_names,
          sample_type = ifelse(grepl("^RPF|^RIBO", sample_names, ignore.case = TRUE), "Ribo-seq", "RNA-seq"),
          sample_display = sample_names,
          group_role = NA_character_,
          stringsAsFactors = FALSE
        )
        return(fallback)
      }

      sample_manifest <- as.data.frame(sample_manifest, stringsAsFactors = FALSE)
      pair_manifest <- as.data.frame(pair_manifest, stringsAsFactors = FALSE)
      sample_manifest$sample_name <- as.character(sample_manifest$sample_name)
      sample_manifest$sample_type <- as.character(sample_manifest$sample_type)
      pair_manifest$rna_sample <- as.character(pair_manifest$rna_sample)
      pair_manifest$ribo_sample <- as.character(pair_manifest$ribo_sample)
      pair_manifest$group_role <- as.character(pair_manifest$group_role)

      role_counters <- list(Control = 0L, Treatment = 0L)
      display_map <- list()

      for (row_index in seq_len(nrow(pair_manifest))) {
        role <- pair_manifest$group_role[[row_index]]
        if (!role %in% c("Control", "Treatment")) {
          next
        }

        role_counters[[role]] <- role_counters[[role]] + 1L
        role_code <- if (identical(role, "Control")) "C" else "T"
        replicate_index <- role_counters[[role]]

        display_map[[pair_manifest$rna_sample[[row_index]]]] <- list(
          sample_display = sprintf("RNA.%s%d", role_code, replicate_index),
          group_role = role
        )
        display_map[[pair_manifest$ribo_sample[[row_index]]]] <- list(
          sample_display = sprintf("RPF.%s%d", role_code, replicate_index),
          group_role = role
        )
      }

      fallback_counters <- list("RNA-seq" = 0L, "Ribo-seq" = 0L, "Sample" = 0L)
      rows <- lapply(sample_names, function(sample_name) {
        sample_row <- sample_manifest[sample_manifest$sample_name == sample_name, , drop = FALSE]
        sample_type <- if (nrow(sample_row) > 0) as.character(sample_row$sample_type[[1]]) else "Sample"
        mapped <- display_map[[sample_name]]

        if (is.null(mapped)) {
          fallback_counters[[sample_type]] <- fallback_counters[[sample_type]] + 1L
          prefix <- switch(
            sample_type,
            "RNA-seq" = "RNA.U",
            "Ribo-seq" = "RPF.U",
            "S"
          )
          sample_display <- sprintf("%s%d", prefix, fallback_counters[[sample_type]])
          group_role <- NA_character_
        } else {
          sample_display <- mapped$sample_display
          group_role <- mapped$group_role
        }

        data.frame(
          sample_name = sample_name,
          sample_type = sample_type,
          sample_display = sample_display,
          group_role = group_role,
          stringsAsFactors = FALSE
        )
      })

      do.call(rbind, rows)
    }

    build_barplot_data <- function(processed_matrix, sample_display_index) {
      count_matrix <- processed_matrix[, setdiff(colnames(processed_matrix), "GeneID"), drop = FALSE]
      totals <- colSums(count_matrix, na.rm = TRUE)
      display_lookup <- sample_display_index$sample_display
      names(display_lookup) <- sample_display_index$sample_name
      type_lookup <- sample_display_index$sample_type
      names(type_lookup) <- sample_display_index$sample_name

      data.frame(
        sample = names(totals),
        sample_display = unname(display_lookup[names(totals)]),
        total_count = as.numeric(totals),
        sample_type = unname(type_lookup[names(totals)]),
        stringsAsFactors = FALSE
      )
    }

    load_gene_biotype_index <- function(org_db_path) {
      if (!requireNamespace("DBI", quietly = TRUE) || !requireNamespace("RSQLite", quietly = TRUE)) {
        return(NULL)
      }

      con <- DBI::dbConnect(RSQLite::SQLite(), org_db_path)
      on.exit(DBI::dbDisconnect(con), add = TRUE)

      gene_info <- DBI::dbGetQuery(
        con,
        "select ensembl_gene_id, gene_biotype, symbol from geneInfo"
      )

      gene_info$ensembl_gene_id <- toupper(as.character(gene_info$ensembl_gene_id))
      gene_info
    }

    build_biotype_summary <- function(processed_matrix, org_db_path) {
      gene_info <- load_gene_biotype_index(org_db_path)

      if (is.null(gene_info) || nrow(gene_info) == 0) {
        return(data.frame(gene_biotype = "Unknown", genes_retained = nrow(processed_matrix), stringsAsFactors = FALSE))
      }

      biotype_map <- gene_info$gene_biotype
      names(biotype_map) <- gene_info$ensembl_gene_id
      biotypes <- biotype_map[toupper(processed_matrix$GeneID)]
      biotypes[is.na(biotypes) | biotypes == ""] <- "Unknown"
      summary <- sort(table(biotypes), decreasing = TRUE)
      summary_df <- data.frame(
        gene_biotype = names(summary),
        genes_retained = as.integer(summary),
        stringsAsFactors = FALSE
      )

      if (nrow(summary_df) > 8) {
        top_rows <- summary_df[seq_len(7), , drop = FALSE]
        other_count <- sum(summary_df$genes_retained[-seq_len(7)])
        summary_df <- rbind(
          top_rows,
          data.frame(gene_biotype = "Other", genes_retained = other_count, stringsAsFactors = FALSE)
        )
      }

      summary_df
    }

    build_rrna_summary <- function(processed_matrix, org_db_path, sample_display_index) {
      gene_info <- load_gene_biotype_index(org_db_path)
      count_matrix <- as.matrix(processed_matrix[, setdiff(colnames(processed_matrix), "GeneID"), drop = FALSE])
      display_lookup <- sample_display_index$sample_display
      names(display_lookup) <- sample_display_index$sample_name

      if (is.null(gene_info) || nrow(gene_info) == 0) {
        rrna_totals <- rep(0, ncol(count_matrix))
      } else {
        biotype_map <- gene_info$gene_biotype
        names(biotype_map) <- gene_info$ensembl_gene_id
        biotypes <- biotype_map[toupper(processed_matrix$GeneID)]
        rrna_mask <- grepl("rrna", biotypes, ignore.case = TRUE)
        rrna_mask[is.na(rrna_mask)] <- FALSE
        rrna_totals <- if (any(rrna_mask)) colSums(count_matrix[rrna_mask, , drop = FALSE], na.rm = TRUE) else rep(0, ncol(count_matrix))
      }

      total_counts <- colSums(count_matrix, na.rm = TRUE)
      non_rrna_totals <- pmax(total_counts - rrna_totals, 0)

      data.frame(
        sample = rep(colnames(count_matrix), each = 2),
        sample_display = rep(unname(display_lookup[colnames(count_matrix)]), each = 2),
        category = rep(c("rRNA", "Non-rRNA"), times = ncol(count_matrix)),
        total_count = as.numeric(as.vector(rbind(rrna_totals, non_rrna_totals))),
        stringsAsFactors = FALSE
      )
    }

    run_preprocess <- function(upload_context, na_strategy, min_cpm, min_libraries) {
      source_path <- upload_context$source_matrix_path
      if (is.null(source_path) || !file.exists(source_path)) {
        source_path <- upload_context$matrix_path
      }

      raw_matrix <- read_count_matrix(source_path)
      processed_matrix <- preprocess_gene_matrix(raw_matrix, na_strategy, min_cpm, min_libraries)
      sample_names <- setdiff(colnames(processed_matrix), "GeneID")
      sample_display_index <- build_sample_display_index(upload_context, sample_names)

      if (nrow(processed_matrix) == 0 || ncol(processed_matrix) <= 1) {
        stop("No genes passed the current preprocessing thresholds.", call. = FALSE)
      }

      list(
        matrix_path = write_matrix_cache(processed_matrix),
        preview = processed_matrix,
        search_index = data_preprocess_search_index(processed_matrix),
        matrix_stats = list(
          genes = nrow(processed_matrix),
          samples = ncol(processed_matrix) - 1L
        ),
        sample_display_index = sample_display_index,
        barplot_data = build_barplot_data(processed_matrix, sample_display_index),
        biotype_summary = build_biotype_summary(processed_matrix, upload_context$resource_paths$org_db_path),
        rrna_summary = build_rrna_summary(processed_matrix, upload_context$resource_paths$org_db_path, sample_display_index),
        parameters = list(
          na_strategy = na_strategy,
          min_cpm = min_cpm,
          min_libraries = min_libraries
        )
      )
    }

    publish_controls <- function() {
      send_control_payload(
        session = session,
        id = session$ns("controls_host"),
        control = "ribote-module-controls",
        config = list(
          title = config$title,
          sections = ribote_client_sections(session, config$sections)
        )
      )
    }

    clear_preprocess_results <- function(reset_navigation = FALSE) {
      session_state$preprocess_context <- NULL
      session_state$preprocess_ready <- FALSE
      session_state$te_context <- NULL
      session_state$te_ready <- FALSE

      if (isTRUE(reset_navigation)) {
        current_page(1L)
        current_search("")
        current_view("data")
      }

      analysis_result(NULL)
      publish_results()
      publish_export()
      invisible(NULL)
    }

    publish_results <- function() {
      if (!ribote_has_preprocess(session_state)) {
        clear_control_payload(session = session, id = session$ns("results_host"))
        return(invisible(NULL))
      }

      payload <- data_preprocess_results_payload(
        preprocess_context = session_state$preprocess_context,
        page = current_page(),
        page_size = 10L,
        page_input_id = session$ns("result_page"),
        search_query = current_search(),
        search_input_id = session$ns("result_search")
      )

      send_control_payload(
        session = session,
        id = session$ns("results_host"),
        control = "ribote-preprocess-results",
        config = utils::modifyList(
          payload,
          list(
            table = utils::modifyList(
              payload$table,
              list(
                activeView = current_view(),
                activeViewInputId = session$ns("result_view")
              )
            )
          )
        )
      )
    }

    publish_export <- function() {
      export_config <- data_preprocess_export_config("data_preprocess")
      export_config$ids <- list(
        format = session$ns("export_format"),
        width = session$ns("export_width"),
        height = session$ns("export_height"),
        dpi = session$ns("export_dpi"),
        trigger = session$ns("export_plot"),
        dataFormat = session$ns("data_export_format"),
        dataTrigger = session$ns("data_export")
      )

      send_control_payload(
        session = session,
        id = session$ns("export_host"),
        control = "ribote-preprocess-export",
        config = utils::modifyList(
          export_config,
          list(
            ready = ribote_has_preprocess(session_state),
            currentView = current_view(),
            figureDisabled = !ribote_has_preprocess(session_state) || identical(current_view(), "data"),
            dataDisabled = !ribote_has_preprocess(session_state)
          )
        )
      )
    }

    capture_preprocess <- function(expr) {
      tryCatch(
        expr,
        error = function(err) {
          showNotification(conditionMessage(err), type = "error", duration = 5)
          NULL
        }
      )
    }

    preprocess_progress_start <- function() {
      progress <- shiny::Progress$new(session = session, min = 0, max = 100)
      attr(progress, "rnameta_last_value") <- 0L
      progress$set(
        value = 0,
        message = "Running Data Preprocess",
        detail = "0% | Initializing preprocessing"
      )
      progress
    }

    preprocess_progress_update <- function(progress, value, detail) {
      if (is.null(progress)) {
        return(invisible(NULL))
      }

      last_value <- attr(progress, "rnameta_last_value", exact = TRUE)
      if (!is.numeric(last_value) || length(last_value) != 1L || is.na(last_value)) {
        last_value <- 0
      }

      normalized_value <- as.integer(round(max(0, min(100, value))))
      display_value <- max(last_value, normalized_value)
      attr(progress, "rnameta_last_value") <- display_value

      progress$set(
        value = display_value,
        message = "Running Data Preprocess",
        detail = sprintf("%s%% | %s", display_value, detail)
      )

      invisible(NULL)
    }

    preprocess_progress_close <- function(progress) {
      if (is.null(progress)) {
        return(invisible(NULL))
      }

      progress$close()
      invisible(NULL)
    }

    session$onFlushed(function() {
      isolate({
        publish_controls()
        publish_export()
      })
    }, once = TRUE)

    observeEvent(input$controls_ready, {
      publish_controls()
    }, ignoreInit = TRUE)

    observeEvent(input$export_ready, {
      publish_export()
    }, ignoreInit = TRUE)

    observeEvent(input$results_ready, {
      publish_results()
    }, ignoreInit = TRUE)

    observeEvent(input$result_page, {
      req(ribote_has_preprocess(session_state))
      requested_page <- suppressWarnings(as.integer(input$result_page))
      if (is.na(requested_page)) {
        return(invisible(NULL))
      }

      current_page(requested_page)
      publish_results()
    }, ignoreInit = TRUE)

    observeEvent(input$result_search, {
      req(ribote_has_preprocess(session_state))

      query <- input$result_search
      if (is.null(query)) {
        query <- ""
      }

      current_search(as.character(query))
      current_page(1L)
      publish_results()
    }, ignoreInit = TRUE)

    observeEvent(input$result_view, {
      next_view <- input$result_view
      if (is.null(next_view)) {
        next_view <- "data"
      }
      next_view <- as.character(next_view)
      if (!nzchar(next_view)) {
        next_view <- "data"
      }

      current_view(next_view)
      publish_export()
    }, ignoreInit = TRUE)

    observe({
      set_native_button_state(
        session = session,
        id = session$ns("run_analysis"),
        disabled = !requirement_met() || is_analysis_locked()
      )
    })

    observeEvent(input$run_analysis, {
      req(requirement_met())
      req(!is_analysis_locked())

      if (!is.null(analysis_lock)) {
        analysis_lock(module_lock_id)
      }

      on.exit({
        if (!is.null(analysis_lock) && identical(analysis_lock(), module_lock_id)) {
          analysis_lock(NULL)
        }
      }, add = TRUE)

      na_strategy <- if (is.null(input$na_strategy) || !nzchar(input$na_strategy)) "Zero Imputation" else input$na_strategy
      min_cpm <- suppressWarnings(as.numeric(input$min_cpm))
      min_libraries <- suppressWarnings(as.integer(input$min_libraries))

      if (is.na(min_cpm)) {
        min_cpm <- 0.5
      }

      if (is.na(min_libraries) || min_libraries < 1) {
        min_libraries <- 1L
      }

      progress <- preprocess_progress_start()
      clear_preprocess_results(reset_navigation = TRUE)
      on.exit(preprocess_progress_close(progress), add = TRUE)

      context <- capture_preprocess({
        preprocess_progress_update(progress, 12, "Loading saved matrix context and species resources")
        preprocess_context <- run_preprocess(
          upload_context = isolate(session_state$upload_context),
          na_strategy = na_strategy,
          min_cpm = min_cpm,
          min_libraries = min_libraries
        )
        preprocess_progress_update(progress, 100, "Building processed matrix, bar plot, and QC summaries")
        preprocess_context
      })

      if (is.null(context)) {
        return(invisible(NULL))
      }

      session_state$preprocess_context <- context
      session_state$preprocess_ready <- TRUE
      session_state$last_module <- "data_preprocess"
      current_page(1L)
      current_search("")
      current_view("data")

      analysis_result(list(
        message = "Processed matrix, bar plot, and QC views are ready.",
        metrics = list(
          list(label = "Genes Retained", value = format(context$matrix_stats$genes, big.mark = ",")),
          list(label = "Samples Retained", value = context$matrix_stats$samples)
        )
      ))

      publish_results()
      publish_export()
    })

    observeEvent(input$export_plot, {
      req(ribote_has_preprocess(session_state))

      active_view <- current_view()
      if (identical(active_view, "data")) {
        showNotification("Switch to Library Size or QC to export a figure.", type = "warning", duration = 4)
        return(invisible(NULL))
      }

      settings <- data_preprocess_export_settings(input)
      if (identical(active_view, "qc")) {
      session$sendCustomMessage(
        "ribote-preprocess-qc-export",
        list(
          hostId = session$ns("results_host"),
          format = settings$format,
          width = settings$width,
          height = settings$height,
          dpi = settings$dpi,
          filename = data_preprocess_archive_filename("qc"),
          exportEntries = list(
            list(
              selector = ".ribote-preprocess-panel--stacked .ribote-d3-card:first-child",
              filename = sprintf("gene_biotype_composition.%s", settings$format),
              exportPadding = list(top = 10, right = 34, bottom = 18, left = 34)
            ),
            list(
              selector = ".ribote-preprocess-panel--stacked .ribote-d3-card:last-child",
              filename = sprintf("rrna_fraction_by_sample.%s", settings$format),
              exportPadding = list(top = 10, right = 34, bottom = 18, left = 26)
            )
          )
        )
      )
        return(invisible(NULL))
      }

      selector <- switch(
        active_view,
        barplot = ".ribote-preprocess-panel .ribote-d3-card",
        ".ribote-preprocess-panel"
      )

      session$sendCustomMessage(
        "ribote-preprocess-figure-export",
        list(
          hostId = session$ns("results_host"),
          selector = selector,
          format = settings$format,
          width = settings$width,
          height = settings$height,
          dpi = settings$dpi,
          filename = data_preprocess_figure_export_filename(active_view, settings$format),
          forceRaster = identical(active_view, "qc"),
          exportPadding = list(top = 10, right = 34, bottom = 18, left = 34)
        )
      )
    }, ignoreInit = TRUE)

    observeEvent(input$data_export, {
      req(ribote_has_preprocess(session_state))

      settings <- data_preprocess_export_settings(input)
      extension <- if (identical(settings$data_format, "txt")) "txt" else "csv"
      active_view <- current_view()

      if (identical(active_view, "barplot")) {
        session$sendCustomMessage(
          "ribote-text-export",
          list(
            filename = sprintf("library_size_data_%s.%s", format(Sys.Date(), "%Y%m%d"), extension),
            mimeType = if (identical(settings$data_format, "txt")) {
              "text/tab-separated-values;charset=utf-8"
            } else {
              "text/csv;charset=utf-8"
            },
            content = data_preprocess_chart_export_content(
              session_state$preprocess_context,
              view = "barplot",
              format = settings$data_format
            )
          )
        )
        return(invisible(NULL))
      }

      if (identical(active_view, "data")) {
        session$sendCustomMessage(
          "ribote-text-export",
          list(
            filename = data_preprocess_data_export_filename(extension),
            mimeType = if (identical(settings$data_format, "txt")) {
              "text/tab-separated-values;charset=utf-8"
            } else {
              "text/csv;charset=utf-8"
            },
            content = data_preprocess_data_export_content(
              session_state$preprocess_context,
              format = settings$data_format
            )
          )
        )
        return(invisible(NULL))
      }

      if (identical(active_view, "qc")) {
        session$sendCustomMessage(
          "ribote-archive-export",
          list(
            filename = data_preprocess_archive_filename("qc_data"),
            entries = data_preprocess_chart_export_entries(
              session_state$preprocess_context,
              format = settings$data_format
            )
          )
        )
        return(invisible(NULL))
      }
    }, ignoreInit = TRUE)

    output$run_hint <- renderUI({
      if (requirement_met()) {
        return(NULL)
      }

      div(class = "ribote-hint", "Save species and count matrix context in Load Data first.")
    })

    output$parameter_snapshot <- renderUI({
      NULL
    })

    output$analysis_notice <- renderUI({
      NULL
    })

    output$analysis_summary <- renderUI({
      result <- analysis_result()
      if (is.null(result)) {
        return(NULL)
      }
      ribote_result_ui(result)
    })

    output$result_tabs <- renderUI({
      req(ribote_has_preprocess(session_state))

      div(
        class = "ribote-result-tabs",
        react_control_host(
          id = session$ns("results_host"),
          control_type = "ribote-preprocess-results",
          class = "ribote-results-root",
          ready_input_id = session$ns("results_ready")
        )
      )
    })

    outputOptions(output, "run_hint", suspendWhenHidden = FALSE)
  })
}
