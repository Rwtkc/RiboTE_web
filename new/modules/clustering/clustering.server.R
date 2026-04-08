mod_clustering_server <- function(id, session_state, analysis_lock = NULL) {
  moduleServer(id, function(input, output, session) {
    config <- clustering_module_config()
    clustering_context <- reactiveVal(NULL)
    clustering_base_cache <- reactiveVal(list())
    clustering_analysis_cache <- reactiveVal(list())
    current_detail_selection <- reactiveVal(NULL)
    last_base_signature <- reactiveVal(NULL)
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
      ribote_has_te(session_state)
    })

    current_base_signature <- reactive({
      req(requirement_met())
      clustering_base_signature(
        te_context = session_state$te_context,
        preprocess_context = session_state$preprocess_context
      )
    })

    current_source_signature <- reactive({
      req(requirement_met())
      clustering_source_signature(
        te_context = session_state$te_context,
        preprocess_context = session_state$preprocess_context
      )
    })

    normalized_parameters <- reactive({
      clustering_normalize_parameters(
        data_space = input$clustering_data_space,
        top_genes = input$clustering_top_genes,
        distance_method = input$clustering_distance,
        linkage = input$clustering_linkage,
        zscore_max = input$clustering_zscore_max,
        gene_centricity = input$clustering_gene_centricity,
        detail_mode = input$clustering_detail_mode,
        detail_gene_ids = input$clustering_detail_gene_ids
      )
    })

    normalized_detail_parameters <- reactive({
      clustering_normalize_parameters(
        detail_mode = input$clustering_detail_mode,
        detail_gene_ids = input$clustering_detail_gene_ids
      )
    })

    detail_context <- reactive({
      context <- clustering_context()

      if (is.null(context)) {
        return(NULL)
      }

      parameters <- normalized_detail_parameters()
      clustering_extract_detail_context(
        clustering_context = context,
        detail_mode = parameters$detail_mode,
        detail_gene_ids = parameters$detail_gene_ids,
        selection = current_detail_selection()
      )
    })

    base_cache_key_for <- function(base_signature = NULL) {
      if (is.null(base_signature)) {
        base_signature <- current_base_signature()
      }

      as.character(base_signature)
    }

    analysis_cache_key_for <- function(base_signature = NULL, parameters = NULL) {
      if (is.null(base_signature)) {
        base_signature <- current_base_signature()
      }

      if (is.null(parameters)) {
        parameters <- normalized_parameters()
      }

      paste(base_cache_key_for(base_signature), clustering_parameters_key(parameters), sep = "::")
    }

    get_cached_base_context <- function(base_signature = NULL) {
      cache <- clustering_base_cache()
      cache[[base_cache_key_for(base_signature)]]
    }

    store_cached_base_context <- function(base_context) {
      cache <- clustering_base_cache()
      cache[[base_cache_key_for(base_context$base_signature)]] <- base_context
      clustering_base_cache(cache)
      invisible(base_context)
    }

    get_cached_analysis_context <- function(base_signature = NULL, parameters = NULL) {
      cache <- clustering_analysis_cache()
      cache[[analysis_cache_key_for(base_signature, parameters)]]
    }

    store_cached_analysis_context <- function(analysis_context) {
      cache <- clustering_analysis_cache()
      cache[[analysis_cache_key_for(
        base_signature = analysis_context$base_signature,
        parameters = analysis_context$parameters
      )]] <- analysis_context
      clustering_analysis_cache(cache)
      invisible(analysis_context)
    }

    get_or_build_base_context <- function() {
      base_context <- get_cached_base_context()

      if (is.null(base_context)) {
        base_context <- pca_build_base_context(
          te_context = isolate(session_state$te_context),
          preprocess_context = isolate(session_state$preprocess_context),
          upload_context = isolate(session_state$upload_context)
        )
        store_cached_base_context(base_context)
      }

      base_context
    }

    get_or_build_analysis_context <- function(parameters, source_signature = current_source_signature()) {
      cache_parameters <- parameters
      cache_parameters$detail_mode <- NULL
      cache_parameters$detail_gene_ids <- NULL

      analysis_context <- get_cached_analysis_context(parameters = cache_parameters)

      if (is.null(analysis_context)) {
        analysis_context <- clustering_build_analysis_context(
          base_context = get_or_build_base_context(),
          parameters = parameters,
          source_signature = source_signature
        )
        store_cached_analysis_context(analysis_context)
      } else {
        analysis_context$source_signature <- source_signature
      }

      analysis_context$palette <- clustering_palette_values(parameters$color_series)
      analysis_context
    }

    clear_clustering_results <- function() {
      clustering_context(NULL)
      current_detail_selection(NULL)
      publish_results()
      publish_export()
      invisible(NULL)
    }

    apply_cached_context <- function(context) {
      clustering_context(context)
      current_detail_selection(NULL)
      session_state$last_module <- "clustering"
      publish_results()
      publish_export()
      invisible(context)
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

    publish_results <- function() {
      context <- clustering_context()

      if (is.null(context)) {
        clear_control_payload(session = session, id = session$ns("results_host"))
        return(invisible(NULL))
      }

      detail <- detail_context()

      send_control_payload(
        session = session,
        id = session$ns("results_host"),
        control = "ribote-clustering-results",
        config = clustering_results_payload(
          clustering_context = context,
          detail_context = detail,
          selection_input_id = session$ns("detail_selection")
        )
      )
    }

    publish_export <- function() {
      export_config <- clustering_export_config("clustering")
      export_config$ids <- list(
        scope = session$ns("export_scope"),
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
        control = "ribote-clustering-export",
        config = utils::modifyList(
          export_config,
          list(
            ready = !is.null(clustering_context()),
            figureDisabled = is.null(clustering_context()),
            dataDisabled = is.null(clustering_context())
          )
        )
      )
    }

    capture_analysis <- function(expr) {
      tryCatch(
        expr,
        error = function(err) {
          showNotification(conditionMessage(err), type = "error", duration = 6)
          NULL
        }
      )
    }

    current_summary_metrics <- reactive({
      context <- clustering_context()
      detail <- detail_context()

      if (is.null(context) || is.null(detail)) {
        return(NULL)
      }

      clustering_result_metrics(context, detail)
    })

    session$onFlushed(function() {
      isolate({
        publish_controls()
        publish_export()
      })
    }, once = TRUE)

    observe({
      next_base_signature <- if (isTRUE(requirement_met())) current_base_signature() else NULL

      if (!identical(next_base_signature, last_base_signature())) {
        clustering_base_cache(list())
        clustering_analysis_cache(list())
        last_base_signature(next_base_signature)
      }
    })

    observe({
      next_source_signature <- if (isTRUE(requirement_met())) current_source_signature() else NULL
      context <- clustering_context()

      if (is.null(next_source_signature)) {
        if (!is.null(context)) {
          clear_clustering_results()
        }
        return(invisible(NULL))
      }

      if (!is.null(context) && !identical(context$source_signature, next_source_signature)) {
        clear_clustering_results()
      }

      invisible(NULL)
    })

    observeEvent(input$controls_ready, {
      publish_controls()
    }, ignoreInit = TRUE)

    observeEvent(input$export_ready, {
      publish_export()
    }, ignoreInit = TRUE)

    observeEvent(input$results_ready, {
      publish_results()
    }, ignoreInit = TRUE)

    observeEvent(input$detail_selection, {
      if (is.null(clustering_context())) {
        return(invisible(NULL))
      }

      current_detail_selection(input$detail_selection)
      publish_results()
    }, ignoreInit = TRUE)

    observeEvent(input$clustering_detail_mode, {
      if (is.null(clustering_context())) {
        return(invisible(NULL))
      }

      if (identical(normalized_detail_parameters()$detail_mode, "genes")) {
        current_detail_selection(NULL)
      }

      publish_results()
      publish_export()
    }, ignoreInit = TRUE)

    observeEvent(input$clustering_detail_gene_ids, {
      if (is.null(clustering_context())) {
        return(invisible(NULL))
      }

      if (!identical(normalized_detail_parameters()$detail_mode, "genes")) {
        return(invisible(NULL))
      }

      publish_results()
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

      parameters <- normalized_parameters()
      cache_parameters <- parameters
      cache_parameters$detail_mode <- NULL
      cache_parameters$detail_gene_ids <- NULL

      progress <- shiny::Progress$new(session = session, min = 0, max = 100)
      on.exit(progress$close(), add = TRUE)
      progress$set(value = 0, message = "Running Clustering", detail = "0% | Initializing clustered heatmap workflow")

      clear_clustering_results()

      context <- capture_analysis({
        progress$set(value = 20, message = "Running Clustering", detail = "20% | Loading TE, RNA, and Ribo matrices")
        base_context <- get_cached_base_context()

        if (is.null(base_context)) {
          progress$set(value = 48, message = "Running Clustering", detail = "48% | Building RNA, Ribo, and TE feature matrices from the current TE result table")
          base_context <- get_or_build_base_context()
        } else {
          progress$set(value = 48, message = "Running Clustering", detail = "48% | Reusing cached RNA, Ribo, and TE feature matrices")
        }

        cached_context <- get_cached_analysis_context(parameters = cache_parameters)

        if (is.null(cached_context)) {
          progress$set(value = 82, message = "Running Clustering", detail = sprintf(
            "82%% | Clustering %s space with %s distance and %s linkage",
            clustering_space_label(parameters$data_space),
            parameters$distance_method,
            parameters$linkage
          ))
        } else {
          progress$set(value = 82, message = "Running Clustering", detail = "82% | Reusing cached clustered heatmap order")
        }

        analysis_context <- get_or_build_analysis_context(parameters = parameters, source_signature = current_source_signature())
        progress$set(value = 100, message = "Running Clustering", detail = "100% | Clustered heatmap and detail view are ready")
        analysis_context
      })

      if (is.null(context)) {
        return(invisible(NULL))
      }

      apply_cached_context(context)
    }, ignoreInit = TRUE)

    observeEvent(input$export_plot, {
      context <- clustering_context()
      detail <- detail_context()
      req(!is.null(context))

      settings <- clustering_export_settings(input)
      figure_extension <- if (identical(settings$format, "pdf")) "pdf" else "png"

      if (identical(settings$scope, "detail") && (is.null(detail) || is.null(detail$matrix))) {
        showNotification("No detail heatmap is available for export.", type = "warning", duration = 5)
        return(invisible(NULL))
      }

      if (identical(settings$scope, "main")) {
        session$sendCustomMessage(
          "ribote-clustering-figure-export",
          list(
            hostId = session$ns("results_host"),
            selector = ".ribote-clustering-panel--main .ribote-d3-host",
            format = settings$format,
            width = settings$width,
            height = settings$height,
            dpi = settings$dpi,
            backgroundColor = "#ffffff",
            filename = clustering_figure_filename(context$parameters$data_space, kind = "main", extension = figure_extension)
          )
        )
        return(invisible(NULL))
      }

      if (identical(settings$scope, "detail")) {
        session$sendCustomMessage(
          "ribote-clustering-figure-export",
          list(
            hostId = session$ns("results_host"),
            selector = ".ribote-clustering-panel--detail .ribote-d3-host",
            format = settings$format,
            width = settings$width,
            height = settings$height,
            dpi = settings$dpi,
            backgroundColor = "#ffffff",
            filename = clustering_figure_filename(context$parameters$data_space, kind = "detail", extension = figure_extension)
          )
        )
        return(invisible(NULL))
      }

      export_entries <- list(
        list(
          selector = ".ribote-clustering-panel--main .ribote-d3-host",
          filename = clustering_figure_filename(context$parameters$data_space, kind = "main", extension = figure_extension)
        )
      )

      if (!is.null(detail) && !is.null(detail$matrix)) {
        export_entries[[length(export_entries) + 1L]] <- list(
          selector = ".ribote-clustering-panel--detail .ribote-d3-host",
          filename = clustering_figure_filename(context$parameters$data_space, kind = "detail", extension = figure_extension)
        )
      }

      session$sendCustomMessage(
        "ribote-clustering-multi-figure-export",
        list(
          filename = clustering_archive_filename("figure"),
          format = settings$format,
          hostId = session$ns("results_host"),
          width = settings$width,
          height = settings$height,
          dpi = settings$dpi,
          backgroundColor = "#ffffff",
          exportEntries = export_entries
        )
      )
    }, ignoreInit = TRUE)

    observeEvent(input$data_export, {
      context <- clustering_context()
      detail <- detail_context()
      req(!is.null(context))

      settings <- clustering_export_settings(input)
      extension <- if (identical(settings$data_format, "txt")) "txt" else "csv"
      mime_type <- if (identical(settings$data_format, "txt")) {
        "text/tab-separated-values;charset=utf-8"
      } else {
        "text/csv;charset=utf-8"
      }

      if (identical(settings$scope, "detail") && (is.null(detail) || is.null(detail$matrix))) {
        showNotification("No detail heatmap data is available for export.", type = "warning", duration = 5)
        return(invisible(NULL))
      }

      if (identical(settings$scope, "main")) {
        session$sendCustomMessage(
          "ribote-text-export",
          list(
            filename = clustering_data_filename(context$parameters$data_space, kind = "main", extension = extension),
            mimeType = mime_type,
            content = clustering_export_content(
              kind = "main",
              clustering_context = context,
              detail_context = detail,
              format = settings$data_format
            )
          )
        )
        return(invisible(NULL))
      }

      if (identical(settings$scope, "detail")) {
        session$sendCustomMessage(
          "ribote-text-export",
          list(
            filename = clustering_data_filename(context$parameters$data_space, kind = "detail", extension = extension),
            mimeType = mime_type,
            content = clustering_export_content(
              kind = "detail",
              clustering_context = context,
              detail_context = detail,
              format = settings$data_format
            )
          )
        )
        return(invisible(NULL))
      }

      archive_entries <- list(
        list(
          filename = clustering_data_filename(context$parameters$data_space, kind = "main", extension = extension),
          content = clustering_export_content(
            kind = "main",
            clustering_context = context,
            detail_context = detail,
            format = settings$data_format
          )
        )
      )

      if (!is.null(detail) && !is.null(detail$matrix)) {
        archive_entries[[length(archive_entries) + 1L]] <- list(
          filename = clustering_data_filename(context$parameters$data_space, kind = "detail", extension = extension),
          content = clustering_export_content(
            kind = "detail",
            clustering_context = context,
            detail_context = detail,
            format = settings$data_format
          )
        )
      }

      session$sendCustomMessage(
        "ribote-archive-export",
        list(
          filename = clustering_archive_filename("data"),
          entries = archive_entries
        )
      )
    }, ignoreInit = TRUE)

    output$run_hint <- renderUI({
      if (ribote_has_te(session_state)) {
        return(NULL)
      }

      if (!ribote_has_upload(session_state)) {
        return(div(class = "ribote-hint", "Save species and count matrix context in Load Data first."))
      }

      if (!ribote_has_preprocess(session_state)) {
        return(div(class = "ribote-hint", "Run Data Preprocess first."))
      }

      div(class = "ribote-hint", "Run Translation Efficiency first.")
    })

    output$parameter_snapshot <- renderUI({
      NULL
    })

    output$analysis_summary <- renderUI({
      metrics <- current_summary_metrics()

      if (is.null(metrics)) {
        return(NULL)
      }

      ribote_result_ui(list(metrics = metrics))
    })

    outputOptions(output, "run_hint", suspendWhenHidden = FALSE)
  })
}
