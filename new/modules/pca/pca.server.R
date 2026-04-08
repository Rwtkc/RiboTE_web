mod_pca_server <- function(id, session_state, analysis_lock = NULL) {
  moduleServer(id, function(input, output, session) {
    config <- pca_module_config()
    pca_context <- reactiveVal(NULL)
    analysis_result <- reactiveVal(NULL)
    pca_base_cache <- reactiveVal(list())
    pca_projection_cache <- reactiveVal(list())
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
      pca_base_signature(
        te_context = session_state$te_context,
        preprocess_context = session_state$preprocess_context
      )
    })

    current_source_signature <- reactive({
      req(requirement_met())
      pca_source_signature(
        te_context = session_state$te_context,
        preprocess_context = session_state$preprocess_context
      )
    })

    base_cache_key_for <- function(base_signature = NULL) {
      if (is.null(base_signature)) {
        base_signature <- current_base_signature()
      }

      as.character(base_signature)
    }

    projection_cache_key_for <- function(base_signature = NULL, data_space = NULL, method = NULL) {
      parameters <- pca_normalize_parameters(data_space = data_space, method = method)

      paste(
        base_cache_key_for(base_signature),
        tolower(parameters$data_space),
        tolower(parameters$method),
        sep = "::"
      )
    }

    get_cached_base_context <- function(base_signature = NULL) {
      cache <- pca_base_cache()
      cache[[base_cache_key_for(base_signature)]]
    }

    store_cached_base_context <- function(base_context) {
      cache <- pca_base_cache()
      cache[[base_cache_key_for(base_context$base_signature)]] <- base_context
      pca_base_cache(cache)
      invisible(base_context)
    }

    get_cached_projection_context <- function(base_signature = NULL, data_space = NULL, method = NULL) {
      cache <- pca_projection_cache()
      cache[[projection_cache_key_for(base_signature, data_space, method)]]
    }

    store_cached_projection_context <- function(projection_context) {
      cache <- pca_projection_cache()
      cache[[projection_cache_key_for(
        base_signature = projection_context$base_signature,
        data_space = projection_context$parameters$data_space,
        method = projection_context$parameters$method
      )]] <- projection_context
      pca_projection_cache(cache)
      invisible(projection_context)
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

    get_or_build_projection_context <- function(base_context, data_space, method, source_signature = current_source_signature()) {
      projection_context <- get_cached_projection_context(
        base_signature = base_context$base_signature,
        data_space = data_space,
        method = method
      )

      if (is.null(projection_context)) {
        projection_context <- pca_projection_context(
          base_context = base_context,
          data_space = data_space,
          method = method,
          source_signature = source_signature
        )
        store_cached_projection_context(projection_context)
      } else {
        projection_context$source_signature <- source_signature
      }

      projection_context
    }

    collect_export_contexts <- function(scope = "current") {
      req(!is.null(pca_context()))

      current_context <- pca_context()
      base_context <- get_or_build_base_context()
      source_signature <- current_source_signature()
      targets <- pca_export_targets(current_context$parameters, scope = scope)

      lapply(targets, function(target) {
        get_or_build_projection_context(
          base_context = base_context,
          data_space = target$data_space[[1]],
          method = target$method[[1]],
          source_signature = source_signature
        )
      })
    }

    clear_pca_results <- function() {
      pca_context(NULL)
      analysis_result(NULL)
      publish_results()
      publish_export()
      invisible(NULL)
    }

    apply_cached_context <- function(context) {
      pca_context(context)
      analysis_result(list(metrics = context$metrics))
      session_state$last_module <- "pca"
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
      context <- pca_context()
      if (is.null(context)) {
        clear_control_payload(session = session, id = session$ns("results_host"))
        return(invisible(NULL))
      }

      send_control_payload(
        session = session,
        id = session$ns("results_host"),
        control = "ribote-pca-results",
        config = pca_results_payload(context)
      )
    }

    publish_export <- function() {
      export_config <- pca_export_config("pca")
      export_config$ids <- list(
        figureScope = session$ns("export_scope"),
        format = session$ns("export_format"),
        width = session$ns("export_width"),
        height = session$ns("export_height"),
        dpi = session$ns("export_dpi"),
        trigger = session$ns("export_plot"),
        dataScope = session$ns("data_export_scope"),
        dataFormat = session$ns("data_export_format"),
        dataTrigger = session$ns("data_export")
      )

      send_control_payload(
        session = session,
        id = session$ns("export_host"),
        control = "ribote-pca-export",
        config = utils::modifyList(
          export_config,
          list(
            ready = !is.null(pca_context()),
            figureDisabled = is.null(pca_context()),
            dataDisabled = is.null(pca_context())
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

    session$onFlushed(function() {
      isolate({
        publish_controls()
        publish_export()
      })
    }, once = TRUE)

    observe({
      next_base_signature <- if (isTRUE(requirement_met())) current_base_signature() else NULL

      if (!identical(next_base_signature, last_base_signature())) {
        pca_base_cache(list())
        pca_projection_cache(list())
        last_base_signature(next_base_signature)
      }
    })

    observe({
      next_source_signature <- if (isTRUE(requirement_met())) current_source_signature() else NULL
      context <- pca_context()

      if (is.null(next_source_signature)) {
        if (!is.null(context) || !is.null(analysis_result())) {
          clear_pca_results()
        }
        return(invisible(NULL))
      }

      if (!is.null(context) && !identical(context$source_signature, next_source_signature)) {
        clear_pca_results()
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

      parameters <- pca_normalize_parameters(
        data_space = input$pca_data_space,
        method = input$pca_method
      )

      progress <- shiny::Progress$new(session = session, min = 0, max = 100)
      on.exit(progress$close(), add = TRUE)
      progress$set(value = 0, message = "Running PCA", detail = "0% | Initializing sample projection")

      clear_pca_results()

      context <- capture_analysis({
        progress$set(value = 18, message = "Running PCA", detail = "18% | Loading current TE result table and paired sample metadata")
        base_context <- get_cached_base_context()

        if (is.null(base_context)) {
          progress$set(value = 46, message = "Running PCA", detail = "46% | Preparing TE, RNA, and Ribo sample matrices from the TE results")
          base_context <- get_or_build_base_context()
        } else {
          progress$set(value = 46, message = "Running PCA", detail = "46% | Reusing previously prepared TE, RNA, and Ribo sample matrices")
        }

        cached_projection <- get_cached_projection_context(
          base_signature = base_context$base_signature,
          data_space = parameters$data_space,
          method = parameters$method
        )

        if (is.null(cached_projection)) {
          progress$set(value = 78, message = "Running PCA", detail = sprintf("78%% | Computing %s projection for %s space", parameters$method, pca_data_space_label(parameters$data_space)))
        } else {
          progress$set(value = 78, message = "Running PCA", detail = sprintf("78%% | Reusing cached %s projection", parameters$method))
        }

        projection_context <- get_or_build_projection_context(
          base_context = base_context,
          data_space = parameters$data_space,
          method = parameters$method,
          source_signature = current_source_signature()
        )

        progress$set(value = 100, message = "Running PCA", detail = "100% | Projection table and sample plot are ready")
        projection_context
      })

      if (is.null(context)) {
        return(invisible(NULL))
      }

      apply_cached_context(context)
    }, ignoreInit = TRUE)

    observeEvent(input$export_plot, {
      req(!is.null(pca_context()))

      settings <- pca_export_settings(input)
      contexts <- collect_export_contexts(settings$figure_scope)

      if (length(contexts) <= 1L) {
        context <- contexts[[1]]

        session$sendCustomMessage(
          "ribote-pca-figure-export",
          list(
            hostId = session$ns("results_host"),
            selector = ".ribote-pca-panel--projection .ribote-d3-card",
            format = settings$format,
            width = settings$width,
            height = settings$height,
            dpi = settings$dpi,
            backgroundColor = "#ffffff",
            forceRaster = TRUE,
            filename = pca_projection_filename(
              data_space = context$parameters$data_space,
              method = context$parameters$method,
              extension = settings$format
            ),
            exportPadding = list(top = 12, right = 34, bottom = 24, left = 24)
          )
        )
        return(invisible(NULL))
      }

      session$sendCustomMessage(
        "ribote-pca-multi-figure-export",
        list(
          format = settings$format,
          width = settings$width,
          height = settings$height,
          dpi = settings$dpi,
          backgroundColor = "#ffffff",
          forceRaster = TRUE,
          filename = pca_archive_filename("figure"),
          exportPadding = list(top = 12, right = 34, bottom = 24, left = 24),
          entries = lapply(contexts, function(context) {
            list(
              filename = pca_projection_filename(
                data_space = context$parameters$data_space,
                method = context$parameters$method,
                extension = settings$format
              ),
              plot = list(
                title = context$plot$title,
                xLabel = context$plot$x_label,
                yLabel = context$plot$y_label,
                points = pca_plot_points_payload(context$plot_points)
              )
            )
          })
        )
      )
    }, ignoreInit = TRUE)

    observeEvent(input$data_export, {
      req(!is.null(pca_context()))

      settings <- pca_export_settings(input)
      extension <- if (identical(settings$data_format, "txt")) "txt" else "csv"
      mime_type <- if (identical(settings$data_format, "txt")) {
        "text/tab-separated-values;charset=utf-8"
      } else {
        "text/csv;charset=utf-8"
      }
      contexts <- collect_export_contexts(settings$data_scope)

      if (length(contexts) <= 1L) {
        context <- contexts[[1]]

        session$sendCustomMessage(
          "ribote-text-export",
          list(
            filename = pca_projection_filename(
              data_space = context$parameters$data_space,
              method = context$parameters$method,
              extension = extension
            ),
            mimeType = mime_type,
            content = pca_data_export_content(context, format = settings$data_format)
          )
        )
        return(invisible(NULL))
      }

      session$sendCustomMessage(
        "ribote-archive-export",
        list(
          filename = pca_archive_filename("data"),
          entries = lapply(contexts, function(context) {
            list(
              filename = pca_projection_filename(
                data_space = context$parameters$data_space,
                method = context$parameters$method,
                extension = extension
              ),
              content = pca_data_export_content(context, format = settings$data_format)
            )
          })
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
      result <- analysis_result()
      if (is.null(result)) {
        return(NULL)
      }

      ribote_result_ui(result)
    })

    outputOptions(output, "run_hint", suspendWhenHidden = FALSE)
  })
}
