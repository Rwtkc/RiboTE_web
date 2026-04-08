mod_signalp_server <- function(id, session_state, analysis_lock = NULL) {
  moduleServer(id, function(input, output, session) {
    signalp_context <- reactiveVal(NULL)
    signalp_resource_cache <- reactiveVal(list())
    signalp_analysis_cache <- reactiveVal(list())
    last_source_signature <- reactiveVal(NULL)
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

    upload_ready <- reactive({
      ribote_has_upload(session_state)
    })

    te_ready <- reactive({
      ribote_has_te(session_state)
    })

    species_key <- reactive({
      if (!isTRUE(upload_ready())) {
        return(NULL)
      }

      signalp_species_key(session_state$upload_context)
    })

    available_methods <- reactive({
      if (!isTRUE(upload_ready())) {
        return(character())
      }

      signalp_available_methods(session_state$upload_context)
    })

    resources_available <- reactive({
      length(available_methods()) > 0L
    })

    requirement_met <- reactive({
      isTRUE(te_ready()) && isTRUE(resources_available())
    })

    current_source_signature <- reactive({
      req(te_ready())
      req(upload_ready())

      signalp_source_signature(
        te_context = session_state$te_context,
        preprocess_context = session_state$preprocess_context,
        upload_context = session_state$upload_context
      )
    })

    normalized_parameters <- reactive({
      signalp_normalize_parameters(
        method = input$signal_method,
        available_methods = available_methods()
      )
    })

    resource_cache_key_for <- function(species_key_value, method) {
      paste(as.character(species_key_value), tolower(as.character(method)), sep = "::")
    }

    analysis_cache_key_for <- function(source_signature = NULL, parameters = NULL) {
      if (is.null(source_signature)) {
        source_signature <- current_source_signature()
      }

      if (is.null(parameters)) {
        parameters <- normalized_parameters()
      }

      paste(signalp_cache_version(), as.character(source_signature), signalp_parameters_key(parameters), sep = "::")
    }

    get_cached_resource <- function(species_key_value, method) {
      cache <- signalp_resource_cache()
      cache[[resource_cache_key_for(species_key_value, method)]]
    }

    store_cached_resource <- function(species_key_value, method, resource) {
      cache <- signalp_resource_cache()
      cache[[resource_cache_key_for(species_key_value, method)]] <- resource
      signalp_resource_cache(cache)
      invisible(resource)
    }

    get_or_load_resource <- function(species_key_value, method) {
      cached <- get_cached_resource(species_key_value, method)

      if (!is.null(cached)) {
        return(cached)
      }

      resource_path <- signalp_resource_path(species_key_value, method)
      if (is.null(resource_path)) {
        stop(sprintf(
          "Local %s annotation resources are not installed for species key '%s'.",
          signalp_method_label(method),
          species_key_value
        ), call. = FALSE)
      }

      resource <- signalp_read_resource(resource_path)
      store_cached_resource(species_key_value, method, resource)
    }

    get_cached_analysis_context <- function(source_signature = NULL, parameters = NULL) {
      cache <- signalp_analysis_cache()
      cache[[analysis_cache_key_for(source_signature, parameters)]]
    }

    store_cached_analysis_context <- function(source_signature, parameters, context) {
      cache <- signalp_analysis_cache()
      cache[[analysis_cache_key_for(source_signature, parameters)]] <- context
      signalp_analysis_cache(cache)
      invisible(context)
    }

    clear_signalp_results <- function() {
      signalp_context(NULL)
      publish_results()
      publish_export()
      invisible(NULL)
    }

    apply_context <- function(context) {
      signalp_context(context)
      session_state$last_module <- "signalp"
      publish_results()
      publish_export()
      invisible(context)
    }

    publish_controls <- function() {
      config <- signalp_module_config(session_state$upload_context)

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
      context <- signalp_context()

      send_control_payload(
        session = session,
        id = session$ns("results_host"),
        control = "ribote-signalp-results",
        config = if (is.null(context)) list() else signalp_results_payload(context)
      )
    }

    publish_export <- function() {
      export_config <- signalp_export_config("signalp")
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
        control = "ribote-signalp-export",
        config = utils::modifyList(
          export_config,
          list(
            ready = !is.null(signalp_context()),
            figureDisabled = is.null(signalp_context()),
            dataDisabled = is.null(signalp_context())
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
      context <- signalp_context()

      if (is.null(context)) {
        return(NULL)
      }

      context$metrics
    })

    session$onFlushed(function() {
      isolate({
        publish_controls()
        publish_export()
      })
    }, once = TRUE)

    observe({
      next_signature <- if (isTRUE(te_ready()) && isTRUE(upload_ready())) current_source_signature() else NULL

      if (!identical(next_signature, last_source_signature())) {
        signalp_analysis_cache(list())
        last_source_signature(next_signature)
      }
    })

    observe({
      next_signature <- if (isTRUE(te_ready()) && isTRUE(upload_ready())) current_source_signature() else NULL
      context <- signalp_context()

      if (is.null(next_signature)) {
        if (!is.null(context)) {
          clear_signalp_results()
        }
        return(invisible(NULL))
      }

      if (!is.null(context) && !identical(context$source_signature, next_signature)) {
        clear_signalp_results()
      }

      invisible(NULL)
    })

    observeEvent(input$controls_ready, {
      publish_controls()
    }, ignoreInit = TRUE)

    observeEvent(input$results_ready, {
      publish_results()
    }, ignoreInit = TRUE)

    observeEvent(input$export_ready, {
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
      source_signature <- current_source_signature()
      species_key_value <- species_key()
      cached_context <- get_cached_analysis_context(source_signature = source_signature, parameters = parameters)
      progress <- shiny::Progress$new(session = session, min = 0, max = 100)
      on.exit(progress$close(), add = TRUE)
      progress$set(value = 0, message = "Running SignalP", detail = "0% | Initializing signal peptide and membrane annotation analysis")

      clear_signalp_results()

      context <- capture_analysis({
        selected_methods <- if (identical(parameters$method, "all")) available_methods() else intersect(parameters$method, available_methods())
        progress$set(value = 24, message = "Running SignalP", detail = sprintf(
          "24%% | Resolving local annotation resources for %s",
          species_key_value
        ))

        if (!is.null(cached_context)) {
          progress$set(value = 88, message = "Running SignalP", detail = "88% | Reusing cached annotation comparison")
          cached_context
        } else {
          resources <- setNames(
            lapply(selected_methods, function(method) get_or_load_resource(species_key_value, method)),
            selected_methods
          )

          progress$set(value = 58, message = "Running SignalP", detail = "58% | Comparing TE groups with selected annotations")
          context <- signalp_compute_context(
            te_context = session_state$te_context,
            preprocess_context = session_state$preprocess_context,
            upload_context = session_state$upload_context,
            resources = resources,
            parameters = parameters
          )

          progress$set(value = 92, message = "Running SignalP", detail = "92% | Preparing SignalP comparison summary")
          store_cached_analysis_context(source_signature, parameters, context)
          context
        }
      })

      if (is.null(context)) {
        return(invisible(NULL))
      }

      progress$set(value = 100, message = "Running SignalP", detail = "100% | SignalP results are ready")
      apply_context(context)
    }, ignoreInit = TRUE)

    observeEvent(input$export_plot, {
      context <- signalp_context()
      req(!is.null(context))

      settings <- signalp_export_settings(input)
      figure_extension <- if (identical(settings$format, "pdf")) "pdf" else "png"

      session$sendCustomMessage(
        "ribote-signalp-figure-export",
        list(
          hostId = session$ns("results_host"),
          selector = ".ribote-signalp-panel--plot .ribote-d3-host",
          format = settings$format,
          width = settings$width,
          height = settings$height,
          dpi = settings$dpi,
          backgroundColor = "#ffffff",
          filename = signalp_figure_filename(
            method_label = context$method_label,
            extension = figure_extension
          )
        )
      )
    }, ignoreInit = TRUE)

    observeEvent(input$data_export, {
      context <- signalp_context()
      req(!is.null(context))

      settings <- signalp_export_settings(input)

      session$sendCustomMessage(
        "ribote-archive-export",
        list(
          filename = signalp_archive_filename("data"),
          entries = signalp_export_entries(context, format = settings$data_format)
        )
      )
    }, ignoreInit = TRUE)

    output$run_hint <- renderUI({
      if (!ribote_has_upload(session_state)) {
        return(div(class = "ribote-hint", "Save species and count matrix context in Load Data first."))
      }

      if (!ribote_has_preprocess(session_state)) {
        return(div(class = "ribote-hint", "Run Data Preprocess first."))
      }

      if (!ribote_has_te(session_state)) {
        return(div(class = "ribote-hint", "Run Translation Efficiency first."))
      }

      if (!isTRUE(resources_available())) {
        return(div(class = "ribote-hint", "Local SignalP annotation resources are not installed for this species yet."))
      }

      NULL
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

    output$export_panel <- renderUI({
      if (is.null(signalp_context())) {
        return(NULL)
      }

      div(
        class = "ribote-sidebar__export",
        react_control_host(
          id = session$ns("export_host"),
          control_type = "ribote-signalp-export",
          class = "ribote-export-root",
          ready_input_id = session$ns("export_ready")
        )
      )
    })

    outputOptions(output, "run_hint", suspendWhenHidden = FALSE)
  })
}
