mod_enrichment_server <- function(id, session_state, analysis_lock = NULL) {
  moduleServer(id, function(input, output, session) {
    config <- enrichment_module_config()
    enrichment_context <- reactiveVal(NULL)
    enrichment_resource_cache <- reactiveVal(list())
    enrichment_analysis_cache <- reactiveVal(list())
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

    resources_available <- reactive({
      if (!isTRUE(upload_ready())) {
        return(FALSE)
      }

      enrichment_supported_for_upload_context(session_state$upload_context)
    })

    requirement_met <- reactive({
      isTRUE(te_ready()) && isTRUE(resources_available())
    })

    species_key <- reactive({
      if (!isTRUE(upload_ready())) {
        return(NULL)
      }

      enrichment_species_key(session_state$upload_context)
    })

    current_source_signature <- reactive({
      req(te_ready())
      req(upload_ready())

      enrichment_source_signature(
        te_context = session_state$te_context,
        preprocess_context = session_state$preprocess_context,
        upload_context = session_state$upload_context
      )
    })

    normalized_parameters <- reactive({
      enrichment_normalize_parameters(
        collection = input$enrichment_collection,
        top_pathways = input$enrichment_top_pathways,
        sort_by = input$enrichment_sort_by,
        filtered_background = input$enrichment_filtered_background,
        remove_redundant = input$enrichment_remove_redundant,
        show_pathway_id = input$enrichment_show_pathway_id,
        species_key = species_key()
      )
    })

    request_control_value <- function(request, key, fallback = NULL) {
      controls <- if (is.list(request) && is.list(request$controls)) request$controls else NULL

      if (is.null(controls)) {
        return(fallback)
      }

      names_to_try <- unique(c(key, session$ns(key)))

      for (name in names_to_try) {
        value <- controls[[name]]
        if (!is.null(value)) {
          return(value)
        }
      }

      fallback
    }

    normalized_parameters_from_request <- function(request = NULL) {
      enrichment_normalize_parameters(
        collection = request_control_value(request, "enrichment_collection", input$enrichment_collection),
        top_pathways = request_control_value(request, "enrichment_top_pathways", input$enrichment_top_pathways),
        sort_by = request_control_value(request, "enrichment_sort_by", input$enrichment_sort_by),
        filtered_background = request_control_value(request, "enrichment_filtered_background", input$enrichment_filtered_background),
        remove_redundant = request_control_value(request, "enrichment_remove_redundant", input$enrichment_remove_redundant),
        show_pathway_id = request_control_value(request, "enrichment_show_pathway_id", input$enrichment_show_pathway_id),
        species_key = species_key()
      )
    }

    resource_cache_key_for <- function(species_key_value, collection) {
      paste(as.character(species_key_value), tolower(as.character(collection)), sep = "::")
    }

    analysis_cache_key_for <- function(source_signature = NULL, parameters = NULL) {
      if (is.null(source_signature)) {
        source_signature <- current_source_signature()
      }

      if (is.null(parameters)) {
        parameters <- normalized_parameters()
      }

      paste(enrichment_cache_version(), as.character(source_signature), enrichment_parameters_key(parameters), sep = "::")
    }

    get_cached_resource <- function(species_key_value, collection) {
      cache <- enrichment_resource_cache()
      cache[[resource_cache_key_for(species_key_value, collection)]]
    }

    store_cached_resource <- function(species_key_value, collection, resource) {
      cache <- enrichment_resource_cache()
      cache[[resource_cache_key_for(species_key_value, collection)]] <- resource
      enrichment_resource_cache(cache)
      invisible(resource)
    }

    get_or_load_resource <- function(species_key_value, collection) {
      cached <- get_cached_resource(species_key_value, collection)

      if (!is.null(cached)) {
        return(cached)
      }

      resource_path <- enrichment_resource_path(species_key_value, collection)
      if (is.null(resource_path)) {
        stop(sprintf(
          "Local %s gene set resources are not installed for species key '%s'.",
          enrichment_collection_label(collection),
          species_key_value
        ), call. = FALSE)
      }

      resource <- enrichment_read_gmt_resource(resource_path, species_key = species_key_value)
      store_cached_resource(species_key_value, collection, resource)
    }

    get_cached_analysis_context <- function(source_signature = NULL, parameters = NULL) {
      cache <- enrichment_analysis_cache()
      cache[[analysis_cache_key_for(source_signature, parameters)]]
    }

    store_cached_analysis_context <- function(source_signature, parameters, context) {
      cache <- enrichment_analysis_cache()
      cache[[analysis_cache_key_for(source_signature, parameters)]] <- context
      enrichment_analysis_cache(cache)
      invisible(context)
    }

    clear_enrichment_results <- function() {
      enrichment_context(NULL)
      publish_results()
      publish_export()
      invisible(NULL)
    }

    apply_context <- function(context) {
      enrichment_context(context)
      session_state$last_module <- "enrichment"
      publish_results()
      publish_export()
      invisible(context)
    }

    publish_controls <- function() {
      sections <- config$sections
      species_key_value <- species_key()

      if (!is.null(species_key_value) && nzchar(species_key_value)) {
        sections <- lapply(sections, function(section) {
          updated_fields <- lapply(section$fields, function(field) {
            if (identical(field$key, "enrichment_collection")) {
              field$options <- enrichment_collection_options(species_key_value)
              field$default <- enrichment_default_collection(species_key_value)
            }
            field
          })

          utils::modifyList(section, list(fields = updated_fields))
        })
      }

      send_control_payload(
        session = session,
        id = session$ns("controls_host"),
        control = "ribote-module-controls",
        config = list(
          title = config$title,
          sections = ribote_client_sections(session, sections)
        )
      )
    }

    publish_results <- function() {
      context <- enrichment_context()

      send_control_payload(
        session = session,
        id = session$ns("results_host"),
        control = "ribote-enrichment-results",
        config = if (is.null(context)) list() else enrichment_results_payload(context)
      )
    }

    publish_export <- function() {
      export_config <- enrichment_export_config("enrichment")
      export_config$ids <- list(
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
        control = "ribote-enrichment-export",
        config = utils::modifyList(
          export_config,
          list(
            ready = !is.null(enrichment_context()),
            figureDisabled = is.null(enrichment_context()),
            dataDisabled = is.null(enrichment_context())
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
      context <- enrichment_context()

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
        enrichment_analysis_cache(list())
        last_source_signature(next_signature)
      }
    })

    observe({
      next_signature <- if (isTRUE(te_ready()) && isTRUE(upload_ready())) current_source_signature() else NULL
      context <- enrichment_context()

      if (is.null(next_signature)) {
        if (!is.null(context)) {
          clear_enrichment_results()
        }
        return(invisible(NULL))
      }

      if (!is.null(context) && !identical(context$source_signature, next_signature)) {
        clear_enrichment_results()
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

    observeEvent(input$analysis_request, {
      req(requirement_met())
      req(!is_analysis_locked())
      session$sendCustomMessage(
        "rnameta:set-analysis-lock",
        list(
          locked = TRUE,
          owner = module_lock_id
        )
      )

      on.exit({
        session$sendCustomMessage(
          "rnameta:set-analysis-lock",
          list(
            locked = FALSE,
            owner = NULL
          )
        )
      }, add = TRUE)

      if (!is.null(analysis_lock)) {
        analysis_lock(module_lock_id)
      }

      on.exit({
        if (!is.null(analysis_lock) && identical(analysis_lock(), module_lock_id)) {
          analysis_lock(NULL)
        }
      }, add = TRUE)

      parameters <- normalized_parameters_from_request(input$analysis_request)
      source_signature <- current_source_signature()
      species_key_value <- species_key()
      cached_context <- get_cached_analysis_context(source_signature = source_signature, parameters = parameters)
      current_context <- enrichment_context()
      current_context_key <- if (is.null(current_context)) {
        NULL
      } else {
        analysis_cache_key_for(
          source_signature = current_context$source_signature,
          parameters = current_context$parameters
        )
      }
      next_context_key <- analysis_cache_key_for(
        source_signature = source_signature,
        parameters = parameters
      )
      progress <- shiny::Progress$new(session = session, min = 0, max = 100)
      on.exit(progress$close(), add = TRUE)
      progress$set(value = 0, message = "Running Enrichment", detail = "0% | Initializing enrichment analysis")

      if (!identical(current_context_key, next_context_key)) {
        clear_enrichment_results()
      }

      context <- capture_analysis({
        progress$set(value = 24, message = "Running Enrichment", detail = sprintf(
          "24%% | Resolving local %s gene sets for %s",
          enrichment_collection_label(parameters$collection),
          species_key_value
        ))

        if (!is.null(cached_context)) {
          progress$set(value = 88, message = "Running Enrichment", detail = "88% | Reusing cached enrichment table")
          cached_context
        } else {
          resource <- get_or_load_resource(species_key_value, parameters$collection)
          progress$set(value = 56, message = "Running Enrichment", detail = "56% | Building TE-derived Up and Down gene lists")

          context <- enrichment_compute_context(
            te_context = session_state$te_context,
            preprocess_context = session_state$preprocess_context,
            upload_context = session_state$upload_context,
            resource = resource,
            parameters = parameters
          )

          progress$set(value = 92, message = "Running Enrichment", detail = "92% | Building grouped enrichment table payload")
          store_cached_analysis_context(source_signature, parameters, context)
          context
        }
      })

      if (is.null(context)) {
        return(invisible(NULL))
      }

      progress$set(value = 100, message = "Running Enrichment", detail = "100% | Enrichment results are ready")
      apply_context(context)
    }, ignoreInit = TRUE)

    observeEvent(input$export_plot, {
      context <- enrichment_context()
      req(!is.null(context))

      settings <- enrichment_export_settings(input)
      figure_extension <- if (identical(settings$format, "pdf")) "pdf" else "png"

      session$sendCustomMessage(
        "ribote-enrichment-figure-export",
        list(
          hostId = session$ns("results_host"),
          selector = ".ribote-enrichment-panel--plot .ribote-d3-host",
          format = settings$format,
          width = settings$width,
          height = settings$height,
          dpi = settings$dpi,
          backgroundColor = "#ffffff",
          filename = enrichment_figure_filename(
            collection_label = context$collection_label,
            extension = figure_extension
          )
        )
      )
    }, ignoreInit = TRUE)

    observeEvent(input$data_export, {
      context <- enrichment_context()
      req(!is.null(context))

      settings <- enrichment_export_settings(input)
      export_table <- enrichment_export_table(context, scope = settings$data_scope)

      if (!is.data.frame(export_table) || nrow(export_table) == 0L) {
        showNotification("No enrichment table is available for export.", type = "warning", duration = 5)
        return(invisible(NULL))
      }

      extension <- if (identical(settings$data_format, "txt")) "txt" else "csv"
      mime_type <- if (identical(settings$data_format, "txt")) {
        "text/tab-separated-values;charset=utf-8"
      } else {
        "text/csv;charset=utf-8"
      }

      session$sendCustomMessage(
        "ribote-text-export",
        list(
          filename = enrichment_data_filename(
            collection_label = context$collection_label,
            scope = settings$data_scope,
            extension = extension
          ),
          mimeType = mime_type,
          content = enrichment_data_export_content(
            context = context,
            scope = settings$data_scope,
            format = settings$data_format
          )
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
        return(div(class = "ribote-hint", "Local Enrichment gene sets are not installed for this species yet."))
      }

      NULL
    })

    output$parameter_snapshot <- renderUI({
      NULL
    })

    output$export_panel <- renderUI({
      if (is.null(enrichment_context())) {
        return(NULL)
      }

      div(
        class = "ribote-sidebar__export",
        react_control_host(
          id = session$ns("export_host"),
          control_type = "ribote-enrichment-export",
          class = "ribote-export-root",
          ready_input_id = session$ns("export_ready")
        )
      )
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
