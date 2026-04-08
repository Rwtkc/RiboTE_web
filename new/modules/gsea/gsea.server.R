mod_gsea_server <- function(id, session_state, analysis_lock = NULL) {
  moduleServer(id, function(input, output, session) {
    config <- gsea_module_config()
    gsea_context <- reactiveVal(NULL)
    gsea_pathway_cache <- reactiveVal(list())
    gsea_analysis_cache <- reactiveVal(list())
    current_selected_pathway <- reactiveVal("")
    last_source_signature <- reactiveVal(NULL)
    last_controls_species_key <- reactiveVal(NULL)
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

      gsea_species_key(session_state$upload_context)
    })

    resources_available <- reactive({
      if (!isTRUE(upload_ready())) {
        return(FALSE)
      }

      gsea_supported_for_upload_context(session_state$upload_context)
    })

    requirement_met <- reactive({
      isTRUE(te_ready()) && isTRUE(resources_available())
    })

    current_source_signature <- reactive({
      req(te_ready())
      req(upload_ready())

      gsea_source_signature(
        te_context = session_state$te_context,
        preprocess_context = session_state$preprocess_context,
        upload_context = session_state$upload_context
      )
    })

    normalized_parameters <- reactive({
      gsea_normalize_parameters(
        collection = input$gsea_collection,
        geneset_min = input$gsea_geneset_min,
        geneset_max = input$gsea_geneset_max,
        fdr_cutoff = input$gsea_fdr_cutoff,
        show_n = input$gsea_show_n,
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
      gsea_normalize_parameters(
        collection = request_control_value(request, "gsea_collection", input$gsea_collection),
        geneset_min = request_control_value(request, "gsea_geneset_min", input$gsea_geneset_min),
        geneset_max = request_control_value(request, "gsea_geneset_max", input$gsea_geneset_max),
        fdr_cutoff = request_control_value(request, "gsea_fdr_cutoff", input$gsea_fdr_cutoff),
        show_n = request_control_value(request, "gsea_show_n", input$gsea_show_n),
        species_key = species_key()
      )
    }

    pathway_cache_key_for <- function(species_key_value, collection) {
      paste(as.character(species_key_value), tolower(as.character(collection)), sep = "::")
    }

    analysis_cache_key_for <- function(source_signature = NULL, parameters = NULL) {
      if (is.null(source_signature)) {
        source_signature <- current_source_signature()
      }

      if (is.null(parameters)) {
        parameters <- normalized_parameters()
      }

      paste(gsea_cache_version(), as.character(source_signature), gsea_parameters_key(parameters), sep = "::")
    }

    get_cached_pathways <- function(species_key_value, collection) {
      cache <- gsea_pathway_cache()
      cache[[pathway_cache_key_for(species_key_value, collection)]]
    }

    store_cached_pathways <- function(species_key_value, collection, pathways) {
      cache <- gsea_pathway_cache()
      cache[[pathway_cache_key_for(species_key_value, collection)]] <- pathways
      gsea_pathway_cache(cache)
      invisible(pathways)
    }

    get_or_load_pathways <- function(species_key_value, collection) {
      cached <- get_cached_pathways(species_key_value, collection)

      if (!is.null(cached)) {
        return(cached)
      }

      resource_path <- gsea_resource_path(species_key_value, collection)
      if (is.null(resource_path)) {
        stop(sprintf(
          "Local %s gene set resources are not installed for species key '%s'.",
          gsea_collection_label(collection),
          species_key_value
        ), call. = FALSE)
      }

      pathways <- gsea_read_pathways(resource_path, species_key = species_key_value)
      store_cached_pathways(species_key_value, collection, pathways)
    }

    get_cached_analysis_context <- function(source_signature = NULL, parameters = NULL) {
      cache <- gsea_analysis_cache()
      cache[[analysis_cache_key_for(source_signature, parameters)]]
    }

    store_cached_analysis_context <- function(source_signature, parameters, context) {
      cache <- gsea_analysis_cache()
      cache[[analysis_cache_key_for(source_signature, parameters)]] <- context
      gsea_analysis_cache(cache)
      invisible(context)
    }

    clear_gsea_results <- function() {
      gsea_context(NULL)
      current_selected_pathway("")
      publish_results()
      publish_export()
      invisible(NULL)
    }

    apply_context <- function(context) {
      gsea_context(context)
      session_state$last_module <- "gsea"

      default_pathway <- if (is.data.frame(context$displayed_results) && nrow(context$displayed_results) > 0L) {
        as.character(context$displayed_results$pathway_id[[1]])
      } else {
        ""
      }

      current_selected_pathway(default_pathway)
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
            if (identical(field$key, "gsea_collection")) {
              field$options <- gsea_collection_options(species_key_value)
              field$default <- gsea_default_collection(species_key_value)
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

    schedule_publish_controls <- function() {
      session$onFlushed(function() {
        isolate(publish_controls())
      }, once = TRUE)
    }

    publish_results <- function() {
      context <- gsea_context()

      if (is.null(context)) {
        send_control_payload(
          session = session,
          id = session$ns("results_host"),
          control = "ribote-gsea-results",
          config = list()
        )
        return(invisible(NULL))
      }

      send_control_payload(
        session = session,
        id = session$ns("results_host"),
        control = "ribote-gsea-results",
        config = gsea_results_payload(
          context = context,
          selected_pathway_id = current_selected_pathway(),
          selected_pathway_input_id = session$ns("selected_pathway")
        )
      )
    }

    publish_export <- function() {
      export_config <- gsea_export_config("gsea")
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
        control = "ribote-gsea-export",
        config = utils::modifyList(
          export_config,
          list(
            ready = !is.null(gsea_context()),
            figureDisabled = is.null(gsea_context()),
            dataDisabled = is.null(gsea_context())
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
      context <- gsea_context()

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
        gsea_analysis_cache(list())
        last_source_signature(next_signature)
      }
    })

    observe({
      next_signature <- if (isTRUE(te_ready()) && isTRUE(upload_ready())) current_source_signature() else NULL
      context <- gsea_context()

      if (is.null(next_signature)) {
        if (!is.null(context)) {
          clear_gsea_results()
        }
        return(invisible(NULL))
      }

      if (!is.null(context) && !identical(context$source_signature, next_signature)) {
        clear_gsea_results()
      }

      invisible(NULL)
    })

    observeEvent(input$controls_ready, {
      publish_controls()
    }, ignoreInit = TRUE)

    observe({
      next_species_key <- species_key()

      if (!identical(next_species_key, last_controls_species_key())) {
        last_controls_species_key(next_species_key)
        schedule_publish_controls()
      }
    })

    observeEvent(input$results_ready, {
      publish_results()
    }, ignoreInit = TRUE)

    observeEvent(input$export_ready, {
      publish_export()
    }, ignoreInit = TRUE)

    observeEvent(input$selected_pathway, {
      context <- gsea_context()

      if (is.null(context)) {
        return(invisible(NULL))
      }

      next_pathway <- if (is.null(input$selected_pathway)) "" else as.character(input$selected_pathway)
      if (!nzchar(next_pathway)) {
        return(invisible(NULL))
      }

      current_selected_pathway(next_pathway)
      publish_results()
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
      current_context <- gsea_context()
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
      progress$set(value = 0, message = "Running GSEA", detail = "0% | Initializing ranked gene set enrichment workflow")

      if (!identical(current_context_key, next_context_key)) {
        clear_gsea_results()
      }

      context <- capture_analysis({
        progress$set(value = 22, message = "Running GSEA", detail = sprintf(
          "22%% | Resolving local %s gene sets for %s",
          gsea_collection_label(parameters$collection),
          species_key_value
        ))

        if (!is.null(cached_context)) {
          progress$set(value = 88, message = "Running GSEA", detail = "88% | Reusing cached enrichment results")
          cached_context
        } else {
          pathways <- get_or_load_pathways(species_key_value, parameters$collection)
          progress$set(value = 56, message = "Running GSEA", detail = "56% | Ranking genes by TE log2 fold change")

          context <- gsea_compute_context(
            te_context = session_state$te_context,
            preprocess_context = session_state$preprocess_context,
            upload_context = session_state$upload_context,
            pathways = pathways,
            parameters = parameters
          )

          progress$set(value = 92, message = "Running GSEA", detail = "92% | Building pathway table and enrichment curve payload")
          store_cached_analysis_context(source_signature, parameters, context)
          context
        }
      })

      if (is.null(context)) {
        return(invisible(NULL))
      }

      progress$set(value = 100, message = "Running GSEA", detail = "100% | GSEA results are ready")
      apply_context(context)
    }, ignoreInit = TRUE)

    observeEvent(input$export_plot, {
      context <- gsea_context()
      req(!is.null(context))

      settings <- gsea_export_settings(input)
      selected_pathway <- gsea_selected_pathway(context, current_selected_pathway())

      if (is.null(selected_pathway)) {
        showNotification("No GSEA pathway plot is available for export.", type = "warning", duration = 5)
        return(invisible(NULL))
      }

      figure_extension <- if (identical(settings$format, "pdf")) "pdf" else "png"

      session$sendCustomMessage(
        "ribote-gsea-figure-export",
        list(
          hostId = session$ns("results_host"),
          selector = ".ribote-gsea-panel--plot .ribote-d3-host",
          format = settings$format,
          width = settings$width,
          height = settings$height,
          dpi = settings$dpi,
          backgroundColor = "#ffffff",
          filename = gsea_figure_filename(
            collection_label = context$collection_label,
            pathway_name = as.character(selected_pathway$pathway[[1]]),
            extension = figure_extension
          )
        )
      )
    }, ignoreInit = TRUE)

    observeEvent(input$data_export, {
      context <- gsea_context()
      req(!is.null(context))

      settings <- gsea_export_settings(input)
      export_table <- gsea_export_table(context, scope = settings$data_scope)

      if (!is.data.frame(export_table) || nrow(export_table) == 0L) {
        showNotification("No GSEA pathway table is available for export.", type = "warning", duration = 5)
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
          filename = gsea_data_filename(
            collection_label = context$collection_label,
            scope = settings$data_scope,
            extension = extension
          ),
          mimeType = mime_type,
          content = gsea_data_export_content(
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
        return(div(class = "ribote-hint", "Local GSEA gene sets are not installed for this species yet."))
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

    outputOptions(output, "run_hint", suspendWhenHidden = FALSE)
  })
}
