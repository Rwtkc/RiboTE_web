mod_codon_server <- function(id, session_state, analysis_lock = NULL) {
  moduleServer(id, function(input, output, session) {
    input_usage_context <- reactiveVal(NULL)
    codon_bias_context <- reactiveVal(NULL)
    te_shift_context <- reactiveVal(NULL)
    pattern_views_context <- reactiveVal(NULL)
    codon_runs_context <- reactiveVal(NULL)
    codon_resource_cache <- reactiveVal(list())
    codon_base_context_cache <- reactiveVal(list())
    codon_analysis_cache <- reactiveVal(list())
    codon_bias_analysis_cache <- reactiveVal(list())
    codon_shift_analysis_cache <- reactiveVal(list())
    codon_pattern_analysis_cache <- reactiveVal(list())
    codon_run_analysis_cache <- reactiveVal(list())
    codon_data_export_cache <- reactiveVal(list())
    current_view <- reactiveVal("input_summary")
    active_group <- reactiveVal("Input and Usage")
    latest_result_view_request_seq <- reactiveVal(0)
    results_publish_token <- 0L
    last_source_signature <- reactiveVal(NULL)
    last_species_key <- reactiveVal(NULL)
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

    resources_ready <- reactive({
      isTRUE(upload_ready()) && codon_has_local_resources(session_state$upload_context)
    })

    bias_resources_ready <- reactive({
      isTRUE(upload_ready()) && isTRUE(resources_ready()) && codon_has_bias_resources(session_state$upload_context)
    })

    codons_selected <- reactive({
      length(normalized_parameters()$codon_select) > 0L
    })

    input_usage_requirement_met <- reactive({
      isTRUE(te_ready()) && isTRUE(resources_ready()) && isTRUE(codons_selected())
    })

    codon_bias_requirement_met <- reactive({
      isTRUE(te_ready()) && isTRUE(resources_ready()) && isTRUE(bias_resources_ready())
    })

    te_shift_requirement_met <- reactive({
      isTRUE(te_ready()) && isTRUE(resources_ready()) && isTRUE(codons_selected())
    })

    pattern_views_requirement_met <- reactive({
      isTRUE(te_ready()) && isTRUE(resources_ready()) && isTRUE(codons_selected())
    })

    codon_runs_requirement_met <- reactive({
      isTRUE(te_ready()) && isTRUE(resources_ready()) && isTRUE(codons_selected())
    })

    current_species_key <- reactive({
      if (!isTRUE(upload_ready())) {
        return(NULL)
      }

      codon_species_key(session_state$upload_context)
    })

    current_source_signature <- reactive({
      req(te_ready())
      req(upload_ready())

      codon_source_signature(
        te_context = session_state$te_context,
        preprocess_context = session_state$preprocess_context,
        upload_context = session_state$upload_context
      )
    })

    normalized_parameters <- reactive({
      codon_normalize_parameters(
        codon_select = input$codon_select,
        codon_direction = input$codon_direction,
        codon_display = input$codon_display
      )
    })

    resource_cache_key_for <- function(species_key_value = NULL) {
      if (is.null(species_key_value)) {
        species_key_value <- current_species_key()
      }

      as.character(species_key_value)
    }

    analysis_cache_key_for <- function(source_signature = NULL, parameters = NULL) {
      if (is.null(source_signature)) {
        source_signature <- current_source_signature()
      }

      if (is.null(parameters)) {
        parameters <- normalized_parameters()
      }

      paste(as.character(source_signature), codon_parameters_key(parameters), sep = "::")
    }

    get_cached_resource_context <- function(species_key_value = NULL) {
      cache <- codon_resource_cache()
      cache[[resource_cache_key_for(species_key_value)]]
    }

    store_cached_resource_context <- function(species_key_value, resource_context) {
      cache <- codon_resource_cache()
      cache[[resource_cache_key_for(species_key_value)]] <- resource_context
      codon_resource_cache(cache)
      invisible(resource_context)
    }

    get_or_build_resource_context <- function() {
      species_key_value <- current_species_key()
      cached_context <- get_cached_resource_context(species_key_value)

      if (!is.null(cached_context)) {
        return(cached_context)
      }

      resource_context <- codon_build_resource_context(session_state$upload_context)
      store_cached_resource_context(species_key_value, resource_context)
    }

    get_cached_base_context <- function(source_signature = NULL) {
      cache <- codon_base_context_cache()
      resolved_signature <- if (is.null(source_signature)) current_source_signature() else source_signature
      cache[[as.character(resolved_signature)]]
    }

    store_cached_base_context <- function(source_signature, context) {
      cache <- codon_base_context_cache()
      cache[[as.character(source_signature)]] <- context
      codon_base_context_cache(cache)
      invisible(context)
    }

    get_or_build_base_context <- function() {
      source_signature <- current_source_signature()
      cached_context <- get_cached_base_context(source_signature)

      if (!is.null(cached_context)) {
        return(cached_context)
      }

      resource_context <- get_or_build_resource_context()
      base_context <- codon_build_base_context(
        te_context = session_state$te_context,
        preprocess_context = session_state$preprocess_context,
        upload_context = session_state$upload_context,
        resource_context = resource_context
      )
      store_cached_base_context(source_signature, base_context)
    }

    get_cached_analysis_context <- function(source_signature = NULL, parameters = NULL) {
      cache <- codon_analysis_cache()
      cache[[analysis_cache_key_for(source_signature, parameters)]]
    }

    store_cached_analysis_context <- function(source_signature, parameters, context) {
      cache <- codon_analysis_cache()
      cache[[analysis_cache_key_for(source_signature, parameters)]] <- context
      codon_analysis_cache(cache)
      invisible(context)
    }

    get_cached_bias_analysis_context <- function(source_signature = NULL) {
      cache <- codon_bias_analysis_cache()
      resolved_signature <- if (is.null(source_signature)) current_source_signature() else source_signature
      cache[[as.character(resolved_signature)]]
    }

    store_cached_bias_analysis_context <- function(source_signature, context) {
      cache <- codon_bias_analysis_cache()
      cache[[as.character(source_signature)]] <- context
      codon_bias_analysis_cache(cache)
      invisible(context)
    }

    get_cached_shift_analysis_context <- function(source_signature = NULL, parameters = NULL) {
      cache <- codon_shift_analysis_cache()
      cache[[analysis_cache_key_for(source_signature, parameters)]]
    }

    store_cached_shift_analysis_context <- function(source_signature, parameters, context) {
      cache <- codon_shift_analysis_cache()
      cache[[analysis_cache_key_for(source_signature, parameters)]] <- context
      codon_shift_analysis_cache(cache)
      invisible(context)
    }

    get_cached_pattern_analysis_context <- function(source_signature = NULL, parameters = NULL) {
      cache <- codon_pattern_analysis_cache()
      cache[[analysis_cache_key_for(source_signature, parameters)]]
    }

    store_cached_pattern_analysis_context <- function(source_signature, parameters, context) {
      cache <- codon_pattern_analysis_cache()
      cache[[analysis_cache_key_for(source_signature, parameters)]] <- context
      codon_pattern_analysis_cache(cache)
      invisible(context)
    }

    get_cached_run_analysis_context <- function(source_signature = NULL, parameters = NULL) {
      cache <- codon_run_analysis_cache()
      cache[[analysis_cache_key_for(source_signature, parameters)]]
    }

    store_cached_run_analysis_context <- function(source_signature, parameters, context) {
      cache <- codon_run_analysis_cache()
      cache[[analysis_cache_key_for(source_signature, parameters)]] <- context
      codon_run_analysis_cache(cache)
      invisible(context)
    }

    group_views <- function(group_name) {
      Filter(function(view_id) {
        view_id %in% codon_supported_result_views() && identical(codon_view_group(view_id), group_name)
      }, codon_all_result_views())
    }

    get_data_export_cache_entry <- function(view = current_view(), format = "csv") {
      cache <- codon_data_export_cache()
      cache[[codon_data_export_cache_key(view, format)]]
    }

    clear_data_export_cache <- function(views = NULL) {
      if (is.null(views)) {
        codon_data_export_cache(list())
        return(invisible(NULL))
      }

      cache <- codon_data_export_cache()
      view_ids <- unique(vapply(views, codon_normalize_result_view, character(1)))

      for (view_id in view_ids) {
        for (format in codon_supported_data_export_formats()) {
          cache[[codon_data_export_cache_key(view_id, format)]] <- NULL
        }
      }

      codon_data_export_cache(cache)
      invisible(NULL)
    }

    warm_data_export_cache <- function(context, views) {
      if (is.null(context)) {
        return(invisible(NULL))
      }

      normalized_views <- unique(vapply(views, codon_normalize_result_view, character(1)))
      existing_cache <- codon_data_export_cache()
      views_to_build <- Filter(function(view_id) {
        codon_view_has_data(context, view_id) && !codon_data_export_cache_ready(existing_cache, view_id)
      }, normalized_views)

      if (!length(views_to_build)) {
        return(invisible(NULL))
      }

      cache <- codon_data_export_cache()
      built_entries <- codon_build_data_export_cache(
        context = context,
        views = views_to_build,
        formats = codon_supported_data_export_formats()
      )

      for (cache_key in names(built_entries)) {
        cache[[cache_key]] <- built_entries[[cache_key]]
      }

      codon_data_export_cache(cache)
      invisible(NULL)
    }

    merged_workspace_context <- function() {
      codon_merge_workspace_contexts(
        usage_context = input_usage_context(),
        bias_context = codon_bias_context(),
        shift_context = te_shift_context(),
        pattern_context = pattern_views_context(),
        run_context = codon_runs_context()
      )
    }

    clear_input_usage_results <- function() {
      clear_data_export_cache(group_views("Input and Usage"))
      input_usage_context(NULL)
      publish_results()
      publish_export()
      invisible(NULL)
    }

    clear_codon_bias_results <- function() {
      clear_data_export_cache(group_views("Codon Bias"))
      codon_bias_context(NULL)
      publish_results()
      publish_export()
      invisible(NULL)
    }

    clear_te_shift_results <- function() {
      clear_data_export_cache(group_views("TE Shift and Enrichment"))
      te_shift_context(NULL)
      publish_results()
      publish_export()
      invisible(NULL)
    }

    clear_pattern_views_results <- function() {
      clear_data_export_cache(group_views("Pattern Views"))
      pattern_views_context(NULL)
      publish_results()
      publish_export()
      invisible(NULL)
    }

    clear_codon_runs_results <- function() {
      clear_data_export_cache(group_views("Codon Runs"))
      codon_runs_context(NULL)
      publish_results()
      publish_export()
      invisible(NULL)
    }

    clear_codon_results <- function() {
      clear_data_export_cache()
      input_usage_context(NULL)
      codon_bias_context(NULL)
      te_shift_context(NULL)
      pattern_views_context(NULL)
      codon_runs_context(NULL)
      publish_results()
      publish_export()
      invisible(NULL)
    }

    apply_input_usage_context <- function(context) {
      input_usage_context(context)
      session_state$last_module <- "codon"
      publish_results()
      publish_export()
      invisible(context)
    }

    apply_codon_bias_context <- function(context) {
      codon_bias_context(context)
      session_state$last_module <- "codon"
      publish_results()
      publish_export()
      invisible(context)
    }

    apply_te_shift_context <- function(context) {
      te_shift_context(context)
      session_state$last_module <- "codon"
      publish_results()
      publish_export()
      invisible(context)
    }

    apply_pattern_views_context <- function(context) {
      pattern_views_context(context)
      session_state$last_module <- "codon"
      publish_results()
      publish_export()
      invisible(context)
    }

    apply_codon_runs_context <- function(context) {
      codon_runs_context(context)
      session_state$last_module <- "codon"
      publish_results()
      publish_export()
      invisible(context)
    }

    publish_controls <- function() {
      config <- codon_module_config()
      active_sections <- codon_sections_for_group("Input and Usage")

      send_control_payload(
        session = session,
        id = session$ns("controls_host"),
        control = "ribote-module-controls",
        config = list(
          title = config$title,
          sections = ribote_client_sections(session, active_sections)
        )
      )
    }

    publish_results <- function() {
      context <- merged_workspace_context()
      results_publish_token <<- results_publish_token + 1L

      send_control_payload(
        session = session,
        id = session$ns("results_host"),
        control = "ribote-codon-results",
        config = utils::modifyList(
          if (is.null(context)) {
            list(
              views = codon_views_payload(),
              inputSummary = list(),
              selectedCodonUsage = list(),
              selectedCodonVsRna = list(),
              cbiTaiByGroup = list(),
              cbiAssociations = list(),
              selectedCodonBurden = list(),
              codonEnrichmentShifted = list(),
              selectedCodonAcrossGroups = list(),
              permutationSupport = list(),
              teBiasSelectedLoad = list(),
              selectedLoadEffect = list(),
              codonClustering = list(),
              codonUsageHeatmap = list(),
              codonRunZscore = list(),
              codonRunEnrichment = list()
            )
          } else {
            codon_results_payload(context)
          },
          list(
            activeView = current_view(),
            activeViewToken = results_publish_token,
            activeViewRequestSeq = latest_result_view_request_seq(),
            activeViewInputId = session$ns("result_view"),
            sidebarGroupHostId = session$ns("sidebar_group_host"),
            summaryHostId = session$ns("analysis_summary_shell")
          )
        )
      )
    }

    publish_export <- function() {
      context <- merged_workspace_context()
      export_config <- codon_export_config("codon")
      ready_by_view <- stats::setNames(
        as.list(vapply(
          codon_supported_result_views(),
          function(view_id) {
            !is.null(context) && codon_view_has_data(context, view_id)
          },
          logical(1)
        )),
        codon_supported_result_views()
      )
      data_ready_by_view <- stats::setNames(
        as.list(vapply(
          codon_supported_result_views(),
          function(view_id) {
            !is.null(context) && codon_view_has_data(context, view_id) && codon_data_export_cache_ready(codon_data_export_cache(), view_id)
          },
          logical(1)
        )),
        codon_supported_result_views()
      )
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
        control = "ribote-codon-export",
        config = utils::modifyList(
          export_config,
          list(
            ready = !is.null(context) && current_view() %in% codon_supported_result_views() && codon_view_has_data(context, current_view()),
            currentView = current_view(),
            activeViewInputId = session$ns("result_view"),
            readyByView = ready_by_view,
            dataReadyByView = data_ready_by_view,
            figureDisabled = is.null(context) || !current_view() %in% codon_supported_result_views() || !codon_view_has_data(context, current_view()),
            dataDisabled = is.null(context) || !current_view() %in% codon_supported_result_views() || !codon_view_has_data(context, current_view()) || !isTRUE(data_ready_by_view[[current_view()]])
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
      context <- merged_workspace_context()

      if (is.null(context)) {
        return(NULL)
      }

      context$metrics
    })

    session$onFlushed(function() {
      isolate({
        publish_controls()
        publish_results()
        publish_export()
      })
    }, once = TRUE)

    observe({
      next_species_key <- if (isTRUE(upload_ready())) current_species_key() else NULL

      if (!identical(next_species_key, last_species_key())) {
        codon_resource_cache(list())
        codon_base_context_cache(list())
        clear_data_export_cache()
        last_species_key(next_species_key)
      }
    })

    observe({
      next_source_signature <- if (isTRUE(te_ready()) && isTRUE(upload_ready())) current_source_signature() else NULL

      if (!identical(next_source_signature, last_source_signature())) {
        codon_base_context_cache(list())
        codon_analysis_cache(list())
        codon_bias_analysis_cache(list())
        codon_shift_analysis_cache(list())
        codon_pattern_analysis_cache(list())
        codon_run_analysis_cache(list())
        clear_data_export_cache()
        last_source_signature(next_source_signature)
      }
    })

    observe({
      next_source_signature <- if (isTRUE(te_ready()) && isTRUE(upload_ready())) current_source_signature() else NULL
      context <- merged_workspace_context()

      if (is.null(next_source_signature)) {
        if (!is.null(context)) {
          clear_codon_results()
        }
        return(invisible(NULL))
      }

      if (!is.null(context) && !identical(context$source_signature, next_source_signature)) {
        clear_codon_results()
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

    observeEvent(input$result_view, {
      request_payload <- input$result_view
      request_seq <- suppressWarnings(as.numeric(if (is.list(request_payload)) request_payload$seq else NA))
      next_view <- codon_normalize_result_view(if (is.list(request_payload)) request_payload$view else request_payload)
      seq_advanced <- FALSE

      if (is.finite(request_seq)) {
        if (request_seq < latest_result_view_request_seq()) {
          return(invisible(NULL))
        }

        seq_advanced <- request_seq > latest_result_view_request_seq()
        latest_result_view_request_seq(request_seq)
      }

      if (!identical(next_view, current_view())) {
        previous_group <- active_group()
        next_group <- codon_view_group(next_view)

        current_view(next_view)
        if (!identical(next_group, previous_group)) {
          active_group(next_group)
        }
        publish_results()
        publish_export()
      } else if (seq_advanced) {
        publish_results()
        publish_export()
      }
    }, ignoreInit = TRUE)

    observe({
      set_native_button_state(
        session = session,
        id = session$ns("run_input_usage"),
        disabled = !input_usage_requirement_met() || is_analysis_locked()
      )
    })

    observe({
      set_native_button_state(
        session = session,
        id = session$ns("run_codon_bias"),
        disabled = !codon_bias_requirement_met() || is_analysis_locked()
      )
    })

    observe({
      set_native_button_state(
        session = session,
        id = session$ns("run_te_shift_enrichment"),
        disabled = !te_shift_requirement_met() || is_analysis_locked()
      )
    })

    observe({
      set_native_button_state(
        session = session,
        id = session$ns("run_pattern_views"),
        disabled = !pattern_views_requirement_met() || is_analysis_locked()
      )
    })

    observe({
      set_native_button_state(
        session = session,
        id = session$ns("run_codon_runs"),
        disabled = !codon_runs_requirement_met() || is_analysis_locked()
      )
    })

    observeEvent(input$run_input_usage, {
      req(input_usage_requirement_met())
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
      species_key_value <- current_species_key()
      cached_context <- get_cached_analysis_context(source_signature = source_signature, parameters = parameters)
      progress <- shiny::Progress$new(session = session, min = 0, max = 100)
      on.exit(progress$close(), add = TRUE)
      progress$set(value = 0, message = "Running Input and Usage", detail = "0% | Initializing the codon input and usage workspace")

      clear_input_usage_results()

      context <- capture_analysis({
        progress$set(value = 20, message = "Running Input and Usage", detail = sprintf(
          "20%% | Resolving representative CDS resources for %s",
          species_key_value
        ))

        if (!is.null(cached_context)) {
          progress$set(value = 88, message = "Running Input and Usage", detail = "88% | Reusing cached codon input and usage summaries")
          cached_context
        } else {
          progress$set(value = 56, message = "Running Input and Usage", detail = "56% | Merging Translation Efficiency results with representative codon frequencies")
          base_context <- get_or_build_base_context()

          progress$set(value = 84, message = "Running Input and Usage", detail = "84% | Preparing current input, TE-group usage, and RNA-abundance summaries")
          computed_context <- codon_compute_context(base_context, parameters)
          store_cached_analysis_context(source_signature, parameters, computed_context)
          computed_context
        }
      })

      if (is.null(context)) {
        return(invisible(NULL))
      }

      progress$set(value = 92, message = "Running Input and Usage", detail = "92% | Preparing export-ready CSV and TXT data for Input and Usage views")
      warm_data_export_cache(context, group_views("Input and Usage"))

      progress$set(value = 100, message = "Running Input and Usage", detail = "100% | Codon input and usage results are ready")
      apply_input_usage_context(context)
    }, ignoreInit = TRUE)

    observeEvent(input$run_codon_bias, {
      req(codon_bias_requirement_met())
      req(!is_analysis_locked())

      if (!is.null(analysis_lock)) {
        analysis_lock(module_lock_id)
      }

      on.exit({
        if (!is.null(analysis_lock) && identical(analysis_lock(), module_lock_id)) {
          analysis_lock(NULL)
        }
      }, add = TRUE)

      source_signature <- current_source_signature()
      species_key_value <- current_species_key()
      cached_context <- get_cached_bias_analysis_context(source_signature = source_signature)
      progress <- shiny::Progress$new(session = session, min = 0, max = 100)
      on.exit(progress$close(), add = TRUE)
      progress$set(value = 0, message = "Running Codon Bias", detail = "0% | Initializing the codon bias workspace")

      clear_codon_bias_results()

      context <- capture_analysis({
        progress$set(value = 18, message = "Running Codon Bias", detail = sprintf(
          "18%% | Resolving representative transcript resources for %s",
          species_key_value
        ))

        if (!is.null(cached_context)) {
          progress$set(value = 90, message = "Running Codon Bias", detail = "90% | Reusing cached codon bias and adaptation summaries")
          cached_context
        } else {
          progress$set(value = 54, message = "Running Codon Bias", detail = "54% | Linking local CBI and tAI resources to the current TE workspace")
          base_context <- get_or_build_base_context()

          progress$set(value = 84, message = "Running Codon Bias", detail = "84% | Preparing TE-group bias summaries and CBI association panels")
          computed_context <- codon_compute_bias_context(base_context, session_state$upload_context)
          store_cached_bias_analysis_context(source_signature, computed_context)
          computed_context
        }
      })

      if (is.null(context)) {
        return(invisible(NULL))
      }

      progress$set(value = 92, message = "Running Codon Bias", detail = "92% | Preparing export-ready CSV and TXT data for Codon Bias views")
      warm_data_export_cache(context, group_views("Codon Bias"))

      progress$set(value = 100, message = "Running Codon Bias", detail = "100% | Codon bias results are ready")
      apply_codon_bias_context(context)
    }, ignoreInit = TRUE)

    observeEvent(input$run_te_shift_enrichment, {
      req(te_shift_requirement_met())
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
      cached_context <- get_cached_shift_analysis_context(source_signature = source_signature, parameters = parameters)
      progress <- shiny::Progress$new(session = session, min = 0, max = 100)
      on.exit(progress$close(), add = TRUE)
      progress$set(value = 0, message = "Running TE Shift and Enrichment", detail = "0% | Initializing TE-shift codon summaries")

      clear_te_shift_results()

      context <- capture_analysis({
        if (!is.null(cached_context)) {
          progress$set(value = 90, message = "Running TE Shift and Enrichment", detail = "90% | Reusing cached TE-shift codon summaries")
          cached_context
        } else {
          progress$set(value = 48, message = "Running TE Shift and Enrichment", detail = "48% | Preparing selected-codon burden, enrichment, permutation, and TE-bias summaries")
          base_context <- get_or_build_base_context()
          computed_context <- codon_compute_shift_context(base_context, parameters)
          store_cached_shift_analysis_context(source_signature, parameters, computed_context)
          computed_context
        }
      })

      if (is.null(context)) {
        return(invisible(NULL))
      }

      progress$set(value = 92, message = "Running TE Shift and Enrichment", detail = "92% | Preparing export-ready CSV and TXT data for TE-shift views")
      warm_data_export_cache(context, group_views("TE Shift and Enrichment"))

      progress$set(value = 100, message = "Running TE Shift and Enrichment", detail = "100% | TE-shift codon results are ready")
      apply_te_shift_context(context)
    }, ignoreInit = TRUE)

    observeEvent(input$run_pattern_views, {
      req(pattern_views_requirement_met())
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
      cached_context <- get_cached_pattern_analysis_context(source_signature = source_signature, parameters = parameters)
      progress <- shiny::Progress$new(session = session, min = 0, max = 100)
      on.exit(progress$close(), add = TRUE)
      progress$set(value = 0, message = "Running Pattern Views", detail = "0% | Initializing codon pattern summaries")

      clear_pattern_views_results()

      context <- capture_analysis({
        if (!is.null(cached_context)) {
          progress$set(value = 90, message = "Running Pattern Views", detail = "90% | Reusing cached codon pattern summaries")
          cached_context
        } else {
          progress$set(value = 56, message = "Running Pattern Views", detail = "56% | Building codon co-usage clustering and codon-usage heatmap payloads")
          base_context <- get_or_build_base_context()
          computed_context <- codon_compute_pattern_context(base_context, parameters)
          store_cached_pattern_analysis_context(source_signature, parameters, computed_context)
          computed_context
        }
      })

      if (is.null(context)) {
        return(invisible(NULL))
      }

      progress$set(value = 92, message = "Running Pattern Views", detail = "92% | Preparing export-ready CSV and TXT data for Pattern Views")
      warm_data_export_cache(context, group_views("Pattern Views"))

      progress$set(value = 100, message = "Running Pattern Views", detail = "100% | Codon pattern results are ready")
      apply_pattern_views_context(context)
    }, ignoreInit = TRUE)

    observeEvent(input$run_codon_runs, {
      req(codon_runs_requirement_met())
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
      cached_context <- get_cached_run_analysis_context(source_signature = source_signature, parameters = parameters)
      progress <- shiny::Progress$new(session = session, min = 0, max = 100)
      on.exit(progress$close(), add = TRUE)
      progress$set(value = 0, message = "Running Codon Runs", detail = "0% | Initializing codon run summaries")

      clear_codon_runs_results()

      context <- capture_analysis({
        if (!is.null(cached_context)) {
          progress$set(value = 90, message = "Running Codon Runs", detail = "90% | Reusing cached codon run summaries")
          cached_context
        } else {
          progress$set(value = 56, message = "Running Codon Runs", detail = "56% | Counting selected-codon runs and summarizing run load behavior across TE groups")
          base_context <- get_or_build_base_context()
          computed_context <- codon_compute_run_context(base_context, parameters)
          store_cached_run_analysis_context(source_signature, parameters, computed_context)
          computed_context
        }
      })

      if (is.null(context)) {
        return(invisible(NULL))
      }

      progress$set(value = 92, message = "Running Codon Runs", detail = "92% | Preparing export-ready CSV and TXT data for Codon Run views")
      warm_data_export_cache(context, group_views("Codon Runs"))

      progress$set(value = 100, message = "Running Codon Runs", detail = "100% | Codon run results are ready")
      apply_codon_runs_context(context)
    }, ignoreInit = TRUE)

    observeEvent(input$export_plot, {
      context <- merged_workspace_context()
      req(!is.null(context))
      req(codon_view_has_data(context, current_view()))

      settings <- codon_export_settings(input)
      figure_extension <- if (identical(settings$format, "pdf")) "pdf" else "png"

      session$sendCustomMessage(
        "ribote-codon-figure-export",
        list(
          hostId = session$ns("results_host"),
          selector = ".ribote-codon-view",
          format = settings$format,
          width = settings$width,
          height = settings$height,
          dpi = settings$dpi,
          backgroundColor = "#ffffff",
          filename = codon_figure_filename("current_view", extension = figure_extension)
        )
      )
    }, ignoreInit = TRUE)

    observeEvent(input$data_export, {
      context <- merged_workspace_context()
      req(!is.null(context))
      req(codon_view_has_data(context, current_view()))

      settings <- codon_export_settings(input)
      mime_type <- if (identical(settings$data_format, "txt")) {
        "text/tab-separated-values;charset=utf-8"
      } else {
        "text/csv;charset=utf-8"
      }
      entry <- get_data_export_cache_entry(
        view = current_view(),
        format = settings$data_format
      )

      if (is.null(entry)) {
        showNotification("Export data are still being prepared for this Codon view.", type = "message", duration = 4)
        publish_export()
        return(invisible(NULL))
      }

      session$sendCustomMessage(
        "ribote-text-export",
        list(
          filename = entry$filename,
          mimeType = mime_type,
          content = entry$content
        )
      )
    }, ignoreInit = TRUE)

    output$run_input_usage_hint <- renderUI({
      if (!ribote_has_upload(session_state)) {
        return(div(class = "ribote-hint", "Save species and count matrix context in Load Data first."))
      }

      if (!ribote_has_preprocess(session_state)) {
        return(div(class = "ribote-hint", "Run Data Preprocess first."))
      }

      if (!ribote_has_te(session_state)) {
        return(div(class = "ribote-hint", "Run Translation Efficiency first."))
      }

      if (!isTRUE(resources_ready())) {
        return(div(class = "ribote-hint", "Local CDS and transcript resources are not installed for this species yet."))
      }

      if (!isTRUE(codons_selected())) {
        return(div(class = "ribote-hint", "Select at least one codon in Input and Usage first."))
      }

      NULL
    })

    output$run_codon_bias_hint <- renderUI({
      if (!ribote_has_upload(session_state)) {
        return(div(class = "ribote-hint", "Save species and count matrix context in Load Data first."))
      }

      if (!ribote_has_preprocess(session_state)) {
        return(div(class = "ribote-hint", "Run Data Preprocess first."))
      }

      if (!ribote_has_te(session_state)) {
        return(div(class = "ribote-hint", "Run Translation Efficiency first."))
      }

      if (!isTRUE(resources_ready())) {
        return(div(class = "ribote-hint", "Representative CDS and transcript resources are required before Codon Bias can be linked to genes."))
      }

      if (!isTRUE(bias_resources_ready())) {
        return(div(class = "ribote-hint", "Local Codon Bias resources (.tai and .cds.m) are not installed for this species yet."))
      }

      NULL
    })

    output$run_te_shift_enrichment_hint <- renderUI({
      if (!ribote_has_upload(session_state)) {
        return(div(class = "ribote-hint", "Save species and count matrix context in Load Data first."))
      }

      if (!ribote_has_preprocess(session_state)) {
        return(div(class = "ribote-hint", "Run Data Preprocess first."))
      }

      if (!ribote_has_te(session_state)) {
        return(div(class = "ribote-hint", "Run Translation Efficiency first."))
      }

      if (!isTRUE(resources_ready())) {
        return(div(class = "ribote-hint", "Representative CDS and transcript resources are required before TE-shift codon views can be computed."))
      }

      if (!isTRUE(codons_selected())) {
        return(div(class = "ribote-hint", "Select at least one codon in Input and Usage first."))
      }

      NULL
    })

    output$run_pattern_views_hint <- renderUI({
      if (!ribote_has_upload(session_state)) {
        return(div(class = "ribote-hint", "Save species and count matrix context in Load Data first."))
      }

      if (!ribote_has_preprocess(session_state)) {
        return(div(class = "ribote-hint", "Run Data Preprocess first."))
      }

      if (!ribote_has_te(session_state)) {
        return(div(class = "ribote-hint", "Run Translation Efficiency first."))
      }

      if (!isTRUE(resources_ready())) {
        return(div(class = "ribote-hint", "Representative CDS and transcript resources are required before codon pattern views can be computed."))
      }

      if (!isTRUE(codons_selected())) {
        return(div(class = "ribote-hint", "Select at least one codon in Input and Usage first."))
      }

      NULL
    })

    output$run_codon_runs_hint <- renderUI({
      if (!ribote_has_upload(session_state)) {
        return(div(class = "ribote-hint", "Save species and count matrix context in Load Data first."))
      }

      if (!ribote_has_preprocess(session_state)) {
        return(div(class = "ribote-hint", "Run Data Preprocess first."))
      }

      if (!ribote_has_te(session_state)) {
        return(div(class = "ribote-hint", "Run Translation Efficiency first."))
      }

      if (!isTRUE(resources_ready())) {
        return(div(class = "ribote-hint", "Representative CDS and transcript resources are required before codon run views can be computed."))
      }

      if (!isTRUE(codons_selected())) {
        return(div(class = "ribote-hint", "Select at least one codon in Input and Usage first."))
      }

      NULL
    })

    output$parameter_snapshot <- renderUI({
      NULL
    })

    output$analysis_summary <- renderUI({
      metrics <- current_summary_metrics()

      if (is.null(metrics) || !length(metrics)) {
        return(NULL)
      }

      ribote_result_ui(list(metrics = metrics))
    })

    outputOptions(output, "run_input_usage_hint", suspendWhenHidden = FALSE)
    outputOptions(output, "run_codon_bias_hint", suspendWhenHidden = FALSE)
    outputOptions(output, "run_te_shift_enrichment_hint", suspendWhenHidden = FALSE)
    outputOptions(output, "run_pattern_views_hint", suspendWhenHidden = FALSE)
    outputOptions(output, "run_codon_runs_hint", suspendWhenHidden = FALSE)
  })
}



