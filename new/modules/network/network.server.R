mod_network_server <- function(id, session_state, analysis_lock = NULL) {
  moduleServer(id, function(input, output, session) {
    network_context <- reactiveVal(NULL)
    analysis_result <- reactiveVal(NULL)
    network_base_cache <- reactiveVal(list())
    network_basis_cache <- reactiveVal(list())
    network_graph_cache <- reactiveVal(list())
    network_basis_context <- reactiveVal(NULL)
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
      network_base_signature(
        te_context = session_state$te_context,
        preprocess_context = session_state$preprocess_context
      )
    })

    normalized_parameters <- reactive({
      network_normalize_parameters(
        data_space = input$network_data_space,
        edge_threshold = input$network_edge_threshold,
        top_genes = input$network_top_genes,
        variable_genes = input$network_variable_genes,
        module_name = input$network_module,
        soft_power = input$network_soft_power,
        min_module_size = input$network_min_module_size
      )
    })

    base_cache_key_for <- function(base_signature = NULL, variable_genes = NULL) {
      if (is.null(base_signature)) {
        base_signature <- current_base_signature()
      }

      if (is.null(variable_genes)) {
        variable_genes <- normalized_parameters()$variable_genes
      }

      paste(as.character(base_signature), as.integer(variable_genes), sep = "::")
    }

    basis_cache_key_for <- function(base_signature = NULL, parameters = NULL) {
      if (is.null(base_signature)) {
        base_signature <- current_base_signature()
      }

      if (is.null(parameters)) {
        parameters <- normalized_parameters()
      }

      network_basis_signature(base_signature, parameters)
    }

    graph_cache_key_for <- function(basis_signature = NULL, parameters = NULL, module_label = NULL) {
      if (is.null(parameters)) {
        parameters <- normalized_parameters()
      }

      if (is.null(basis_signature)) {
        basis_signature <- basis_cache_key_for(parameters = parameters)
      }

      network_graph_signature(
        basis_signature = basis_signature,
        parameters = parameters,
        module_label = module_label
      )
    }

    get_cached_base_context <- function(base_signature = NULL, variable_genes = NULL) {
      cache <- network_base_cache()
      cache[[base_cache_key_for(base_signature = base_signature, variable_genes = variable_genes)]]
    }

    store_cached_base_context <- function(base_context, variable_genes) {
      cache <- network_base_cache()
      cache[[base_cache_key_for(base_signature = base_context$base_signature, variable_genes = variable_genes)]] <- base_context
      network_base_cache(cache)
      invisible(base_context)
    }

    get_cached_basis_context <- function(base_signature = NULL, parameters = NULL) {
      cache <- network_basis_cache()
      cache[[basis_cache_key_for(base_signature = base_signature, parameters = parameters)]]
    }

    store_cached_basis_context <- function(basis_context) {
      cache <- network_basis_cache()
      cache[[basis_context$basis_signature]] <- basis_context
      network_basis_cache(cache)
      invisible(basis_context)
    }

    get_cached_graph_context <- function(basis_signature = NULL, parameters = NULL, module_label = NULL) {
      cache <- network_graph_cache()
      cache[[graph_cache_key_for(
        basis_signature = basis_signature,
        parameters = parameters,
        module_label = module_label
      )]]
    }

    store_cached_graph_context <- function(graph_context) {
      cache <- network_graph_cache()
      cache[[graph_context$graph_signature]] <- graph_context
      network_graph_cache(cache)
      invisible(graph_context)
    }

    get_or_build_base_context <- function(parameters) {
      base_context <- get_cached_base_context(variable_genes = parameters$variable_genes)

      if (is.null(base_context)) {
        base_context <- network_build_base_context(
          te_context = isolate(session_state$te_context),
          preprocess_context = isolate(session_state$preprocess_context),
          upload_context = isolate(session_state$upload_context),
          variable_genes = parameters$variable_genes
        )
        store_cached_base_context(base_context, variable_genes = parameters$variable_genes)
      }

      base_context
    }

    get_or_build_basis_context <- function(parameters) {
      basis_context <- get_cached_basis_context(parameters = parameters)

      if (is.null(basis_context)) {
        basis_context <- network_build_basis_context(
          base_context = get_or_build_base_context(parameters),
          parameters = parameters
        )
        store_cached_basis_context(basis_context)
      }

      basis_context
    }

    get_or_build_graph_context <- function(parameters) {
      basis_context <- get_or_build_basis_context(parameters)
      module_selection <- network_resolve_module_selection(basis_context, parameters$module_name)
      cached_graph <- get_cached_graph_context(
        basis_signature = basis_context$basis_signature,
        parameters = parameters,
        module_label = module_selection$label
      )

      if (!is.null(cached_graph)) {
        return(cached_graph)
      }

      graph_context <- network_build_graph_context(
        basis_context = basis_context,
        parameters = parameters
      )
      store_cached_graph_context(graph_context)
    }

    current_module_options <- reactive({
      basis_context <- network_basis_context()

      if (is.null(basis_context) || is.null(basis_context$module_options) || !length(basis_context$module_options)) {
        return(network_default_module_options())
      }

      basis_context$module_options
    })

    current_module_default <- reactive({
      context <- network_context()
      option_values <- vapply(current_module_options(), function(option) option$value, character(1))

      if (!is.null(context) && context$module_value %in% option_values) {
        return(context$module_value)
      }

      option_values[[1]]
    })

    network_config <- reactive({
      network_module_config(
        module_options = current_module_options(),
        selected_module = current_module_default()
      )
    })

    clear_network_results <- function() {
      network_context(NULL)
      analysis_result(NULL)
      publish_results()
      publish_export()
      invisible(NULL)
    }

    apply_context <- function(context, basis_context) {
      network_context(context)
      network_basis_context(basis_context)
      analysis_result(list(metrics = context$metrics))
      session_state$last_module <- "network"
      publish_controls()
      publish_results()
      publish_export()
      invisible(context)
    }

    publish_controls <- function() {
      config <- network_config()

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
      context <- network_context()

      if (is.null(context)) {
        clear_control_payload(session = session, id = session$ns("results_host"))
        return(invisible(NULL))
      }

      send_control_payload(
        session = session,
        id = session$ns("results_host"),
        control = "ribote-network-results",
        config = network_results_payload(context)
      )
    }

    publish_export <- function() {
      export_config <- network_export_config("network")
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
        control = "ribote-network-export",
        config = utils::modifyList(
          export_config,
          list(
            ready = !is.null(network_context()),
            figureDisabled = is.null(network_context()),
            dataDisabled = is.null(network_context())
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
        network_base_cache(list())
        network_basis_cache(list())
        network_graph_cache(list())
        network_basis_context(NULL)
        last_base_signature(next_base_signature)
      }
    })

    observe({
      next_base_signature <- if (isTRUE(requirement_met())) current_base_signature() else NULL
      context <- network_context()

      if (is.null(next_base_signature)) {
        if (!is.null(context)) {
          clear_network_results()
        }
        network_basis_context(NULL)
        publish_controls()
        return(invisible(NULL))
      }

      if (!is.null(context) && !identical(context$basis_signature, next_base_signature) && !startsWith(context$basis_signature, next_base_signature)) {
        clear_network_results()
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

      parameters <- normalized_parameters()
      data_space_label <- network_data_space_label(parameters$data_space)
      progress <- shiny::Progress$new(session = session, min = 0, max = 100)
      on.exit(progress$close(), add = TRUE)
      progress$set(
        value = 0,
        message = "Running Network",
        detail = sprintf("0%% | Initializing %s co-expression workflow", data_space_label)
      )

      clear_network_results()

      context <- capture_analysis({
        progress$set(
          value = 18,
          message = "Running Network",
          detail = sprintf("18%% | Preparing %s values from the current Translation Efficiency results", data_space_label)
        )
        base_context <- get_or_build_base_context(parameters)

        progress$set(
          value = 48,
          message = "Running Network",
          detail = sprintf("48%% | Detecting WGCNA modules on the staged %s matrix", data_space_label)
        )
        basis_context <- get_or_build_basis_context(parameters)
        network_basis_context(basis_context)
        publish_controls()

        resolved_module <- network_resolve_module_selection(basis_context, parameters$module_name)
        graph_parameters <- parameters
        graph_parameters$module_name <- resolved_module$value

        progress$set(value = 82, message = "Running Network", detail = sprintf(
          "82%% | Rendering %s with top %s genes and edge threshold %.2f",
          resolved_module$label,
          format(graph_parameters$top_genes, big.mark = ","),
          graph_parameters$edge_threshold
        ))

        graph_context <- get_or_build_graph_context(graph_parameters)
        progress$set(value = 100, message = "Running Network", detail = "100% | Network graph is ready")

        list(
          graph_context = graph_context,
          basis_context = basis_context
        )
      })

      if (is.null(context)) {
        return(invisible(NULL))
      }

      apply_context(context$graph_context, context$basis_context)
    }, ignoreInit = TRUE)

    observeEvent(input$export_plot, {
      context <- network_context()
      req(!is.null(context))

      settings <- network_export_settings(input)
      figure_extension <- if (identical(settings$format, "pdf")) "pdf" else "png"

      session$sendCustomMessage(
        "ribote-network-figure-export",
        list(
          hostId = session$ns("results_host"),
          selector = ".ribote-network-panel--graph .ribote-d3-host",
          format = settings$format,
          width = settings$width,
          height = settings$height,
          dpi = settings$dpi,
          backgroundColor = "#ffffff",
          filename = network_figure_filename(
            data_space = context$parameters$data_space,
            module_label = context$module_label,
            extension = figure_extension
          )
        )
      )
    }, ignoreInit = TRUE)

    observeEvent(input$data_export, {
      context <- network_context()
      req(!is.null(context))

      settings <- network_export_settings(input)
      extension <- if (identical(settings$data_format, "txt")) "txt" else "csv"

      session$sendCustomMessage(
        "ribote-archive-export",
        list(
          filename = network_archive_filename("data"),
          entries = list(
            list(
              filename = network_nodes_filename(
                data_space = context$parameters$data_space,
                module_label = context$module_label,
                extension = extension
              ),
              content = network_nodes_export_content(context, format = settings$data_format)
            ),
            list(
              filename = network_edges_filename(
                data_space = context$parameters$data_space,
                module_label = context$module_label,
                extension = extension,
                kind = "thresholded"
              ),
              content = network_edges_export_content(context, format = settings$data_format)
            ),
            list(
              filename = network_edges_filename(
                data_space = context$parameters$data_space,
                module_label = context$module_label,
                extension = extension,
                kind = "displayed"
              ),
              content = network_display_edges_export_content(context, format = settings$data_format)
            )
          )
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
