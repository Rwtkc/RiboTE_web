mod_translation_efficiency_server <- function(id, session_state, analysis_lock = NULL) {
  moduleServer(id, function(input, output, session) {
    config <- translation_efficiency_module_config()
    analysis_result <- reactiveVal(NULL)
    current_page <- reactiveVal(1L)
    current_search <- reactiveVal("")
    current_view <- reactiveVal("data")
    progress_handle <- reactiveVal(NULL)
    awaiting_result_render <- reactiveVal(FALSE)
    result_render_token <- reactiveVal(NULL)
    te_base_cache <- reactiveVal(list())
    last_preprocess_signature <- reactiveVal(NULL)
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
      ribote_has_preprocess(session_state)
    })

    current_preprocess_signature <- reactive({
      req(requirement_met())
      translation_efficiency_preprocess_signature(session_state$preprocess_context)
    })

    cache_key_for <- function(te_tool, preprocess_signature) {
      paste0(tolower(as.character(te_tool)), "::", as.character(preprocess_signature))
    }

    get_cached_base_context <- function(te_tool, preprocess_signature = NULL) {
      if (is.null(preprocess_signature)) {
        if (!isTRUE(requirement_met())) {
          return(NULL)
        }
        preprocess_signature <- current_preprocess_signature()
      }

      cache <- te_base_cache()
      cache[[cache_key_for(te_tool, preprocess_signature)]]
    }

    store_cached_base_context <- function(base_context) {
      cache <- te_base_cache()
      cache[[cache_key_for(base_context$te_tool, base_context$preprocess_signature)]] <- base_context
      te_base_cache(cache)
      invisible(base_context)
    }

    normalize_te_parameters <- function() {
      translation_efficiency_normalize_parameters(
        te_tool = input$te_tool,
        fvalue = input$fvalue,
        p_cutoff = input$p_cutoff,
        p_type = input$p_type
      )
    }

    update_analysis_result <- function(context) {
      te_status_column <- if ("TE_Status" %in% colnames(context$result_table)) {
        context$result_table$TE_Status
      } else {
        context$result_table$diffTE
      }
      diff_counts <- table(factor(te_status_column, levels = c("Up", "Non", "Down")))

      analysis_result(list(
        metrics = list(
          list(label = "Genes Assessed", value = format(nrow(context$result_table), big.mark = ",")),
          list(label = "Up TE", value = unname(diff_counts[["Up"]])),
          list(label = "Down TE", value = unname(diff_counts[["Down"]])),
          list(label = "Tool", value = context$parameters$te_tool)
        )
      ))
    }

    clear_te_results <- function(reset_navigation = FALSE) {
      session_state$te_context <- NULL
      session_state$te_ready <- FALSE

      if (isTRUE(reset_navigation)) {
        current_page(1L)
        current_search("")
        current_view("data")
      }

      awaiting_result_render(FALSE)
      result_render_token(NULL)
      analysis_result(NULL)
      publish_results()
      publish_export()
      invisible(NULL)
    }

    apply_cached_context <- function(context, reset_navigation = FALSE) {
      session_state$te_context <- context
      session_state$te_ready <- TRUE
      session_state$last_module <- "translation_efficiency"

      if (isTRUE(reset_navigation)) {
        current_page(1L)
        current_search("")
        current_view("data")
      }

      update_analysis_result(context)
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
      if (!ribote_has_te(session_state)) {
        clear_control_payload(session = session, id = session$ns("results_host"))
        return(invisible(NULL))
      }

      payload <- translation_efficiency_results_payload(
        te_context = session_state$te_context,
        page = current_page(),
        page_size = 10L,
        page_input_id = session$ns("result_page"),
        search_query = current_search(),
        search_input_id = session$ns("result_search"),
        active_view = current_view()
      )

      send_control_payload(
        session = session,
        id = session$ns("results_host"),
        control = "ribote-translation-results",
        config = utils::modifyList(
          payload,
          list(
            renderReadyInputId = session$ns("result_render_ready"),
            renderToken = result_render_token(),
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
      export_config <- translation_efficiency_export_config("translation_efficiency")
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
        control = "ribote-translation-export",
        config = utils::modifyList(
          export_config,
          list(
            ready = ribote_has_te(session_state),
            currentView = current_view(),
            figureDisabled = !ribote_has_te(session_state) || identical(current_view(), "data"),
            dataDisabled = !ribote_has_te(session_state)
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

    build_render_token <- function() {
      paste0(
        "translation-efficiency-",
        gsub("[^0-9]", "", format(Sys.time(), "%Y%m%d%H%M%OS3")),
        "-",
        sample.int(1000000, 1)
      )
    }

    te_progress_start <- function() {
      progress <- shiny::Progress$new(session = session, min = 0, max = 100)
      attr(progress, "rnameta_last_value") <- 0L
      progress$set(
        value = 0,
        message = "Running Translation Efficiency",
        detail = "0% | Initializing TE analysis"
      )
      progress
    }

    te_progress_update <- function(progress, value, detail) {
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
        message = "Running Translation Efficiency",
        detail = sprintf("%s%% | %s", display_value, detail)
      )

      invisible(NULL)
    }

    te_progress_close <- function(progress) {
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

    observe({
      preprocess_signature <- if (isTRUE(requirement_met())) current_preprocess_signature() else NULL
      if (!identical(preprocess_signature, last_preprocess_signature())) {
        te_base_cache(list())
        last_preprocess_signature(preprocess_signature)
      }
    })

    observe({
      te_available <- ribote_has_te(session_state)
      te_context <- session_state$te_context

      if (isTRUE(te_available) && !is.null(te_context)) {
        return(invisible(NULL))
      }

      analysis_result(NULL)
      awaiting_result_render(FALSE)
      result_render_token(NULL)
      publish_results()
      publish_export()
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

    observeEvent(input$result_render_ready, {
      if (!awaiting_result_render()) {
        return(invisible(NULL))
      }

      rendered_token <- input$result_render_ready
      if (is.null(rendered_token) || !identical(as.character(rendered_token), result_render_token())) {
        return(invisible(NULL))
      }

      te_progress_update(progress_handle(), 100, "Results are ready")
      te_progress_close(progress_handle())
      progress_handle(NULL)
      awaiting_result_render(FALSE)
      result_render_token(NULL)
    }, ignoreInit = TRUE)

    observeEvent(input$result_page, {
      req(ribote_has_te(session_state))
      requested_page <- suppressWarnings(as.integer(input$result_page))
      if (is.na(requested_page)) {
        return(invisible(NULL))
      }

      current_page(requested_page)
      publish_results()
    }, ignoreInit = TRUE)

    observeEvent(input$result_search, {
      req(ribote_has_te(session_state))

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

      parameters <- normalize_te_parameters()

      if (!is.null(progress_handle())) {
        te_progress_close(progress_handle())
        progress_handle(NULL)
      }

      progress <- te_progress_start()
      progress_handle(progress)
      awaiting_result_render(FALSE)
      result_render_token(NULL)
      clear_te_results(reset_navigation = TRUE)
      close_progress_on_exit <- TRUE
      on.exit({
        if (close_progress_on_exit) {
          te_progress_close(progress_handle())
          progress_handle(NULL)
          awaiting_result_render(FALSE)
          result_render_token(NULL)
        }
      }, add = TRUE)

      context <- capture_analysis({
        te_progress_update(progress, 18, "Loading processed matrix and saved sample pairing")
        base_context <- get_cached_base_context(parameters$te_tool)
        cache_hit <- !is.null(base_context)

        if (!cache_hit) {
          te_progress_update(progress, 56, sprintf("Computing %s TE statistics and significance summaries", parameters$te_tool))
          base_timing <- system.time({
            base_context <- translation_efficiency_build_base_context(
              preprocess_context = isolate(session_state$preprocess_context),
              upload_context = isolate(session_state$upload_context),
              te_tool = parameters$te_tool
            )
          })
          base_context$build_elapsed_sec <- unname(as.numeric(base_timing[["elapsed"]]))
          store_cached_base_context(base_context)
        } else {
          te_progress_update(progress, 56, sprintf("Reusing cached %s TE statistics", parameters$te_tool))
        }

        apply_timing <- system.time({
          te_context <- translation_efficiency_apply_parameters(
            base_context = base_context,
            fvalue = parameters$fvalue,
            p_cutoff = parameters$p_cutoff,
            p_type = parameters$p_type
          )
        })
        te_context <- translation_efficiency_attach_performance(
          te_context = te_context,
          base_context = base_context,
          cache_hit = cache_hit,
          base_elapsed_sec = if (cache_hit) 0 else base_context$build_elapsed_sec,
          apply_elapsed_sec = apply_timing[["elapsed"]]
        )
        te_progress_update(progress, 92, "Finalizing Translation Efficiency summaries")
        te_context
      })

      if (is.null(context)) {
        return(invisible(NULL))
      }

      apply_cached_context(context, reset_navigation = TRUE)

      result_render_token(build_render_token())
      awaiting_result_render(TRUE)
      te_progress_update(progress, 96, "Preparing results")

      publish_results()
      publish_export()
      close_progress_on_exit <- FALSE
    }, ignoreInit = TRUE)

    observeEvent(input$export_plot, {
      req(ribote_has_te(session_state))

      active_view <- current_view()
      if (identical(active_view, "data")) {
        showNotification("Switch to TE Volcano Plot or TE Scatter Plots to export figures.", type = "warning", duration = 4)
        return(invisible(NULL))
      }

      settings <- translation_efficiency_export_settings(input)
      if (identical(active_view, "volcano")) {
        session$sendCustomMessage(
          "ribote-translation-figure-export",
          list(
            hostId = session$ns("results_host"),
            selector = ".ribote-translation-panel--volcano .ribote-d3-card:first-child",
            format = settings$format,
            width = settings$width,
            height = settings$height,
            dpi = settings$dpi,
            backgroundColor = "#ffffff",
            forceRaster = TRUE,
            filename = sprintf("translation_efficiency_volcano.%s", settings$format),
            exportPadding = list(top = 12, right = 34, bottom = 22, left = 24)
          )
        )
        return(invisible(NULL))
      }

      export_entries <- switch(
        active_view,
        scatter = list(
          list(
            selector = ".ribote-translation-panel--scatter .ribote-d3-card:first-child",
            filename = sprintf("translation_efficiency_te_expression.%s", settings$format),
            exportPadding = list(top = 12, right = 34, bottom = 22, left = 24)
          ),
          list(
            selector = ".ribote-translation-panel--scatter .ribote-d3-card:nth-child(2)",
            filename = sprintf("translation_efficiency_input_expression.%s", settings$format),
            exportPadding = list(top = 12, right = 34, bottom = 22, left = 24)
          ),
          list(
            selector = ".ribote-translation-panel--scatter .ribote-d3-card:last-child",
            filename = sprintf("translation_efficiency_input_vs_te.%s", settings$format),
            exportPadding = list(top = 12, right = 34, bottom = 22, left = 24)
          )
        ),
        NULL
      )

      session$sendCustomMessage(
        "ribote-translation-multi-figure-export",
        list(
          hostId = session$ns("results_host"),
          format = settings$format,
          width = settings$width,
          height = settings$height,
          dpi = settings$dpi,
          backgroundColor = "#ffffff",
          forceRaster = TRUE,
          filename = translation_efficiency_archive_filename(paste0(active_view, "_figure")),
          exportEntries = export_entries
        )
      )
    }, ignoreInit = TRUE)

    observeEvent(input$data_export, {
      req(ribote_has_te(session_state))

      settings <- translation_efficiency_export_settings(input)
      extension <- if (identical(settings$data_format, "txt")) "txt" else "csv"
      mime_type <- if (identical(settings$data_format, "txt")) {
        "text/tab-separated-values;charset=utf-8"
      } else {
        "text/csv;charset=utf-8"
      }

      active_view <- current_view()

      if (identical(active_view, "data")) {
        session$sendCustomMessage(
          "ribote-text-export",
          list(
            filename = translation_efficiency_data_export_filename(extension),
            mimeType = mime_type,
            content = translation_efficiency_data_export_content(
              session_state$te_context,
              format = settings$data_format
            )
          )
        )
        return(invisible(NULL))
      }

      if (identical(active_view, "volcano")) {
        entries <- translation_efficiency_chart_export_entries(
          session_state$te_context,
          view = active_view,
          format = settings$data_format
        )

        session$sendCustomMessage(
          "ribote-text-export",
          list(
            filename = entries[[1]]$filename,
            mimeType = mime_type,
            content = entries[[1]]$content
          )
        )
        return(invisible(NULL))
      }

      session$sendCustomMessage(
        "ribote-archive-export",
        list(
          filename = translation_efficiency_archive_filename(paste0(active_view, "_data")),
          entries = translation_efficiency_chart_export_entries(
            session_state$te_context,
            view = active_view,
            format = settings$data_format
          )
        )
      )
    }, ignoreInit = TRUE)

    output$parameter_snapshot <- renderUI({
      NULL
    })

    output$analysis_notice <- renderUI({
      if (!isTRUE(ribote_has_upload(session_state))) {
        return(
          div(
            class = "ribote-hint",
            "Save species and count matrix context in Load Data first."
          )
        )
      }

      if (!isTRUE(ribote_has_preprocess(session_state))) {
        return(
          div(
            class = "ribote-hint",
            "Run Data Preprocess first."
          )
        )
      }

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
      req(requirement_met())

      div(
        class = "ribote-result-tabs",
        react_control_host(
          id = session$ns("results_host"),
          control_type = "ribote-translation-results",
          class = "ribote-results-root",
          ready_input_id = session$ns("results_ready")
        )
      )
    })

    outputOptions(output, "analysis_notice", suspendWhenHidden = FALSE)
  })
}
