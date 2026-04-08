source("new/global.R", local = TRUE, encoding = "UTF-8")
source("new/shared/react_bridge.R", local = TRUE, encoding = "UTF-8")
source("new/shared/module_shell.R", local = TRUE, encoding = "UTF-8")
source("new/services/session_state.R", local = TRUE, encoding = "UTF-8")
source("new/modules/load_data/load_data.shared.R", local = TRUE, encoding = "UTF-8")
source("new/modules/load_data/load_data.server.R", local = TRUE, encoding = "UTF-8")
source("new/modules/data_preprocess/data_preprocess.shared.R", local = TRUE, encoding = "UTF-8")
source("new/modules/data_preprocess/data_preprocess.server.R", local = TRUE, encoding = "UTF-8")
source("new/modules/translation_efficiency/translation_efficiency.shared.R", local = TRUE, encoding = "UTF-8")
source("new/modules/translation_efficiency/translation_efficiency.server.R", local = TRUE, encoding = "UTF-8")

session_state <- create_session_state()

shiny::testServer(mod_load_data_server, args = list(session_state = session_state), {
  session$setInputs(load_demo = "demo")
})

preprocess_lock <- shiny::reactiveVal(NULL)

shiny::testServer(
  mod_data_preprocess_server,
  args = list(session_state = session_state, analysis_lock = preprocess_lock),
  {
    session$setInputs(
      na_strategy = "Zero Imputation",
      min_cpm = 0.5,
      min_libraries = 1
    )
    session$setInputs(run_analysis = 1)
  }
)

analysis_lock <- shiny::reactiveVal(NULL)

shiny::testServer(
  mod_translation_efficiency_server,
  args = list(session_state = session_state, analysis_lock = analysis_lock),
  {
    session$setInputs(
      te_tool = "Xtail",
      fvalue = 1.5,
      p_cutoff = 0.05,
      p_type = "Fdr"
    )
    session$setInputs(run_analysis = 1)

    te_context <- isolate(session_state$te_context)
    module_result <- analysis_result()
    tool_metric <- Filter(function(metric) identical(metric$label, "Tool"), module_result$metrics)

    stopifnot(isTRUE(isolate(session_state$te_ready)))
    stopifnot(is.list(te_context))
    stopifnot(is.data.frame(te_context$result_table))
    stopifnot(nrow(te_context$result_table) > 0)
    stopifnot("TE_Status" %in% colnames(te_context$result_table))
    stopifnot(sum(te_context$result_table$TE_Status == "Up", na.rm = TRUE) > 0)
    stopifnot(sum(te_context$result_table$TE_Status == "Down", na.rm = TRUE) > 0)
    stopifnot(length(tool_metric) == 1L)
    stopifnot(identical(tool_metric[[1]]$value, "Xtail"))

    volcano_payload <- translation_efficiency_results_payload(
      te_context,
      page = 1L,
      page_size = 10L,
      active_view = "volcano"
    )

    stopifnot(length(volcano_payload$charts$volcano) > 0)
    stopifnot(isTRUE(all(c("foldChange", "significance") %in% names(volcano_payload$thresholds))))
  }
)

cat("ribote translation efficiency xtail server test passed\n")
