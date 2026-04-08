source("new/global.R", local = TRUE, encoding = "UTF-8")
source("new/shared/react_bridge.R", local = TRUE, encoding = "UTF-8")
source("new/shared/module_shell.R", local = TRUE, encoding = "UTF-8")
source("new/services/session_state.R", local = TRUE, encoding = "UTF-8")
source("new/modules/load_data/load_data.shared.R", local = TRUE, encoding = "UTF-8")
source("new/modules/load_data/load_data.server.R", local = TRUE, encoding = "UTF-8")
source("new/modules/data_preprocess/data_preprocess.shared.R", local = TRUE, encoding = "UTF-8")
source("new/modules/data_preprocess/data_preprocess.server.R", local = TRUE, encoding = "UTF-8")

preprocess_server_code <- paste(
  readLines("new/modules/data_preprocess/data_preprocess.server.R", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

stopifnot(grepl("clear_control_payload\\(session = session, id = session\\$ns\\(\"results_host\"\\)\\)", preprocess_server_code))
stopifnot(grepl("session_state\\$te_context <- NULL", preprocess_server_code))
stopifnot(grepl("session_state\\$te_ready <- FALSE", preprocess_server_code))
stopifnot(grepl("analysis_result\\(NULL\\)", preprocess_server_code))
stopifnot(grepl("if \\(is.null\\(result\\)\\) \\{", preprocess_server_code))

session_state <- create_session_state()

shiny::testServer(mod_load_data_server, args = list(session_state = session_state), {
  session$setInputs(load_demo = "demo")
})

analysis_lock <- shiny::reactiveVal(NULL)

shiny::testServer(
  mod_data_preprocess_server,
  args = list(session_state = session_state, analysis_lock = analysis_lock),
  {
    session$setInputs(
      na_strategy = "Zero Imputation",
      min_cpm = 0.5,
      min_libraries = 1,
      run_analysis = "run"
    )

    stopifnot(isTRUE(session_state$preprocess_ready))
    stopifnot(is.list(session_state$preprocess_context))
    stopifnot(file.exists(session_state$preprocess_context$matrix_path))
    stopifnot(is.data.frame(session_state$preprocess_context$preview))
    stopifnot(nrow(session_state$preprocess_context$preview) > 0)
    stopifnot(is.data.frame(session_state$preprocess_context$biotype_summary))
    stopifnot(is.data.frame(session_state$preprocess_context$rrna_summary))
    stopifnot(is.data.frame(session_state$preprocess_context$sample_display_index))
    stopifnot("sample_display" %in% colnames(session_state$preprocess_context$barplot_data))
    stopifnot("sample_display" %in% colnames(session_state$preprocess_context$rrna_summary))

    payload <- data_preprocess_results_payload(
      session_state$preprocess_context,
      page = 1L,
      page_size = 10L,
      search_query = "ENSG"
    )
    filtered_payload <- data_preprocess_results_payload(
      session_state$preprocess_context,
      page = 1L,
      page_size = 10L,
      search_query = "NO_MATCH_TOKEN"
    )
    summary_html <- output$analysis_summary$html
    tabs_html <- output$result_tabs$html

    stopifnot(identical(payload$table$pageSize, 10L))
    stopifnot(length(payload$table$rows) == 10L)
    stopifnot(payload$table$pageCount >= 2L)
    stopifnot(identical(payload$table$columns[[1]], "GeneID"))
    stopifnot(identical(payload$table$searchQuery, "ENSG"))
    stopifnot(identical(filtered_payload$table$totalRows, 0L))
    stopifnot(length(filtered_payload$table$rows) == 0L)
    stopifnot(identical(payload$charts$barplot[[1]]$sample_display, "RNA.C1"))
    stopifnot(identical(payload$charts$barplot[[1]]$sample, "RNA.WT1"))
    stopifnot(identical(payload$charts$rrna[[1]]$sample_display, "RNA.C1"))
    stopifnot(grepl("Genes Retained", summary_html, fixed = TRUE))
    stopifnot(grepl("Samples Retained", summary_html, fixed = TRUE))
    stopifnot(!grepl("QC Views", summary_html, fixed = TRUE))
    stopifnot(!grepl("Updated", summary_html, fixed = TRUE))
    stopifnot(grepl("ribote-preprocess-results", tabs_html, fixed = TRUE))
  }
)

cat("ribote data preprocess server test passed\n")
