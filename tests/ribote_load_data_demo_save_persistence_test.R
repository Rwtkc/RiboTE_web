source("new/global.R", local = TRUE, encoding = "UTF-8")
source("new/shared/react_bridge.R", local = TRUE, encoding = "UTF-8")
source("new/services/session_state.R", local = TRUE, encoding = "UTF-8")
source("new/modules/load_data/load_data.shared.R", local = TRUE, encoding = "UTF-8")
source("new/modules/load_data/load_data.server.R", local = TRUE, encoding = "UTF-8")

session_state <- create_session_state()

shiny::testServer(mod_load_data_server, args = list(session_state = session_state), {
  session$setInputs(load_demo = "demo")

  html_after_demo <- output$session_summary$html
  stopifnot(grepl("ribote-preview-table", html_after_demo, fixed = TRUE))
  stopifnot(grepl("all.count.txt", html_after_demo, fixed = TRUE))
  stopifnot(grepl("Sample Pairing", html_after_demo, fixed = TRUE))
  stopifnot(grepl("RNA.WT1", html_after_demo, fixed = TRUE))
  stopifnot(grepl("RPF.WT1", html_after_demo, fixed = TRUE))
  stopifnot(isTRUE(session_state$upload_saved))
  stopifnot(grepl("Sample pairing confirmed", html_after_demo, fixed = TRUE))
  stopifnot(identical(session_state$upload_context$species_meta$acronym, "hg38"))
  stopifnot(file.exists(session_state$upload_context$matrix_path))
  stopifnot(file.exists(session_state$upload_context$resource_paths$org_db_path))
  stopifnot(isTRUE(session_state$upload_saved))
})

cat("ribote load data demo save persistence test passed\n")
