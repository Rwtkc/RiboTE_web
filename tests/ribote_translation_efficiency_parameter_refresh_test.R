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
      te_tool = "Riborex",
      fvalue = 1.5,
      p_cutoff = 0.05,
      p_type = "Fdr"
    )
    session$setInputs(run_analysis = 1)

    riborex_context <- isolate(session_state$te_context)
    riborex_token <- riborex_context$performance$baseToken
    riborex_up_initial <- sum(riborex_context$result_table$TE_Status == "Up", na.rm = TRUE)

    stopifnot(isFALSE(riborex_context$performance$cacheHit))
    stopifnot(identical(riborex_context$parameters$te_tool, "Riborex"))
    stopifnot(is.finite(riborex_context$performance$baseElapsedSec))
    stopifnot(is.finite(riborex_context$performance$applyElapsedSec))

    session$setInputs(fvalue = 2.0, p_cutoff = 0.01)

    unchanged_riborex_context <- isolate(session_state$te_context)

    stopifnot(identical(unchanged_riborex_context$performance$baseToken, riborex_token))
    stopifnot(identical(unchanged_riborex_context$parameters$fvalue, 1.5))
    stopifnot(identical(unchanged_riborex_context$parameters$p_cutoff, 0.05))

    session$setInputs(run_analysis = 2)
    refreshed_riborex_context <- isolate(session_state$te_context)
    riborex_up_refreshed <- sum(refreshed_riborex_context$result_table$TE_Status == "Up", na.rm = TRUE)

    stopifnot(identical(refreshed_riborex_context$performance$baseToken, riborex_token))
    stopifnot(isTRUE(refreshed_riborex_context$performance$cacheHit))
    stopifnot(identical(refreshed_riborex_context$parameters$fvalue, 2.0))
    stopifnot(identical(refreshed_riborex_context$parameters$p_cutoff, 0.01))
    stopifnot(riborex_up_refreshed <= riborex_up_initial)

    session$setInputs(p_type = "RawPvalue")
    unchanged_rawp_context <- isolate(session_state$te_context)
    stopifnot(identical(unchanged_rawp_context$performance$baseToken, riborex_token))
    stopifnot(identical(unchanged_rawp_context$parameters$p_type, "Fdr"))

    session$setInputs(run_analysis = 3)
    refreshed_rawp_context <- isolate(session_state$te_context)

    stopifnot(identical(refreshed_rawp_context$parameters$te_tool, "Riborex"))
    stopifnot(identical(refreshed_rawp_context$performance$baseToken, riborex_token))
    stopifnot(isTRUE(refreshed_rawp_context$performance$cacheHit))
    stopifnot(identical(refreshed_rawp_context$parameters$p_type, "RawPvalue"))
  }
)

cat("ribote translation efficiency parameter refresh test passed\n")
