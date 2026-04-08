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

te_server_code <- paste(
  readLines("new/modules/translation_efficiency/translation_efficiency.server.R", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

stopifnot(grepl("active_view = current_view\\(\\)", te_server_code))
stopifnot(grepl("req\\(requirement_met\\(\\)\\)", te_server_code))
stopifnot(grepl("publish_results\\(\\)", te_server_code))
stopifnot(grepl("result_render_ready", te_server_code, fixed = TRUE))
stopifnot(grepl("result_render_token", te_server_code, fixed = TRUE))
stopifnot(grepl("awaiting_result_render", te_server_code, fixed = TRUE))
stopifnot(grepl("forceRaster = TRUE", te_server_code, fixed = TRUE))
stopifnot(grepl("te_available <- ribote_has_te\\(session_state\\)", te_server_code))
stopifnot(grepl("analysis_result\\(NULL\\)", te_server_code))
stopifnot(grepl("publish_results\\(\\)", te_server_code))
stopifnot(grepl("publish_export\\(\\)", te_server_code))

session_state <- create_session_state()

shiny::testServer(
  mod_translation_efficiency_server,
  args = list(session_state = session_state, analysis_lock = shiny::reactiveVal(NULL)),
  {
    notice_html <- output$analysis_notice$html
    stopifnot(length(notice_html) > 0L)
    stopifnot(any(grepl("Load Data", notice_html, fixed = TRUE)))
    stopifnot(!any(grepl("Data Preprocess", notice_html, fixed = TRUE)))
  }
)

shiny::testServer(mod_load_data_server, args = list(session_state = session_state), {
  session$setInputs(load_demo = "demo")
})

preprocess_lock <- shiny::reactiveVal(NULL)

shiny::testServer(
  mod_translation_efficiency_server,
  args = list(session_state = session_state, analysis_lock = shiny::reactiveVal(NULL)),
  {
    notice_html <- output$analysis_notice$html
    stopifnot(length(notice_html) > 0L)
    stopifnot(any(grepl("Data Preprocess", notice_html, fixed = TRUE)))
    stopifnot(!any(grepl("Load Data", notice_html, fixed = TRUE)))
  }
)

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

    te_ready <- isolate(session_state$te_ready)
    te_context <- isolate(session_state$te_context)
    last_module <- isolate(session_state$last_module)
    module_result <- analysis_result()

    stopifnot(isTRUE(te_ready))
    stopifnot(is.list(te_context))
    stopifnot(is.list(module_result))
    stopifnot(is.list(module_result$metrics))
    stopifnot(is.data.frame(te_context$result_table))
    stopifnot(nrow(te_context$result_table) > 0)
    stopifnot(is.data.frame(te_context$volcano_points))
    stopifnot(is.data.frame(te_context$scatter_points))
    stopifnot(is.data.frame(te_context$scatter_input))
    stopifnot(is.data.frame(te_context$scatter_te_expression))
    stopifnot(is.data.frame(te_context$scatter_rpf))
    stopifnot(is.data.frame(te_context$scatter_te))

    payload <- translation_efficiency_results_payload(
      te_context,
      page = 1L,
      page_size = 10L,
      search_query = "ENSG",
      active_view = "data"
    )

    stopifnot(identical(payload$table$pageSize, 10L))
    stopifnot(length(payload$table$rows) == 10L)
    stopifnot(identical(payload$table$columns[[1]], "GeneID"))
    stopifnot("RNA_Control_Mean" %in% unlist(payload$table$columns))
    stopifnot("RNA_Treatment_Mean" %in% unlist(payload$table$columns))
    stopifnot("RNA_log2FC" %in% unlist(payload$table$columns))
    stopifnot("RNA_Expression_Status" %in% unlist(payload$table$columns))
    stopifnot("Ribo_Control_Mean" %in% unlist(payload$table$columns))
    stopifnot("Ribo_Treatment_Mean" %in% unlist(payload$table$columns))
    stopifnot("Ribo_log2FC" %in% unlist(payload$table$columns))
    stopifnot("Ribo_Expression_Status" %in% unlist(payload$table$columns))
    stopifnot("TE_Control_Mean" %in% unlist(payload$table$columns))
    stopifnot("TE_Treatment_Mean" %in% unlist(payload$table$columns))
    stopifnot("TE_log2FC" %in% unlist(payload$table$columns))
    stopifnot("TE_Status" %in% unlist(payload$table$columns))
    pvalue_index <- match("pvalue", unlist(payload$table$columns))
    stopifnot(identical(unlist(payload$table$columns)[pvalue_index - 4L], "RNA_log2FC"))
    stopifnot(identical(unlist(payload$table$columns)[pvalue_index - 3L], "RNA_Expression_Status"))
    stopifnot(identical(unlist(payload$table$columns)[pvalue_index - 2L], "Ribo_log2FC"))
    stopifnot(identical(unlist(payload$table$columns)[pvalue_index - 1L], "Ribo_Expression_Status"))
    stopifnot(identical(unlist(payload$table$columns)[pvalue_index + 1L], "padj"))
    stopifnot(identical(unlist(payload$table$columns)[pvalue_index + 2L], "TE_log2FC"))
    stopifnot(!("input1" %in% unlist(payload$table$columns)))
    stopifnot(!("input2" %in% unlist(payload$table$columns)))
    stopifnot(!("logInputFC" %in% unlist(payload$table$columns)))
    stopifnot(!("diffExp" %in% unlist(payload$table$columns)))
    stopifnot(!("rpf1" %in% unlist(payload$table$columns)))
    stopifnot(!("rpf2" %in% unlist(payload$table$columns)))
    stopifnot(!("logRPFfc" %in% unlist(payload$table$columns)))
    stopifnot(!("diffRibo" %in% unlist(payload$table$columns)))
    stopifnot(!("RPF_Control_Mean" %in% unlist(payload$table$columns)))
    stopifnot(!("RPF_Treatment_Mean" %in% unlist(payload$table$columns)))
    stopifnot(!("RPF_log2FC" %in% unlist(payload$table$columns)))
    stopifnot(!("TE_A1" %in% unlist(payload$table$columns)))
    stopifnot(!("TE_A2" %in% unlist(payload$table$columns)))
    stopifnot(!("logTEfc" %in% unlist(payload$table$columns)))
    stopifnot(!("diffTE" %in% unlist(payload$table$columns)))
    stopifnot(abs(as.numeric(payload$table$rows[[1]][["RNA_Control_Mean"]]) - round(as.numeric(payload$table$rows[[1]][["RNA_Control_Mean"]]), 4)) < 1e-12)
    stopifnot(abs(as.numeric(payload$table$rows[[1]][["log2FoldChange"]]) - round(as.numeric(payload$table$rows[[1]][["log2FoldChange"]]), 4)) < 1e-12)
    stopifnot(is.list(payload$thresholds))
    stopifnot(isTRUE(all(c("foldChange", "significance") %in% names(payload$thresholds))))
    stopifnot(isTRUE(all.equal(as.numeric(payload$thresholds$foldChange), log2(1.5), tolerance = 1e-8)))
    stopifnot(isTRUE(all.equal(as.numeric(payload$thresholds$significance), -log10(0.05), tolerance = 1e-8)))
    stopifnot(length(payload$charts$volcano) == 0)
    stopifnot(length(payload$charts$scatter) == 0)
    stopifnot(length(payload$charts$status) == 0)

    volcano_payload <- translation_efficiency_results_payload(
      te_context,
      page = 1L,
      page_size = 10L,
      active_view = "volcano"
    )
    stopifnot(length(volcano_payload$charts$volcano) > 0)
    stopifnot(length(volcano_payload$charts$status) == 0)
    stopifnot(length(volcano_payload$charts$scatter) == 0)

    scatter_payload <- translation_efficiency_results_payload(
      te_context,
      page = 1L,
      page_size = 10L,
      active_view = "scatter"
    )
    stopifnot(length(scatter_payload$charts$scatter) > 0)
    stopifnot(length(scatter_payload$charts$scatterInput) > 0)
    stopifnot(length(scatter_payload$charts$scatterTeExpression) > 0)
    stopifnot(length(scatter_payload$charts$scatterTe) > 0)
    stopifnot(length(scatter_payload$charts$volcano) == 0)
    te_status_counts <- table(factor(te_context$result_table$TE_Status, levels = c("Up", "Non", "Down")))
    up_metric <- module_result$metrics[[2]]$value
    down_metric <- module_result$metrics[[3]]$value
    stopifnot(identical(as.integer(up_metric), unname(as.integer(te_status_counts[["Up"]]))))
    stopifnot(identical(as.integer(down_metric), unname(as.integer(te_status_counts[["Down"]]))))
    stopifnot(identical(last_module, "translation_efficiency"))

  }
)

cat("ribote translation efficiency server test passed\n")
