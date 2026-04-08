source("new/global.R", local = TRUE, encoding = "UTF-8")
source("new/shared/react_bridge.R", local = TRUE, encoding = "UTF-8")
source("new/services/session_state.R", local = TRUE, encoding = "UTF-8")
source("new/modules/load_data/load_data.shared.R", local = TRUE, encoding = "UTF-8")
source("new/modules/load_data/load_data.server.R", local = TRUE, encoding = "UTF-8")

session_state <- create_session_state()

sample_type_manifest <- jsonlite::toJSON(
  list(
    list(sample_name = "RNA.WT1", sample_type = "RNA-seq"),
    list(sample_name = "RNA.WT2", sample_type = "RNA-seq"),
    list(sample_name = "RNA.KO1", sample_type = "RNA-seq"),
    list(sample_name = "RNA.KO2", sample_type = "RNA-seq"),
    list(sample_name = "RPF.WT1", sample_type = "Ribo-seq"),
    list(sample_name = "RPF.WT2", sample_type = "Ribo-seq"),
    list(sample_name = "RPF.KO1", sample_type = "Ribo-seq"),
    list(sample_name = "RPF.KO2", sample_type = "Ribo-seq")
  ),
  auto_unbox = TRUE
)

pair_manifest <- jsonlite::toJSON(
  list(
    list(rna_sample = "RNA.WT1", ribo_sample = "RPF.WT1", group_role = "Control"),
    list(rna_sample = "RNA.WT2", ribo_sample = "RPF.WT2", group_role = "Control"),
    list(rna_sample = "RNA.KO1", ribo_sample = "RPF.KO1", group_role = "Treatment"),
    list(rna_sample = "RNA.KO2", ribo_sample = "RPF.KO2", group_role = "Treatment")
  ),
  auto_unbox = TRUE
)

shiny::testServer(mod_load_data_server, args = list(session_state = session_state), {
  session$setInputs(load_demo = "demo")

  html_after_demo <- output$session_summary$html
  stopifnot(grepl("RNA.WT1", html_after_demo, fixed = TRUE))
  stopifnot(grepl("RPF.WT1", html_after_demo, fixed = TRUE))
  stopifnot(grepl("Control", html_after_demo, fixed = TRUE))
  stopifnot(isTRUE(session_state$upload_saved))
  stopifnot(is.data.frame(session_state$upload_context$sample_type_manifest))
  stopifnot(is.data.frame(session_state$upload_context$pair_manifest))
  stopifnot(nrow(session_state$upload_context$sample_type_manifest) == 8L)
  stopifnot(nrow(session_state$upload_context$pair_manifest) == 4L)
  stopifnot(identical(sort(unique(session_state$upload_context$pair_manifest$group_role)), c("Control", "Treatment")))
})

cat("ribote load data pair manifest test passed\n")
