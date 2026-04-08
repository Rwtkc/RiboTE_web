source(app_path("shared", "react_bridge.R"), local = TRUE, encoding = "UTF-8")
source(app_path("shared", "module_shell.R"), local = TRUE, encoding = "UTF-8")
source(app_path("services", "module_registry.R"), local = TRUE, encoding = "UTF-8")
source(app_path("services", "session_state.R"), local = TRUE, encoding = "UTF-8")
source(app_path("modules", "welcome", "welcome.server.R"), local = TRUE, encoding = "UTF-8")
source(app_path("modules", "load_data", "load_data.shared.R"), local = TRUE, encoding = "UTF-8")
source(app_path("modules", "load_data", "load_data.server.R"), local = TRUE, encoding = "UTF-8")
source(app_path("modules", "data_preprocess", "data_preprocess.shared.R"), local = TRUE, encoding = "UTF-8")
source(app_path("modules", "data_preprocess", "data_preprocess.server.R"), local = TRUE, encoding = "UTF-8")
source(app_path("modules", "translation_efficiency", "translation_efficiency.shared.R"), local = TRUE, encoding = "UTF-8")
source(app_path("modules", "translation_efficiency", "translation_efficiency.server.R"), local = TRUE, encoding = "UTF-8")
source(app_path("modules", "pca", "pca.shared.R"), local = TRUE, encoding = "UTF-8")
source(app_path("modules", "pca", "pca.server.R"), local = TRUE, encoding = "UTF-8")
source(app_path("modules", "clustering", "clustering.shared.R"), local = TRUE, encoding = "UTF-8")
source(app_path("modules", "clustering", "clustering.server.R"), local = TRUE, encoding = "UTF-8")
source(app_path("modules", "gsea", "gsea.shared.R"), local = TRUE, encoding = "UTF-8")
source(app_path("modules", "gsea", "gsea.server.R"), local = TRUE, encoding = "UTF-8")
source(app_path("modules", "enrichment", "enrichment.shared.R"), local = TRUE, encoding = "UTF-8")
source(app_path("modules", "enrichment", "enrichment.server.R"), local = TRUE, encoding = "UTF-8")
source(app_path("modules", "network", "network.shared.R"), local = TRUE, encoding = "UTF-8")
source(app_path("modules", "network", "network.server.R"), local = TRUE, encoding = "UTF-8")
source(app_path("modules", "signalp", "signalp.shared.R"), local = TRUE, encoding = "UTF-8")
source(app_path("modules", "signalp", "signalp.server.R"), local = TRUE, encoding = "UTF-8")
source(app_path("modules", "codon", "codon.shared.R"), local = TRUE, encoding = "UTF-8")
source(app_path("modules", "codon", "codon.server.R"), local = TRUE, encoding = "UTF-8")

server <- function(input, output, session) {
  analysis_lock <- reactiveVal(NULL)
  session_state <- create_session_state()

  observe({
    session$sendCustomMessage(
      "rnameta:set-analysis-lock",
      list(
        locked = !is.null(analysis_lock()),
        owner = analysis_lock()
      )
    )
  })

  mod_welcome_server("welcome")
  mod_load_data_server("load_data", session_state = session_state)
  mod_data_preprocess_server("data_preprocess", session_state = session_state, analysis_lock = analysis_lock)
  mod_translation_efficiency_server("translation_efficiency", session_state = session_state, analysis_lock = analysis_lock)
  mod_pca_server("pca", session_state = session_state, analysis_lock = analysis_lock)
  mod_clustering_server("clustering", session_state = session_state, analysis_lock = analysis_lock)
  mod_gsea_server("gsea", session_state = session_state, analysis_lock = analysis_lock)
  mod_enrichment_server("enrichment", session_state = session_state, analysis_lock = analysis_lock)
  mod_network_server("network", session_state = session_state, analysis_lock = analysis_lock)
  mod_signalp_server("signalp", session_state = session_state, analysis_lock = analysis_lock)
  mod_codon_server("codon", session_state = session_state, analysis_lock = analysis_lock)
}
