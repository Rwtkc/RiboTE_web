source(app_path("shared", "react_bridge.R"), local = TRUE, encoding = "UTF-8")
source(app_path("shared", "module_shell.R"), local = TRUE, encoding = "UTF-8")
source(app_path("services", "module_registry.R"), local = TRUE, encoding = "UTF-8")
source(app_path("modules", "welcome", "welcome.shared.R"), local = TRUE, encoding = "UTF-8")
source(app_path("modules", "welcome", "welcome.ui.R"), local = TRUE, encoding = "UTF-8")
source(app_path("modules", "load_data", "load_data.shared.R"), local = TRUE, encoding = "UTF-8")
source(app_path("modules", "load_data", "load_data.ui.R"), local = TRUE, encoding = "UTF-8")
source(app_path("modules", "data_preprocess", "data_preprocess.shared.R"), local = TRUE, encoding = "UTF-8")
source(app_path("modules", "data_preprocess", "data_preprocess.ui.R"), local = TRUE, encoding = "UTF-8")
source(app_path("modules", "translation_efficiency", "translation_efficiency.shared.R"), local = TRUE, encoding = "UTF-8")
source(app_path("modules", "translation_efficiency", "translation_efficiency.ui.R"), local = TRUE, encoding = "UTF-8")
source(app_path("modules", "pca", "pca.shared.R"), local = TRUE, encoding = "UTF-8")
source(app_path("modules", "pca", "pca.ui.R"), local = TRUE, encoding = "UTF-8")
source(app_path("modules", "clustering", "clustering.shared.R"), local = TRUE, encoding = "UTF-8")
source(app_path("modules", "clustering", "clustering.ui.R"), local = TRUE, encoding = "UTF-8")
source(app_path("modules", "gsea", "gsea.shared.R"), local = TRUE, encoding = "UTF-8")
source(app_path("modules", "gsea", "gsea.ui.R"), local = TRUE, encoding = "UTF-8")
source(app_path("modules", "enrichment", "enrichment.shared.R"), local = TRUE, encoding = "UTF-8")
source(app_path("modules", "enrichment", "enrichment.ui.R"), local = TRUE, encoding = "UTF-8")
source(app_path("modules", "network", "network.shared.R"), local = TRUE, encoding = "UTF-8")
source(app_path("modules", "network", "network.ui.R"), local = TRUE, encoding = "UTF-8")
source(app_path("modules", "signalp", "signalp.shared.R"), local = TRUE, encoding = "UTF-8")
source(app_path("modules", "signalp", "signalp.ui.R"), local = TRUE, encoding = "UTF-8")
source(app_path("modules", "codon", "codon.shared.R"), local = TRUE, encoding = "UTF-8")
source(app_path("modules", "codon", "codon.ui.R"), local = TRUE, encoding = "UTF-8")
source(app_path("modules", "help", "help.ui.R"), local = TRUE, encoding = "UTF-8")

react_shell_asset_tags <- function() {
  asset_dir <- app_path("www", "react", "app_shell", "assets")
  css_file <- file.path(asset_dir, "app-shell.css")
  js_file <- file.path(asset_dir, "app-shell.js")
  asset_tags <- list()

  asset_version <- function(path) {
    if (!file.exists(path)) {
      return(NULL)
    }

    unname(tools::md5sum(path))
  }

  if (file.exists(css_file)) {
    css_version <- asset_version(css_file)
    css_href <- "react/app_shell/assets/app-shell.css"

    if (!is.null(css_version)) {
      css_href <- sprintf("%s?v=%s", css_href, css_version)
    }

    asset_tags <- c(asset_tags, list(tags$link(rel = "stylesheet", href = css_href)))
  }

  if (file.exists(js_file)) {
    js_version <- asset_version(js_file)
    js_src <- "react/app_shell/assets/app-shell.js"

    if (!is.null(js_version)) {
      js_src <- sprintf("%s?v=%s", js_src, js_version)
    }

    asset_tags <- c(asset_tags, list(tags$script(type = "module", src = js_src)))
  }

  tagList(asset_tags)
}

app_head_resources <- function() {
  tags$head(
    tags$title("RiboTE"),
    tags$meta(charset = "utf-8"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$link(rel = "stylesheet", href = "css/app.css"),
    react_shell_asset_tags()
  )
}

ui <- shinyUI(
  tagList(
    navbarPage(
      title = "RiboTE",
      id = "main_navbar",
      selected = "welcome",
      header = app_head_resources(),
      tabPanel("Welcome", value = "welcome", mod_welcome_ui("welcome")),
      tabPanel("Load Data", value = "load_data", mod_load_data_ui("load_data")),
      tabPanel("Data Preprocess", value = "data_preprocess", mod_data_preprocess_ui("data_preprocess")),
      tabPanel("Translation Efficiency", value = "translation_efficiency", mod_translation_efficiency_ui("translation_efficiency")),
      tabPanel("PCA", value = "pca", mod_pca_ui("pca")),
      tabPanel("Clustering", value = "clustering", mod_clustering_ui("clustering")),
      tabPanel("GSEA", value = "gsea", mod_gsea_ui("gsea")),
      tabPanel("Enrichment", value = "enrichment", mod_enrichment_ui("enrichment")),
      tabPanel("Network", value = "network", mod_network_ui("network")),
      tabPanel("SignalP", value = "signalp", mod_signalp_ui("signalp")),
      tabPanel("Codon", value = "codon", mod_codon_ui("codon")),
      tabPanel("Help", value = "help", mod_help_ui("help"))
    ),
    tags$footer(
      class = "rm-footer",
      "RiboTE | Rice Research Institute, Guangdong Academy of Agricultural Sciences | Guangzhou, Guangdong, China | 2026"
    )
  )
)
