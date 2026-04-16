ribote_welcome_shell_config <- function() {
  list(
    hero = list(
      eyebrow = "Translation Efficiency Analysis Workspace",
      title = "RiboTE",
      description = paste(
        "RiboTE supports comparative translation efficiency analysis",
        "from matched RNA-seq and Ribo-seq count matrices."
      ),
      supporting = paste(
        "Follow the workflow from data loading and preprocessing to translation efficiency,",
        "sample relationships, network analysis, and pathway interpretation."
      ),
      logo_src = "/ribote-resources/logo.webp",
      logo_alt = "RiboTE project artwork",
      badges = c(
        "Matched RNA / Ribo inputs",
        "Translation efficiency workflows",
        "Codon and pathway interpretation"
      )
    )
  )
}

ribote_welcome_shell_json <- function(config = ribote_welcome_shell_config()) {
  jsonlite::toJSON(
    config,
    auto_unbox = TRUE,
    pretty = FALSE,
    null = "null"
  )
}
