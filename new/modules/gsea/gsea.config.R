gsea_collection_options <- function(species_key = "hg38") {
  lapply(gsea_collection_values(species_key), function(collection) {
    ribote_choice(gsea_collection_label(collection), collection)
  })
}

gsea_collection_values <- function(species_key = "hg38") {
  switch(
    as.character(species_key),
    hg38 = c("hallmark", "reactome", "go_bp"),
    osa_IRGSP_1 = c("go_bp", "go_mf", "go_cc", "kegg"),
    character()
  )
}

gsea_default_collection <- function(species_key = "hg38") {
  values <- gsea_collection_values(species_key)

  if (length(values) == 0L) {
    "hallmark"
  } else {
    values[[1]]
  }
}

gsea_collection_label <- function(collection) {
  switch(
    as.character(collection),
    hallmark = "Hallmark",
    reactome = "Reactome",
    go_bp = "GO Biological Process",
    go_mf = "GO Molecular Function",
    go_cc = "GO Cellular Component",
    kegg = "KEGG",
    as.character(collection)
  )
}

gsea_resource_root <- function(species_key) {
  species_key <- as.character(species_key)

  if (identical(species_key, "osa_IRGSP_1")) {
    return(app_path("resources", "gene_sets", "plantgsea", species_key))
  }

  app_path("resources", "gene_sets", "msigdb", species_key)
}

gsea_collection_pattern <- function(species_key, collection) {
  species_key <- as.character(species_key)
  collection <- as.character(collection)

  if (identical(species_key, "hg38")) {
    return(switch(
      collection,
      hallmark = "^h\\.all\\..*\\.Hs\\.symbols\\.gmt$",
      reactome = "^c2\\.cp\\.reactome\\..*\\.Hs\\.symbols\\.gmt$",
      go_bp = "^c5\\.go\\.bp\\..*\\.Hs\\.symbols\\.gmt$",
      NULL
    ))
  }

  if (identical(species_key, "osa_IRGSP_1")) {
    return(switch(
      collection,
      go_bp = "^osa_IRGSP_1\\.go\\.bp\\.gmt$",
      go_mf = "^osa_IRGSP_1\\.go\\.mf\\.gmt$",
      go_cc = "^osa_IRGSP_1\\.go\\.cc\\.gmt$",
      kegg = "^osa_IRGSP_1\\.kegg\\.gmt$",
      NULL
    ))
  }

  NULL
}

gsea_resource_path <- function(species_key, collection) {
  resource_dir <- gsea_resource_root(species_key)
  pattern <- gsea_collection_pattern(species_key, collection)

  if (is.null(pattern) || !dir.exists(resource_dir)) {
    return(NULL)
  }

  candidates <- list.files(resource_dir, pattern = pattern, full.names = TRUE)

  if (length(candidates) == 0L) {
    return(NULL)
  }

  normalizePath(candidates[[1]], winslash = "/", mustWork = TRUE)
}

gsea_species_key <- function(upload_context) {
  species_meta <- upload_context$species_meta

  if (is.list(species_meta) && !is.null(species_meta$acronym) && nzchar(as.character(species_meta$acronym))) {
    return(as.character(species_meta$acronym))
  }

  if (!is.null(upload_context$species) && nzchar(as.character(upload_context$species))) {
    species_meta <- ribote_species_meta(upload_context$species)
    if (!is.null(species_meta$acronym) && nzchar(as.character(species_meta$acronym))) {
      return(as.character(species_meta$acronym))
    }
  }

  NULL
}

gsea_supported_for_upload_context <- function(upload_context) {
  species_key <- gsea_species_key(upload_context)

  if (is.null(species_key)) {
    return(FALSE)
  }

  collections <- gsea_collection_values(species_key)
  if (length(collections) == 0L) {
    return(FALSE)
  }

  all(vapply(
    collections,
    function(collection) !is.null(gsea_resource_path(species_key, collection)),
    logical(1)
  ))
}

gsea_cache_version <- function() {
  "gsea_v3_prebuilt_payload"
}

gsea_fixed_seed <- function() {
  20260403L
}

gsea_export_defaults <- function() {
  list(
    format = "png",
    width = 2800,
    height = 1800,
    dpi = 300,
    data_scope = "displayed",
    data_format = "csv"
  )
}

gsea_export_data_scope_options <- function() {
  list(
    list(label = "Displayed Pathways", value = "displayed"),
    list(label = "All Significant Pathways", value = "significant"),
    list(label = "All Tested Pathways", value = "tested")
  )
}

gsea_export_config <- function(id) {
  ns <- NS(id)
  defaults <- gsea_export_defaults()

  list(
    ids = list(
      format = ns("export_format"),
      width = ns("export_width"),
      height = ns("export_height"),
      dpi = ns("export_dpi"),
      trigger = ns("export_plot"),
      dataScope = ns("data_export_scope"),
      dataFormat = ns("data_export_format"),
      dataTrigger = ns("data_export")
    ),
    defaults = defaults,
    choices = list(
      format = list(
        list(label = "PNG", value = "png"),
        list(label = "PDF", value = "pdf")
      ),
      dataScope = gsea_export_data_scope_options(),
      dataFormat = list(
        list(label = "CSV", value = "csv"),
        list(label = "TXT (tab-delimited)", value = "txt")
      )
    ),
    sectioned = TRUE
  )
}

gsea_normalize_export_data_scope <- function(scope = NULL) {
  normalized_scope <- if (is.null(scope) || !nzchar(as.character(scope))) "displayed" else as.character(scope)

  if (!normalized_scope %in% c("displayed", "significant", "tested")) {
    normalized_scope <- "displayed"
  }

  normalized_scope
}

gsea_export_settings <- function(input) {
  defaults <- gsea_export_defaults()
  width <- suppressWarnings(as.numeric(input$export_width))
  height <- suppressWarnings(as.numeric(input$export_height))
  dpi <- suppressWarnings(as.numeric(input$export_dpi))
  format <- tolower(if (is.null(input$export_format) || identical(input$export_format, "")) defaults$format else input$export_format)
  data_scope <- if (is.null(input$data_export_scope) || identical(input$data_export_scope, "")) defaults$data_scope else input$data_export_scope
  data_format <- tolower(if (is.null(input$data_export_format) || identical(input$data_export_format, "")) defaults$data_format else input$data_export_format)

  list(
    format = if (format %in% c("png", "pdf")) format else defaults$format,
    width = if (is.finite(width) && width > 0) width else defaults$width,
    height = if (is.finite(height) && height > 0) height else defaults$height,
    dpi = if (is.finite(dpi) && dpi > 0) dpi else defaults$dpi,
    data_scope = gsea_normalize_export_data_scope(data_scope),
    data_format = if (data_format %in% c("csv", "txt")) data_format else defaults$data_format
  )
}

gsea_safe_filename_token <- function(value) {
  normalized <- tolower(as.character(value))
  normalized <- gsub("[^a-z0-9]+", "_", normalized)
  normalized <- gsub("^_+|_+$", "", normalized)

  if (!nzchar(normalized)) {
    "gsea"
  } else {
    normalized
  }
}

gsea_figure_filename <- function(collection_label, pathway_name, extension = "png") {
  sprintf(
    "gsea_%s_%s_%s.%s",
    gsea_safe_filename_token(collection_label),
    gsea_safe_filename_token(pathway_name),
    format(Sys.Date(), "%Y%m%d"),
    extension
  )
}

gsea_data_filename <- function(collection_label, scope = "displayed", extension = "csv") {
  sprintf(
    "gsea_%s_%s_pathways_%s.%s",
    gsea_safe_filename_token(collection_label),
    gsea_safe_filename_token(scope),
    format(Sys.Date(), "%Y%m%d"),
    extension
  )
}

gsea_normalize_parameters <- function(
  collection = NULL,
  geneset_min = NULL,
  geneset_max = NULL,
  fdr_cutoff = NULL,
  show_n = NULL,
  species_key = "hg38"
) {
  available_collections <- gsea_collection_values(species_key)
  if (length(available_collections) == 0L) {
    available_collections <- unique(c(gsea_collection_values("hg38"), gsea_collection_values("osa_IRGSP_1")))
  }

  default_collection <- gsea_default_collection(species_key)
  normalized_collection <- if (is.null(collection) || !nzchar(as.character(collection))) default_collection else tolower(as.character(collection))

  min_value <- suppressWarnings(as.integer(geneset_min))
  max_value <- suppressWarnings(as.integer(geneset_max))
  fdr_value <- suppressWarnings(as.numeric(fdr_cutoff))
  show_value <- suppressWarnings(as.integer(show_n))

  if (!normalized_collection %in% available_collections) {
    normalized_collection <- default_collection
  }

  if (length(min_value) != 1L || !is.finite(min_value) || min_value < 5L) {
    min_value <- 5L
  }

  if (length(max_value) != 1L || !is.finite(max_value) || max_value < min_value) {
    max_value <- max(500L, min_value)
  }

  if (length(fdr_value) != 1L || !is.finite(fdr_value) || fdr_value <= 0 || fdr_value > 1) {
    fdr_value <- 0.05
  }

  if (length(show_value) != 1L || !is.finite(show_value) || show_value < 5L) {
    show_value <- 20L
  }

  list(
    collection = normalized_collection,
    geneset_min = as.integer(min_value),
    geneset_max = as.integer(max_value),
    fdr_cutoff = as.numeric(fdr_value),
    show_n = as.integer(show_value)
  )
}

gsea_parameters_key <- function(parameters) {
  paste(
    parameters$collection,
    parameters$geneset_min,
    parameters$geneset_max,
    parameters$fdr_cutoff,
    parameters$show_n,
    sep = "::"
  )
}

gsea_source_signature <- function(te_context, preprocess_context, upload_context) {
  paste(
    pca_source_signature(te_context, preprocess_context),
    gsea_species_key(upload_context),
    sep = "::"
  )
}

gsea_with_seed <- function(seed, expr) {
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)

  if (had_seed) {
    old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  }

  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)

  set.seed(as.integer(seed))
  force(expr)
}

