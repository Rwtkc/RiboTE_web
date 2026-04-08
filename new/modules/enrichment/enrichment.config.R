enrichment_collection_options <- function(species_key = "hg38") {
  lapply(enrichment_collection_values(species_key), function(collection) {
    ribote_choice(enrichment_collection_label(collection), collection)
  })
}

enrichment_collection_values <- function(species_key = "hg38") {
  switch(
    as.character(species_key),
    hg38 = c("go_bp", "go_mf", "go_cc", "kegg"),
    osa_IRGSP_1 = c("go_bp", "go_mf", "go_cc", "kegg"),
    character()
  )
}

enrichment_default_collection <- function(species_key = "hg38") {
  values <- enrichment_collection_values(species_key)

  if (length(values) == 0L) {
    "go_bp"
  } else {
    values[[1]]
  }
}

enrichment_collection_label <- function(collection) {
  switch(
    as.character(collection),
    go_bp = "GO Biological Process",
    go_mf = "GO Molecular Function",
    go_cc = "GO Cellular Component",
    kegg = "KEGG",
    as.character(collection)
  )
}

enrichment_resource_root <- function(species_key) {
  species_key <- as.character(species_key)

  if (identical(species_key, "osa_IRGSP_1")) {
    return(app_path("resources", "gene_sets", "plantgsea", species_key))
  }

  app_path("resources", "gene_sets", "msigdb", species_key)
}

enrichment_species_key <- function(upload_context) {
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

enrichment_collection_pattern <- function(species_key, collection) {
  species_key <- as.character(species_key)
  collection <- as.character(collection)

  if (identical(species_key, "hg38")) {
    return(switch(
      collection,
      go_bp = "^c5\\.go\\.bp\\..*\\.Hs\\.symbols\\.gmt$",
      go_mf = "^c5\\.go\\.mf\\..*\\.Hs\\.symbols\\.gmt$",
      go_cc = "^c5\\.go\\.cc\\..*\\.Hs\\.symbols\\.gmt$",
      kegg = "^c2\\.cp\\.kegg_medicus\\..*\\.Hs\\.symbols\\.gmt$",
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

enrichment_resource_path <- function(species_key, collection) {
  resource_dir <- enrichment_resource_root(species_key)
  pattern <- enrichment_collection_pattern(species_key, collection)

  if (is.null(pattern) || !dir.exists(resource_dir)) {
    return(NULL)
  }

  candidates <- list.files(resource_dir, pattern = pattern, full.names = TRUE)

  if (length(candidates) == 0L) {
    return(NULL)
  }

  normalizePath(candidates[[1]], winslash = "/", mustWork = TRUE)
}

enrichment_supported_for_upload_context <- function(upload_context) {
  species_key <- enrichment_species_key(upload_context)

  if (is.null(species_key)) {
    return(FALSE)
  }

  collections <- enrichment_collection_values(species_key)
  if (length(collections) == 0L) {
    return(FALSE)
  }

  all(vapply(
    collections,
    function(collection) !is.null(enrichment_resource_path(species_key, collection)),
    logical(1)
  ))
}

enrichment_cache_version <- function() {
  "enrichment_v2_prebuilt_payload"
}

enrichment_export_defaults <- function() {
  list(
    format = "png",
    width = 2800,
    height = 1800,
    dpi = 300,
    data_scope = "displayed",
    data_format = "csv"
  )
}

enrichment_export_data_scope_options <- function() {
  list(
    list(label = "Displayed Terms", value = "displayed"),
    list(label = "All Significant Terms", value = "significant"),
    list(label = "All Tested Terms", value = "tested")
  )
}

enrichment_export_config <- function(id) {
  ns <- NS(id)
  defaults <- enrichment_export_defaults()

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
      dataScope = enrichment_export_data_scope_options(),
      dataFormat = list(
        list(label = "CSV", value = "csv"),
        list(label = "TXT (tab-delimited)", value = "txt")
      )
    )
  )
}

enrichment_normalize_export_data_scope <- function(scope = NULL) {
  normalized_scope <- if (is.null(scope) || !nzchar(as.character(scope))) "displayed" else as.character(scope)

  if (!normalized_scope %in% c("displayed", "significant", "tested")) {
    normalized_scope <- "displayed"
  }

  normalized_scope
}

enrichment_export_settings <- function(input) {
  defaults <- enrichment_export_defaults()
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
    data_scope = enrichment_normalize_export_data_scope(data_scope),
    data_format = if (data_format %in% c("csv", "txt")) data_format else defaults$data_format
  )
}

enrichment_safe_filename_token <- function(value) {
  normalized <- tolower(as.character(value))
  normalized <- gsub("[^a-z0-9]+", "_", normalized)
  normalized <- gsub("^_+|_+$", "", normalized)

  if (!nzchar(normalized)) {
    "enrichment"
  } else {
    normalized
  }
}

enrichment_data_filename <- function(collection_label, scope = "displayed", extension = "csv") {
  sprintf(
    "enrichment_%s_%s_terms_%s.%s",
    enrichment_safe_filename_token(collection_label),
    enrichment_safe_filename_token(scope),
    format(Sys.Date(), "%Y%m%d"),
    extension
  )
}

enrichment_figure_filename <- function(collection_label, extension = "png") {
  sprintf(
    "enrichment_%s_plot_%s.%s",
    enrichment_safe_filename_token(collection_label),
    format(Sys.Date(), "%Y%m%d"),
    extension
  )
}

enrichment_normalize_parameters <- function(
  collection = NULL,
  top_pathways = NULL,
  sort_by = NULL,
  filtered_background = NULL,
  remove_redundant = NULL,
  show_pathway_id = NULL,
  species_key = "hg38"
) {
  available_collections <- enrichment_collection_values(species_key)
  if (length(available_collections) == 0L) {
    available_collections <- unique(c(enrichment_collection_values("hg38"), enrichment_collection_values("osa_IRGSP_1")))
  }

  default_collection <- enrichment_default_collection(species_key)
  normalized_collection <- if (is.null(collection) || !nzchar(as.character(collection))) default_collection else tolower(as.character(collection))

  top_value <- suppressWarnings(as.integer(top_pathways))
  normalized_sort <- if (is.null(sort_by) || !nzchar(as.character(sort_by))) "FDR" else toupper(as.character(sort_by))

  if (!normalized_collection %in% available_collections) {
    normalized_collection <- default_collection
  }

  if (length(top_value) != 1L || !is.finite(top_value) || top_value < 1L) {
    top_value <- 10L
  }

  if (top_value > 30L) {
    top_value <- 30L
  }

  if (!normalized_sort %in% c("FDR", "FOLD")) {
    normalized_sort <- "FDR"
  }

  list(
    collection = normalized_collection,
    top_pathways = as.integer(top_value),
    sort_by = if (identical(normalized_sort, "FOLD")) "Fold" else "FDR",
    filtered_background = isTRUE(filtered_background),
    remove_redundant = isTRUE(remove_redundant),
    show_pathway_id = isTRUE(show_pathway_id)
  )
}

enrichment_parameters_key <- function(parameters) {
  paste(
    parameters$collection,
    parameters$top_pathways,
    parameters$sort_by,
    parameters$filtered_background,
    parameters$remove_redundant,
    parameters$show_pathway_id,
    sep = "::"
  )
}

enrichment_source_signature <- function(te_context, preprocess_context, upload_context) {
  paste(
    pca_source_signature(te_context, preprocess_context),
    enrichment_species_key(upload_context),
    sep = "::"
  )
}

