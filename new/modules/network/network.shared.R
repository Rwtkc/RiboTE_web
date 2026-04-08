network_export_defaults <- function() {
  list(
    format = "png",
    width = 2800,
    height = 1800,
    dpi = 300,
    data_format = "csv"
  )
}

network_export_config <- function(id) {
  ns <- NS(id)
  defaults <- network_export_defaults()

  list(
    ids = list(
      format = ns("export_format"),
      width = ns("export_width"),
      height = ns("export_height"),
      dpi = ns("export_dpi"),
      trigger = ns("export_plot"),
      dataFormat = ns("data_export_format"),
      dataTrigger = ns("data_export")
    ),
    defaults = defaults,
    choices = list(
      format = list(
        list(label = "PNG", value = "png"),
        list(label = "PDF", value = "pdf")
      ),
      dataFormat = list(
        list(label = "CSV", value = "csv"),
        list(label = "TXT (tab-delimited)", value = "txt")
      )
    )
  )
}

network_export_settings <- function(input) {
  defaults <- network_export_defaults()
  width <- suppressWarnings(as.numeric(input$export_width))
  height <- suppressWarnings(as.numeric(input$export_height))
  dpi <- suppressWarnings(as.numeric(input$export_dpi))
  format <- tolower(if (is.null(input$export_format) || identical(input$export_format, "")) defaults$format else input$export_format)
  data_format <- tolower(if (is.null(input$data_export_format) || identical(input$data_export_format, "")) defaults$data_format else input$data_export_format)

  list(
    format = if (format %in% c("png", "pdf")) format else defaults$format,
    width = if (is.finite(width) && width > 0) width else defaults$width,
    height = if (is.finite(height) && height > 0) height else defaults$height,
    dpi = if (is.finite(dpi) && dpi > 0) dpi else defaults$dpi,
    data_format = if (data_format %in% c("csv", "txt")) data_format else defaults$data_format
  )
}

network_safe_filename_token <- function(value, fallback = "network") {
  normalized <- tolower(as.character(value))
  normalized <- gsub("[^a-z0-9]+", "_", normalized)
  normalized <- gsub("^_+|_+$", "", normalized)

  if (!nzchar(normalized)) {
    fallback
  } else {
    normalized
  }
}

network_figure_filename <- function(data_space, module_label, extension = "png") {
  safe_space <- gsub("[^a-z0-9]+", "_", tolower(as.character(data_space)))
  sprintf(
    "network_%s_%s_%s.%s",
    safe_space,
    network_safe_filename_token(module_label, fallback = "module"),
    format(Sys.Date(), "%Y%m%d"),
    extension
  )
}

network_archive_filename <- function(kind = "data") {
  sprintf(
    "network_%s_%s.zip",
    network_safe_filename_token(kind, fallback = "data"),
    format(Sys.Date(), "%Y%m%d")
  )
}

network_nodes_filename <- function(data_space, module_label, extension = "csv") {
  safe_space <- gsub("[^a-z0-9]+", "_", tolower(as.character(data_space)))
  sprintf(
    "network_nodes_%s_%s_%s.%s",
    safe_space,
    network_safe_filename_token(module_label, fallback = "module"),
    format(Sys.Date(), "%Y%m%d"),
    extension
  )
}

network_edges_filename <- function(data_space, module_label, extension = "csv", kind = "thresholded") {
  safe_space <- gsub("[^a-z0-9]+", "_", tolower(as.character(data_space)))
  safe_kind <- gsub("[^a-z0-9]+", "_", tolower(as.character(kind)))
  sprintf(
    "network_edges_%s_%s_%s_%s.%s",
    safe_space,
    safe_kind,
    network_safe_filename_token(module_label, fallback = "module"),
    format(Sys.Date(), "%Y%m%d"),
    extension
  )
}

network_default_module_options <- function() {
  list(
    ribote_choice("Entire network", "entire_network")
  )
}

network_data_space_options <- function() {
  pca_data_space_options()
}

network_data_space_label <- function(data_space) {
  pca_data_space_label(data_space)
}

network_render_limits <- function(node_count) {
  node_count <- max(1L, as.integer(node_count))

  if (node_count <= 20L) {
    return(list(display_edge_cap = 900L, max_edges_per_node = 12L))
  }

  if (node_count <= 40L) {
    return(list(display_edge_cap = 700L, max_edges_per_node = 10L))
  }

  if (node_count <= 80L) {
    return(list(display_edge_cap = 500L, max_edges_per_node = 8L))
  }

  list(display_edge_cap = 360L, max_edges_per_node = 6L)
}

network_module_config <- function(module_options = NULL, selected_module = "entire_network") {
  if (is.null(module_options) || !length(module_options)) {
    module_options <- network_default_module_options()
  }

  ribote_build_module_config(
    key = "network",
    title = "Network",
    eyebrow = "Co-Expression Network",
    description = "Explore coordinated gene patterns in TE ratio, RNA abundance, or Ribo abundance, and view the resulting co-expression network.",
    requires = "te",
    run_label = "Run Network",
    sections = list(
      list(
        title = "Network Settings",
        fields = list(
          ribote_field("network_data_space", "Data Space", "select", "TE", options = network_data_space_options()),
          ribote_field("network_edge_threshold", "Edge Threshold", "number", 0.4, min = 0, max = 1, step = 0.05),
          ribote_field("network_top_genes", "Top Genes", "number", 10, min = 10, max = 1000, step = 10),
          ribote_field("network_variable_genes", "Most Variable Genes", "number", 1000, min = 50, max = 3000, step = 50),
          ribote_field("network_module", "Module", "select", selected_module, options = module_options),
          ribote_field("network_soft_power", "Soft Threshold", "number", 5, min = 1, max = 20, step = 1),
          ribote_field("network_min_module_size", "Min Module Size", "number", 20, min = 10, max = 100, step = 1)
        )
      )
    ),
    wide_sidebar = TRUE,
    show_export_panel = FALSE,
    blank_analysis_panel = TRUE
  )
}

network_normalize_parameters <- function(
  data_space = NULL,
  edge_threshold = NULL,
  top_genes = NULL,
  variable_genes = NULL,
  module_name = NULL,
  soft_power = NULL,
  min_module_size = NULL
) {
  normalized_data_space <- pca_normalize_parameters(data_space = data_space, method = "PCA")$data_space
  edge_threshold_value <- suppressWarnings(as.numeric(edge_threshold))
  top_genes_value <- suppressWarnings(as.integer(top_genes))
  variable_genes_value <- suppressWarnings(as.integer(variable_genes))
  soft_power_value <- suppressWarnings(as.integer(soft_power))
  min_module_size_value <- suppressWarnings(as.integer(min_module_size))
  module_name_value <- if (is.null(module_name) || !nzchar(as.character(module_name))) {
    "entire_network"
  } else {
    as.character(module_name)
  }

  list(
    data_space = normalized_data_space,
    edge_threshold = if (is.finite(edge_threshold_value) && edge_threshold_value >= 0 && edge_threshold_value <= 1) edge_threshold_value else 0.4,
    top_genes = if (is.finite(top_genes_value) && top_genes_value >= 10L) min(top_genes_value, 1000L) else 10L,
    variable_genes = if (is.finite(variable_genes_value) && variable_genes_value >= 50L) min(variable_genes_value, 3000L) else 1000L,
    module_name = module_name_value,
    soft_power = if (is.finite(soft_power_value) && soft_power_value >= 1L) min(soft_power_value, 20L) else 5L,
    min_module_size = if (is.finite(min_module_size_value) && min_module_size_value >= 10L) min(min_module_size_value, 100L) else 20L
  )
}

network_base_signature <- function(te_context, preprocess_context) {
  pca_base_signature(te_context, preprocess_context)
}

network_basis_signature <- function(base_signature, parameters) {
  paste(
    as.character(base_signature),
    parameters$data_space,
    parameters$variable_genes,
    parameters$soft_power,
    parameters$min_module_size,
    sep = "::"
  )
}

network_graph_signature <- function(basis_signature, parameters, module_label = NULL) {
  paste(
    as.character(basis_signature),
    parameters$module_name,
    parameters$top_genes,
    parameters$edge_threshold,
    if (is.null(module_label)) "" else as.character(module_label),
    sep = "::"
  )
}

network_extract_gene_info <- function(result_table) {
  stopifnot(is.data.frame(result_table))

  gene_name <- if ("gene_name" %in% colnames(result_table)) {
    as.character(result_table$gene_name)
  } else {
    rep("unknown", nrow(result_table))
  }

  gene_name[is.na(gene_name) | !nzchar(trimws(gene_name))] <- "unknown"

  data.frame(
    GeneID = as.character(result_table$GeneID),
    gene_name = gene_name,
    stringsAsFactors = FALSE
  )
}

network_build_te_ratio_matrix <- function(result_table, pair_manifest) {
  stopifnot(is.data.frame(result_table))
  stopifnot(is.data.frame(pair_manifest))

  sample_map <- pca_build_short_names(pair_manifest)$te
  matrix_values <- sapply(seq_len(nrow(sample_map)), function(index) {
    actual_rna <- sample_map$actual_rna[[index]]
    actual_ribo <- sample_map$actual_ribo[[index]]

    if (!all(c(actual_rna, actual_ribo) %in% colnames(result_table))) {
      stop("TE result table is missing RNA or Ribo sample columns required for Network.", call. = FALSE)
    }

    as.numeric(result_table[[actual_ribo]]) / as.numeric(result_table[[actual_rna]])
  })

  if (is.null(dim(matrix_values))) {
    matrix_values <- matrix(matrix_values, ncol = 1L)
  }

  rownames(matrix_values) <- as.character(result_table$GeneID)
  colnames(matrix_values) <- sample_map$display_sample
  matrix_values
}

network_build_expression_matrix <- function(result_table, pair_manifest, data_space = c("RNA", "Ribo")) {
  data_space <- match.arg(data_space)
  sample_map <- pca_build_short_names(pair_manifest)
  space_key <- if (identical(data_space, "RNA")) "rna" else "ribo"
  actual_samples <- sample_map[[space_key]]$actual_sample
  matrix_values <- pca_extract_numeric_matrix(result_table, actual_samples)
  log2(matrix_values + 1)
}

network_prepare_feature_matrix <- function(feature_matrix, variable_genes = 1000L) {
  if (!is.matrix(feature_matrix) || nrow(feature_matrix) == 0L || ncol(feature_matrix) == 0L) {
    stop("No feature matrix is available for Network analysis.", call. = FALSE)
  }

  if (ncol(feature_matrix) < 4L) {
    stop("Network analysis requires at least four samples.", call. = FALSE)
  }

  finite_rows <- apply(feature_matrix, 1, function(row) all(is.finite(row)))
  prepared <- feature_matrix[finite_rows, , drop = FALSE]

  if (nrow(prepared) < 50L) {
    stop("At least 50 finite genes are required for Network analysis.", call. = FALSE)
  }

  variable_rows <- apply(prepared, 1, function(row) stats::sd(row, na.rm = TRUE) > 0)
  prepared <- prepared[variable_rows, , drop = FALSE]

  if (nrow(prepared) < 50L) {
    stop("At least 50 variable genes are required for Network analysis.", call. = FALSE)
  }

  requested_genes <- min(max(50L, as.integer(variable_genes)), nrow(prepared), 3000L)
  row_sd <- apply(prepared, 1, stats::sd, na.rm = TRUE)
  row_sd[!is.finite(row_sd)] <- -Inf
  ranked_rows <- order(row_sd, decreasing = TRUE)
  prepared[ranked_rows[seq_len(requested_genes)], , drop = FALSE]
}

network_build_base_context <- function(te_context, preprocess_context, upload_context, variable_genes = 1000L) {
  stopifnot(is.data.frame(te_context$result_table))
  stopifnot(is.data.frame(upload_context$pair_manifest))

  pair_manifest <- upload_context$pair_manifest
  sample_map <- pca_build_short_names(pair_manifest)
  result_table <- te_context$result_table

  te_matrix <- network_build_te_ratio_matrix(
    result_table = result_table,
    pair_manifest = pair_manifest
  )
  rna_matrix <- network_build_expression_matrix(
    result_table = result_table,
    pair_manifest = pair_manifest,
    data_space = "RNA"
  )
  ribo_matrix <- network_build_expression_matrix(
    result_table = result_table,
    pair_manifest = pair_manifest,
    data_space = "Ribo"
  )

  list(
    base_signature = network_base_signature(te_context, preprocess_context),
    gene_info = network_extract_gene_info(te_context$result_table),
    spaces = list(
      TE = list(
        label = network_data_space_label("TE"),
        feature_matrix = network_prepare_feature_matrix(te_matrix, variable_genes = variable_genes),
        sample_map = sample_map$te
      ),
      RNA = list(
        label = network_data_space_label("RNA"),
        feature_matrix = network_prepare_feature_matrix(rna_matrix, variable_genes = variable_genes),
        sample_map = sample_map$rna
      ),
      Ribo = list(
        label = network_data_space_label("Ribo"),
        feature_matrix = network_prepare_feature_matrix(ribo_matrix, variable_genes = variable_genes),
        sample_map = sample_map$ribo
      )
    )
  )
}

network_require_packages <- function() {
  required_packages <- c("WGCNA", "dynamicTreeCut")
  missing <- required_packages[!vapply(required_packages, requireNamespace, quietly = TRUE, logical(1))]

  if (length(missing)) {
    stop(sprintf("Network analysis requires missing packages: %s", paste(missing, collapse = ", ")), call. = FALSE)
  }

  invisible(TRUE)
}

network_module_options_from_info <- function(module_info) {
  if (is.null(module_info) || !nrow(module_info)) {
    return(network_default_module_options())
  }

  module_counts <- sort(table(module_info$module_color), decreasing = TRUE)
  module_choices <- lapply(names(module_counts), function(module_color) {
    ribote_choice(
      sprintf("%s (%s genes)", module_color, format(as.integer(module_counts[[module_color]]), big.mark = ",")),
      module_color
    )
  })

  c(module_choices, network_default_module_options())
}

network_build_basis_context <- function(base_context, parameters, source_signature = NULL) {
  network_require_packages()

  space_context <- base_context$spaces[[parameters$data_space]]

  if (is.null(space_context)) {
    stop(sprintf("Unsupported Network data space: %s", parameters$data_space), call. = FALSE)
  }

  feature_matrix <- space_context$feature_matrix
  dat_expr <- t(feature_matrix)
  sub_gene_names <- colnames(dat_expr)

  tom <- WGCNA::TOMsimilarityFromExpr(
    dat_expr,
    networkType = "unsigned",
    TOMType = "unsigned",
    power = parameters$soft_power
  )
  colnames(tom) <- sub_gene_names
  rownames(tom) <- sub_gene_names

  gene_tree <- stats::hclust(stats::as.dist(1 - tom), method = "average")
  dynamic_mods <- dynamicTreeCut::cutreeDynamic(
    dendro = gene_tree,
    method = "tree",
    minClusterSize = parameters$min_module_size
  )
  dynamic_colors <- WGCNA::labels2colors(dynamic_mods)

  gene_info <- base_context$gene_info
  gene_info <- gene_info[match(sub_gene_names, gene_info$GeneID), , drop = FALSE]
  gene_info$gene_name[is.na(gene_info$gene_name) | !nzchar(gene_info$gene_name)] <- "unknown"

  module_info <- data.frame(
    GeneID = sub_gene_names,
    gene_name = gene_info$gene_name,
    module_color = dynamic_colors,
    module_index = dynamic_mods,
    stringsAsFactors = FALSE
  )

  non_grey_module_info <- module_info[module_info$module_color != "grey", , drop = FALSE]

  list(
    base_signature = base_context$base_signature,
    source_signature = if (is.null(source_signature)) base_context$base_signature else source_signature,
    basis_signature = network_basis_signature(base_context$base_signature, parameters),
    data_space = parameters$data_space,
    data_space_label = space_context$label,
    feature_matrix = feature_matrix,
    tom = tom,
    module_info = module_info,
    non_grey_module_info = non_grey_module_info,
    module_options = network_module_options_from_info(non_grey_module_info),
    variable_genes_used = nrow(feature_matrix),
    module_count = length(unique(non_grey_module_info$module_color))
  )
}

network_resolve_module_selection <- function(basis_context, module_name) {
  options <- basis_context$module_options
  option_values <- vapply(options, function(option) option$value, character(1))
  resolved_name <- if (module_name %in% option_values) module_name else option_values[[1]]

  if (identical(resolved_name, "entire_network")) {
    return(list(value = "entire_network", label = "Entire network"))
  }

  match_index <- match(resolved_name, option_values)
  list(
    value = resolved_name,
    label = options[[match_index]]$label
  )
}

network_edge_table <- function(adjacency_matrix, display_edge_cap = Inf, max_edges_per_node = Inf) {
  if (!is.matrix(adjacency_matrix) || !nrow(adjacency_matrix) || !ncol(adjacency_matrix)) {
    return(list(
      edges = data.frame(),
      total_edges = 0L,
      capped = FALSE
    ))
  }

  edge_index <- which(upper.tri(adjacency_matrix) & adjacency_matrix > 0, arr.ind = TRUE)

  if (!nrow(edge_index)) {
    return(list(
      edges = data.frame(),
      total_edges = 0L,
      capped = FALSE
    ))
  }

  edges <- data.frame(
    source = rownames(adjacency_matrix)[edge_index[, 1]],
    target = colnames(adjacency_matrix)[edge_index[, 2]],
    weight = adjacency_matrix[edge_index],
    stringsAsFactors = FALSE
  )
  edges <- edges[order(edges$weight, decreasing = TRUE), , drop = FALSE]
  total_edges <- nrow(edges)
  displayed_edges <- edges

  if (is.finite(max_edges_per_node)) {
    node_degree_cap <- setNames(integer(0), character(0))
    kept_index <- logical(total_edges)

    for (index in seq_len(total_edges)) {
      source_id <- edges$source[[index]]
      target_id <- edges$target[[index]]
      source_degree <- if (source_id %in% names(node_degree_cap)) node_degree_cap[[source_id]] else 0L
      target_degree <- if (target_id %in% names(node_degree_cap)) node_degree_cap[[target_id]] else 0L

      if (source_degree >= max_edges_per_node || target_degree >= max_edges_per_node) {
        next
      }

      kept_index[[index]] <- TRUE
      node_degree_cap[[source_id]] <- source_degree + 1L
      node_degree_cap[[target_id]] <- target_degree + 1L
    }

    displayed_edges <- edges[kept_index, , drop = FALSE]
  }

  if (is.finite(display_edge_cap) && nrow(displayed_edges) > display_edge_cap) {
    displayed_edges <- displayed_edges[seq_len(display_edge_cap), , drop = FALSE]
  }

  list(
    edges = displayed_edges,
    total_edges = total_edges,
    capped = nrow(displayed_edges) < total_edges
  )
}

network_build_graph_context <- function(basis_context, parameters, display_edge_cap = NULL) {
  network_require_packages()

  module_selection <- network_resolve_module_selection(basis_context, parameters$module_name)
  module_info <- basis_context$module_info

  in_module <- if (identical(module_selection$value, "entire_network")) {
    rep(TRUE, nrow(module_info))
  } else {
    module_info$module_color == module_selection$value
  }

  selected_info <- module_info[in_module, , drop = FALSE]

  if (!nrow(selected_info)) {
    stop("The selected module does not contain any genes.", call. = FALSE)
  }

  dat_expr <- t(basis_context$feature_matrix)
  mod_probes <- selected_info$GeneID
  mod_tom <- basis_context$tom[in_module, in_module, drop = FALSE]
  dimnames(mod_tom) <- list(mod_probes, mod_probes)

  top_gene_count <- min(parameters$top_genes, ncol(dat_expr), nrow(selected_info), 1000L)
  connectivity <- WGCNA::softConnectivity(dat_expr[, mod_probes, drop = FALSE])
  connectivity[!is.finite(connectivity)] <- 0
  top_mask <- rank(-connectivity, ties.method = "first") <= top_gene_count
  top_genes <- mod_probes[top_mask]
  top_connectivity <- connectivity[top_mask]
  adjacency_matrix <- mod_tom[top_mask, top_mask, drop = FALSE]
  adjacency_matrix[adjacency_matrix <= parameters$edge_threshold] <- 0
  diag(adjacency_matrix) <- 0

  render_limits <- network_render_limits(length(top_genes))
  effective_display_cap <- if (is.null(display_edge_cap)) render_limits$display_edge_cap else as.integer(display_edge_cap)
  threshold_edge_context <- network_edge_table(
    adjacency_matrix,
    display_edge_cap = Inf,
    max_edges_per_node = Inf
  )
  display_edge_context <- network_edge_table(
    adjacency_matrix,
    display_edge_cap = effective_display_cap,
    max_edges_per_node = render_limits$max_edges_per_node
  )
  selected_info <- selected_info[match(top_genes, selected_info$GeneID), , drop = FALSE]
  selected_info$gene_name[is.na(selected_info$gene_name) | !nzchar(selected_info$gene_name)] <- selected_info$GeneID

  degree_values <- setNames(integer(length(top_genes)), top_genes)
  displayed_degree_values <- setNames(integer(length(top_genes)), top_genes)

  if (nrow(threshold_edge_context$edges)) {
    degree_table <- table(c(threshold_edge_context$edges$source, threshold_edge_context$edges$target))
    degree_values[names(degree_table)] <- as.integer(degree_table)
  }

  if (nrow(display_edge_context$edges)) {
    displayed_degree_table <- table(c(display_edge_context$edges$source, display_edge_context$edges$target))
    displayed_degree_values[names(displayed_degree_table)] <- as.integer(displayed_degree_table)
  }

  node_table <- data.frame(
    id = top_genes,
    label = ifelse(
      selected_info$gene_name == "unknown" | !nzchar(selected_info$gene_name),
      selected_info$GeneID,
      selected_info$gene_name
    ),
    GeneID = selected_info$GeneID,
    GeneName = selected_info$gene_name,
    Module = selected_info$module_color,
    ModuleIndex = selected_info$module_index,
    Connectivity = as.numeric(top_connectivity),
    Degree = as.integer(degree_values[top_genes]),
    DisplayDegree = as.integer(displayed_degree_values[top_genes]),
    stringsAsFactors = FALSE
  )

  note <- NULL
  if (!nrow(threshold_edge_context$edges)) {
    note <- sprintf(
      "No edges passed the current threshold of %.2f. Lower the threshold or choose a denser module.",
      parameters$edge_threshold
    )
  } else if (isTRUE(display_edge_context$capped)) {
    note <- sprintf(
      paste(
        "For browser performance, the on-page graph shows the strongest %s of %s threshold-passed edges",
        "(max %s displayed edges per node). Data export includes both the full threshold-passed edge table",
        "and the displayed edge subset so the biological interpretation stays transparent."
      ),
      format(nrow(display_edge_context$edges), big.mark = ","),
      format(threshold_edge_context$total_edges, big.mark = ","),
      format(render_limits$max_edges_per_node, big.mark = ",")
    )
  }

  graph_signature <- network_graph_signature(
    basis_signature = basis_context$basis_signature,
    parameters = parameters,
    module_label = module_selection$label
  )

  list(
    source_signature = basis_context$source_signature,
    basis_signature = basis_context$basis_signature,
    graph_signature = graph_signature,
    data_space = basis_context$data_space,
    data_space_label = basis_context$data_space_label,
    module_value = module_selection$value,
    module_label = module_selection$label,
    parameters = parameters,
    node_table = node_table,
    edge_table = display_edge_context$edges,
    full_edge_table = threshold_edge_context$edges,
    total_edge_count = threshold_edge_context$total_edges,
    display_edge_count = nrow(display_edge_context$edges),
    display_capped = display_edge_context$capped,
    render_limits = render_limits,
    note = note,
    metrics = list(
      list(label = "Nodes Drawn", value = format(nrow(node_table), big.mark = ",")),
      list(label = "Edges Passed Threshold", value = format(threshold_edge_context$total_edges, big.mark = ",")),
      list(label = "Edges Drawn", value = format(nrow(display_edge_context$edges), big.mark = ",")),
      list(label = "Data Space", value = basis_context$data_space_label),
      list(label = "Module", value = module_selection$label),
      list(label = "Genes Used", value = format(basis_context$variable_genes_used, big.mark = ","))
    )
  )
}

network_nodes_payload <- function(node_table) {
  if (is.null(node_table) || !is.data.frame(node_table) || !nrow(node_table)) {
    return(list())
  }

  lapply(seq_len(nrow(node_table)), function(index) {
    row_values <- node_table[index, , drop = FALSE]
    list(
      id = as.character(row_values$id[[1]]),
      label = as.character(row_values$label[[1]]),
      geneId = as.character(row_values$GeneID[[1]]),
      geneName = as.character(row_values$GeneName[[1]]),
      module = as.character(row_values$Module[[1]]),
      moduleIndex = unname(as.numeric(row_values$ModuleIndex[[1]])),
      connectivity = unname(as.numeric(row_values$Connectivity[[1]])),
      degree = unname(as.integer(row_values$Degree[[1]])),
      displayDegree = unname(as.integer(row_values$DisplayDegree[[1]]))
    )
  })
}

network_edges_payload <- function(edge_table) {
  if (is.null(edge_table) || !is.data.frame(edge_table) || !nrow(edge_table)) {
    return(list())
  }

  lapply(seq_len(nrow(edge_table)), function(index) {
    row_values <- edge_table[index, , drop = FALSE]
    list(
      source = as.character(row_values$source[[1]]),
      target = as.character(row_values$target[[1]]),
      weight = unname(as.numeric(row_values$weight[[1]]))
    )
  })
}

network_results_payload <- function(network_context) {
  list(
    note = network_context$note,
    graph = list(
      title = sprintf("Co-expression Network | %s", network_context$module_label),
      subtitle = sprintf(
        "%s | %s nodes | %s threshold-passed edges | %s displayed edges | threshold >= %s",
        network_context$data_space_label,
        format(nrow(network_context$node_table), big.mark = ","),
        format(network_context$total_edge_count, big.mark = ","),
        format(nrow(network_context$edge_table), big.mark = ","),
        format(round(network_context$parameters$edge_threshold, 2), nsmall = 2)
      ),
      signature = network_context$graph_signature,
      moduleLabel = network_context$module_label,
      displayCapped = isTRUE(network_context$display_capped),
      totalEdges = unname(as.integer(network_context$total_edge_count)),
      displayedEdges = unname(as.integer(nrow(network_context$edge_table))),
      autoHideLabels = isTRUE(nrow(network_context$node_table) > 60L || nrow(network_context$edge_table) > 320L),
      dragEnabled = !isTRUE(nrow(network_context$node_table) > 90L || nrow(network_context$edge_table) > 420L),
      nodes = network_nodes_payload(network_context$node_table),
      edges = network_edges_payload(network_context$edge_table)
    )
  )
}

network_table_export_content <- function(data_frame, format = "csv") {
  pca_table_export_content(data_frame, format = format)
}

network_nodes_export_content <- function(network_context, format = "csv") {
  network_table_export_content(network_context$node_table, format = format)
}

network_edges_export_content <- function(network_context, format = "csv") {
  edge_table <- if (!is.null(network_context$full_edge_table)) network_context$full_edge_table else network_context$edge_table
  network_table_export_content(edge_table, format = format)
}

network_display_edges_export_content <- function(network_context, format = "csv") {
  network_table_export_content(network_context$edge_table, format = format)
}
