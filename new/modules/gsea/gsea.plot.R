gsea_read_pathways <- function(gmt_path, species_key = NULL) {
  pathways <- fgsea::gmtPathways(gmt_path)
  pathways <- lapply(pathways, function(genes) {
    unique(gsea_normalize_gene_ids(genes, species_key))
  })
  pathways[vapply(pathways, length, integer(1)) > 0L]
}

gsea_rank_column <- function(result_table) {
  for (column_name in c("TE_log2FC", "log2FoldChange", "logTEfc")) {
    if (column_name %in% colnames(result_table)) {
      return(column_name)
    }
  }

  NULL
}

gsea_identifier_column <- function(result_table, species_key = NULL) {
  preferred_columns <- if (identical(as.character(species_key), "osa_IRGSP_1")) {
    c("GeneID", "gene_id", "gene_name", "GeneSymbol", "SYMBOL")
  } else {
    c("gene_name", "GeneSymbol", "SYMBOL", "GeneID", "gene_id")
  }

  for (column_name in preferred_columns) {
    if (column_name %in% colnames(result_table)) {
      return(column_name)
    }
  }

  NULL
}

gsea_normalize_gene_ids <- function(genes, species_key = NULL) {
  normalized <- trimws(as.character(genes))

  if (identical(as.character(species_key), "osa_IRGSP_1")) {
    normalized <- sub("^OS([0-9]{2})G", "Os\\1g", normalized, perl = TRUE)
    return(normalized)
  }

  if (is.null(species_key) || identical(as.character(species_key), "hg38")) {
    normalized <- toupper(normalized)
  }

  normalized
}

gsea_prepare_ranked_stats <- function(result_table, species_key = NULL) {
  stopifnot(is.data.frame(result_table))

  score_column <- gsea_rank_column(result_table)
  identifier_column <- gsea_identifier_column(result_table, species_key)

  if (is.null(score_column)) {
    stop("Translation Efficiency results do not include a TE log2 fold-change column for GSEA ranking.", call. = FALSE)
  }

  if (is.null(identifier_column)) {
    stop("Translation Efficiency results do not include a gene identifier column required by the local GSEA gene sets.", call. = FALSE)
  }

  ranked <- data.frame(
    gene_id = gsea_normalize_gene_ids(result_table[[identifier_column]], species_key),
    score = suppressWarnings(as.numeric(result_table[[score_column]])),
    stringsAsFactors = FALSE
  )

  ranked <- ranked[
    nzchar(ranked$gene_id) &
      !is.na(ranked$gene_id) &
      is.finite(ranked$score),
    ,
    drop = FALSE
  ]

  if (nrow(ranked) == 0L) {
    stop("No finite gene identifiers remain after preparing the ranked TE list for GSEA.", call. = FALSE)
  }

  ranked <- ranked[order(abs(ranked$score), decreasing = TRUE), , drop = FALSE]
  ranked <- ranked[!duplicated(ranked$gene_id), , drop = FALSE]
  ranked <- ranked[order(-ranked$score, ranked$gene_id), , drop = FALSE]

  stats <- ranked$score - (seq_len(nrow(ranked)) * 1e-12)
  names(stats) <- ranked$gene_id
  stats
}

gsea_pretty_pathway_name <- function(pathway_id, collection) {
  display_name <- as.character(pathway_id)

  if (identical(collection, "hallmark")) {
    display_name <- sub("^HALLMARK_", "", display_name)
  } else if (identical(collection, "reactome")) {
    display_name <- sub("^REACTOME_", "", display_name)
  } else if (identical(collection, "go_bp")) {
    display_name <- sub("^GOBP_", "", display_name)
  } else if (identical(collection, "go_mf")) {
    display_name <- sub("^GOMF_", "", display_name)
  } else if (identical(collection, "go_cc")) {
    display_name <- sub("^GOCC_", "", display_name)
  } else if (identical(collection, "kegg")) {
    display_name <- sub("^KEGG_", "", display_name)
  }

  gsub("_", " ", display_name)
}

gsea_running_score <- function(stats, pathway_genes, species_key = NULL) {
  ranked_stats <- sort(stats, decreasing = TRUE)
  ranked_names <- names(ranked_stats)
  pathway_hits <- ranked_names %in% unique(gsea_normalize_gene_ids(pathway_genes, species_key))
  hit_count <- sum(pathway_hits)
  gene_count <- length(ranked_stats)

  if (hit_count == 0L || hit_count >= gene_count) {
    return(NULL)
  }

  weighted_hits <- abs(ranked_stats)
  hit_weight_total <- sum(weighted_hits[pathway_hits])

  if (!is.finite(hit_weight_total) || hit_weight_total <= 0) {
    return(NULL)
  }

  running_score <- cumsum(ifelse(
    pathway_hits,
    weighted_hits / hit_weight_total,
    -1 / (gene_count - hit_count)
  ))

  peak_index <- which.max(abs(running_score))

  list(
    points = data.frame(
      x = seq_along(running_score),
      y = as.numeric(running_score),
      stringsAsFactors = FALSE
    ),
    metric_points = data.frame(
      x = seq_along(ranked_stats),
      y = as.numeric(ranked_stats),
      stringsAsFactors = FALSE
    ),
    hits = which(pathway_hits),
    peak_index = peak_index,
    peak_x = peak_index,
    peak_y = as.numeric(running_score[[peak_index]]),
    gene_count = gene_count
  )
}

gsea_build_plot_payload <- function(stats, pathway_row, pathway_genes, collection_label, species_key = NULL) {
  running_score <- gsea_running_score(stats, pathway_genes, species_key = species_key)

  if (is.null(running_score)) {
    return(NULL)
  }

  list(
    pathwayId = as.character(pathway_row$pathway_id),
    pathway = as.character(pathway_row$pathway),
    collection = collection_label,
    nes = unname(as.numeric(pathway_row$NES)),
    padj = unname(as.numeric(pathway_row$padj)),
    pvalue = unname(as.numeric(pathway_row$pval)),
    size = unname(as.integer(pathway_row$size)),
    leadingEdgeSize = unname(as.integer(pathway_row$leading_edge_size)),
    peakX = unname(as.integer(running_score$peak_x)),
    peakY = unname(as.numeric(running_score$peak_y)),
    maxRank = unname(as.integer(running_score$gene_count)),
    points = lapply(seq_len(nrow(running_score$points)), function(index) {
      list(
        x = unname(as.integer(running_score$points$x[[index]])),
        y = unname(as.numeric(running_score$points$y[[index]]))
      )
    }),
    metricPoints = lapply(seq_len(nrow(running_score$metric_points)), function(index) {
      list(
        x = unname(as.integer(running_score$metric_points$x[[index]])),
        y = unname(as.numeric(running_score$metric_points$y[[index]]))
      )
    }),
    hits = as.list(as.integer(running_score$hits))
  )
}

gsea_metric_points_payload <- function(stats) {
  ranked_stats <- sort(stats, decreasing = TRUE)

  lapply(seq_along(ranked_stats), function(index) {
    list(
      x = unname(as.integer(index)),
      y = unname(as.numeric(ranked_stats[[index]]))
    )
  })
}

gsea_plot_catalog_payload <- function(context) {
  displayed <- context$displayed_results

  if (is.null(displayed) || !is.data.frame(displayed) || nrow(displayed) == 0L) {
    return(list(
      maxRank = as.integer(length(context$ranked_stats)),
      metricPoints = list(),
      pathways = list()
    ))
  }

  ranked_names <- names(sort(context$ranked_stats, decreasing = TRUE))

  pathway_entries <- lapply(seq_len(nrow(displayed)), function(index) {
    pathway_id <- as.character(displayed$pathway_id[[index]])
    pathway_genes <- context$pathways[[pathway_id]]
    hits <- which(ranked_names %in% unique(gsea_normalize_gene_ids(pathway_genes, context$resource_species_key)))

    list(
      pathwayId = pathway_id,
      hits = as.list(as.integer(hits))
    )
  })

  list(
    maxRank = as.integer(length(context$ranked_stats)),
    metricPoints = gsea_metric_points_payload(context$ranked_stats),
    pathways = pathway_entries
  )
}

