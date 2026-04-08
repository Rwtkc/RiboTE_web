enrichment_read_gmt_resource <- function(gmt_path, species_key = NULL) {
  lines <- readLines(gmt_path, warn = FALSE, encoding = "UTF-8")

  parsed <- lapply(lines, function(line) {
    fields <- strsplit(line, "\t", fixed = TRUE)[[1]]

    if (length(fields) < 3L) {
      return(NULL)
    }

    genes <- enrichment_normalize_gene_ids(fields[-c(1, 2)], species_key)
    genes <- genes[nzchar(genes)]
    genes <- unique(genes)

    if (length(genes) == 0L) {
      return(NULL)
    }

    list(
      pathway_id = as.character(fields[[1]]),
      memo = as.character(fields[[2]]),
      genes = genes
    )
  })

  parsed <- Filter(Negate(is.null), parsed)

  list(
    pathways = stats::setNames(
      lapply(parsed, `[[`, "genes"),
      vapply(parsed, `[[`, character(1), "pathway_id")
    ),
    metadata = data.frame(
      pathway_id = vapply(parsed, `[[`, character(1), "pathway_id"),
      memo = vapply(parsed, `[[`, character(1), "memo"),
      stringsAsFactors = FALSE
    )
  )
}

enrichment_valid_url <- function(value) {
  value <- trimws(as.character(value))
  length(value) == 1L && !is.na(value) && isTRUE(grepl("^https?://", value, ignore.case = TRUE))
}

enrichment_extract_go_id <- function(...) {
  text <- paste(trimws(as.character(unlist(list(...)))), collapse = " ")
  match <- regexpr("GO:[0-9]{7}", text, perl = TRUE)

  if (match[[1]] < 0L) {
    return("")
  }

  regmatches(text, match)
}

enrichment_pathway_url <- function(pathway_id, memo, collection, species_key = NULL) {
  pathway_id <- trimws(as.character(pathway_id))
  memo <- trimws(as.character(memo))
  collection <- as.character(collection)
  species_key <- as.character(species_key)

  if (enrichment_valid_url(memo)) {
    return(memo)
  }

  if (enrichment_valid_url(pathway_id)) {
    return(pathway_id)
  }

  if (collection %in% c("go_bp", "go_mf", "go_cc")) {
    go_id <- enrichment_extract_go_id(pathway_id, memo)
    if (nzchar(go_id)) {
      return(paste0("https://amigo.geneontology.org/amigo/term/", go_id))
    }
  }

  if (identical(species_key, "osa_IRGSP_1") && identical(collection, "kegg")) {
    search_term <- if (nzchar(memo)) memo else enrichment_pretty_pathway_name(pathway_id, collection)
    if (nzchar(search_term)) {
      return(paste0(
        "https://www.kegg.jp/kegg-bin/search_pathway_text?map=osa&keyword=",
        utils::URLencode(search_term, reserved = TRUE)
      ))
    }
  }

  ""
}

enrichment_pretty_pathway_name <- function(pathway_id, collection) {
  display_name <- as.character(pathway_id)

  if (identical(collection, "go_bp")) {
    display_name <- sub("^GOBP_", "", display_name)
  } else if (identical(collection, "go_mf")) {
    display_name <- sub("^GOMF_", "", display_name)
  } else if (identical(collection, "go_cc")) {
    display_name <- sub("^GOCC_", "", display_name)
  } else if (identical(collection, "kegg")) {
    display_name <- sub("^KEGG_MEDICUS_", "", display_name)
    display_name <- sub("^KEGG_", "", display_name)
  }

  gsub("_", " ", display_name)
}

enrichment_identifier_column <- function(result_table, species_key = NULL) {
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

enrichment_rank_column <- function(result_table) {
  for (column_name in c("TE_log2FC", "log2FoldChange", "logTEfc")) {
    if (column_name %in% colnames(result_table)) {
      return(column_name)
    }
  }

  NULL
}

enrichment_normalize_gene_ids <- function(genes, species_key = NULL) {
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

enrichment_prepare_gene_table <- function(result_table, species_key = NULL) {
  stopifnot(is.data.frame(result_table))

  score_column <- enrichment_rank_column(result_table)
  identifier_column <- enrichment_identifier_column(result_table, species_key)
  padj_column <- if ("padj" %in% colnames(result_table)) "padj" else NULL

  if (is.null(score_column) || is.null(identifier_column) || is.null(padj_column)) {
    stop("Translation Efficiency results do not include the gene identifier, TE log2FC, and padj columns required for Enrichment.", call. = FALSE)
  }

  gene_table <- data.frame(
    gene_symbol = enrichment_normalize_gene_ids(result_table[[identifier_column]], species_key),
    log2fc = suppressWarnings(as.numeric(result_table[[score_column]])),
    padj = suppressWarnings(as.numeric(result_table[[padj_column]])),
    stringsAsFactors = FALSE
  )

  gene_table <- gene_table[
    nzchar(gene_table$gene_symbol) &
      !is.na(gene_table$gene_symbol) &
      is.finite(gene_table$log2fc) &
      is.finite(gene_table$padj),
    ,
    drop = FALSE
  ]

  if (nrow(gene_table) == 0L) {
    stop("No valid gene symbols remain after preparing Enrichment inputs.", call. = FALSE)
  }

  gene_table <- gene_table[order(abs(gene_table$log2fc), decreasing = TRUE), , drop = FALSE]
  gene_table <- gene_table[!duplicated(gene_table$gene_symbol), , drop = FALSE]
  rownames(gene_table) <- NULL
  gene_table
}

enrichment_gene_lists <- function(gene_table) {
  up_genes <- unique(gene_table$gene_symbol[gene_table$log2fc > log2(2) & gene_table$padj < 0.1])
  down_genes <- unique(gene_table$gene_symbol[gene_table$log2fc < -log2(2) & gene_table$padj < 0.1])

  list(
    Up = up_genes,
    Down = down_genes
  )
}

enrichment_filtered_background <- function(gene_table) {
  unique(gene_table$gene_symbol)
}

enrichment_full_background <- function(resource) {
  unique(unlist(resource$pathways, use.names = FALSE))
}

