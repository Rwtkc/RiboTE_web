enrichment_overlap_rows <- function(
  query_genes,
  background_genes,
  resource,
  collection,
  species_key = NULL,
  sort_by = "FDR",
  remove_redundant = FALSE,
  top_pathways = 10L
) {
  if (length(query_genes) == 0L) {
    return(list(
      tested = data.frame(),
      significant = data.frame(),
      displayed = data.frame()
    ))
  }

  background_genes <- unique(as.character(background_genes))
  query_genes <- intersect(unique(as.character(query_genes)), background_genes)

  if (length(query_genes) == 0L) {
    return(list(
      tested = data.frame(),
      significant = data.frame(),
      displayed = data.frame()
    ))
  }

  tested_rows <- lapply(names(resource$pathways), function(pathway_id) {
    pathway_genes <- intersect(resource$pathways[[pathway_id]], background_genes)
    overlap_genes <- intersect(query_genes, pathway_genes)

    if (length(overlap_genes) == 0L || length(pathway_genes) == 0L) {
      return(NULL)
    }

    p_value <- stats::phyper(
      q = length(overlap_genes) - 1L,
      m = length(pathway_genes),
      n = length(background_genes) - length(pathway_genes),
      k = length(query_genes),
      lower.tail = FALSE
    )

    fold_enriched <- (length(overlap_genes) / length(query_genes)) /
      (length(pathway_genes) / length(background_genes))

    metadata_row <- resource$metadata[resource$metadata$pathway_id %in% pathway_id, , drop = FALSE]
    memo <- if (nrow(metadata_row) > 0L) as.character(metadata_row$memo[[1]]) else ""

    data.frame(
      pathway_id = as.character(pathway_id),
      pathway = enrichment_pretty_pathway_name(pathway_id, collection),
      memo = memo,
      url = enrichment_pathway_url(pathway_id, memo, collection, species_key),
      overlap = as.integer(length(overlap_genes)),
      query_size = as.integer(length(query_genes)),
      pathway_size = as.integer(length(pathway_genes)),
      background_size = as.integer(length(background_genes)),
      fold = as.numeric(fold_enriched),
      pval = as.numeric(p_value),
      genes = I(list(sort(overlap_genes))),
      stringsAsFactors = FALSE
    )
  })

  tested <- do.call(rbind, Filter(Negate(is.null), tested_rows))

  if (is.null(tested) || !is.data.frame(tested) || nrow(tested) == 0L) {
    return(list(
      tested = data.frame(),
      significant = data.frame(),
      displayed = data.frame()
    ))
  }

  tested$padj <- stats::p.adjust(tested$pval, method = "fdr")
  significant <- tested[is.finite(tested$padj) & tested$padj < 0.1, , drop = FALSE]

  if (nrow(significant) == 0L) {
    if (identical(sort_by, "Fold")) {
      tested <- tested[order(tested$fold, decreasing = TRUE), , drop = FALSE]
    } else {
      tested <- tested[order(tested$padj, -tested$fold, tested$pval), , drop = FALSE]
    }

    return(list(
      tested = tested,
      significant = significant,
      displayed = utils::head(tested, as.integer(top_pathways))
    ))
  }

  if (identical(sort_by, "Fold")) {
    significant <- significant[order(significant$fold, decreasing = TRUE), , drop = FALSE]
  } else {
    significant <- significant[order(significant$padj, -significant$fold, significant$pval), , drop = FALSE]
  }

  if (isTRUE(remove_redundant) && nrow(significant) > 5L) {
    keep_mask <- rep(TRUE, nrow(significant))
    token_lists <- lapply(significant$pathway, function(value) {
      unlist(strsplit(tolower(as.character(value)), "\\s+"))
    })

    for (row_index in seq_len(nrow(significant))) {
      if (!keep_mask[[row_index]]) {
        next
      }

      for (compare_index in seq_len(row_index - 1L)) {
        if (!keep_mask[[compare_index]]) {
          next
        }

        gene_overlap <- length(intersect(significant$genes[[row_index]], significant$genes[[compare_index]])) /
          length(union(significant$genes[[row_index]], significant$genes[[compare_index]]))

        if (is.finite(gene_overlap) && gene_overlap > 0.9) {
          token_overlap <- length(intersect(token_lists[[row_index]], token_lists[[compare_index]])) /
            length(union(token_lists[[row_index]], token_lists[[compare_index]]))

          if (is.finite(token_overlap) && token_overlap > 0.5) {
            keep_mask[[row_index]] <- FALSE
            break
          }
        }
      }
    }

    significant <- significant[keep_mask, , drop = FALSE]
  }

  displayed <- utils::head(significant, as.integer(top_pathways))

  list(
    tested = tested,
    significant = significant,
    displayed = displayed
  )
}

enrichment_combined_table <- function(grouped_rows, scope = c("displayed", "significant", "tested")) {
  scope <- match.arg(scope)

  combined <- lapply(names(grouped_rows), function(group_name) {
    table_rows <- grouped_rows[[group_name]][[scope]]

    if (is.null(table_rows) || !is.data.frame(table_rows) || nrow(table_rows) == 0L) {
      return(NULL)
    }

    table_rows$group <- group_name
    table_rows
  })

  combined <- Filter(Negate(is.null), combined)

  if (length(combined) == 0L) {
    return(data.frame())
  }

  combined_df <- do.call(rbind, combined)
  rownames(combined_df) <- NULL
  combined_df
}

