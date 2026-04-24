#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

arg_value <- function(flag, default = NULL) {
  match_index <- match(flag, args)
  if (is.na(match_index) || match_index >= length(args)) {
    return(default)
  }

  args[[match_index + 1L]]
}

has_flag <- function(flag) {
  flag %in% args
}

script_path <- tryCatch(normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = FALSE), error = function(...) NULL)
if (!is.null(script_path) && nzchar(script_path)) {
  repo_root <- normalizePath(file.path(dirname(script_path), "..", ".."), winslash = "/", mustWork = TRUE)
} else {
  repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

usage <- function() {
  cat(
    paste(
      "Usage:",
      "  Rscript new/scripts/build_mouse_kegg_gmt.R [--out path] [--force]",
      "",
      "Default output:",
      "  new/resources/gene_sets/mm10/mm10.kegg.gmt",
      "",
      "Data source:",
      "  KEGG REST API endpoints for mouse (mmu).",
      sep = "\n"
    )
  )
}

if (has_flag("--help") || has_flag("-h")) {
  usage()
  quit(status = 0)
}

out_path <- arg_value("--out", file.path(repo_root, "new", "resources", "gene_sets", "mm10", "mm10.kegg.gmt"))
force <- has_flag("--force")

if (file.exists(out_path) && !force) {
  stop(sprintf("Output already exists. Use --force to overwrite: %s", out_path), call. = FALSE)
}

dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)

read_remote_lines <- function(url) {
  con <- url(url, open = "rb")
  on.exit(close(con), add = TRUE)
  readLines(con, warn = FALSE, encoding = "UTF-8")
}

safe_pathway_token <- function(value) {
  normalized <- toupper(trimws(as.character(value)))
  normalized <- gsub(" - MUS MUSCULUS \\(HOUSE MOUSE\\)$", "", normalized)
  normalized <- gsub("[^A-Z0-9]+", "_", normalized)
  normalized <- gsub("^_+|_+$", "", normalized)
  normalized
}

extract_primary_symbol <- function(description) {
  text <- trimws(as.character(description))
  if (!nzchar(text)) {
    return("")
  }

  symbol_field <- strsplit(text, ";", fixed = TRUE)[[1]][1]
  symbol_field <- trimws(symbol_field)
  symbol <- strsplit(symbol_field, ",", fixed = TRUE)[[1]][1]
  trimws(symbol)
}

message("Downloading KEGG mouse pathway definitions...")
pathway_lines <- read_remote_lines("https://rest.kegg.jp/list/pathway/mmu")
link_lines <- read_remote_lines("https://rest.kegg.jp/link/mmu/pathway")
gene_lines <- read_remote_lines("https://rest.kegg.jp/list/mmu")

pathway_parts <- strsplit(pathway_lines, "\t", fixed = TRUE)
pathway_table <- do.call(
  rbind,
  lapply(pathway_parts, function(parts) {
    if (length(parts) < 2L) {
      return(NULL)
    }

    pathway_id <- trimws(parts[[1]])
    pathway_name <- trimws(parts[[2]])

    data.frame(
      pathway_id = pathway_id,
      pathway_name = pathway_name,
      stringsAsFactors = FALSE
    )
  })
)

link_parts <- strsplit(link_lines, "\t", fixed = TRUE)
link_table <- do.call(
  rbind,
  lapply(link_parts, function(parts) {
    if (length(parts) < 2L) {
      return(NULL)
    }

    data.frame(
      pathway_id = sub("^path:", "", trimws(parts[[1]])),
      gene_id = trimws(parts[[2]]),
      stringsAsFactors = FALSE
    )
  })
)

gene_parts <- strsplit(gene_lines, "\t", fixed = TRUE)
gene_table <- do.call(
  rbind,
  lapply(gene_parts, function(parts) {
    if (length(parts) < 4L) {
      return(NULL)
    }

    data.frame(
      gene_id = trimws(parts[[1]]),
      gene_symbol = extract_primary_symbol(parts[[4]]),
      stringsAsFactors = FALSE
    )
  })
)

gene_table <- gene_table[nzchar(gene_table$gene_symbol), , drop = FALSE]
gene_table <- gene_table[!duplicated(gene_table$gene_id), , drop = FALSE]
rownames(gene_table) <- NULL

gene_symbol_map <- gene_table$gene_symbol
names(gene_symbol_map) <- gene_table$gene_id

pathway_to_genes <- split(link_table$gene_id, link_table$pathway_id)

gmt_lines <- lapply(seq_len(nrow(pathway_table)), function(index) {
  pathway_id <- pathway_table$pathway_id[[index]]
  pathway_name <- pathway_table$pathway_name[[index]]
  gene_ids <- unique(pathway_to_genes[[pathway_id]])

  if (is.null(gene_ids) || length(gene_ids) == 0L) {
    return(NULL)
  }

  gene_symbols <- unique(unname(gene_symbol_map[gene_ids]))
  gene_symbols <- gene_symbols[!is.na(gene_symbols) & nzchar(gene_symbols)]

  if (length(gene_symbols) == 0L) {
    return(NULL)
  }

  gmt_name <- paste(
    "KEGG",
    toupper(pathway_id),
    safe_pathway_token(pathway_name),
    sep = "_"
  )
  pathway_url <- sprintf("https://www.kegg.jp/entry/%s", pathway_id)

  paste(c(gmt_name, pathway_url, gene_symbols), collapse = "\t")
})

gmt_lines <- Filter(Negate(is.null), gmt_lines)
gmt_lines <- unlist(gmt_lines, use.names = FALSE)

writeLines(gmt_lines, out_path, useBytes = TRUE)

message(sprintf("Wrote %s KEGG mouse pathways.", format(length(gmt_lines), big.mark = ",")))
message(sprintf("Output GMT: %s", normalizePath(out_path, winslash = "/", mustWork = TRUE)))
