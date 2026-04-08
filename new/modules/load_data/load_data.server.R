mod_load_data_server <- function(id, session_state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    staged_context <- reactiveVal(NULL)
    saved_context <- reactiveVal(NULL)

    detect_delimiter <- function(file_path) {
      preview <- readLines(file_path, n = 5, warn = FALSE)
      delimiters <- c("\t", ",", ";", "|")
      delimiter_count <- sapply(delimiters, function(delim) {
        sum(vapply(preview, function(line) length(strsplit(line, delim, fixed = TRUE)[[1]]) > 1, logical(1)))
      })

      delimiters[[which.max(delimiter_count)]]
    }

    read_count_matrix_preview <- function(file_path, n = 10L) {
      delimiter <- detect_delimiter(file_path)

      utils::read.table(
        file_path,
        header = TRUE,
        sep = delimiter,
        check.names = FALSE,
        stringsAsFactors = FALSE,
        nrows = as.integer(n)
      )
    }

    resolve_species_resources <- function(species_meta) {
      org_db_dir <- app_data_path("orgDB")
      local_db_path <- file.path(org_db_dir, species_meta$org_db_name)

      if (!file.exists(local_db_path)) {
        stop(sprintf("Missing orgDB resource for %s: %s", species_meta$label, species_meta$org_db_name), call. = FALSE)
      }

      org_db_path <- normalizePath(local_db_path, winslash = "/", mustWork = TRUE)

      optional_path <- function(...) {
        path <- app_data_path(...)
        if (file.exists(path)) {
          normalizePath(path, winslash = "/", mustWork = TRUE)
        } else {
          NULL
        }
      }

      first_existing_path <- function(...) {
        candidates <- list(...)
        for (candidate in candidates) {
          if (!is.null(candidate) && nzchar(candidate) && file.exists(candidate)) {
            return(normalizePath(candidate, winslash = "/", mustWork = TRUE))
          }
        }

        NULL
      }

      list(
        org_db_path = org_db_path,
        gff_rda_path = optional_path("gff", species_meta$gff_rda_name),
        txdb_path = optional_path("txdb", species_meta$txdb_name),
        txlens_path = optional_path("txlens", species_meta$txlens_name),
        fasta_path = optional_path("fa", species_meta$fasta_name),
        tai_path = first_existing_path(optional_path("cds", species_meta$tai_name)),
        cbi_path = first_existing_path(optional_path("cds", species_meta$cbi_name))
      )
    }

    build_load_context <- function(species_label, data_source, matrix_file_path, matrix_name, is_demo = FALSE) {
      species_meta <- ribote_species_meta(species_label)

      if (is.null(species_meta)) {
        stop(sprintf("Unsupported species selection: %s", species_label), call. = FALSE)
      }

      if (is.null(matrix_file_path) || !file.exists(matrix_file_path)) {
        stop("Gene count matrix file is missing.", call. = FALSE)
      }

      resource_paths <- resolve_species_resources(species_meta)
      source_matrix_path <- normalizePath(matrix_file_path, winslash = "/", mustWork = TRUE)
      preview <- read_count_matrix_preview(source_matrix_path, n = 10L)
      sample_names <- colnames(preview)
      sample_names <- if (length(sample_names) > 1L) sample_names[-1] else character()

      list(
        data_source = data_source,
        species = species_label,
        species_meta = species_meta,
        resource_paths = resource_paths,
        matrix_name = matrix_name,
        source_matrix_path = source_matrix_path,
        matrix_path = source_matrix_path,
        preview = preview,
        sample_names = sample_names,
        matrix_stats = list(
          genes = NA_integer_,
          samples = length(sample_names)
        ),
        input_names = "",
        rpf_names = "",
        is_demo = is_demo,
        sample_type_manifest = NULL,
        pair_manifest = NULL
      )
    }

    load_saved_preview <- function(context) {
      if (is.data.frame(context$preview)) {
        return(utils::head(context$preview, 10))
      }

      preview_path <- context$matrix_path

      if (is.null(preview_path) || !file.exists(preview_path)) {
        return(NULL)
      }

      tryCatch(
        read_count_matrix_preview(preview_path, n = 10L),
        error = function(...) NULL
      )
    }

    current_context <- reactive({
      context <- staged_context()
      if (!is.null(context)) {
        return(context)
      }

      saved_context()
    })

    reset_downstream_state <- function() {
      session_state$upload_context <- NULL
      session_state$upload_saved <- FALSE
      session_state$preprocess_context <- NULL
      session_state$preprocess_ready <- FALSE
      session_state$te_context <- NULL
      session_state$te_ready <- FALSE
      session_state$last_module <- "load_data"
    }

    stage_context <- function(context) {
      staged_context(context)
      saved_context(NULL)
      reset_downstream_state()
    }

    store_context <- function(context) {
      staged_context(context)
      saved_context(context)
      session_state$upload_context <- context
      session_state$upload_saved <- TRUE
      session_state$preprocess_context <- NULL
      session_state$preprocess_ready <- FALSE
      session_state$te_context <- NULL
      session_state$te_ready <- FALSE
      session_state$last_module <- "load_data"
    }

    parse_json_input <- function(value) {
      if (is.null(value) || !nzchar(trimws(as.character(value)))) {
        return(NULL)
      }

      jsonlite::fromJSON(as.character(value), simplifyDataFrame = TRUE)
    }

    normalize_manifest_rows <- function(value) {
      if (is.null(value)) {
        return(NULL)
      }

      if (is.data.frame(value)) {
        return(value)
      }

      if (is.list(value) && length(value) > 0 && all(vapply(value, is.list, logical(1)))) {
        rows <- lapply(value, function(row) {
          as.data.frame(row, stringsAsFactors = FALSE)
        })
        normalized <- do.call(rbind, rows)
        rownames(normalized) <- NULL
        return(normalized)
      }

      value
    }

    build_pairing_state_from_manifest <- function(context, sample_manifest, pair_manifest) {
      sample_names <- context$sample_names
      if (length(sample_names) == 0) {
        stop("No sample columns were detected in the uploaded matrix.", call. = FALSE)
      }

      sample_manifest <- normalize_manifest_rows(sample_manifest)
      pair_manifest <- normalize_manifest_rows(pair_manifest)
      sample_manifest <- as.data.frame(sample_manifest, stringsAsFactors = FALSE)
      pair_manifest <- as.data.frame(pair_manifest, stringsAsFactors = FALSE)

      required_sample_cols <- c("sample_name", "sample_type")
      required_pair_cols <- c("rna_sample", "ribo_sample", "group_role")

      if (!all(required_sample_cols %in% colnames(sample_manifest))) {
        stop("Sample type manifest is incomplete.", call. = FALSE)
      }

      if (!all(required_pair_cols %in% colnames(pair_manifest))) {
        stop("Pair manifest is incomplete.", call. = FALSE)
      }

      sample_manifest$sample_name <- as.character(sample_manifest$sample_name)
      sample_manifest$sample_type <- as.character(sample_manifest$sample_type)
      pair_manifest$rna_sample <- as.character(pair_manifest$rna_sample)
      pair_manifest$ribo_sample <- as.character(pair_manifest$ribo_sample)
      pair_manifest$group_role <- as.character(pair_manifest$group_role)

      if (!setequal(sample_manifest$sample_name, sample_names)) {
        stop("Configured sample cards do not match the uploaded matrix columns.", call. = FALSE)
      }

      if (nrow(sample_manifest) != length(sample_names) || anyDuplicated(sample_manifest$sample_name)) {
        stop("Each sample column must appear exactly once in the sample type assignment.", call. = FALSE)
      }

      if (!all(sample_manifest$sample_type %in% c("RNA-seq", "Ribo-seq"))) {
        stop("Sample type assignment must use RNA-seq or Ribo-seq.", call. = FALSE)
      }

      rna_samples <- sample_manifest$sample_name[sample_manifest$sample_type == "RNA-seq"]
      ribo_samples <- sample_manifest$sample_name[sample_manifest$sample_type == "Ribo-seq"]

      if (length(rna_samples) == 0L || length(ribo_samples) == 0L) {
        stop("Assign at least one RNA-seq sample and one Ribo-seq sample.", call. = FALSE)
      }

      if (length(rna_samples) != length(ribo_samples)) {
        stop("RNA-seq and Ribo-seq partitions must have the same number of samples for one-to-one pairing.", call. = FALSE)
      }

      if (nrow(pair_manifest) != length(rna_samples)) {
        stop("Provide one pair mapping for each RNA-seq sample.", call. = FALSE)
      }

      if (!setequal(pair_manifest$rna_sample, rna_samples)) {
        stop("Each RNA-seq sample must be paired exactly once.", call. = FALSE)
      }

      if (!setequal(pair_manifest$ribo_sample, ribo_samples)) {
        stop("Each Ribo-seq sample must be paired exactly once.", call. = FALSE)
      }

      if (anyDuplicated(pair_manifest$rna_sample) || anyDuplicated(pair_manifest$ribo_sample)) {
        stop("Pair mappings must be one-to-one.", call. = FALSE)
      }

      if (!all(pair_manifest$group_role %in% c("Control", "Treatment"))) {
        stop("Each pair must be assigned to Control or Treatment.", call. = FALSE)
      }

      if (!all(c("Control", "Treatment") %in% unique(pair_manifest$group_role))) {
        stop("At least one Control pair and one Treatment pair are required.", call. = FALSE)
      }

      ordered_pairs <- pair_manifest[match(rna_samples, pair_manifest$rna_sample), , drop = FALSE]
      ordered_pairs$group_role <- factor(ordered_pairs$group_role, levels = c("Control", "Treatment"))
      ordered_pairs <- ordered_pairs[order(ordered_pairs$group_role, ordered_pairs$rna_sample), , drop = FALSE]
      ordered_pairs$group_role <- as.character(ordered_pairs$group_role)
      rownames(ordered_pairs) <- NULL

      input_names <- vapply(
        c("Control", "Treatment"),
        function(role) paste(ordered_pairs$rna_sample[ordered_pairs$group_role == role], collapse = ","),
        character(1)
      )
      rpf_names <- vapply(
        c("Control", "Treatment"),
        function(role) paste(ordered_pairs$ribo_sample[ordered_pairs$group_role == role], collapse = ","),
        character(1)
      )

      list(
        sample_type_manifest = sample_manifest[match(sample_names, sample_manifest$sample_name), , drop = FALSE],
        pair_manifest = ordered_pairs,
        input_names = paste(input_names, collapse = "#"),
        rpf_names = paste(rpf_names, collapse = "#")
      )
    }

    build_pairing_state <- function(context) {
      sample_manifest <- parse_json_input(input$sample_type_manifest)
      pair_manifest <- parse_json_input(input$pair_manifest)

      if ((is.null(sample_manifest) || is.null(pair_manifest)) &&
        !is.null(context$sample_type_manifest) &&
        !is.null(context$pair_manifest)) {
        sample_manifest <- context$sample_type_manifest
        pair_manifest <- context$pair_manifest
      }

      if (is.null(sample_manifest) || is.null(pair_manifest)) {
        stop("Complete sample pairing before saving.", call. = FALSE)
      }

      build_pairing_state_from_manifest(context, sample_manifest, pair_manifest)
    }

    capture_context <- function(expr) {
      tryCatch(
        expr,
        error = function(err) {
          message("Load Data error: ", conditionMessage(err))
          showNotification(conditionMessage(err), type = "error", duration = 5)
          NULL
        }
      )
    }

    publish_controls <- function() {
      config <- ribote_load_data_controls_config(id)
      config$uploaded_count <- if (is.null(input$gene_matrix)) 0 else 1

      send_control_payload(
        session = session,
        id = session$ns("load_data_controls"),
        control = "ribote-load-data-controls",
        config = config
      )
    }

    session$onFlushed(function() {
      isolate(publish_controls())
    }, once = TRUE)

    observeEvent(input$controls_ready, {
      publish_controls()
    }, ignoreInit = TRUE)

    observe({
      input$gene_matrix
      input$data_source
      input$species
      input$sample_type_manifest
      input$pair_manifest
      publish_controls()
    })

    observeEvent(input$load_demo, {
      defaults <- ribote_load_data_controls_config(id)$demo_values
      context <- capture_context(
        build_load_context(
          species_label = defaults$species,
          data_source = "Demo Data",
          matrix_file_path = app_data_path("all.count.txt"),
          matrix_name = defaults$file_name,
          is_demo = TRUE
        )
      )

      if (!is.null(context)) {
        demo_pairing <- capture_context(
          build_pairing_state_from_manifest(
            context,
            sample_manifest = defaults$sample_type_manifest,
            pair_manifest = defaults$pair_manifest
          )
        )
        if (!is.null(demo_pairing)) {
          context$sample_type_manifest <- demo_pairing$sample_type_manifest
          context$pair_manifest <- demo_pairing$pair_manifest
          context$input_names <- demo_pairing$input_names
          context$rpf_names <- demo_pairing$rpf_names
        }
        store_context(context)
      }
    })

    observeEvent(input$save_context, {
      req(nzchar(input$species))

      active_context <- current_context()
      has_uploaded_file <- !is.null(input$gene_matrix$datapath) && nzchar(input$gene_matrix$datapath)
      preserve_demo_context <- !has_uploaded_file && !is.null(active_context) && isTRUE(active_context$is_demo)

      context <- capture_context({
        if (has_uploaded_file) {
          build_load_context(
            species_label = input$species,
            data_source = if (is.null(input$data_source) || !nzchar(input$data_source)) "Upload File" else input$data_source,
            matrix_file_path = input$gene_matrix$datapath,
            matrix_name = input$gene_matrix$name,
            is_demo = FALSE
          )
        } else if (preserve_demo_context) {
          active_context
        } else {
          stop("Select a gene count matrix or load the demo dataset first.", call. = FALSE)
        }
      })

      if (!is.null(context)) {
        pairing_state <- capture_context(build_pairing_state(context))
        if (is.null(pairing_state)) {
          stage_context(context)
          showNotification("Matrix staged. Configure sample pairing before saving.", type = "message", duration = 4)
          return(invisible(NULL))
        }

        context$sample_type_manifest <- pairing_state$sample_type_manifest
        context$pair_manifest <- pairing_state$pair_manifest
        context$input_names <- pairing_state$input_names
        context$rpf_names <- pairing_state$rpf_names
        store_context(context)
      }
    })

    output$session_summary <- renderUI({
      context <- current_context()

      if (is.null(context)) {
        return(
          div(
            class = "ribote-load-summary",
            tags$h3(class = "ribote-main__title", "Current input context"),
            tags$p(
              class = "ribote-main__copy",
              "Select a species and upload a gene count matrix to unlock downstream modules."
            )
          )
        )
      }

      preview <- load_saved_preview(context)
      pair_manifest <- context$pair_manifest
      is_confirmed <- isTRUE(session_state$upload_saved) && !is.null(saved_context())

      tags$div(
        class = "ribote-load-summary",
        tags$h3(class = "ribote-main__title", "Current input context"),
        tags$p(
          class = "ribote-main__copy",
          "Select a species and upload a gene count matrix to unlock downstream modules."
        ),
        tags$p(
          class = "ribote-preview__file",
          tags$span(class = "ribote-preview__file-label", "Species"),
          tags$span(class = "ribote-preview__file-name", context$species)
        ),
        if (!is.null(preview)) {
          div(
            class = "ribote-preview",
            div(
              class = "ribote-preview__header",
              tags$h4(class = "ribote-preview__title", "Saved file preview (first 10 rows)"),
              tags$p(
                class = "ribote-preview__file",
                tags$span(class = "ribote-preview__file-label", "Saved File"),
                tags$span(class = "ribote-preview__file-name", context$matrix_name)
              )
            ),
            div(
              class = "ribote-table-wrap",
              tags$table(
                class = "ribote-preview-table",
                tags$thead(
                  tags$tr(lapply(colnames(preview), function(column) tags$th(column)))
                ),
                tags$tbody(
                  lapply(seq_len(nrow(preview)), function(index) {
                    tags$tr(lapply(preview[index, , drop = FALSE], function(value) tags$td(as.character(value))))
                  })
                )
              )
            )
          )
        } else {
          NULL
        },
        div(
          class = "ribote-annotation",
          div(
            class = "ribote-annotation__header",
            tags$h4(class = "ribote-preview__title", "Sample Pairing"),
            tags$p(
              class = "ribote-annotation__status",
              if (is_confirmed) {
                "Sample pairing confirmed"
              } else {
                "Sample pairing is required before saving."
              }
            )
          ),
          if (!is.null(pair_manifest) && nrow(pair_manifest) > 0) {
            div(
              class = "ribote-table-wrap",
              tags$table(
                class = "ribote-preview-table ribote-preview-table--annotation",
                tags$thead(
                  tags$tr(
                    tags$th("RNA-seq"),
                    tags$th("Ribo-seq"),
                    tags$th("Group Role")
                  )
                ),
                tags$tbody(
                  lapply(seq_len(nrow(pair_manifest)), function(index) {
                    tags$tr(
                      tags$td(pair_manifest$rna_sample[[index]]),
                      tags$td(pair_manifest$ribo_sample[[index]]),
                      tags$td(pair_manifest$group_role[[index]])
                    )
                  })
                )
              )
            )
          } else {
            tags$p(class = "ribote-annotation__status", "Open Configure Samples to assign RNA-seq and Ribo-seq cards, then build one-to-one Control/Treatment pairs.")
          }
        )
      )
    })
  })
}
