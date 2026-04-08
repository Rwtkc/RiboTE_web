ribote_species_catalog <- function() {
  list(
    "Homo sapiens (hg38)" = list(
      label = "Homo sapiens (hg38)",
      acronym = "hg38",
      org_id = 99L,
      org_db_name = "hg38.geneInfo.sqlite",
      gff_rda_name = "hg38.gff.rda",
      txdb_name = "hg38.gencode.sqlite",
      txlens_name = "hg38.txlens.rda",
      fasta_name = "hg38.txdb.fa",
      tai_name = "hg38.tai",
      cbi_name = "hg38.cds.m"
    ),
    "Oryza sativa (IRGSP 1.0)" = list(
      label = "Oryza sativa (IRGSP 1.0)",
      acronym = "osa_IRGSP_1",
      org_id = NA_integer_,
      org_db_name = "osa_IRGSP_1.geneInfo.sqlite",
      gff_rda_name = "osa_IRGSP_1.gff.rda",
      txdb_name = "osa_IRGSP_1.gencode.sqlite",
      txlens_name = "osa_IRGSP_1.txlens.rda",
      fasta_name = "osa_IRGSP_1.txdb.fa",
      tai_name = "osa_IRGSP_1.tai",
      cbi_name = "osa_IRGSP_1.cds.m"
    )
  )
}

ribote_species_choices <- function() {
  unname(names(ribote_species_catalog()))
}

ribote_species_meta <- function(species_label) {
  catalog <- ribote_species_catalog()
  meta <- catalog[[species_label]]

  if (is.null(meta)) {
    return(NULL)
  }

  meta
}

ribote_load_data_controls_config <- function(id) {
  ns <- NS(id)

  list(
    species_choices = as.list(ribote_species_choices()),
    file_input_id = ns("gene_matrix"),
    data_source_id = ns("data_source"),
    species_id = ns("species"),
    sample_type_manifest_id = ns("sample_type_manifest"),
    pair_manifest_id = ns("pair_manifest"),
    save_button_id = ns("save_context"),
    demo_button_id = ns("load_demo"),
    demo_values = list(
      species = "Homo sapiens (hg38)",
      file_name = "all.count.txt",
      sample_names = list("RNA.WT1", "RNA.WT2", "RNA.KO1", "RNA.KO2", "RPF.WT1", "RPF.WT2", "RPF.KO1", "RPF.KO2"),
      sample_type_manifest = list(
        list(sample_name = "RNA.WT1", sample_type = "RNA-seq"),
        list(sample_name = "RNA.WT2", sample_type = "RNA-seq"),
        list(sample_name = "RNA.KO1", sample_type = "RNA-seq"),
        list(sample_name = "RNA.KO2", sample_type = "RNA-seq"),
        list(sample_name = "RPF.WT1", sample_type = "Ribo-seq"),
        list(sample_name = "RPF.WT2", sample_type = "Ribo-seq"),
        list(sample_name = "RPF.KO1", sample_type = "Ribo-seq"),
        list(sample_name = "RPF.KO2", sample_type = "Ribo-seq")
      ),
      pair_manifest = list(
        list(rna_sample = "RNA.WT1", ribo_sample = "RPF.WT1", group_role = "Control"),
        list(rna_sample = "RNA.WT2", ribo_sample = "RPF.WT2", group_role = "Control"),
        list(rna_sample = "RNA.KO1", ribo_sample = "RPF.KO1", group_role = "Treatment"),
        list(rna_sample = "RNA.KO2", ribo_sample = "RPF.KO2", group_role = "Treatment")
      )
    )
  )
}
