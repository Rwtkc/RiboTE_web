help_sections <- function() {
  list(
    list(
      id = "overview",
      label = "Overview",
      summary = paste(
        "Learn what RiboTE is used for and how the analysis pages fit together",
        "from count data to biological interpretation."
      ),
      blocks = list(
        list(
          title = "What RiboTE Web helps you answer",
          paragraphs = c(
            paste(
              "RiboTE Web is designed for paired RNA-seq and ribosome profiling",
              "experiments. It helps you compare gene expression, ribosome occupancy,",
              "and translation efficiency between control and treatment samples."
            ),
            paste(
              "The main question is whether a gene changes at the RNA level, the",
              "ribosome profiling level, or the translation efficiency level. After",
              "that, downstream pages help explain the pattern with sample structure,",
              "gene sets, network context, protein signal information, and codon usage."
            )
          ),
          lists = list(
            list(
              label = "What each analysis area is for",
              items = c(
                "Load Data: choose species, load the count matrix, and define RNA / Ribo sample pairs.",
                "Data Preprocess: prepare counts and check whether sample libraries look reasonable.",
                "Translation Efficiency: identify TE Up, TE Down, and unchanged genes.",
                "PCA and Clustering: inspect whether samples and genes form expected patterns.",
                "GSEA and Enrichment: ask which biological pathways or gene sets are associated with TE changes.",
                "Network and SignalP: inspect downstream biological context after TE analysis.",
                "Codon: ask whether codon usage, codon bias, or codon runs are associated with TE behavior."
              )
            )
          )
        ),
        list(
          title = "A practical way to use the app",
          paragraphs = c(
            paste(
              "Start with the input files and move step by step. Do not interpret",
              "codon, enrichment, clustering, or network results before the upstream",
              "translation efficiency result has been produced."
            ),
            paste(
              "If you change the input data or rerun an upstream analysis, rerun the",
              "downstream pages that depend on it. This keeps the displayed figures,",
              "tables, and exports consistent with the current experiment."
            )
          )
        )
      )
    ),
    list(
      id = "getting-started",
      label = "Getting Started",
      summary = paste(
        "Follow the recommended order: Load Data, Data Preprocess, Translation",
        "Efficiency, then downstream interpretation pages."
      ),
      blocks = list(
        list(
          title = "Step 1: Load the paired count data",
          paragraphs = c(
            paste(
              "Open Load Data and choose the species used by your experiment.",
              "Then load the count matrix and assign each sample as RNA-seq or",
              "Ribo-seq. Each RNA sample should be paired with the matching Ribo",
              "sample from the same biological condition."
            ),
            paste(
              "The sample roles also need to be assigned as Control or Treatment.",
              "These labels are used throughout the app, so check them carefully",
              "before moving to the next step."
            )
          )
        ),
        list(
          title = "Step 2: Run Data Preprocess",
          paragraphs = c(
            paste(
              "Data Preprocess prepares the count matrix for downstream analysis",
              "and shows quality-control summaries. Use this page to catch obvious",
              "library-size or count-distribution issues before interpreting TE."
            )
          )
        ),
        list(
          title = "Step 3: Run Translation Efficiency",
          paragraphs = c(
            paste(
              "Translation Efficiency compares treatment and control conditions",
              "after accounting for RNA abundance and ribosome profiling signal.",
              "The result separates genes into TE Up, TE Down, and Non groups."
            ),
            paste(
              "TE Up means translation efficiency is higher in treatment. TE Down",
              "means translation efficiency is lower in treatment. Non means the",
              "gene does not pass the current threshold for a TE change."
            )
          )
        )
      )
    ),
    list(
      id = "load-data",
      label = "Load Data",
      summary = paste(
        "Define the species, count matrix, and RNA / Ribo sample pairing used",
        "by the rest of the workflow."
      ),
      blocks = list(
        list(
          title = "Why this page matters",
          paragraphs = c(
            paste(
              "Most RiboTE results depend on correct sample pairing. If an RNA",
              "sample is paired with the wrong Ribo sample, or if Control and",
              "Treatment labels are swapped, downstream TE results will be misleading."
            ),
            paste(
              "Use the demo loader when you want to check the full workflow quickly.",
              "For your own data, confirm the species and sample labels before",
              "running preprocessing."
            )
          ),
          lists = list(
            list(
              label = "Before you continue",
              items = c(
                "The species should match the annotation and CDS resources.",
                "Each RNA-seq sample should have a matching Ribo-seq sample.",
                "Control and Treatment labels should match the experimental design."
              )
            )
          )
        )
      )
    ),
    list(
      id = "preprocess-te",
      label = "Preprocess & TE",
      summary = paste(
        "Prepare the count data and identify genes with changed translation",
        "efficiency."
      ),
      blocks = list(
        list(
          title = "Data Preprocess",
          paragraphs = c(
            paste(
              "Preprocessing prepares the count matrix for analysis and gives you",
              "basic quality-control views. The goal is to make sure the input",
              "data are usable before TE results are interpreted."
            ),
            paste(
              "If preprocessing is run again, old downstream results should be",
              "treated as outdated because they were calculated from the previous",
              "processed data."
            )
          )
        ),
        list(
          title = "Translation Efficiency",
          paragraphs = c(
            paste(
              "The TE page asks whether ribosome profiling changes are larger or",
              "smaller than expected from the RNA abundance changes. This helps",
              "separate transcriptional effects from translational effects."
            ),
            paste(
              "The volcano plot shows the strength and significance of TE changes.",
              "The scatter plots help compare RNA abundance, ribosome signal, and",
              "TE behavior across genes."
            )
          ),
          lists = list(
            list(
              label = "How to read TE groups",
              items = c(
                "TE Up: translation efficiency is higher in treatment.",
                "TE Down: translation efficiency is lower in treatment.",
                "Non: no significant TE change under the current thresholds."
              )
            )
          )
        )
      )
    ),
    list(
      id = "exploration",
      label = "PCA & Clustering",
      summary = paste(
        "Use global views to check sample separation and gene-pattern structure",
        "after TE analysis."
      ),
      blocks = list(
        list(
          title = "PCA",
          paragraphs = c(
            paste(
              "PCA and related projection methods reduce many genes into a simple",
              "sample map. Samples that appear close together have similar overall",
              "profiles in the selected data space."
            ),
            paste(
              "Use this page to check whether biological replicates cluster together",
              "and whether Control and Treatment samples separate in the expected",
              "direction."
            )
          )
        ),
        list(
          title = "Clustering",
          paragraphs = c(
            paste(
              "Clustering groups genes and samples by similar patterns. The heatmap",
              "is useful when you want to see whether a set of genes shares a common",
              "RNA, Ribo, or TE behavior."
            ),
            paste(
              "Strong blocks of color usually indicate groups of genes with similar",
              "profiles. Use the detail heatmap when you need to inspect a smaller",
              "region more closely."
            )
          )
        )
      )
    ),
    list(
      id = "gene-sets",
      label = "GSEA & Enrichment",
      summary = paste(
        "Ask whether TE-changing genes are associated with known pathways or",
        "gene sets."
      ),
      blocks = list(
        list(
          title = "GSEA",
          paragraphs = c(
            paste(
              "GSEA uses all genes ranked by TE change. It asks whether genes from",
              "a known pathway tend to appear near the top or bottom of the ranked",
              "list rather than being scattered randomly."
            ),
            paste(
              "Use GSEA when you care about a coordinated shift across many genes,",
              "including genes that may not pass a strict individual cutoff."
            )
          )
        ),
        list(
          title = "Enrichment",
          paragraphs = c(
            paste(
              "Enrichment focuses on the genes already classified as TE Up or TE",
              "Down. It asks whether those selected genes contain more members of",
              "a pathway than expected by background chance."
            ),
            paste(
              "Use Enrichment when you want a direct summary of pathway terms among",
              "the TE Up and TE Down gene sets."
            )
          ),
          lists = list(
            list(
              label = "GSEA vs Enrichment",
              items = c(
                "GSEA uses the full ranked gene list.",
                "Enrichment uses selected TE Up and TE Down gene groups.",
                "Both can be useful, but they answer different questions."
              )
            )
          )
        )
      )
    ),
    list(
      id = "network-signalp",
      label = "Network & SignalP",
      summary = paste(
        "Use downstream pages to inspect biological context after core TE",
        "results are available."
      ),
      blocks = list(
        list(
          title = "Network",
          paragraphs = c(
            paste(
              "Network views are useful when you want to inspect relationships",
              "among genes or proteins after TE-changing genes have been identified.",
              "They should be interpreted together with the TE result rather than",
              "as a standalone input check."
            )
          )
        ),
        list(
          title = "SignalP",
          paragraphs = c(
            paste(
              "SignalP helps identify proteins with signal peptide features. Use it",
              "when your biological question involves secreted proteins or proteins",
              "entering the secretory pathway."
            )
          )
        )
      )
    ),
    list(
      id = "codon",
      label = "Codon",
      summary = paste(
        "Ask whether codon usage, codon bias, or consecutive selected-codon",
        "patterns are associated with TE behavior."
      ),
      blocks = list(
        list(
          title = "Input and Usage",
          paragraphs = c(
            paste(
              "Input and Usage prepares the genes that have usable CDS and TE",
              "information. It also lets you choose the codon or codons that will",
              "be used in downstream selected-codon analyses."
            ),
            paste(
              "If another codon page asks you to select at least one codon, return",
              "to Input and Usage first. The selected codon controls which genes",
              "and codon-load summaries are used by the later codon views."
            )
          )
        ),
        list(
          title = "Codon Bias",
          paragraphs = c(
            paste(
              "Codon Bias compares TE groups with broader codon-use metrics such as",
              "CBI and tAI. In simple terms, it asks whether TE Up, Non, and TE Down",
              "genes differ in how strongly they prefer certain synonymous codons or",
              "how well their codon use matches tRNA adaptation."
            )
          )
        ),
        list(
          title = "TE Shift, Pattern Views, and Codon Runs",
          paragraphs = c(
            paste(
              "TE Shift views ask whether the selected codon appears more often in",
              "TE Up genes, TE Down genes, or genes without a TE change. Pattern",
              "Views show whether codons share similar usage patterns across genes."
            ),
            paste(
              "Codon Runs count whether selected codons appear as single, double, or",
              "triple consecutive runs in CDS sequences. These views are useful when",
              "you want to inspect repeated selected-codon patterns rather than only",
              "overall codon frequency."
            )
          ),
          lists = list(
            list(
              label = "Important interpretation notes",
              items = c(
                "A codon association does not prove causality by itself.",
                "Some dense plots show fewer points than the number of genes measured, but statistics can still use the full gene set.",
                "When a chart says TE Up or TE Down, it refers to the TE group of the gene, not to the codon itself being up or down."
              )
            )
          )
        )
      )
    ),
    list(
      id = "export",
      label = "Export & Results",
      summary = paste(
        "Use exports for figures and tables, and check the active result view",
        "before downloading."
      ),
      blocks = list(
        list(
          title = "Figure and data export",
          paragraphs = c(
            paste(
              "Export panels appear after a result is available. Figure export is",
              "for the current chart. Data export is for the table or values behind",
              "the current result view."
            ),
            paste(
              "Before exporting, check that the selected result tab or codon subview",
              "is the one you want. Some modules contain several result views, and",
              "the export should follow the active view."
            )
          )
        ),
        list(
          title = "Why old results may disappear",
          paragraphs = c(
            paste(
              "If an upstream analysis is rerun, downstream results may disappear",
              "until they are rerun. This is intentional: old results should not",
              "remain visible when they no longer match the current input data."
            )
          )
        )
      )
    ),
    list(
      id = "faq",
      label = "FAQ / Troubleshooting",
      summary = paste(
        "Resolve common workflow and interpretation issues before assuming the",
        "analysis result is wrong."
      ),
      blocks = list(
        list(
          title = "Why is a Run button disabled",
          paragraphs = c(
            paste(
              "A downstream Run button is usually disabled because an earlier step",
              "has not been completed. Check the order: Load Data, Data Preprocess,",
              "Translation Efficiency, then the downstream page."
            )
          )
        ),
        list(
          title = "Why are fewer points displayed than genes measured",
          paragraphs = c(
            paste(
              "Some scatter plots contain too many genes to display clearly. In that",
              "case, the plot may show a fixed display subset while the correlation",
              "or statistical summary still uses all available genes."
            )
          )
        ),
        list(
          title = "How should I treat a significant p-value",
          paragraphs = c(
            paste(
              "A small p-value means the observed difference is unlikely under the",
              "test assumption, but it does not automatically mean the effect is",
              "large or causal. Always read it together with the effect size, the",
              "plot shape, and the biological context."
            )
          )
        )
      )
    )
  )
}

help_highlight_cards <- function() {
  list(
    list(
      title = "From Counts to TE",
      copy = paste(
        "Load paired RNA-seq and Ribo-seq counts, preprocess them, then identify",
        "genes whose translation efficiency changes between conditions."
      )
    ),
    list(
      title = "Interpret the Result",
      copy = paste(
        "Use PCA, clustering, GSEA, enrichment, network, SignalP, and codon pages",
        "to understand the biological meaning behind TE Up and TE Down genes."
      )
    ),
    list(
      title = "Check Before Export",
      copy = paste(
        "Confirm the active chart or result view before downloading figures or",
        "tables, especially in modules with multiple subviews."
      )
    )
  )
}

help_list_card_ui <- function(list_definition) {
  div(
    class = "help-list-card",
    tags$strong(class = "help-list-card__title", list_definition$label),
    tags$ul(
      class = "help-list-card__list",
      lapply(list_definition$items, function(item) tags$li(item))
    )
  )
}

help_block_ui <- function(block) {
  div(
    class = "help-panel help-panel--section",
    tags$h3(class = "help-section-card__title", block$title),
    tagList(lapply(block$paragraphs, function(paragraph) tags$p(class = "help-section-card__copy", paragraph))),
    if (length(block$lists)) {
      div(
        class = "help-list-grid",
        lapply(block$lists, help_list_card_ui)
      )
    }
  )
}

help_topic_ui <- function(ns, section) {
  tags$section(
    id = ns(section$id),
    class = "help-topic",
    div(
      class = "help-topic__head",
      div(class = "help-topic__eyebrow", section$label),
      tags$h2(class = "help-topic__title", section$label),
      tags$p(class = "help-topic__summary", section$summary)
    ),
    div(
      class = "help-topic__stack",
      lapply(section$blocks, help_block_ui)
    )
  )
}

mod_help_ui <- function(id) {
  ns <- NS(id)
  sections <- help_sections()
  highlight_cards <- help_highlight_cards()

  div(
    class = "page-shell help-shell",
    div(
      class = "help-workspace",
      tags$aside(
        class = "help-sidebar",
        div(
          class = "help-panel help-panel--sidebar",
          div(class = "help-panel__eyebrow", "Help Navigation"),
          tags$h2(class = "help-panel__title", "Help"),
          tags$p(
            class = "help-panel__copy",
            paste(
              "Use this page as a practical guide for the RiboTE analysis workflow,",
              "from paired count loading to downstream biological interpretation."
            )
          ),
          tags$nav(
            class = "help-nav",
            lapply(sections, function(section) {
              tags$a(
                class = "help-nav__link",
                href = paste0("#", ns(section$id)),
                tags$span(class = "help-nav__label", section$label),
                tags$span(class = "help-nav__summary", section$summary)
              )
            })
          )
        )
      ),
      tags$section(
        class = "help-main",
        div(
          class = "help-panel help-panel--hero",
          div(class = "help-panel__eyebrow", "RiboTE Web Guide"),
          tags$h1(class = "help-hero__title", "RiboTE Help"),
          tags$p(
            class = "help-hero__copy",
            paste(
              "This Help page explains how to use the current RiboTE web workflow.",
              "It focuses on what each analysis page means, when to run it, and how",
              "to interpret the results, rather than describing technical details",
              "behind the page."
            )
          ),
          div(
            class = "help-highlight-grid",
            lapply(highlight_cards, function(card) {
              div(
                class = "help-highlight-card",
                tags$h3(class = "help-highlight-card__title", card$title),
                tags$p(class = "help-highlight-card__copy", card$copy)
              )
            })
          )
        ),
        lapply(sections, function(section) help_topic_ui(ns, section))
      )
    )
  )
}
