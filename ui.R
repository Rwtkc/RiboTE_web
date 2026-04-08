# nolint start: line_length_linter

library(shiny)
library(shinyjs)
library(DT)

shinyUI(navbarPage(
  id = "tabs",
  shinyjs::useShinyjs(),
  tags$link(rel = "stylesheet", href = "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.10.0/font/bootstrap-icons.css"),
  tags$script(
    HTML(
      "$(document).on('shiny:connected', function(event) {
          $('ul.navbar-nav').find('li:lt(28)').remove();
        });
        $(document).ready(function(){
          $('#tip1').hover(
            function() {
              $('<span id=\"tipText\">Enter multiple RNA-seq sample names in the gene count matrix, separated by \",\" for the same group and \"#\" for different groups, e.g. RNA.WT1,RNA.WT2#RNA.KO1,RNA.KO2</span>').appendTo('body');
              $('#tipText').css({
                top: event.clientY - 20,
                left: event.clientX + 15
              });
            },
            function() {
              $('#tipText').remove();
            }
          );
          $('#tip2').hover(
            function(event) {
              $('<span id=\"tipText2\">Enter multiple Ribo-seq sample names in the gene count matrix, separated by \",\" for the same group and \"#\" for different groups, e.g. RPF.WT1,RPF.WT2#RPF.KO1,RPF.KO2</span>').appendTo('body');
              $('#tipText2').css({
                position: 'fixed',
                top: event.clientY - 20,
                left: event.clientX + 15
              });
            },
            function() {
              $('#tipText2').remove();
            }
          );
          $('#tip3').hover(
            function(event) {
              $('<span id=\"tipText3\">To view the data, you can use Shift+mouse wheel to swipe left and right.</span>').appendTo('body');
              $('#tipText3').css({
                position: 'fixed',
                top: event.clientY - 20,
                left: event.clientX + 15
              });
            },
            function() {
              $('#tipText3').remove();
            }
          );
          $('#tip4').hover(
            function(event) {
              $('<span id=\"tipText4\">To select a specific codon, simply click on it. This module offers the ability to make multiple selections and to search for specific items. Once a codon has been selected, it can be removed from the list by pressing the Delete or Backspace key.</span>').appendTo('body');
              $('#tipText4').css({
                position: 'fixed',
                top: event.clientY - 20,
                left: event.clientX + 15
              });
            },
            function() {
              $('#tipText4').remove();
            }
          );
        });"
    )
  ),
  tags$style(HTML("
      #tipText, #tipText2, #tipText3{
        width: auto;
        position: absolute;
        background-color: #fff;
        border: 1px solid #ccc;
        padding: 5px;
        border-radius: 3px;
        box-shadow: 0 2px 5px rgba(0,0,0,0.2);
        z-index: 9999;
      }
      #tipText4 {
        width: auto;
        max-width: 400px;
        position: absolute;
        background-color: #fff;
        border: 1px solid #ccc;
        padding: 5px;
        border-radius: 3px;
        box-shadow: 0 2px 5px rgba(0,0,0,0.2);
        z-index: 9999;
        text-align: justify;
        overflow-wrap: break-word;
        font-size: 13.8px
      }
      .navbar .navbar-header .navbar-brand {
        font-weight: bold !important;
        display: inline-block;
        float: none;
        background: -webkit-linear-gradient(#4568DC, #b06ab3);
        -webkit-background-clip: text;
        -webkit-text-fill-color: transparent;
        background-clip: text;
        color: transparent;
      }
      html {
        min-width:1520px;
      }
      #plotBtn1, #plotBtn2{
        margin-top:20px;
      }
      .fileInput {
        margin-bottom: -20px;
      }

      .tab-pane[data-value='3'] .col-sm-4:nth-child(1){
        width:30%;
      }
      .tab-pane[data-value='3'] .col-sm-8 .tabbable .nav-tabs {
        width: 106%;
      }
      .tab-pane[data-value='1'] .col-sm-4:nth-child(1){
        width:30%;
      }
      .tab-pane[data-value='10'] .col-sm-4:nth-child(1){
        width:30%;
      }
      #demoData {
        background-color: #1772f6;
        color: white;
        border: none;
        cursor: pointer;
      }
      #demoData:hover {
        background-color: #0056b3;
      }
      #TEvolcano, #volcanoAB, #TEscatter, #inputScatter, #TErpkmFcScatter, #pcaPlot, #SignalPPlot, #barPlot, #qcPlot1, #qcPlot2, #cbiUpDownPlot, #taiUpDownPlot, #cbiInputTE1Plot,
      #cbiInputTE2Plot, #logFCPercPlot, #logFCPer1kPlot, #ZscorePer1kPlot, #codonFrequency, #codonHeatmap, #codonRatioUpDown2, #codonRatioUpDown1, #dendrogramPlot, #distributionUpDown1,
      #distributionUpDown2, #codonZscoreUpDown, #codonPer1kFcAndPvaluePlot, #per1kZscoreUpDownPlot1, #per1kZscoreUpDownPlot2, #per1kZscoreUpDownPlot3, #enrichedPlot1, #enrichedPlot2,
      #enrichedPlot3{
        margin: 0 auto;
        margin-top: 10px;
      }
      #rpfInputScatter{
        margin: 0 21% 0 26%;
        margin-top: 10px;;
      }
      .loader-overlay, .loader-overlay1, .loader-overlay2, .loader-overlay3, .loader-overlay4, .loader-overlay5, .loader-overlay6, .loader-overlay7, .loader-overlay8, .loader-overlay9,
      .loader-overlay10, .loader-overlay11{
        z-index: 9999;
        position: fixed;
        top: 50%;
        left: 65%;
        transform: translate(-50%, -50%);
        display: flex;
        flex-direction: column;
        justify-content: center;
        align-items: center;
        background-color: rgba(255, 255, 255, 0);
        pointer-events: none;
      }
      .loader, .loader1, .loader2, .loader3, .loader4, .loader5, .loader6, .loader7, .loader8, .loader9, .loader10, .loader11{
        width: 60px;
        height: 25px;
        border: 2px solid;
        box-sizing: border-box;
        border-radius: 50%;
        display: grid;
        animation: l2 2s infinite linear;
      }
      .loader:before,
      .loader:after,
       .loader1:before,
      .loader1:after,
      .loader2:before,
      .loader2:after,
      .loader3:before,
      .loader3:after,
      .loader4:before,
      .loader4:after,
      .loader5:before,
      .loader5:after,
      .loader6:before,
      .loader6:after,
      .loader7:before,
      .loader7:after,
      .loader8:before,
      .loader8:after,
      .loader9:before,
      .loader9:after,
      .loader10:before,
      .loader10:after,
      .loader11:before,
      .loader11:after{
        content: '';
        grid-area: 1/1;
        border: inherit;
        border-radius: 50%;
        animation: inherit;
        animation-duration: 3s;
      }
      .loader:after,
      .loader:after2{
        --s: -1;
      }
      @keyframes l2 {
        100% { transform: rotate(calc(var(--s, 1) * 1turn)); }
      }
      .loading-text {
        margin-top: 20px;
        font-size: 16px;
      }

      #userGeneMatrixStat .table{
        margin: 0 auto;
      }
      #heatmapPlot {
        margin-top: 16px;
      }
      #refresh {
        margin-bottom: 10px;
      }
      #gseaTable, #keggImage, #enrichmentTable{
        margin-top: 10px;
      }
      #overlay, #overlay2{
        position: fixed;
        width: 100%;
        height: 100%;
        top: 0;
        left: 0;
        right: 0;
        bottom: 0;
        background-color: rgba(0,0,0,0.1);
        z-index: 9999;
        display: none;
      }
      #goButton {
        color: white;
        background-color: #19c37d;
        cursor: pointer;
        float: right;
      }
      #goButton:hover {
        background-color: green;
      }
      .shiny-notification {
        position: fixed;
        top: 22%;
        right: 0%;
        background-color: #f2dede;
        color: #a94442;
        border: 1px solid #ebccd1;
        padding: 10px 10px 10px 10px;
      }
      .shiny-notification-close {
        display: none;
      }
      .tab-pane[data-value='9'] .codonsSelectDiv .selectize-input .item{
        width: 43px;
        text-align: center;
      }
      .tab-pane[data-value='9'] .tabbable{
        width:109%;
      }
      [id^='ExportBtn'] {
        margin-top: 5px;
        margin-bottom: 5px;
      }
      ul.custom-bullets-RiboTE {
        list-style-type: none;
        padding-left: 0;
      }

      ul.custom-bullets-RiboTE li {
        position: relative;
        padding-left: 1.2em;
        margin-bottom: 0.5em;
      }

      ul.custom-bullets-RiboTE li::before {
        content: '◆';
        position: absolute;
        left: 0;
        top: 0;
        color: black;
        font-size: 1.5em;
        line-height: 1;
      }
      .ribote-home-hero {
        margin-top: 12px;
        margin-bottom: 18px;
        padding: 22px 24px 18px 24px;
        border-radius: 28px;
        background:
          radial-gradient(circle at top right, rgba(132, 197, 194, 0.16), transparent 28%),
          radial-gradient(circle at top left, rgba(87, 142, 200, 0.10), transparent 24%),
          linear-gradient(180deg, #fbfdfd 0%, #f4f8f7 100%);
        border: 1px solid #dfe9e7;
        box-shadow: 0 18px 40px rgba(56, 79, 92, 0.08);
      }
      .ribote-home-hero__eyebrow {
        display: inline-flex;
        align-items: center;
        padding: 8px 16px;
        border-radius: 999px;
        background: rgba(220, 237, 231, 0.85);
        color: #1b6f77;
        font-size: 14px;
        font-weight: 800;
        letter-spacing: 0.18em;
        text-transform: uppercase;
      }
      .ribote-home-hero__title {
        margin-top: 18px;
        color: #0f1f1d;
        font-size: 60px;
        font-weight: 800;
        line-height: 1.04;
        letter-spacing: -0.03em;
      }
      .ribote-home-hero__copy {
        margin-top: 16px;
        max-width: 1040px;
        color: #607378;
        font-size: 18px;
        line-height: 1.72;
      }
      .ribote-home-hero__chips {
        display: flex;
        flex-wrap: wrap;
        gap: 14px;
        margin-top: 22px;
      }
      .ribote-home-hero__chip {
        display: inline-flex;
        align-items: center;
        min-height: 48px;
        padding: 0 18px;
        border-radius: 999px;
        border: 1px solid #cfe1dd;
        background: rgba(239, 247, 244, 0.95);
        color: #1b6f77;
        font-size: 14px;
        font-weight: 700;
      }
      [class^='notification_'] {;
          bottom: 30px;
          position: fixed;
          left: 50%;
          transform: translateX(-50%);
          padding: 15px;
          background: #f2dede;
          color: #a94442;
          box-shadow: 0 7px 6px rgba(0, 0, 0, 0.2), 2px 4px 6px rgba(0, 0, 0, 0.5);
          border-radius: 8px;
          z-index:9999;
      }
      .notification_data_preprocess {
          width: 238px;
      }
      .notification_translation_efficiency {
          width: 280px;
      }
      .notification_te_front {
        width: 249px;
      }
      .notification_codon_data {
        width: 320px;
      }
      .notification_codon_data_front {
        width: 320px;
      }
      .notification_cbi_tai {
        width: 360px;
      }
      .notification_codon_specific {
        width: 380px;
      }
      .notification_codon_perc_per1k{
        width: 445px;
      }
      .notification_codon_ratio {
        width: 422px;
      }
      [class^='title_'] {
        font-size: 16px;
        margin-bottom: 8px;
        font-weight: bold;
      }

      [class^='message_'] {
        font-size: 14px;
      }
      :root {
        --color-gray: #666;
        --color-black: #000;
        --stripe-height: 7px;
        --btn-color: var(--color-gray);
        --btn-background: #fff;
        --btn-color-hover: #fff;
        --btn-background-hover: var(--color-gray);
        --border-color: var(--color-gray);
        --border-color-hover: var(--color-black);
      }

      @keyframes stripe-slide {
        0% {
          background-position: 0% 0;
        }

        100% {
          background-position: 100% 0;
        }
      }
      [class^='RiboTE-working-btn-'] {
        cursor: pointer;
        display: block;
        background-color:#fff;
        text-decoration: none;
        padding: 16px 36px 22px;
        border: 2px solid var(--border-color-hover);
        border-radius: 6px;
        margin-bottom: 16px;
        transition: all .5s ease;
        position: relative;
        overflow: hidden;
        z-index:9999;
      }

      .RiboTE-working-btn--stripe::after {
        content: '';
        display: block;
        height: var(--stripe-height);
        width: 100%;
        background-image: repeating-linear-gradient(45deg,
                var(--color-gray),
                var(--color-gray) 1px,
                transparent 2px,
                transparent 5px);
        backface-visibility: hidden;
        border-top: 1px solid var(--border-color-hover);
        position: absolute;
        left: 0;
        bottom: 0;
        background-size: var(--stripe-height) var(--stripe-height);
        animation: stripe-slide 12s infinite linear forwards;
      }

      .fixed-bottom-left {
        position: fixed;
        left: 0;
        bottom: 0;
        margin: 20px;
      }
      .fixed-bottom-left-network {
        position: fixed;
        left: 0;
        bottom: 0;
        margin: 20px;
      }
      .fixed-bottom-left-enrichment {
        position: fixed;
        left: 0;
        bottom: 0;
        margin: 20px;
      }

      .RiboTE-working-hidden {
        display: none;
      }

      .RiboTE-sakana-hidden {
        display: none;
      }

      #subHeatmap {
        margin-left: 5.5em;
      }
    ")),
  title = "RiboTE",
  tags$div(class = "RiboTE-working-btn-data-preprocess RiboTE-working-btn--stripe fixed-bottom-left RiboTE-working-hidden", "Data preprocess working..."),
  tags$div(class = "RiboTE-working-btn-translation-efficiency RiboTE-working-btn--stripe fixed-bottom-left RiboTE-working-hidden", "Translation efficiency working..."),
  tags$div(class = "RiboTE-working-btn-pca RiboTE-working-btn--stripe fixed-bottom-left RiboTE-working-hidden", "PCA working..."),
  tags$div(class = "RiboTE-working-btn-clustering RiboTE-working-btn--stripe fixed-bottom-left RiboTE-working-hidden", "Clustering working..."),
  tags$div(class = "RiboTE-working-btn-gsea RiboTE-working-btn--stripe fixed-bottom-left RiboTE-working-hidden", "GSEA working..."),
  tags$div(class = "RiboTE-working-btn-enrichment RiboTE-working-btn--stripe fixed-bottom-left-enrichment RiboTE-working-hidden", "Enrichment working..."),
  tags$div(class = "RiboTE-working-btn-network RiboTE-working-btn--stripe fixed-bottom-left-network RiboTE-working-hidden", "Network working..."),
  tags$div(class = "RiboTE-working-btn-signalP RiboTE-working-btn--stripe fixed-bottom-left RiboTE-working-hidden", "SignalP working..."),
  tags$div(class = "RiboTE-working-btn-codon RiboTE-working-btn--stripe fixed-bottom-left RiboTE-working-hidden", "Codon working..."),
  tags$div(class = "RiboTE-working-btn-codon-data RiboTE-working-btn--stripe fixed-bottom-left RiboTE-working-hidden", "Codon preprocess data working..."),
  tags$div(class = "RiboTE-working-btn-codon-specific RiboTE-working-btn--stripe fixed-bottom-left RiboTE-working-hidden", "Codon specific working..."),
  tags$div(class = "RiboTE-working-btn-codon-input RiboTE-working-btn--stripe fixed-bottom-left RiboTE-working-hidden", "Codon input working..."),
  tags$div(class = "RiboTE-working-btn-codon-cbi-tai RiboTE-working-btn--stripe fixed-bottom-left RiboTE-working-hidden", "Codon cbi tai working..."),
  tags$div(class = "RiboTE-working-btn-codon-cbi RiboTE-working-btn--stripe fixed-bottom-left RiboTE-working-hidden", "Codon cbi working..."),
  tags$div(class = "RiboTE-working-btn-codon-perc-per1k RiboTE-working-btn--stripe fixed-bottom-left RiboTE-working-hidden", "Codon perc per1k working..."),
  tags$div(class = "RiboTE-working-btn-codon-ratio RiboTE-working-btn--stripe fixed-bottom-left RiboTE-working-hidden", "Codon ratio working..."),
  tags$div(class = "RiboTE-working-btn-codon-frequency RiboTE-working-btn--stripe fixed-bottom-left RiboTE-working-hidden", "Codon frequency working..."),
  tags$div(class = "RiboTE-working-btn-codon-dendrogram RiboTE-working-btn--stripe fixed-bottom-left RiboTE-working-hidden", "Codon dendrogram working..."),
  tags$div(class = "RiboTE-working-btn-codon-distribution RiboTE-working-btn--stripe fixed-bottom-left RiboTE-working-hidden", "Codon distribution working..."),
  tags$div(class = "RiboTE-working-btn-codon-zscore RiboTE-working-btn--stripe fixed-bottom-left RiboTE-working-hidden", "Codon zscore working..."),
  tags$div(class = "RiboTE-working-btn-codon-Per1kFcAndPvalue RiboTE-working-btn--stripe fixed-bottom-left RiboTE-working-hidden", "Codon Per1kFcAndPvalue working..."),
  tags$div(class = "RiboTE-working-btn-codon-per1k-zscore RiboTE-working-btn--stripe fixed-bottom-left RiboTE-working-hidden", "Codon per1k zscore working..."),
  tags$div(class = "RiboTE-working-btn-codon-enriched RiboTE-working-btn--stripe fixed-bottom-left RiboTE-working-hidden", "Codon enriched working..."),
  tags$div(class = "RiboTE-working-btn-codon-heatmap RiboTE-working-btn--stripe fixed-bottom-left RiboTE-working-hidden", "Codon heatmap working..."),
  tabPanel("Load data",
    value = 1,
    div(class = "RiboTE-sakana .RiboTE-sakana-hidden", includeHTML("www/xbj.html")),
    div(id = "overlay"),
    shinyjs::hidden(div(
      id = "loader-overlay8", class = "loader-overlay8",
      div(class = "loader8"),
      p(class = "loading-text", "Loading")
    )),
    sidebarLayout(
      sidebarPanel(
        selectizeInput("selectOrg",
          label = "1.Select Species",
          choices = c("", "Waiting."),
          multiple = FALSE,
          options = list(
            maxItems = 1,
            placeholder = "Choose your species",
            onInitialize = I('function() { this.setValue(""); }')
          )
        ),
        div(fileInput("geneMatrix", "2.Upload Gene Matrix",
          accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv",
            "text/tab-separated-values,text/plain",
            ".txt",
            "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
            ".xlsx"
          ),
        ), class = "fileInput"),
        textInput("inputnames",
          label = HTML("3.RNA-seq samples <i id='tip1' class='bi bi-chat' style='font-size: 2rem; color: cornflowerblue;'></i>"),
          placeholder = "Enter your inputnames..."
        ),
        textInput("rpfnames",
          label = HTML("4.Ribo-seq samples <i id='tip2' class='bi bi-chat' style='font-size: 2rem; color: cornflowerblue;'></i>"),
          placeholder = "Enter your rpfnames..."
        ),
        actionButton("demoData", "Load demo data"),
        actionButton("goButton", "GO!-->")
      ),
      mainPanel(
        div(
          class = "ribote-home-hero",
          div(class = "ribote-home-hero__eyebrow", "TRANSLATION EFFICIENCY ANALYSIS WORKSPACE"),
          div(class = "ribote-home-hero__title", "RiboTE"),
          p(
            class = "ribote-home-hero__copy",
            "RiboTE is an online analysis workspace designed for comparative translation efficiency studies based on matched RNA-seq and Ribo-seq count matrices."
          ),
          p(
            class = "ribote-home-hero__copy",
            "The current workflow links data preprocessing, differential TE estimation, PCA and heatmap visualization, enrichment and network analysis, and codon-level statistics into a single interface for biological interpretation."
          ),
          div(
            class = "ribote-home-hero__chips",
            span(class = "ribote-home-hero__chip", "Matched RNA/Ribo inputs"),
            span(class = "ribote-home-hero__chip", "Differential TE analysis"),
            span(class = "ribote-home-hero__chip", "Codon and pathway interpretation")
          )
        ),
        div(id = "RiboTE_CAROUSEL", includeHTML("www/carousel.html")),
        DT::DTOutput("userGeneMatrixStat")
      )
    )
  ),
  tabPanel("Data preprocess",
    value = 2,
    tags$div(
      class = "notification_data_preprocess",
      tags$div(class = "title_data_preprocess", "Server Tips"),
      tags$div(class = "message_data_preprocess", "Load data needs to be completed before this can be done.")
    ),
    div(id = "overlay2"),
    shinyjs::hidden(div(
      id = "loader-overlay9", class = "loader-overlay9",
      div(class = "loader9"),
      p(class = "loading-text", "Loading")
    )),
    sidebarLayout(
      sidebarPanel(
        selectInput("NAestimate", "1.Missing value estimation",
          choices = c("Replace Zero" = "treatAsZero", "Median" = "geneMedian")
        ),
        p("Keep genes with minimal counts per million (CPM) in at
                  least n libraries:"),
        fluidRow(
          column(
            width = 6,
            numericInput(
              inputId = "minCounts",
              label = "Min. CPM",
              value = 0.5
            )
          ),
          column(
            width = 6,
            numericInput(
              inputId = "nMinSamplesCount",
              label = "n libraries",
              value = 1
            )
          )
        ),
        fluidRow(
          conditionalPanel(
            condition = "input.preprocess_tabs == 'Data'",
            column(12, downloadButton("ExportBtn1", "Export Data(.csv)", class = "btn-primary"))
          ),
          conditionalPanel(
            condition = "input.preprocess_tabs == 'BarPlot'",
            column(12, downloadButton("ExportBtn2", "Export Graph Data", class = "btn-primary")),
            column(12, downloadButton("ExportBtn3", "Export PDF", class = "btn-primary")),
            column(12, downloadButton("ExportBtn4", "Export PNG", class = "btn-primary")),
          ),
          conditionalPanel(
            condition = "input.preprocess_tabs == 'QC'",
            column(12, downloadButton("ExportBtn5", "Export Graph Data(.zip)", class = "btn-primary")),
            column(12, downloadButton("ExportBtn6", "Export PDF(.zip)", class = "btn-primary")),
            column(12, downloadButton("ExportBtn7", "Export PNG(.zip)", class = "btn-primary")),
          ),
        ),
      ),
      mainPanel(
        tabsetPanel(
          id = "preprocess_tabs",
          tabPanel("Data", DT::DTOutput("preData")),
          tabPanel("BarPlot", plotOutput("barPlot", width = 800, height = 480)),
          tabPanel("QC", plotOutput("qcPlot1", width = 960, height = 520), plotOutput("qcPlot2", width = 800, height = 480)),
        )
      )
    )
  ),
  tabPanel("Translation efficiency",
    value = 3,
    tags$div(
      class = "notification_translation_efficiency",
      tags$div(class = "title_translation_efficiency", "Server Tips"),
      tags$div(class = "message_translation_efficiency", "Data preprocess needs to be completed before this can be done.")
    ),
    sidebarLayout(
      sidebarPanel(
        selectInput("TEtools", "1.Choice of tools to calculate translation efficiency",
          choices = c("Riborex" = "riborex", "Xtail" = "xtail")
        ),
        numericInput("fvalue", "2.fvalue", value = 1.5, min = 0),
        numericInput("pCutoff", "3.pCutoff", value = 0.05, min = 0, step = 0.01),
        selectInput("pCutoffType", "4.pCutoffType:",
          choices = list("Fdr" = "fdr", "RawPvalue" = "rawPvalue"),
          selected = "Fdr"
        ),
        fluidRow(
          conditionalPanel(
            condition = "input.te_tabs == 'Data'",
            column(12, downloadButton("ExportBtn8", "Export Data(.csv)", class = "btn-primary")),
            column(12, downloadButton("ExportBtn8_geneid_standard", "Export Data(.csv Geneid Standard)", class = "btn-primary"))
          )
        ),
        fluidRow(
          conditionalPanel(
            condition = "input.te_tabs == 'Volcano plot'",
            column(12, div(
              style = "display:flex;justify-content:space-between;align-items: center;",
              div(
                style = "width:80%",
                textInput("volcanoGeneFinder", "5.Enter target gene for volcano plot", placeholder = "Enter your target gene...")
              ),
              div(
                div(style = "margin-top: 10px;"),
                actionButton("volcanoGeneFinderBtn", "submit")
              )
            )),
            column(12, actionButton("markerClearVolcano", "Clear Filtered Genes", class = "btn-danger")),
            column(12, downloadButton("ExportBtn9", "Export Graph Data(.zip)", class = "btn-primary")),
            column(12, downloadButton("ExportBtn10", "Export PDF(.zip)", class = "btn-primary")),
            column(12, downloadButton("ExportBtn11", "Export PNG(.zip)", class = "btn-primary")),
          ),
          conditionalPanel(
            condition = "input.te_tabs == 'Scatter plot'",
            column(12, div(
              style = "display:flex;justify-content:space-between;align-items: center;",
              div(
                style = "width:80%",
                textInput("scatterGeneFinder", "5.Enter target gene for scatter plot", placeholder = "Enter your target gene...")
              ),
              div(
                div(style = "margin-top: 10px;"),
                actionButton("scatterGeneFinderBtn", "submit")
              )
            )),
            column(12, actionButton("markerClearScatter", "Clear Filtered Genes", class = "btn-danger")),
            column(12, downloadButton("ExportBtn12", "Export Graph Data(.zip)", class = "btn-primary")),
            column(12, downloadButton("ExportBtn13", "Export PDF(.zip)", class = "btn-primary")),
            column(12, downloadButton("ExportBtn14", "Export PNG(.zip)", class = "btn-primary")),
          ),
        ),
        div(style = "text-align: right;", HTML('<i id="tip3" class="bi bi-question-circle" style="font-size: 2rem; color: cornflowerblue;"></i>'))
      ),
      mainPanel(
        shinyjs::hidden(div(
          id = "loader-overlay", class = "loader-overlay",
          div(class = "loader"),
          p(class = "loading-text", "Loading")
        )),
        tabsetPanel(
          id = "te_tabs",
          tabPanel("Data", DT::DTOutput("proTotaldata")),
          tabPanel("Volcano plot", imageOutput("TEvolcano", width = 600, height = 600), imageOutput("volcanoAB", width = 600, height = 600)),
          tabPanel("Scatter plot", imageOutput("TEscatter", width = 600, height = 600), imageOutput("rpfInputScatter", width = 600, height = 600), imageOutput("inputScatter", width = 600, height = 600), imageOutput("TErpkmFcScatter", width = 600, height = 600))
        ),
      )
    )
  ),
  tabPanel("PCA",
    value = 4,
    tags$div(
      class = "notification_te_front",
      tags$div(class = "title_te_front", "Server Tips"),
      tags$div(class = "message_te_front", "Translation efficiency needs to be completed before this can be done.")
    ),
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "pcaData",
          label = "1.Select PCA data type",
          choices = c("INPUT", "RPF", "TE")
        ),
        selectInput(
          inputId = "pcaMethod",
          label = "2.Select PCA Methods",
          choices = c("PCA", "MDS", "T-SNE")
        ),
        fluidRow(
          column(12, downloadButton("ExportBtn15", "Export Graph Data(.csv)", class = "btn-primary")),
          column(12, downloadButton("ExportBtn16", "Export PDF", class = "btn-primary")),
          column(12, downloadButton("ExportBtn17", "Export PNG", class = "btn-primary")),
        )
      ),
      mainPanel(
        shinyjs::hidden(div(
          id = "loader-overlay2", class = "loader-overlay2",
          div(class = "loader2"),
          p(class = "loading-text", "Loading")
        )),
        plotOutput("pcaPlot", width = 800, height = 640),
      )
    )
  ),
  tabPanel("Clustering",
    value = 5,
    tags$div(
      class = "notification_te_front",
      tags$div(class = "title_te_front", "Server Tips"),
      tags$div(class = "message_te_front", "Translation efficiency needs to be completed before this can be done.")
    ),
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "heatmapDataType",
          label = "1.Select heatmap data type",
          choices = c("INPUT", "RPF", "TE")
        ),
        numericInput(
          inputId = "nGenes",
          label = "2.Top Genes",
          min = 10,
          max = 12000,
          value = 2000,
          step = 100
        ),
        selectInput(
          inputId = "distanceCalculation",
          label = "3.Distance calculation",
          choices = c("Pearson", "Euclidean", "Absolute_Pearson")
        ),
        selectInput(
          inputId = "heatFunction",
          label = "4.Linkage",
          choices = c(
            "average", "complete", "single",
            "median", "centroid", "mcquitty"
          ),
        ),
        selectInput(
          inputId = "heatmapColorSeries",
          label = "5.Heatmap Color Series:",
          choices = c(
            "Green-Black-Red",
            "Red-Black-Green",
            "Blue-White-Red",
            "Green-Black-Magenta",
            "Blue-Yellow-Red",
            "Blue-White-Brown",
            "Orange-White-Blue"
          )
        ),
        numericInput(
          inputId = "heatmapCutoff",
          label = "6.Max Z score:",
          value = 3,
          min = 2,
          step = 1
        ),
        selectInput(
          inputId = "subHeatmapDisplayMode",
          label = "7.Subheatmap display mode:",
          choices = c("Select area", "Base on GeneID")
        ),
        conditionalPanel(
          condition = "input.subHeatmapDisplayMode == 'Base on GeneID'",
          textInput(inputId = "enterGeneIDs", label = "8.GeneID:", placeholder = "Enter GeneID...")
        ),
        p(style = "color: red; font-weight: 700; font-size: 13px;", "Should the sub-heatmap be unable to view its details, it is recommended that the user click the button below in order to resolve the issue."),
        actionButton("refresh", "Refresh subHeatmap"),
        fluidRow(
          column(12, p(style = "font-weight: 700; margin-bottom: 0px;", "More options")),
          column(12, checkboxInput("geneCentricity", "Gene Centricity (subtract mean)", value = TRUE)),
        ),
        fluidRow(
          column(12, downloadButton("ExportBtn18", "Export Graph Data(.csv)", class = "btn-primary")),
          column(12, downloadButton("ExportBtn19", "Export PDF", class = "btn-primary")),
          column(12, downloadButton("ExportBtn20", "Export PNG", class = "btn-primary")),
        )
      ),
      mainPanel(
        shinyjs::hidden(div(
          id = "loader-overlay3", class = "loader-overlay3",
          div(class = "loader3"),
          p(class = "loading-text", "Loading")
        )),
        tabsetPanel(
          tabPanel(
            "Heatmap",
            fluidRow(
              column(5, plotOutput("heatmapPlot", width = 500, height = 800, brush = "heatmapBrush"), uiOutput("htClickContent")),
              column(7, plotOutput("subHeatmap", height = "100%", width = "100%", click = "htClick")),
            )
          ),
        ),
      )
    )
  ),
  tabPanel("GSEA",
    value = 6,
    tags$div(
      class = "notification_te_front",
      tags$div(class = "title_te_front", "Server Tips"),
      tags$div(class = "message_te_front", "Translation efficiency needs to be completed before this can be done.")
    ),
    sidebarLayout(
      sidebarPanel(
        fluidRow(
          column(width = 6, numericInput(inputId = "geneSetSizeMin", label = "1.Geneset size: Min.", min = 2, max = 30, value = 5, step = 1)),
          column(width = 6, numericInput(inputId = "geneSetSizeMax", label = "Max.", min = 1000, max = 2000, value = 2000, step = 100))
        ),
        numericInput("fdrCutOffValue", "2.FDR Cutoff Value", value = 1, min = 1e-20, max = 1, step = .05),
        numericInput("pathwayPValueCutOff", "3.Pathway pValue Cutoff", value = 0.1, min = 1e-20, max = 1, step = .05),
        numericInput("nPathwayShow", "4.Number of Pathways to Show", value = 20, min = 5, max = 100, step = 5),
        conditionalPanel(
          condition = "input.gseaTabs == 'KEGG Plot'",
          uiOutput("keggPathwaySelect")
        ),
        checkboxInput("showPathwayId", "Show Pathway ID", value = FALSE),
        checkboxInput("absoluteFold", "Absolute Fold", value = FALSE),
        fluidRow(
          conditionalPanel(
            condition = "input.gseaTabs == 'Significant Pathways Table'",
            column(12, downloadButton("ExportBtn21", "Export Table Data(.csv)", class = "btn-primary")),
          ),
          conditionalPanel(
            condition = "input.gseaTabs == 'KEGG Plot'",
            column(12, downloadButton("ExportBtn23", "Export PNG", class = "btn-primary")),
          )
        ),
      ),
      mainPanel(
        shinyjs::hidden(div(
          id = "loader-overlay5", class = "loader-overlay5",
          div(class = "loader5"),
          p(class = "loading-text", "Loading")
        )),
        tabsetPanel(
          id = "gseaTabs",
          tabPanel("Significant Pathways Table", tableOutput("gseaTable")),
          tabPanel("KEGG Plot", imageOutput("keggImage")),
        )
      )
    )
  ),
  tabPanel("Enrichment",
    value = 7,
    tags$div(
      class = "notification_te_front",
      tags$div(class = "title_te_front", "Server Tips"),
      tags$div(class = "message_te_front", "Translation efficiency needs to be completed before this can be done.")
    ),
    sidebarLayout(
      sidebarPanel(
        uiOutput("GeneOntology"),
        numericInput(
          inputId = "topPathways",
          label = "Top pathways",
          min = 1,
          max = 30,
          value = 10
        ),
        selectInput(
          inputId = "sortBy",
          label = NULL,
          choices = list(
            "Sort by FDR" = "FDR",
            "Sort by fold enriched" = "Fold"
          ),
          selected = "FDR"
        ),
        checkboxInput(
          inputId = "filteredBackground",
          label = "Use filtered genes as background.",
          value = TRUE
        ),
        checkboxInput(
          inputId = "removeRedudant",
          label = "Remove Redudant Gene Sets",
          value = FALSE
        ),
        checkboxInput(
          inputId = "showPathwayId",
          label = "Show pathway IDs",
          value = FALSE
        ),
        fluidRow(
          column(12, downloadButton("ExportBtn24", "Export Table Data(.csv)", class = "btn-primary")),
        )
      ),
      mainPanel(
        shinyjs::hidden(div(
          id = "loader-overlay6", class = "loader-overlay6",
          div(class = "loader6"),
          p(class = "loading-text", "Loading")
        )),
        tabsetPanel(
          id = "enrichmentTabs",
          tabPanel("Enrichment Table", tableOutput("enrichmentTable")),
        )
      )
    )
  ),
  tabPanel("Network",
    value = 8,
    tags$div(
      class = "notification_te_front",
      tags$div(class = "title_te_front", "Server Tips"),
      tags$div(class = "message_te_front", "Translation efficiency needs to be completed before this can be done.")
    ),
    sidebarLayout(
      sidebarPanel(
        numericInput(
          inputId = "edgeThreshold",
          label = "1.Edge Threshold",
          min = 0,
          max = 1,
          value = .4,
          step = .1
        ),
        numericInput(
          inputId = "topGenesNetwork",
          label = "2.Top genes",
          min = 10,
          max = 2000,
          value = 10,
          step = 10
        ),
        numericInput(
          inputId = "nGenesNetwork",
          label = "3.Most variable genes to include",
          min = 10,
          max = 3000,
          value = 1000
        ),
        uiOutput("moduleList"),
        fluidRow(
          column(
            width = 6,
            numericInput(
              inputId = "softPower",
              label = "Soft Threshold",
              min = 1,
              max = 20,
              value = 5
            )
          ),
          column(
            width = 6,
            numericInput(
              inputId = "minModuleSize",
              label = "Min. Module Size",
              min = 10,
              max = 100,
              value = 20
            )
          )
        ),
        fluidRow(
          column(12, downloadButton("ExportBtn25", "Export Graph Data(.rds)", class = "btn-primary")),
          column(12, downloadButton("ExportBtn26", "Export PDF", class = "btn-primary")),
          column(12, downloadButton("ExportBtn27", "Export PNG", class = "btn-primary")),
        ),
      ),
      mainPanel(
        shinyjs::hidden(div(
          id = "loader-overlay7", class = "loader-overlay7",
          div(class = "loader7"),
          p(class = "loading-text", "Loading")
        )),
        plotOutput("networkPlot", width = 900, height = 540),
      )
    )
  ),
  tabPanel("SignalP",
    value = 9,
    tags$div(
      class = "notification_te_front",
      tags$div(class = "title_te_front", "Server Tips"),
      tags$div(class = "message_te_front", "Translation efficiency needs to be completed before this can be done.")
    ),
    sidebarLayout(
      sidebarPanel(
        selectInput("SignalPMethod",
          "1.Select a method:",
          choices = c("All", "SignalP" = "signalP", "TMHMM" = "tmhmm", "Phobius" = "phobius")
        ),
        fluidRow(
          column(12, downloadButton("ExportBtn28", "Export Graph Data(.csv)", class = "btn-primary")),
          column(12, downloadButton("ExportBtn29", "Export PDF", class = "btn-primary")),
          column(12, downloadButton("ExportBtn30", "Export PNG", class = "btn-primary")),
        ),
      ),
      mainPanel(
        shinyjs::hidden(div(
          id = "loader-overlay11", class = "loader-overlay11",
          div(class = "loader11"),
          p(class = "loading-text", "Loading")
        )),
        plotOutput("SignalPPlot", width = 900, height = 540)
      )
    )
  ),
  tabPanel("Codon",
    value = 10,
    sidebarLayout(
      sidebarPanel(
        div(
          class = "codonsSelectDiv",
          selectizeInput("codonSelect",
            label = HTML("1. Select of codons <i id='tip4' class='bi bi-chat' style='font-size: 2rem; color: cornflowerblue;'></i>"),
            choices = c(
              "AAA", "AAC", "AAG", "AAT", "ACA", "ACC", "ACG", "ACT",
              "AGA", "AGC", "AGG", "AGT", "ATA", "ATC", "ATG", "ATT",
              "CAA", "CAC", "CAG", "CAT", "CCA", "CCC", "CCG", "CCT",
              "CGA", "CGC", "CGG", "CGT", "CTA", "CTC", "CTG", "CTT",
              "GAA", "GAC", "GAG", "GAT", "GCA", "GCC", "GCG", "GCT",
              "GGA", "GGC", "GGG", "GGT", "GTA", "GTC", "GTG", "GTT",
              "TAA", "TAC", "TAG", "TAT", "TCA", "TCC", "TCG", "TCT",
              "TGA", "TGC", "TGG", "TGT", "TTA", "TTC", "TTG", "TTT"
            ),
            multiple = TRUE,
            options = list(placeholder = "Select codons...")
          )
        ),
        actionButton("submitButton", "Submit", style = "margin-top: -17px"),
        conditionalPanel(
          condition = "input.codonTabs == 'Data Process'",
          fluidRow(
            column(12, downloadButton("ExportBtn31", "Export Graph Data(.csv)", class = "btn-primary")),
          )
        ),
        conditionalPanel(
          condition = "input.codonTabs == 'codon Specific'",
          fluidRow(
            column(12, downloadButton("ExportBtn33", "Export PDF(.zip)", class = "btn-primary")),
            column(12, downloadButton("ExportBtn34", "Export PNG(.zip)", class = "btn-primary")),
          )
        ),
        conditionalPanel(
          condition = "input.codonTabs == 'Codon Ratio'",
          div(selectInput("codoNatioUpDownSelect", "2.Select Up Down", choices = c("Up", "Down")), style = "margin-top:10px; margin-bottom:-15px"),
          fluidRow(
            column(12, downloadButton("ExportBtn35", "Export PDF(.zip)", class = "btn-primary")),
            column(12, downloadButton("ExportBtn36", "Export PNG(.zip)", class = "btn-primary")),
          )
        ),
        conditionalPanel(
          condition = "input.codonTabs == 'CBI TAI'",
          fluidRow(
            column(12, downloadButton("ExportBtn37", "Export PDF(.zip)", class = "btn-primary")),
            column(12, downloadButton("ExportBtn38", "Export PNG(.zip)", class = "btn-primary")),
          )
        ),
        conditionalPanel(
          condition = "input.codonTabs == 'CBI'",
          div(selectInput("cbiMothodSelect", "2.Select CBI Method", choices = c("input", "TE")), style = "margin-top:10px; margin-bottom:-15px"),
          fluidRow(
            column(12, downloadButton("ExportBtn39", "Export PDF(.zip)", class = "btn-primary")),
            column(12, downloadButton("ExportBtn40", "Export PNG(.zip)", class = "btn-primary")),
          )
        ),
        conditionalPanel(
          condition = "input.codonTabs == 'codon Input'",
          fluidRow(
            column(12, downloadButton("ExportBtn41", "Export PDF(.zip)", class = "btn-primary")),
            column(12, downloadButton("ExportBtn42", "Export PNG(.zip)", class = "btn-primary")),
          )
        ),
        conditionalPanel(
          condition = "input.codonTabs == 'Perc Per1k'",
          div(selectInput("percPer1kUpDownSelect", "2.Select Up Down", choices = c("Up", "Down", "Up and Down")), style = "margin-top:10px; margin-bottom:-15px"),
          fluidRow(
            column(12, downloadButton("ExportBtn43", "Export PDF(.zip)", class = "btn-primary")),
            column(12, downloadButton("ExportBtn44", "Export PNG(.zip)", class = "btn-primary")),
          )
        ),
        conditionalPanel(
          condition = "input.codonTabs == 'Codon Frequency'",
          fluidRow(
            column(12, downloadButton("ExportBtn45", "Export PDF(.zip)", class = "btn-primary")),
            column(12, downloadButton("ExportBtn46", "Export PNG(.zip)", class = "btn-primary")),
          )
        ),
        conditionalPanel(
          condition = "input.codonTabs == 'Dendrogram'",
          div(selectInput("dendrogramHclustMethod", "2.Select Hclust Method", choices = c("ward.D", "single", "complete", "average", "mcquitty", "median", "centroid", "ward.D2")), style = "margin-top:10px; margin-bottom:-10px"),
          div(selectInput("dendrogramDistMethod", "3.Select Dist Method", choices = c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")), style = "margin-bottom:-15px"),
          fluidRow(
            column(12, downloadButton("ExportBtn47", "Export PDF", class = "btn-primary")),
            column(12, downloadButton("ExportBtn48", "Export PNG", class = "btn-primary")),
          )
        ),
        conditionalPanel(
          condition = "input.codonTabs == 'Distribution'",
          div(selectInput("distributionUpDownSelect", "2.Select Up Down", choices = c("Up", "Down")), style = "margin-top:10px; margin-bottom:-10px"),
          fluidRow(
            column(12, downloadButton("ExportBtn49", "Export PDF(.zip)", class = "btn-primary")),
            column(12, downloadButton("ExportBtn50", "Export PNG(.zip)", class = "btn-primary")),
          )
        ),
        conditionalPanel(
          condition = "input.codonTabs == 'codon Zscore'",
          div(selectInput("codonZscoreUpDownSelect", "2.Select Up Down", choices = c("Up", "Down")), style = "margin-top:10px; margin-bottom:-10px"),
          fluidRow(
            column(12, downloadButton("ExportBtn51", "Export PDF(.zip)", class = "btn-primary")),
            column(12, downloadButton("ExportBtn52", "Export PNG(.zip)", class = "btn-primary")),
          )
        ),
        conditionalPanel(
          condition = "input.codonTabs == 'codonPer1kFcAndPvalue'",
          fluidRow(
            column(12, downloadButton("ExportBtn53", "Export PDF(.zip)", class = "btn-primary")),
            column(12, downloadButton("ExportBtn54", "Export PNG(.zip)", class = "btn-primary")),
          )
        ),
        conditionalPanel(
          condition = "input.codonTabs == 'Per1k Zscore'",
          div(selectInput("per1kZscoreUpDownSelect", "2.Select Up Down", choices = c("Up", "Down")), style = "margin-top:10px; margin-bottom:-10px"),
          fluidRow(
            column(12, downloadButton("ExportBtn55", "Export PDF(.zip)", class = "btn-primary")),
            column(12, downloadButton("ExportBtn56", "Export PNG(.zip)", class = "btn-primary")),
          )
        ),
        conditionalPanel(
          condition = "input.codonTabs == 'Enriched'",
          div(selectInput("enrichedDisplaySelect", "2.Select Display Method", choices = c("Obj", "All")), style = "margin-top:10px; margin-bottom:-10px"),
          fluidRow(
            column(12, downloadButton("ExportBtn57", "Export PDF(.zip)", class = "btn-primary")),
            column(12, downloadButton("ExportBtn58", "Export PNG(.zip)", class = "btn-primary")),
          )
        ),
        conditionalPanel(
          condition = "input.codonTabs == 'Heatmap'",
          fluidRow(
            column(12, downloadButton("ExportBtn59", "Export PDF", class = "btn-primary")),
            column(12, downloadButton("ExportBtn60", "Export PNG", class = "btn-primary")),
          )
        ),
      ),
      mainPanel(
        shinyjs::hidden(div(
          id = "loader-overlay10", class = "loader-overlay10",
          div(class = "loader10"),
          p(class = "loading-text", "Loading")
        )),
        tabsetPanel(
          id = "codonTabs",
          tabPanel(
            "Data Process", DT::DTOutput("codonPreData"),
            tags$div(
              class = "notification_codon_data",
              tags$div(class = "title_te_front", "Server Tips"),
              tags$div(class = "message_te_front", "Translation efficiency and codon optimization need to be completed before this can be done.")
            )
          ),
          tabPanel(
            "codon Specific", uiOutput("codonSpecific"),
            tags$div(
              class = "notification_codon_data_front",
              tags$div(class = "title_te_front", "Server Tips"),
              tags$div(class = "message_te_front", "Translation efficiency and the completion of the Data Process module need to be finalized before this can be done.")
            )
          ),
          tabPanel(
            "codon Input", uiOutput("codonPerc"),
            tags$div(
              class = "notification_codon_data_front",
              tags$div(class = "title_te_front", "Server Tips"),
              tags$div(class = "message_te_front", "Translation efficiency and the completion of the Data Process module need to be finalized before this can be done.")
            )
          ),
          tabPanel(
            "CBI TAI", imageOutput("cbiUpDownPlot", width = 700, height = 500), imageOutput("taiUpDownPlot", width = 700, height = 500),
            tags$div(
              class = "notification_codon_data_front",
              tags$div(class = "title_te_front", "Server Tips"),
              tags$div(class = "message_te_front", "Translation efficiency and the completion of the Data Process module need to be finalized before this can be done.")
            )
          ),
          tabPanel(
            "CBI", imageOutput("cbiInputTE1Plot", width = 700, height = 500), imageOutput("cbiInputTE2Plot", width = 700, height = 500),
            tags$div(
              class = "notification_cbi_tai",
              tags$div(class = "title_te_front", "Server Tips"),
              tags$div(class = "message_te_front", "Translation efficiency, the Data Process module, and the CBI TAI module need to be completed before this can be done.")
            )
          ),
          tabPanel(
            "Perc Per1k", imageOutput("logFCPercPlot", width = 700, height = 500),
            imageOutput("logFCPer1kPlot", width = 700, height = 500),
            imageOutput("ZscorePer1kPlot", width = 700, height = 500),
            tags$div(
              class = "notification_codon_specific",
              tags$div(class = "title_te_front", "Server Tips"),
              tags$div(class = "message_te_front", "Translation efficiency, the Data Process module, and the codon-specific module need to be completed before this can be done.")
            )
          ),
          tabPanel(
            "Codon Ratio", imageOutput("codonRatioUpDown1", width = 600, height = 600), imageOutput("codonRatioUpDown2", width = 600, height = 600),
            tags$div(
              class = "notification_codon_specific",
              tags$div(class = "title_te_front", "Server Tips"),
              tags$div(class = "message_te_front", "Translation efficiency, the Data Process module, and the codon-specific module need to be completed before this can be done.")
            )
          ),
          tabPanel(
            "Codon Frequency", imageOutput("codonFrequency", width = 700, height = 600),
            tags$div(
              class = "notification_codon_perc_per1k",
              tags$div(class = "title_te_front", "Server Tips"),
              tags$div(class = "message_te_front", "Translation efficiency, the Data Process module, the codon specific module, and the Perc Per1k module need to be completed before this can be done.")
            )
          ),
          tabPanel(
            "Dendrogram", imageOutput("dendrogramPlot", width = 900, height = 450),
            tags$div(
              class = "notification_codon_specific",
              tags$div(class = "title_te_front", "Server Tips"),
              tags$div(class = "message_te_front", "Translation efficiency, the Data Process module, and the codon specific module need to be completed before this can be done.")
            )
          ),
          tabPanel(
            "Distribution", imageOutput("distributionUpDown1", width = 800, height = 500), imageOutput("distributionUpDown2", width = 800, height = 500),
            tags$div(
              class = "notification_codon_ratio",
              tags$div(class = "title_te_front", "Server Tips"),
              tags$div(class = "message_te_front", "Translation efficiency, the Data Process module, and the codon ratio module need to be completed before this can be done.")
            )
          ),
          tabPanel(
            "codon Zscore", imageOutput("codonZscoreUpDown", width = 480, height = 700),
            tags$div(
              class = "notification_codon_ratio",
              tags$div(class = "title_te_front", "Server Tips"),
              tags$div(class = "message_te_front", "Translation efficiency, the Data Process module, and the codon ratio module need to be completed before this can be done.")
            )
          ),
          tabPanel(
            "codonPer1kFcAndPvalue", imageOutput("codonPer1kFcAndPvaluePlot", width = 600, height = 600),
            tags$div(
              class = "notification_codon_ratio",
              tags$div(class = "title_te_front", "Server Tips"),
              tags$div(class = "message_te_front", "Translation efficiency, the Data Process module, and the codon ratio module need to be completed before this can be done.")
            )
          ),
          tabPanel(
            "Per1k Zscore", imageOutput("per1kZscoreUpDownPlot1", width = 600, height = 600), imageOutput("per1kZscoreUpDownPlot2", width = 600, height = 600), imageOutput("per1kZscoreUpDownPlot3", width = 600, height = 600),
            tags$div(
              class = "notification_codon_ratio",
              tags$div(class = "title_te_front", "Server Tips"),
              tags$div(class = "message_te_front", "Translation efficiency, the Data Process module, and the codon ratio module need to be completed before this can be done.")
            )
          ),
          tabPanel(
            "Enriched", imageOutput("enrichedPlot1", width = 600, height = 600), imageOutput("enrichedPlot2", width = 600, height = 600), imageOutput("enrichedPlot3", width = 600, height = 600),
            tags$div(
              class = "notification_codon_ratio",
              tags$div(class = "title_te_front", "Server Tips"),
              tags$div(class = "message_te_front", "Translation efficiency, the Data Process module, and the codon ratio module need to be completed before this can be done.")
            )
          ),
          tabPanel(
            "Heatmap", imageOutput("codonHeatmap", width = 750, height = 750),
            tags$div(
              class = "notification_codon_specific",
              tags$div(class = "title_te_front", "Server Tips"),
              tags$div(class = "message_te_front", "Translation efficiency, the Data Process module, and the codon specific module need to be completed before this can be done.")
            )
          )
        )
      )
    )
  ),
  tabPanel("About",
    value = 11,
    HTML('<script type="text/javascript" src="//rf.revolvermaps.com/0/0/5.js?i=5gssv1fu1f2&amp;m=0&amp;c=ff0000&amp;cr1=ffffff&amp;rs=100&amp;as=100" async="async"></script>')
  ),
  div(
    class = "mask",
    style = "position: fixed; top: 0; left: 0; width: 100%; height: 100%; background-color: rgba(255, 255, 255, 0.5); z-index: 9999; display: none;"
  )
))

# nolint end
