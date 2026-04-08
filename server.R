library(riborex)
library(data.table)
library(DESeq2)
library(edgeR)
library(ggplot2)
library(ggpubr)
library(cowplot)
library(EnhancedVolcano)
library(jsonlite)
library(Rtsne)
library(circlize)
library(ComplexHeatmap)
library(InteractiveComplexHeatmap)
library(GetoptLong)
library(fgsea)
library(DBI)
library(RSQLite)
library(pathview)
library(KEGGREST)
library(graphics)
library(grDevices)
library(WGCNA)
library(DT)
library(seqinr)
library(parallel)
library(dendextend)
library(ggdendro)
library(dplyr)
library(zip)
library(ggrepel)
library(xtail)
library(gtools)
library(Cairo)


shinyServer(function(input, output,session){
  RiboTE_Org <- read.csv("/public/shiny/RiboTE/TEShinyData/RiboTE_Org.csv", check.names = FALSE)
  updateSelectizeInput(session, "selectOrg", choices = RiboTE_Org$species, selected = NULL)
  options(shiny.http.response.timeout = 120)
  options(shiny.maxRequestSize = 50 * 1024 ^ 20)
  ####R 环境----
  shinyEnv <- new.env()
  reactiveEnv <- reactiveValues()
  reactiveEnv$numberGeneIDs <- 0
  notificationId <- reactiveVal(NULL)
  shinyEnv$loadDataflag <- FALSE
  shinyEnv$transcriptSeqs <- NULL
  shinyEnv$objCodonStat <- NULL
  shinyEnv$submitDone <- FALSE
  shinyEnv$analysisDone2 <- FALSE
  shinyEnv$pathwayImagePath <- NULL
  shinyEnv$signalPImagePath1 <- NULL
  shinyEnv$signalPImagePath2 <- NULL
  shinyEnv$signalPImagePath3 <- NULL
  shinyEnv$te_p1 <- NULL
  shinyEnv$te_p2 <- NULL
  shinyEnv$te_p3 <- NULL
  shinyEnv$te_p4 <- NULL
  shinyEnv$te_p4_1 <- NULL
  shinyEnv$te_p4_2 <- NULL
  shinyEnv$te_p4_3 <- NULL
  shinyEnv$te_p5 <- NULL
  shinyEnv$te_p6 <- NULL
  shinyEnv$te_telist <- NULL
  shinyEnv$normalcount <- NULL
  shinyEnv$WGCNA <- NULL
  cbidone <- reactiveVal(FALSE)
  codonspecific <- reactiveVal(FALSE)
  percper1k <- reactiveVal(FALSE)
  codonratio <- reactiveVal(FALSE)
  datapreprocess <- reactiveVal(FALSE)
  te <- reactiveVal(FALSE)
  wgcnadata <- reactiveVal(FALSE)
  ####用户独立文件目录----
  TEFileuserDir <- tempfile(pattern = "RiboTE_TE_User_", tmpdir = "/public/shiny/RiboTE/TE")
  dir.create(TEFileuserDir,recursive = TRUE)
  session$onSessionEnded(function() {
    unlink(paste0(TEFileuserDir,"/"), recursive = TRUE)
    while(dev.cur() > 1) {
      dev.off()
    }
    gc()
  })
  ####-----
  observeEvent(input$refresh, {
    new_value <- paste(input$enterGeneIDs, " ")
    updateTextInput(session, "enterGeneIDs", value = new_value)
  })
  observeEvent(input$tabs, {
    if (!is.null(notificationId())) {
      removeNotification(notificationId())
      notificationId(NULL)
    }
    if (input$tabs == "3") {
      if (file.exists(file.path(TEFileuserDir, "baseData.json"))) {
        runjs("$('.notification_translation_efficiency').hide();")
      }
    } else if (input$tabs == "4" || input$tabs == "5" || input$tabs == "6" || input$tabs == "7" || input$tabs == "8" || input$tabs == "9") {
      if(file.exists(file.path(TEFileuserDir, "/normalcount.csv"))){
        runjs("$('.notification_te_front').hide();")
      }
    } else if (input$tabs == "2") {
      if(shinyEnv$loadDataflag){
        runjs("$('.notification_data_preprocess').hide();")
      }
    }
  }, ignoreInit = TRUE)
  observeEvent(input$goButton,{
    inputnames <- input$inputnames
    rpfnames <- input$rpfnames
    if(inputnames == "zwc" && rpfnames == "sakana"){
      shinyjs::runjs('$(".RiboTE-sakana").removeClass("RiboTE-sakana-hidden");')
    }
  })
  ####辅助函数----
  #辅助函数1-DESeq2Rex
  DESeq2Rex <- function(rnaCntTable, riboCntTable, rnaCond, riboCond, contrast=NULL, minMeanCount=1) {
    group <- rnaCond
    if (!identical(rownames(rnaCntTable), rownames(riboCntTable)))
      stop ("RNA- and Ribo-seq data must have the same set of genes")
    if (!is.data.frame(rnaCond)) rnaCond <- data.frame(cond = rnaCond)
    if (!is.data.frame(riboCond)) riboCond <- data.frame(cond = riboCond)
    if (!identical(colnames(rnaCond), colnames(riboCond)))
      stop("RNA- and Ribo-seq data must have the same set of conditions")
    if (ncol(rnaCntTable) != nrow(rnaCond))
      stop(paste("RNA-seq count table must have the",
                 "same number of samples as in rnaCond"))
    if (ncol(riboCntTable) != nrow(riboCond))
      stop(paste("Ribo-seq count table must have the",
                 "same number of samples as in riboCond"))
    if (minMeanCount < 1)
      stop("minMeanCount must at least be 1")
    keep.rna <- rownames(rnaCntTable)[rowMeans(rnaCntTable) >= minMeanCount]
    keep.ribo <- rownames(riboCntTable)[rowMeans(riboCntTable) >= minMeanCount]
    keep <- intersect(keep.rna, keep.ribo)
    rnaCntTable <- rnaCntTable[keep,]
    riboCntTable <- riboCntTable[keep,]
    numCond <- ncol(rnaCond)
    numRNASmps <- nrow(rnaCond)
    numRiboSmps <- nrow(riboCond)
    combCntTbl <- cbind(rnaCntTable, riboCntTable)
    combinedCond <- rbind(rnaCond, riboCond)
    combinedCond <- combinedCond[,rep(1:ncol(combinedCond),2)]
    INTERCEPT <- c(rep("CONTROL", numRNASmps), rep("TREATED", numRiboSmps))
    combinedCond <- cbind(combinedCond[1:numCond], INTERCEPT,
                          combinedCond[(numCond+1):ncol(combinedCond)])
    for( i in (numCond+2) : ncol(combinedCond)) {
      combinedCond[1:numRNASmps,i] <- combinedCond[1,i]
    }
    colnames(combinedCond)[(numCond+2):ncol(combinedCond)] <- paste0("EXTRA", seq(numCond))
    extendedConds <- colnames(combinedCond)
    fmla <- as.formula(paste("~", paste(extendedConds, collapse= "+")))
    combCntTbl <- round(combCntTbl, 0)
    combCntTbl <- combCntTbl[ which( apply( cpm(DGEList(counts = combCntTbl)), 1,
                                            function(y) sum(y >= input$minCounts)) >= input$nMinSamplesCount ) , ]
    dds <- DESeqDataSetFromMatrix(countData = combCntTbl,
                                  colData = combinedCond,
                                  design = fmla)
    dds <- DESeq(dds)
    normalcount <- as.data.frame(counts(dds,normalized=TRUE))
    normalcount <- setDT(normalcount,keep.rownames = TRUE)
    setnames(normalcount,"rn","GeneID")
    res <- results(dds)
    res <- setDT(as.data.frame(res),keep.rownames = TRUE)
    setnames(res,"rn","GeneID")
    res[,c("baseMean","lfcSE","stat") := NULL]
    res[is.na(padj),padj:=1]
    res = merge(normalcount, res, by="GeneID",all.y=T)
    res <- res[order(padj),]
    return(res)
  }
  #辅助函数2-ggColorHue
  ggColorHue <- function(n) {
    hues <- seq(15, 375, length = n + 1)
    grDevices::hcl(h = hues, l = 65, c = 100)[1:n]
  }
  #辅助函数3-heightSubHeatmap
  heightSubHeatmap <- reactive({
    numberGeneIDs <- reactiveEnv$numberGeneIDs
    if (is.null(input$heatmapBrush) && input$subHeatmapDisplayMode == "Select area") {
      return(400)
    }
    if (is.null(input$heatmapBrush) && input$subHeatmapDisplayMode == "Base on GeneID") {
      if(is.null(numberGeneIDs) || numberGeneIDs == 0){
        return(400)
      }
      else{
        heigth <- 170 * numberGeneIDs
        return(heigth)
      }
    }
    if (!is.null(input$heatmapBrush) && input$subHeatmapDisplayMode == "Base on GeneID") {
      if(is.null(numberGeneIDs) || numberGeneIDs == 0){
        return(400)
      }
      else{
        heigth <- 170 * numberGeneIDs
        return(heigth)
      }
    }
    lt <- InteractiveComplexHeatmap::getPositionFromBrush(input$heatmapBrush)
    pos1 <- lt[[1]]
    pos2 <- lt[[2]]
    pos <- InteractiveComplexHeatmap::selectArea(
      shinyEnv$heatMain,
      mark = FALSE,
      pos1 = pos1,
      pos2 = pos2,
      verbose = FALSE,
      ht_pos = shinyEnv$heatPosMain
    )
    row_index <- unlist(pos[1, "row_index"])
    height1 <- max(
      400,
      min(
        30000,
        12 * length(row_index)
      )
    )
    return(height1)
  })
  #辅助函数4-subHeatAnn
  subHeatAnn <- function(data, sample_info, select_factors_heatmap) {
    groups <- colnames(data)
    if (!is.null(sample_info) && !is.null(select_factors_heatmap)) {
      if (select_factors_heatmap == "Names") {
        groups <- colnames(data)
      }
    }
    groups_colors <- ggColorHue(length(unique(groups)))
    group_colors <- setNames(groups_colors, unique(groups))
    if (length(unique(groups)) < 10) {
      lgd <- ComplexHeatmap::Legend(
        at = unique(groups),
        legend_gp = grid::gpar(fill = groups_colors),
        nrow = 1
      )
    } else {
      lgd <- NULL
    }
    heat_sub_ann <- ComplexHeatmap::HeatmapAnnotation(
      Group = groups,
      col = list(Group = setNames(groups_colors, unique(groups))),
      show_annotation_name = list(Group = FALSE),
      show_legend = FALSE
    )
    return(list(
      heat_sub_ann = heat_sub_ann,
      lgd = lgd,
      groups = groups,
      group_colors = group_colors
    ))
  }
  #辅助函数5-connectDb
  connectDb <- function() {
    return(DBI::dbConnect(
      drv = RSQLite::dbDriver("SQLite"),
      dbname = "/public/shiny/RiboTE/TEShinyData/orgInfo.db",
      flags = RSQLite::SQLITE_RO
    ))
  }
  #辅助函数6-getAllOrgData
  getAllOrgData <- function(){
    connectDb <- connectDb()
    orgInfo <- DBI::dbGetQuery(conn = connectDb,statement = "select * from orgInfo;")
    orgTopChoices <- c(
      "Human", "Mouse", "Rat", "Cow",
      "Zebrafish", "Pig", "Chicken", "Macaque", "Dog", "Drosophila melanogaster",
      "Caenorhabditis elegans", "Saccharomyces cerevisiae",
      "Arabidopsis thaliana", "Zea mays", "Glycine max",
      "Oryza sativa Indica Group", "Oryza sativa Japonica Group", "Vitis vinifera"
    )
    ix <- match(orgInfo$name2, orgTopChoices)
    orgInfo <- orgInfo[order(ix), ]
    orgInfo <- orgInfo[order(orgInfo$group == "STRINGv11.5"), ]
    return <- list(orgInfo)
  }
  #辅助函数7-connectConvertDbOrg
  connectConvertDbOrg <- function(selectOrg, allOrgData) {
    ix <- which(allOrgData$orgInfo$id == selectOrg)
    dbFile <- allOrgData$orgInfo[ix, "file"]
    message(dbFile)
    return(
      DBI::dbConnect(
        drv = RSQLite::dbDriver("SQLite"),
        dbname = paste0("/public/shiny/RiboTE/TEShinyData/", "orgDB/", dbFile),
        flags = RSQLite::SQLITE_RO
      ))
  }
  #辅助函数8-cleanGeneSet
  cleanGeneSet <- function(geneSet) {
    geneSet <- unique(toupper(gsub("\n| ", "", geneSet)))
    geneSet <- geneSet[which(nchar(geneSet) > 1)]
    return(geneSet)
  }
  #辅助函数9-cleanQuery
  cleanQuery <- function(queryInput) {
    return(cleanGeneSet(unlist(strsplit(
      x = toupper(queryInput),
      split = "\t| |\n|\\,"
    ))))
  }
  #辅助函数10-getSelectOrgDb
  getSelectOrgDb <- function(selectOrg, allOrgData) {
    ix <- which(allOrgData$orgInfo$id == selectOrg)
    if(allOrgData == "NEW" || length(ix) == 0) {
      return(NULL)
    }
    dbFile <- allOrgData$orgInfo[ix, "file"]
    return(
      DBI::dbConnect(
        drv = RSQLite::dbDriver("SQLite"),
        dbname = paste0("/public/shiny/RiboTE/TEShinyData/", "orgDB/", dbFile),
        flags = RSQLite::SQLITE_RO
      ))
  }
  #辅助函数11-findSpeciesByIdName
  findSpeciesByIdName <- function(speciesId, orgInfo) {
    return(orgInfo[which(orgInfo$id == speciesId), 3])
  }
  #辅助函数12-findSpeciesById
  findSpeciesById <- function(speciesId, orgInfo) {
    return(orgInfo[which(orgInfo$id == speciesId), ])
  }
  #辅助函数13-convertId
  convertId <- function(query,allOrgData, selectOrg = "BestMatch", maxSampleIds = 100) {
    query <- gsub(pattern = "\"|\'", "", x = query)
    querySet <- cleanQuery(queryInput = query)
    queryString <- paste0("('", paste(querySet, collapse = "', '"), "')")
    nSampleIds <- length(querySet)
    testQueryString <- queryString
    if (length(querySet) > maxSampleIds) {
      nSampleIds <- maxSampleIds
      testQueryString <- sample(querySet, maxSampleIds)
      test_query_string <- paste0(
        "('",
        paste(testQueryString, collapse = "', '"),
        "')"
      )
    }

    orgDb <- getSelectOrgDb(
      selectOrg = selectOrg,
      allOrgData = allOrgData
    )

    queryStatement <- paste0(
      "select id,ens,idType from mapping where id IN ",
      queryString
    )

    result <- DBI::dbGetQuery(orgDb, queryStatement)

    DBI::dbDisconnect(orgDb)
    if (nrow(result) == 0) {
      return(NULL)
    }

    bestIdType <- as.integer(
      names(
        sort(
          table(result$idType),
          decreasing = TRUE
        )
      )[1]
    )
    result <- result[result$idType == bestIdType, ]

    matched <- as.data.frame(paste(
      "Selected:",
      findSpeciesByIdName(
        speciesId = selectOrg,
        orgInfo = allOrgData$orgInfo
      )
    ))

    result <- result[which(!duplicated(result[, 1])), ]
    fwrite(result, paste0(TEFileuserDir, "/zea_may_mapping_data_frame.csv"))
    colnames(matched) <- c("Matched Species")

    conversionTable <- result[, 1:2]
    colnames(conversionTable) <- c("User_input", "ensembl_gene_id")
    conversionTable$Species <- findSpeciesByIdName(
      speciesId = as.integer(selectOrg),
      orgInfo = allOrgData$orgInfo
    )

    species <- findSpeciesById(
      speciesId = as.integer(selectOrg),
      orgInfo = allOrgData$orgInfo
    )

    return(list(
      origninalIds = querySet,
      ids = unique(result[, 2]),
      species = species,
      speciesMatch = matched,
      conversionTable = conversionTable
    ))
  }
  #辅助函数14-geneInfo
  geneInfo <- function(converted, selectOrg, allOrgData) {
    querySet <- converted$ids
    connDb <- getSelectOrgDb(
      selectOrg = selectOrg,
      allOrgData = allOrgData
    )
    queryStatement <- "select * from geneInfo;"
    geneInfoCsv <- DBI::dbGetQuery(connDb, queryStatement)
    DBI::dbDisconnect(connDb)
    set <- match(geneInfoCsv$ensembl_gene_id, querySet)
    set[which(is.na(set))] <- "Genome"
    set[which(set != "Genome")] <- "List"
    return(cbind(geneInfoCsv, set))
  }
  #辅助函数15-convertData
  convertData <- function(converted, data) {
    return(list(
      data = data,
      mappedIds = rownames(data)
    ))
  }
  #辅助函数16-getAllGeneNames
  getAllGeneNames <- function(mappedIds, allGeneInfo) {
    return(data.frame(
      "User_ID" = mappedIds,
      "ensembl_ID" = mappedIds,
      "symbol" = mappedIds
    ))
  }
  #辅助函数17-gmtCategory
  gmtCategory <- function(converted, convertedData, selectOrg, gmtFile, allOrgData) {
    querySet <- rownames(convertedData)
    connDb <- getSelectOrgDb(
      selectOrg = selectOrg,
      allOrgData = allOrgData
    )
    geneSetCategory <- DBI::dbGetQuery(connDb, "select distinct * from categories")
    DBI::dbDisconnect(connDb)
    geneSetCategory <- sort(geneSetCategory[, 1])
    categoryChoices <- setNames(as.list(geneSetCategory), geneSetCategory)
    topChoices <- c("GOBP", "GOCC", "GOMF", "KEGG")
    topChoices <- topChoices[topChoices %in% geneSetCategory]
    otherChoices <- names(categoryChoices)[
      !(names(categoryChoices) %in% topChoices)
    ]
    categoryChoices <- categoryChoices[c(topChoices, otherChoices)]
    names(categoryChoices)[match("GOBP", categoryChoices)] <- "GO Biological Process"
    names(categoryChoices)[match("GOCC", categoryChoices)] <- "GO Cellular Component"
    names(categoryChoices)[match("GOMF", categoryChoices)] <- "GO Molecular Function"
    categoryChoices <- append(setNames("All", "All available gene sets"), categoryChoices)
    return(categoryChoices)
  }
  #辅助函数18-conversionInfoMake
  conversionInfoMake <- function(geneMatrix, allOrgData, selectOrg){
    rownames(geneMatrix) <- geneMatrix$GeneID
    geneMatrix <- geneMatrix[, -1]
    ncol <- ncol(geneMatrix)
    converted <- convertId(
      query = rownames(geneMatrix),
      allOrgData = allOrgData,
      selectOrg = selectOrg,
      maxSampleIds = 200
    )
    allGeneInfo <- geneInfo(
      converted = converted,
      selectOrg = selectOrg,
      allOrgData = allOrgData
    )
    convertedData <- convertData(
      converted = converted,
      data = geneMatrix
    )
    allGeneNames <- getAllGeneNames(
      mappedIds = convertedData$mappedIds,
      allGeneInfo = allGeneInfo
    )
    gmtChoices <- gmtCategory(
      converted = converted,
      convertedData = convertedData$data,
      selectOrg = selectOrg,
      gmtFile = NULL,
      allOrgData = allOrgData
    )
    return(list(converted, allGeneInfo, convertedData, allGeneNames, gmtChoices, allOrgData, ncol))
  }
  #辅助函数19-readGeneSets
  readGeneSets <- function(converted, allGeneNames, pathwayDatabase, selectOrg, allOrgData, geneSetSizeRange) {
    if(selectOrg  == "5843334"){
      zea_may_id_mapping <- fread(paste0(TEFileuserDir, "/zea_may_mapping_data_frame.csv"))
      allGeneNames <- allGeneNames %>%
        left_join(zea_may_id_mapping, by = c("ensembl_ID" = "id")) %>%
        mutate(new_id = ifelse(is.na(ens), "null", ens)) %>%
        select(User_ID, ensembl_ID, symbol, new_id)
      if (!is.null(allGeneNames[, 4])) {
        querySet <- allGeneNames[, 4]
      }
    } else {
      if (!is.null(allGeneNames[, 2])) {
        querySet <- allGeneNames[, 2]
      }
    }
    pathway <- connectConvertDbOrg(
      selectOrg = selectOrg,
      allOrgData = allOrgData
    )
    ix <- grep(
      pattern = "pathway",
      x = DBI::dbListTables(pathway)
    )
    sqlQuery <- "SELECT  gene, pathwayID FROM pathway "
    if (pathwayDatabase != "All") {
      sqlQuery <- paste0(sqlQuery, " WHERE category = '", pathwayDatabase, "'")
    }
    result <- DBI::dbGetQuery(pathway, sqlQuery)
    result <- result[result$gene %in% querySet, ]
    pathwayIds <- aggregate(
      result$pathwayID,
      by = list(unique.values = result$pathwayID),
      FUN = length
    )
    pathwayIds <- pathwayIds[which(pathwayIds[, 2] >= geneSetSizeRange[1]), ]
    pathwayIds <- pathwayIds[which(pathwayIds[, 2] <= geneSetSizeRange[2]), ]
    if (dim(pathwayIds)[1] == 0) {
      geneSets <- NULL
    }
    result <- result[result$pathwayID %in% pathwayIds[, 1], ]
    if(selectOrg  == "5843334"){
      zea_may_id_mapping <- fread(paste0(TEFileuserDir, "/zea_may_mapping_data_frame.csv"))
      result <- result %>%
        left_join(zea_may_id_mapping, by = c("gene" = "ens")) %>%
        mutate(new_id = ifelse(is.na(id), "null", id)) %>%
        select(-id, -idType)
      result <- result[, -1]
      names(result) <- c("pathwayID","gene")
    }
    geneSets <- split(result$gene, result$pathwayID)
    pathwayInfo <- DBI::dbGetQuery(
      pathway,
      paste(
        "select distinct id,Description,memo from pathwayInfo where id IN ('",
        paste(pathwayIds[, 1], collapse = "', '"),
        "') ",
        sep = ""
      )
    )
    ix <- match(names(geneSets), pathwayInfo[, 1])
    names(geneSets) <- pathwayInfo[ix, 2]
    DBI::dbDisconnect(pathway)
    return(
      list(
        geneLists = geneSets,
        pathwayInfo = pathwayInfo
      )
    )
  }
  #辅助函数20-proper
  proper <- function(x) paste0(toupper(substr(x, 1, 1)), substring(x, 2))
  #辅助函数21-removePathwayId
  removePathwayId <- function(strings, pathwayDatabase) {
    if (is.null(strings)) {
      return(NULL)
    } else {
      if (pathwayDatabase %in% c("GOBP", "GOCC", "GOMF", "KEGG")) {
        strings <- sub(
          "^\\S+\\s",
          "",
          strings
        )
        strings <- proper(strings)
      }
      return(strings)
    }
  }
  #辅助函数22-hyperText
  hyperText <- function(textVector, urlVector) {
    if (sum(is.null(urlVector)) == length(urlVector)) {
      return(textVector)
    }

    if (length(textVector) != length(urlVector)) {
      return(textVector)
    }
    urlVector <- gsub(
      "cgi-bin/amigo/term_details\\?term=",
      "amigo/term/",
      urlVector
    )
    urlVector <- gsub(" ", "", urlVector)

    ix <- grepl("http:", urlVector, ignore.case = TRUE)
    if (sum(ix) > 0) {
      tem <- paste0(
        "<a href='",
        urlVector, "' target='_blank'>",
        textVector,
        "</a>"
      )
      textVector[ix] <- tem[ix]
    }
    return(textVector)
  }
  #辅助函数23-pathview.stamp
  pathview.stamp <- function(x = NULL, y = NULL, position = "bottomright", graph.sizes, on.kegg = TRUE, cex = 1) {
    if (on.kegg) {
      labels <- "Data on KEGG graph\nRendered by Pathview"
    } else {
      labels <- "-Data with KEGG pathway-\n-Rendered  by  Pathview-"
    }
    if (is.null(x) | is.null(y)) {
      x <- graph.sizes[1] * .80
      y <- graph.sizes[2] / 40
      if (length(grep("left", position)) == 1) x <- graph.sizes[1] / 40
      if (length(grep("top", position)) == 1) y <- graph.sizes[2] - y
    }
    text(x = x, y = y, labels = labels, adj = 0, cex = cex, font = 2)
  }
  #辅助函数24-colorpanel2
  colorpanel2 <- function(n, low, mid, high) {
    if (missing(mid) || missing(high)) {
      low <- grDevices::col2rgb(low)
      if (missing(high)) {
        high <- grDevices::col2rgb(mid)
      } else {
        high <- grDevices::col2rgb(high)
      }
      red <- seq(low[1, 1], high[1, 1], length = n) / 255
      green <- seq(low[3, 1], high[3, 1], length = n) / 255
      blue <- seq(low[2, 1], high[2, 1], length = n) / 255
    } else {
      isodd <- n %% 2 == 1
      if (isodd) {
        n <- n + 1
      }
      low <- grDevices::col2rgb(low)
      mid <- grDevices::col2rgb(mid)
      high <- grDevices::col2rgb(high)
      lower <- floor(n / 2)
      upper <- n - lower
      red <- c(
        seq(low[1, 1], mid[1, 1], length = lower),
        seq(mid[1, 1], high[1, 1], length = upper)
      ) / 255
      green <- c(
        seq(low[3, 1], mid[3, 1], length = lower),
        seq(mid[3, 1], high[3, 1], length = upper)
      ) / 255
      blue <- c(
        seq(low[2, 1], mid[2, 1], length = lower),
        seq(mid[2, 1], high[2, 1], length = upper)
      ) / 255
      if (isodd) {
        red <- red[-(lower + 1)]
        green <- green[-(lower + 1)]
        blue <- blue[-(lower + 1)]
      }
    }
    grDevices::rgb(red, blue, green)
  }
  #辅助函数25-render.kegg.node
  render.kegg.node <- function(plot.data, cols.ts, img, same.layer = TRUE, type = c("gene", "compound")[1], text.col = "black", cex = 0.25) {
    width <- ncol(img)
    height <- nrow(img)
    nn <- nrow(plot.data)
    pwids <- plot.data$width
    if (!all(pwids == max(pwids))) {
      message("Info: ", "some node width is different from others, and hence adjusted!")
      wc <- table(pwids)
      pwids <- plot.data$width <- as.numeric(names(wc)[which.max(wc)])
    }

    if (type == "gene") {
      if (same.layer != T) {
        rect.out <- sliced.shapes(plot.data$x + 0.5, height - plot.data$y, plot.data$width / 2 - 0.5, plot.data$height / 2 - 0.25, cols = cols.ts, draw.border = F, shape = "rectangle")
        text(plot.data$x + 0.5, height - plot.data$y,
             labels = as.character(plot.data$labels),
             cex = cex, col = text.col
        )
        return(invisible(1))
      } else {
        img2 <- img
        pidx <- cbind(
          ceiling(plot.data$x - plot.data$width / 2) + 1,
          floor(plot.data$x + plot.data$width / 2) + 1,
          ceiling(plot.data$y - plot.data$height / 2) + 1,
          floor(plot.data$y + plot.data$height / 2) + 1
        )
        cols.ts <- cbind(cols.ts)
        ns <- ncol(cols.ts)
        brk.x <- sapply(plot.data$width / 2, function(wi) seq(-wi, wi, length = ns + 1))
        for (k in 1:ns) {
          col.rgb <- col2rgb(cols.ts[, k]) / 255
          pxr <- t(apply(pidx[, 1:2], 1, function(x) x[1]:x[2])) - plot.data$x - 1
          sel <- pxr >= ceiling(brk.x[k, ]) & pxr <= floor(brk.x[k + 1, ])
          for (i in 1:nn) {
            sel.px <- (pidx[i, 1]:pidx[i, 2])[sel[i, ]]
            node.rgb <- img[pidx[i, 3]:pidx[i, 4], sel.px, 1:3]
            node.rgb.sum <- apply(node.rgb, c(1, 2), sum)
            blk.ind <- which(node.rgb.sum == 0 | node.rgb.sum == 1, arr.ind = T)
            node.rgb <- array(col.rgb[, i], dim(node.rgb)[3:1])
            node.rgb <- aperm(node.rgb, 3:1)
            for (j in 1:3) node.rgb[cbind(blk.ind, j)] <- 0
            img2[pidx[i, 3]:pidx[i, 4], sel.px, 1:3] <- node.rgb
          }
        }
        return(img2)
      }
    } else if (type == "compound") {
      if (same.layer != T) {
        nc.cols <- ncol(cbind(cols.ts))
        if (nc.cols > 2) {
          na.cols <- rep("#FFFFFF", nrow(plot.data))
          cir.out <- sliced.shapes(plot.data$x, height - plot.data$y, plot.data$width[1], plot.data$width[1], cols = na.cols, draw.border = F, shape = "ellipse", lwd = 0.2)
        }
        cir.out <- sliced.shapes(plot.data$x, height - plot.data$y, plot.data$width[1], plot.data$width[1], cols = cols.ts, shape = "ellipse", blwd = 0.2)
        return(invisible(1))
      } else {

        blk <- c(0, 0, 0)
        img2 <- img
        w <- ncol(img)
        h <- nrow(img)
        cidx <- rep(1:w, each = h)
        ridx <- rep(1:h, w)
        pidx <- lapply(1:nn, function(i) {
          ii <- which(
            (cidx - plot.data$x[i])^2 + (ridx - plot.data$y[i])^2 < (plot.data$width[i])^2
          )
          imat <- cbind(cbind(ridx, cidx)[rep(ii, each = 3), ], 1:3)
          imat[, 1:2] <- imat[, 1:2] + 1
          ib <- which(
            abs((cidx - plot.data$x[i])^2 + (ridx - plot.data$y[i])^2 - (plot.data$width[i])^2) <= 8
          )
          ibmat <- cbind(cbind(ridx, cidx)[rep(ib, each = 3), ], 1:3)
          ibmat[, 1:2] <- ibmat[, 1:2] + 1
          return(list(fill = imat, border = ibmat))
        })

        cols.ts <- cbind(cols.ts)
        ns <- ncol(cols.ts)
        brk.x <- sapply(plot.data$width, function(wi) seq(-wi, wi, length = ns + 1))
        for (i in 1:nn) {
          pxr <- pidx[[i]]$fill[, 2] - 1 - plot.data$x[i]
          col.rgb <- col2rgb(cols.ts[i, ]) / 255
          for (k in 1:ns) {
            sel <- pxr >= brk.x[k, i] & pxr <= brk.x[k + 1, i]
            img2[pidx[[i]]$fill[sel, ]] <- col.rgb[, k]
          }
          img2[pidx[[i]]$border] <- blk
        }
        return(img2)
      }
    } else {
      stop("unrecognized node type!")
    }
  }
  #辅助函数26-convertEnsemblToEntrez
  convertEnsemblToEntrez <- function(query, species, orgInfo, allOrgData) {
    querySet <- cleanGeneSet(
      unlist(strsplit(toupper(names(query)), "\t| |\n|\\, "))
    )
    speciesId <- orgInfo$id[which(orgInfo$ensembl_dataset == species)]

    convert <- connectConvertDbOrg(
      selectOrg = speciesId,
      allOrgData = allOrgData
    )
    if(inherits(convert, "try-error")) {
      return(NULL)
    }
    idTypeEntrez <- DBI::dbGetQuery(
      convert,
      paste(
        "select distinct * from idIndex where idType = 'entrezgene_id'"
      )
    )
    if (dim(idTypeEntrez)[1] != 1) {
      cat("Warning! entrezgene ID not found!")
    }
    idTypeEntrez <- as.numeric(idTypeEntrez[1, 1])
    result <- DBI::dbGetQuery(
      convert,
      paste0(
        "select  id,ens from mapping where ",
        " idType ='", idTypeEntrez, "' ",
        " AND ens IN ('", paste(querySet, collapse = "', '"), "')"
      )
    )

    DBI::dbDisconnect(convert)
    if (dim(result)[1] == 0) {
      return(NULL)
    }
    ix <- match(result$ens, names(query))

    tem <- query[ix]
    names(tem) <- result$id
    return(tem)
  }
  #辅助函数27-findContrastSamples
  findContrastSamples <- function(selectContrast, allSampleNames) {
    iz <- match(allSampleNames, unlist(strsplit(selectContrast, "-")))
    iz <- which(!is.na(iz))
    return(iz)
  }
  #辅助函数28-contrastSamples
  contrastSamples <- function(nGruops){
    findContrastSamples(
      selectContrast = "CONTROL-TREATED",
      allSampleNames = rep(c("CONTROL", "TREATED"), each = nGruops / 2)
    )
  }
  #辅助函数29-degHeatData
  degHeatData <- function(limma, selectContrast, processedData, contrastSamples) {
    genes <- limma
    if (!grepl("I:", selectContrast)) {
      ix <- match(selectContrast, colnames(genes))
    }
    query <- rownames(genes)[which(genes[, ix] != 0)]
    iy <- match(query, rownames(processedData))
    iz <- contrastSamples
    bar <- as.vector(genes[, ix])
    names(bar) <- row.names(genes)
    bar <- bar[bar != 0]
    genes <- processedData[iy, iz, drop = FALSE]
    genes <- genes[order(bar), , drop = FALSE]
    bar <- sort(bar)
    return(list(
      genes = genes,
      bar = bar
    ))
  }
  #辅助函数30-heatData
  heatData <- function(normalcount, res){
    res$calls <- 0
    minFcLimma <- 0.1
    maxPLimma <- 2

    res$calls[which(
      res$log2FoldChange > log2(maxPLimma) &
        res$padj < minFcLimma
    )] <- 1
    res$calls[which(
      res$log2FoldChange < -log2(maxPLimma) &
        res$padj < minFcLimma
    )] <- -1
    colnames(res)[grep("calls", colnames(res))] <- "CONTROL-TREATED"
    res$ancillary <- 0
    allCalls <- res[, c("GeneID", "CONTROL-TREATED", "ancillary")]
    rownames(allCalls) <- res[, 1]
    allCalls <- allCalls[, -1]
    nGruops <- ncol(normalcount)
    degHeatData <- degHeatData(
      limma = allCalls[, !names(allCalls) %in% "ancillary", drop = FALSE],
      selectContrast = "CONTROL-TREATED",
      processedData = normalcount,
      contrastSamples = contrastSamples(nGruops = nGruops)
    )
    return(degHeatData)
  }
  #辅助函数31-upRegData
  upRegData <- function(normalcount, res){
    req(!is.null(heatData(normalcount, res)))
    return(
      heatData(normalcount, res)$genes[heatData(normalcount, res)$bar == 1, ]
    )
  }
  #辅助函数32-downRegData
  downRegData <- function(normalcount, res){
    req(!is.null(heatData(normalcount, res)))
    return(
      heatData(normalcount, res)$genes[heatData(normalcount, res)$bar == -1, ]
    )
  }
  #辅助函数33-mergeData
  mergeData <- function(allGeneNames, data, mergeID) {
    isolate({
      if (dim(allGeneNames)[2] == 1) {
        newData <- data
        newData <- as.data.frame(newData)
        newData$User_id <- rownames(newData)
        newData <- dplyr::select(
          newData,
          User_id,
          tidyselect::everything()
        )
        rownames(newData) <- seq(1, nrow(newData), 1)
        tmp <- apply(newData[, 2:dim(newData)[2]], 1, sd)
        newData <- newData[order(-tmp), ]

        return(newData)
      } else if (dim(allGeneNames)[2] == 2) {
        newData <- merge(
          allGeneNames,
          data,
          by.x = mergeID,
          by.y = "row.names",
          all.y = T
        )
        newData <- dplyr::select(
          newData,
          User_ID,
          ensembl_ID,
          tidyselect::everything()
        )
        rownames(newData) <- seq(1, nrow(newData), 1)
        tmp <- apply(newData[, 3:dim(newData)[2]], 1, sd)
        newData <- newData[order(-tmp), ]
        return(newData)
      } else {
        newData <- merge(
          allGeneNames,
          data,
          by.x = mergeID,
          by.y = "row.names",
          all.y = T
        )
        newData <- dplyr::select(
          newData,
          User_ID,
          ensembl_ID,
          symbol,
          tidyselect::everything()
        )
        rownames(newData) <- seq(1, nrow(newData), 1)
        tmp <- apply(newData[, 4:dim(newData)[2]], 1, sd)
        newData <- newData[order(-tmp), ]
        return(newData)
      }
    })
  }
  #辅助函数34-pathwayDeg
  pathwayDeg <- function(conversionInfo, normalcount, res){
    req(!is.null(upRegData(normalcount, res)))
    degLists <- list()
    lists <- c("Up", "Down")
    for (direction in lists) {
      if (direction == lists[1]) {
        data <- upRegData(normalcount, res)
      } else {
        data <- downRegData(normalcount, res)
      }
      geneNames <- mergeData(
        allGeneNames = conversionInfo[[4]],
        data = data,
        mergeID = "ensembl_ID"
      )
      degLists[[direction]] <- dplyr::select_if(geneNames, is.character)
    }
    return(degLists)
  }
  #辅助函数35-readGmtRobust
  readGmtRobust <- function(inFile) {
    x <- scan(inFile, what = "", sep = "\n")
    x <- gsub(" ", "", x)
    x <- toupper(x)
    y <- strsplit(x, "\t")
    names(y) <- sapply(y, "[[", 1)
    y <- lapply(y, "[", -c(1, 2))
    for (i in 1:length(y)) {
      y[[i]] <- cleanGeneSet(y[[i]])
    }
    if (max(sapply(y, length)) < 5) {
      cat("Warning! Gene sets have very small number of genes!\n Please double check format.")
    }
    y <- y[which(sapply(y, length) > 1)]

    return(y)
  }
  #辅助函数36-buildPathwayQuery
  buildPathwayQuery <- function(pathwayDatabase, querySet) {
    sqlQuery <- "SELECT gene, pathwayID FROM pathway WHERE "
    if (pathwayDatabase != "All") {
      sqlQuery <- paste0(sqlQuery, " category = '", pathwayDatabase, "' AND ")
    }
    sqlQuery <- paste(
      sqlQuery,
      " gene IN ('",
      paste(querySet, collapse = "', '"),
      "')",
      sep = ""
    )
    return(sqlQuery)
  }
  #辅助函数37-readPathwaySets
  readPathwaySets <- function(allGeneNamesQuery, converted, pathwayDatabase, selectOrg, gmtFile, allOrgData, geneInfo) {
    if(selectOrg == "5843334"){
      zea_may_id_mapping <- fread(paste0(TEFileuserDir, "/zea_may_mapping_data_frame.csv"))
      allGeneNamesQuery <- allGeneNamesQuery %>%
        left_join(zea_may_id_mapping, by = c("ensembl_ID" = "id")) %>%
        mutate(ensembl_ID = ifelse(is.na(ens), "null", ens)) %>%
        select(User_ID, ensembl_ID, symbol) %>%
        filter(ensembl_ID != "null")
    }
    querySet <- allGeneNamesQuery[, 2]
    pathway <- connectConvertDbOrg(
      selectOrg = selectOrg,
      allOrgData = allOrgData
    )
    ix <- grep(
      pattern = "pathway",
      x = DBI::dbListTables(pathway)
    )
    sqlQuery <- buildPathwayQuery(pathwayDatabase, querySet)
    result <- DBI::dbGetQuery(pathway, sqlQuery)
    if (dim(result)[1] == 0) {
      return(pathwayTable <- NULL)
    }
    pathwayIds <- stats::aggregate(
      result$pathwayID,
      by = list(unique_values = result$pathwayID),
      FUN = length
    )
    colnames(pathwayIds) <- c("pathway_id", "overlap")
    if (dim(pathwayIds)[1] == 0) {
      return(pathwayTable <- NULL)
    } else {
      pathwayInfo <- DBI::dbGetQuery(
        pathway,
        paste(
          "SELECT DISTINCT id,n,Description,memo FROM pathwayInfo WHERE id IN ('",
          paste(pathwayIds[, 1], collapse = "', '"), "') ",
          sep = ""
        )
      )
      pathwayInfo <- pathwayInfo[
        order(
          pathwayInfo$id,
          pathwayInfo$memo,
          decreasing = TRUE
        ),
      ]
      pathwayInfo <- pathwayInfo[!duplicated(pathwayInfo$id), ]
      pathwayMerge <- merge(
        x = pathwayIds,
        y = pathwayInfo,
        by.x = "pathway_id",
        by.y = "id",
        all = TRUE
      )
      geneSets <- lapply(
        pathwayMerge$pathway_id,
        function(x) result[which(result$pathwayID == x), 1]
      )
      names(geneSets) <- pathwayMerge$description
      pathwayMerge$gene_sets <- geneSets
    }
    querySetDb <- unique(result$gene)
    querySet <- querySetDb
    sqlQuery <- "SELECT COUNT ( DISTINCT gene ) FROM pathway"
    if (pathwayDatabase != "All") {
      sqlQuery <- paste(
        sqlQuery,
        " WHERE category='", pathwayDatabase, "'",
        sep = ""
      )
    }
    totalGenesDb <- DBI::dbGetQuery(
      pathway,
      sqlQuery
    )
    totalGenesDb <- as.integer(totalGenesDb)
    totalGenes <- totalGenesDb
    DBI::dbDisconnect(pathway)
    return(list(
      pathwayTable = pathwayMerge,
      querySet = querySet,
      totalGenes = totalGenes
    ))
  }
  #辅助函数38-backgroundPathwaySets
  backgroundPathwaySets <- function(processedData, geneInfo, subQuery, pathwayDatabase, pathwayTable, allOrgData, selectOrg) {
    if(selectOrg == "5843334"){
      zea_may_id_mapping <- fread(paste0(TEFileuserDir, "/zea_may_mapping_data_frame.csv"))
      processedData <- processedData %>%
        mutate(new_id = rownames(processedData))
      processedData <- processedData %>%
        left_join(zea_may_id_mapping, by = c("new_id" = "id")) %>%
        mutate(ens_new_id = ifelse(is.na(ens), "null", ens)) %>%
        select(-new_id, -ens, -idType) %>%
        filter(ens_new_id != "null") %>%  
        distinct(ens_new_id, .keep_all = TRUE)
      rownames(processedData) <- processedData$ens_new_id
      processedData <- processedData %>% select(-ens_new_id)
    }
    querySet <- rownames(processedData)
    pathway <- connectConvertDbOrg(
      selectOrg = selectOrg,
      allOrgData = allOrgData
    )
    querySet <- unique(c(querySet, subQuery))
    sqlQuery <- "SELECT gene, pathwayID FROM pathway "
    if (pathwayDatabase != "All") {
      sqlQuery <- paste0(sqlQuery, " WHERE category = '", pathwayDatabase, "'")
    }
    results <- DBI::dbGetQuery(pathway, sqlQuery)
    results <- results[results$gene %in% querySet, ]
    bgResult <- table(results$pathwayID)
    bgResult <- as.data.frame(bgResult)
    colnames(bgResult) <- c("pathway_id", "overlap_bg")
    pathwayTableBg <- merge(
      pathwayTable,
      bgResult,
      by = "pathway_id",
      all.x = TRUE
    )
    pathwayTableBg$total_genes_bg <- length(unique(results$gene))
    DBI::dbDisconnect(pathway)
    return(pathwayTableBg)
  }
  #辅助函数39-findOverlap
  findOverlap <- function(pathwayTable, querySet, totalGenes, processedData, geneInfo, pathwayDatabase, allOrgData, useFilteredBackground, selectOrg,
                          reduced = FALSE, maxTerms = 15, sortByFold = "FALSE") {
    maxPvalFilter <- 0.3
    maxGenesBackground <- 30000
    minGenesBackground <- 1000
    minFdr <- 0.1
    minOverlap <- 2
    minWordOverlap <- 0.5
    if (reduced) {
      reduced <- .9
    }
    if(1){
      pathwayTable$pval <- stats::phyper(
        pathwayTable$overlap - 1,
        length(querySet),
        totalGenes - length(querySet),
        as.numeric(pathwayTable$n),
        lower.tail = FALSE
      )
      pathwayTable$fold <- pathwayTable$overlap / length(querySet) / (
        as.numeric(pathwayTable$n) / totalGenes
      )
      if (!is.null(useFilteredBackground)) {
        if (
          useFilteredBackground &&
          length(row.names(processedData)) > minGenesBackground &&
          length(row.names(processedData)) < maxGenesBackground + 1
        ) {
          pathwayTableBg <- backgroundPathwaySets(
            processedData = processedData,
            geneInfo = geneInfo,
            subQuery = querySet,
            pathwayDatabase = pathwayDatabase,
            pathwayTable = pathwayTable,
            allOrgData = allOrgData,
            selectOrg = selectOrg
          )


          pathwayTable$pval <- phyper(
            pathwayTableBg$overlap - 1,
            length(querySet),
            pathwayTableBg$total_genes_bg[1] - length(querySet),
            as.numeric(pathwayTableBg$overlap_bg),
            lower.tail = FALSE
          )
          pathwayTable$fold <- (pathwayTable$overlap / length(querySet)) / (
            as.numeric(pathwayTableBg$overlap_bg) / pathwayTableBg$total_genes_bg[1]
          )
        }
      }

      pathwayTable$fdr <- stats::p.adjust(pathwayTable$pval, method = "fdr")

    }

    if (min(pathwayTable$fdr) > minFdr) {
      pathwayTable <- data.frame("Enrichment" = "No significant enrichment found!")
    } else {
      pathwayTable <- pathwayTable[which(pathwayTable$fdr < minFdr), ]

      pathwayTable <- subset(
        pathwayTable,
        select = c(
          fdr,
          overlap,
          n,
          fold,
          description,
          memo,
          gene_sets
        )
      )

      if (sortByFold) {
        pathwayTable <- pathwayTable[order(
          pathwayTable$fold,
          decreasing = TRUE
        ), ]

        pathwayTable <- pathwayTable[pathwayTable$overlap >= minOverlap, ]
      } else {
        pathwayTable <- pathwayTable[order(pathwayTable$fdr), ]
      }

      if (!is.numeric(maxTerms)) {
        maxTerms <- 15
      }
      if (maxTerms > 100) {
        maxTerms <- 100
      }
      if (maxTerms < 1) {
        maxTerms <- 1
      }
      if (dim(pathwayTable)[1] > maxTerms) {
        pathwayTable <- pathwayTable[1:maxTerms, ]
      }

      pathwayTable$n <- as.numeric(pathwayTable$n)
      pathwayTable$fdr <- formatC(pathwayTable$fdr, format = "e", digits = 2)
      colnames(pathwayTable) <- c(
        "FDR", "nGenes", "Pathway size", "Fold enriched",
        "Pathway", "URL", "Genes"
      )

      if (reduced != FALSE && dim(pathwayTable)[1] > 5) {
        n <- nrow(pathwayTable)
        flag1 <- rep(TRUE, n)


        geneLists <- pathwayTable$Genes


        pathways <- lapply(
          pathwayTable$Pathway,
          function(y) unlist(strsplit(as.character(y), " |  |   "))
        )
        for (i in 2:n) {
          for (j in 1:(i - 1)) {
            if (flag1[j]) {
              ratio1 <- length(intersect(geneLists[[i]], geneLists[[j]])) /
                length(union(geneLists[[i]], geneLists[[j]]))


              if (ratio1 > reduced) {

                ratio2 <- length(intersect(pathways[[i]], pathways[[j]])) /
                  length(union(pathways[[i]], pathways[[j]]))

                if (ratio2 > minWordOverlap) {
                  flag1[i] <- FALSE
                }
              }
            }
          }
        }
        pathwayTable <- pathwayTable[which(flag1), ]
      }
    }

    return(pathwayTable)
  }
  #辅助函数40-dataFrameWithList
  dataFrameWithList <- function(dataObject) {
    setListsToChars <- function(x) {
      if (class(x) == "list") {
        y <- paste(unlist(x[1]), sep = "", collapse = ", ")
      } else {
        y <- x
      }
      return(y)
    }
    newFrame <- data.frame(
      lapply(dataObject, setListsToChars),
      stringsAsFactors = F
    )
    return(newFrame)
  }
  #辅助函数41-getWGCNA
  getWGCNA <- function(data, nGenes, softPower, minModuleSize) {
    maxGeneWgcna <- 3000
    if (nGenes > dim(data)[1]) {
      nGenes <- dim(data)[1]
    }
    if (nGenes < 50) {
      return(NULL)
    }
    if (dim(data)[2] < 4) {
      return(NULL)
    }
    if (nGenes > maxGeneWgcna) {
      n <- maxGeneWgcna
    }

    datExpr <- t(data[1:nGenes, ])
    datExpr <- datExpr[, colSums(is.na(datExpr)) == 0]
    datExpr <- datExpr[, !apply(datExpr, 2, function(x) sd(x) == 0)]
    subGeneNames <- colnames(datExpr)

    powers <- c(c(1:10), seq(from = 12, to = 20, by = 2))

    sft <- WGCNA::pickSoftThreshold(
      datExpr,
      dataIsExpr = TRUE,
      powerVector = powers,
      corFnc = cor,
      corOptions = list(use = "p"),
      networkType = "unsigned"
    )

    adj <- WGCNA::adjacency(
      datExpr,
      type = "unsigned",
      power = softPower
    )

    tom <- WGCNA::TOMsimilarityFromExpr(
      datExpr,
      networkType = "unsigned",
      TOMType = "unsigned",
      power = softPower
    )
    colnames(tom) <- subGeneNames
    rownames(tom) <- subGeneNames

    geneTree <- flashClust::flashClust(
      as.dist(1 - tom),
      method = "average"
    )

    dynamicMods <- dynamicTreeCut::cutreeDynamic(
      dendro = geneTree,
      method = "tree",
      minClusterSize = minModuleSize
    )

    dynamicColors <- WGCNA::labels2colors(dynamicMods)
    moduleInfo <- cbind(subGeneNames, dynamicColors, dynamicMods)

    moduleInfo <- moduleInfo[which(moduleInfo[, 2] != "grey"), ]
    moduleInfo <- moduleInfo[order(moduleInfo[, 3]), ]
    nModules <- length(unique(dynamicColors)) - 1
    nGenes <- dim(moduleInfo)[1]

    return(list(
      data = t(datExpr),
      powers = powers,
      sft = sft,
      tom = tom,
      dynamicColors = dynamicColors,
      moduleInfo = moduleInfo,
      nModules = nModules,
      nGenes = nGenes
    ))
  }
  #辅助函数42-wgcna
  WGCNA <- function(WGCNAData, nGenesNetwork, softPower, minModuleSize){
    WGCNA <- getWGCNA( data = WGCNAData, nGenes = nGenesNetwork, softPower = softPower, minModuleSize = minModuleSize)
    return(WGCNA)
  }
  #辅助函数43-getWGCNAModules
  getWGCNAModules <- function(WGCNA) {
    if (dim(WGCNA$moduleInfo)[1] == 0) {

      return(NULL)
    } else {
      modules <- unique(WGCNA$moduleInfo[, c("dynamicMods", "dynamicColors")])
      moduleList <- apply(modules, 1, paste, collapse = ". ")
      moduleList <- paste0(
        moduleList,
        " (",
        table(WGCNA$moduleInfo[, "dynamicMods"]),
        " genes)"
      )
      moduleList <- c(moduleList, "Entire network")

      return(moduleList)
    }
  }
  #辅助函数44-getNetwork
  getNetwork <- function(selectWGCNAModule, WGCNA, topGenesNetwork, selectOrg, allGeneInfo) {
    module <- unlist(strsplit(selectWGCNAModule, " "))[2]
    moduleColors <- WGCNA$dynamicColors
    inModule <- (moduleColors == module)

    if (selectWGCNAModule == "Entire network") {
      inModule <- rep(TRUE, length(inModule))
    }
    datExpr <- t(WGCNA$data)
    probes <- colnames(datExpr)
    modProbes <- probes[inModule]

    modTom <- WGCNA$tom[inModule, inModule]
    dimnames(modTom) <- list(modProbes, modProbes)

    nTop <- min(topGenesNetwork, 1000)
    imConn <- WGCNA::softConnectivity(datExpr[, modProbes])
    top <- (rank(-imConn) <= nTop)

    probeToGene <- NULL
    if (!"symbol" %in% colnames(allGeneInfo)) {
      allGeneInfo$symbol <- "null"
    }
    dimAllGeneInfo <- dim(allGeneInfo)
    
    if (selectOrg != "NEW" &&
        !is.null(dimAllGeneInfo) &&
        dimAllGeneInfo[1] > 1) {

      if (sum(is.na(allGeneInfo$symbol)) / dimAllGeneInfo[1] < .5) {
        probeToGene <- allGeneInfo[, c("ensembl_gene_id", "symbol")]
        probeToGene$symbol <- gsub(" ", "", probeToGene$symbol)

        ix <- which(
          is.na(probeToGene$symbol) |
            nchar(probeToGene$symbol) < 2 |
            toupper(probeToGene$symbol) == "NA" |
            toupper(probeToGene$symbol) == "0"
        )

        probeToGene[ix, 2] <- probeToGene[ix, 1]
      }
    }

    net <- modTom[top, top]

    if (!is.null(probeToGene)) {
      ix <- match(colnames(net), probeToGene[, 1])
      colnames(net) <- probeToGene[ix, 2]
      ix <- match(rownames(net), probeToGene[, 1])
      rownames(net) <- probeToGene[ix, 2]
    }

    return(net)
  }
  #辅助函数45-getNetworkPlot
  getNetworkPlot <- function(adjacencyMatrix, edgeThreshold) {
    adjacencyMatrix <- adjacencyMatrix > edgeThreshold
    for (i in 1:dim(adjacencyMatrix)[1]) {
      adjacencyMatrix[i, i] <- FALSE
    }
    graph <- igraph::graph_from_adjacency_matrix(adjacencyMatrix, mode = "undirected")
    saveRDS(graph, paste0(TEFileuserDir, "/network_data.rds"))
    netPlot <- function() {
      pdf(paste0(TEFileuserDir, "/network_plot.pdf"), width = 9, height = 5.4)
      plot(
        graph,
        vertex.label.color = "black",
        vertex.label.dist = 3,
        vertex.size = 7,
        edge.width = 0.5,
        edge.color = "black"
      )
      dev.off()
      png(paste0(TEFileuserDir,"/network_plot.png"), width = 900 * 2, height = 540 * 2, res = 200)
      plot(
        graph,
        vertex.label.color = "black",
        vertex.label.dist = 3,
        vertex.size = 7,
        edge.width = 0.5,
        edge.color = "black"
      )
      dev.off()
      plot(
        graph,
        vertex.label.color = "black",
        vertex.label.dist = 3,
        vertex.size = 7,
        edge.width = 0.5,
        edge.color = "black"
      )
    }
    return(netPlot)
  }
  #辅助函数46-adjMatrix
  adjMatrix <- function(moduleList, selectOrg, topGenesNetwork, conversionInfo, edgeThreshold, nGenesNetwork, WGCNAData, softPower, minModuleSize){
    req(!is.null(input$moduleSelect))
    req(!is.null(WGCNA(WGCNAData, nGenesNetwork, softPower, minModuleSize)))
    network <- getNetwork(
      selectWGCNAModule = input$moduleSelect,
      WGCNA = WGCNA(WGCNAData, nGenesNetwork, softPower, minModuleSize),
      topGenesNetwork = topGenesNetwork,
      selectOrg = selectOrg,
      allGeneInfo = conversionInfo[[2]]
    )
    return(network)
  }
  #辅助函数47-moduleNDetwork
  moduleNetwork <- function(moduleList, network, WGCNAData, nGenesNetwork, softPower, minModuleSize){
    req(!is.null(moduleList[[1]]))
    req(!is.null(WGCNA(WGCNAData, nGenesNetwork, softPower, minModuleSize)))
    return(network$networkPlot())
  }
  #辅助函数48-geneMatrixPreprocessing
  geneMatrixPreprocessing <- function(geneMatrix){
    missing_value <- input$NAestimate
    geneMatrix <- geneMatrix[!duplicated(geneMatrix[, 1]), ]
    geneMatrix <- geneMatrix[!is.na(geneMatrix[, 1]), ]
    rownames(geneMatrix) <- toupper(geneMatrix[, 1])
    geneMatrix <- geneMatrix[, -1]
    geneMatrix[] <- lapply(geneMatrix, function(x) as.numeric(as.character(x)))
    geneMatrix <- geneMatrix[!apply(is.na(geneMatrix), 1, all), ]
    ix <- apply(geneMatrix, 2, function(col) all(col == 0))
    if (sum(ix) > 0) {
      geneMatrix <- geneMatrix[, !ix]
    }
    geneMatrix <- geneMatrix[order(-apply(
      geneMatrix[, 1:dim(geneMatrix)[2]],
      1,
      function(x) sd(x, na.rm = TRUE)
    )), ]
    if (sum(is.na(geneMatrix)) > 0) {
      if (missing_value == "geneMedian") {
        row_medians <- apply(geneMatrix, 1, function(y) median(y, na.rm = T))
        for (i in 1:ncol(geneMatrix)) {
          val_miss_row <- which(is.na(geneMatrix[, i]))
          geneMatrix[val_miss_row, i] <- row_medians[val_miss_row]
        }
      } else if (missing_value == "treatAsZero") {
        geneMatrix[is.na(geneMatrix)] <- 0
      }
    }
    geneMatrix <- geneMatrix[rowSums(geneMatrix, na.rm = TRUE) >= 1, ]
    geneMatrix <- as.data.frame(geneMatrix)
    geneMatrix <- cbind(GeneID = rownames(geneMatrix), geneMatrix)
    fwrite(geneMatrix, paste0(TEFileuserDir,"/qc1Table.csv"))
    rownames(geneMatrix) <- NULL
    return(geneMatrix)
  }
  #辅助函数49-geneMatrixStat
  geneMatrixStat <- function(){
    req(input$geneMatrix)
    geneMatrixPath <- input$geneMatrix$datapath
    geneMatrix <- read.table(geneMatrixPath,header = T, sep = "\t", check.names = FALSE)
    return(geneMatrix)
  }
  #辅助函数50-distFunctions
  distFunctions <- function() {
    distPcc <- function(x, ...) {
      as.dist(1 - cor(t(x), method = "pearson"))
    }
    distAbsPcc <- function(x, ...) {
      as.dist(1 - abs(cor(t(x), method = "pearson")))
    }
    return(list(
      Pearson = distPcc,
      Euclidean = dist,
      Absolute_Pearson = distAbsPcc
    ))
  }
  #辅助函数51-rawCountPre
  rawCountPre <- function(rnaCntTable, riboCntTable, rnaCond, riboCond, contrast=NULL, minMeanCount=1) {
    group <- rnaCond
    if (!identical(rownames(rnaCntTable), rownames(riboCntTable)))
      stop ("RNA- and Ribo-seq data must have the same set of genes")
    if (!is.data.frame(rnaCond)) rnaCond <- data.frame(cond = rnaCond)
    if (!is.data.frame(riboCond)) riboCond <- data.frame(cond = riboCond)
    if (!identical(colnames(rnaCond), colnames(riboCond)))
      stop("RNA- and Ribo-seq data must have the same set of conditions")
    if (ncol(rnaCntTable) != nrow(rnaCond))
      stop(paste("RNA-seq count table must have the",
                 "same number of samples as in rnaCond"))
    if (ncol(riboCntTable) != nrow(riboCond))
      stop(paste("Ribo-seq count table must have the",
                 "same number of samples as in riboCond"))
    if (minMeanCount < 1)
      stop("minMeanCount must at least be 1")
    keep.rna <- rownames(rnaCntTable)[rowMeans(rnaCntTable) >= minMeanCount]
    keep.ribo <- rownames(riboCntTable)[rowMeans(riboCntTable) >= minMeanCount]
    keep <- intersect(keep.rna, keep.ribo)
    rnaCntTable <- rnaCntTable[keep,]
    riboCntTable <- riboCntTable[keep,]
    numCond <- ncol(rnaCond)
    numRNASmps <- nrow(rnaCond)
    numRiboSmps <- nrow(riboCond)
    combCntTbl <- cbind(rnaCntTable, riboCntTable)
    combinedCond <- rbind(rnaCond, riboCond)
    combinedCond <- combinedCond[,rep(1:ncol(combinedCond),2)]
    INTERCEPT <- c(rep("CONTROL", numRNASmps), rep("TREATED", numRiboSmps))
    combinedCond <- cbind(combinedCond[1:numCond], INTERCEPT,
                          combinedCond[(numCond+1):ncol(combinedCond)])
    for( i in (numCond+2) : ncol(combinedCond)) {
      combinedCond[1:numRNASmps,i] <- combinedCond[1,i]
    }
    colnames(combinedCond)[(numCond+2):ncol(combinedCond)] <- paste0("EXTRA", seq(numCond))
    extendedConds <- colnames(combinedCond)
    fmla <- as.formula(paste("~", paste(extendedConds, collapse= "+")))
    combCntTbl <- round(combCntTbl, 0)

    combCntTbl <- combCntTbl[ which( apply( cpm(DGEList(counts = combCntTbl)), 1,
                                            function(y) sum(y >= input$minCounts)) >= input$nMinSamplesCount ) , ]
    fwrite(combCntTbl, paste0(TEFileuserDir,"/combCntTbl.csv"), row.names = T)
  }
  #辅助函数52-groupCreation
  groupCreation <- function(inputnames, rpfnames){
    combinedNames <- paste(inputnames, rpfnames, sep = "#")
    parts <- strsplit(combinedNames, "#")[[1]]
    groupSequence <- c()
    groupCounter <- 1
    for (part in parts) {
      elements <- strsplit(part, ",")[[1]]
      numElements <- length(elements)
      groupSequence <- c(groupSequence, rep(paste0("Group", groupCounter), numElements))
      groupCounter <- groupCounter + 1
    }
    return(groupSequence)
  }
  #辅助函数53-rawCountPlot
  rawCountPlot <- function(countsData, type, inputnames, rpfnames) {
    colorTotal <- c("#6B7EB9", "#A5C2E2", "#6AB4C1", "#72C3A3", "#70B48F")
    counts <- countsData
    memo <- ""
    Groups <- groupCreation(inputnames, rpfnames)
    plotData <- data.frame(
      sample = as.factor(colnames(counts)),
      counts = colSums(counts) / 1e6,
      Group = Groups
    )
    numColors <- length(unique(plotData$Group))
    extendedColors <- colorRampPalette(colorTotal)(numColors)
    plot <- ggplot2::ggplot(
      data = plotData,
      ggplot2::aes(x = sample, y = counts, fill = Groups)
    ) +
      ggplot2::scale_fill_manual(values = extendedColors)
    plot <- plot +
      ggplot2::geom_bar(stat = "identity", color = "black", width = 0.6) +
      ggplot2::geom_text(aes(label = sprintf("%.2f", counts)),
                         vjust = 0.5, size = 4, color = "black", angle = 90, hjust = -0.5) +
      ggplot2::theme(
        legend.position = "right",
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_text(
          color = "black",
          size = 14
        ),
        axis.text.x = ggplot2::element_text(
          angle = 60,
          size = 12,
          hjust = 1,
          vjust = 1
        ),
        axis.text.y = ggplot2::element_text(
          size = 16
        ),
        plot.title = ggplot2::element_text(
          color = "black",
          size = 16,
          face = "bold",
          hjust = .5
        ),
        panel.grid = ggplot2::element_blank(),
        axis.line.x = ggplot2::element_line(color = "black"),
        axis.line.y = ggplot2::element_line(color = "black"),
        panel.background = ggplot2::element_rect(fill = "transparent", color = NA),
        plot.background = ggplot2::element_rect(fill = "transparent", color = NA)
      ) +
      ggplot2::labs(
        title = paste("Total", type, "Read Counts (Millions)"),
        y = paste(type, "Counts (Millions)")
      ) + ggplot2::scale_y_continuous(expand = c(0, 0), limits=c(0, max(plotData$counts)+7))
    fwrite(plotData, paste0(TEFileuserDir, "/rawCountPlotData.csv"))
    ggplot2::ggsave(filename = paste0(TEFileuserDir, "/rawCountPlot.png"), plot = plot, width = 8, height = 4.8, dpi = 250, bg = "#ffffff")
    ggplot2::ggsave(filename = paste0(TEFileuserDir, "/rawCountPlot.pdf"), plot = plot, width = 8, height = 4.8, bg = "transparent")
    return(plot)
  }
  #辅助函数54-geneTypePlot
  geneTypePlot <- function(countsData, allGeneInfo) {
    counts <- countsData
    df <- merge(countsData, allGeneInfo, by.x = "row.names", by.y = "ensembl_gene_id")
    fwrite(df,"df111.csv")
    if(nrow(df) == 0){
      plot <- ggplot() + 
        theme_void() +
        annotate("text", x = 0.5, y = 0.5, label = "NULL", size = 15, color = "black") + 
        theme(plot.background = element_rect(fill = "#ffffff"))
      fwrite(df, paste0(TEFileuserDir, "/geneTypePlotData.csv"))
      ggsave(filename = paste0(TEFileuserDir, "/geneTypePlot.png"), plot = plot, width = 9.6, height = 5.2, dpi = 250, bg = "#ffffff")
      ggplot2::ggsave(filename = paste0(TEFileuserDir, "/geneTypePlot.pdf"), plot = plot, width = 9.6, height = 5.2, bg = "transparent")
      return(plot)
    }
    df$gene_biotype <- gsub(".*pseudogene", "Pseudogene", df$gene_biotype)
    df$gene_biotype <- gsub("TEC", "Unknown", df$gene_biotype)
    df$gene_biotype <- gsub("artifact", "Artifact", df$gene_biotype)
    df$gene_biotype <- gsub("IG_.*", "IG", df$gene_biotype)
    df$gene_biotype <- gsub("TR_.*", "TR", df$gene_biotype)
    df$gene_biotype <- gsub("protein_coding", "Coding", df$gene_biotype)

    counts <- table(df$gene_biotype)
    data <- data.frame(
      category = names(counts),
      value = as.numeric(counts)
    )
    data <- data[order(-data$value), ]
    plot <- ggplot2::ggplot(data, ggplot2::aes(x = reorder(category, value), y = value, fill = category)) +
      ggplot2::geom_bar(stat = "identity", width = 0.6, color = "black") +
      ggplot2::scale_y_log10(limits = c(1, 3 * max(data$value)), expand = c(0, 0),
                             breaks = scales::trans_breaks("log10", function(x) 10^x)) +
      ggplot2::coord_flip() +
      ggplot2::labs(x = NULL, y = "Number of Genes", title = "Number of Genes by type") +
      ggplot2::geom_text(ggplot2::aes(label = value), hjust = -0.3, vjust = 0.5)

    plot <- plot +
      ggplot2::theme_light() +
      ggplot2::theme(
        legend.position = "none",
        axis.title.y = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_text(
          color = "black",
          size = 15
        ),
        axis.text.x = ggplot2::element_text(
          size = 15
        ),
        axis.text.y = ggplot2::element_text(
          size = 14
        ),
        axis.ticks = ggplot2::element_line(color = "black", size = 0.5),
        plot.title = ggplot2::element_text(
          color = "black",
          size = 15,
          face = "bold",
          hjust = .5
        ),
        panel.border = ggplot2::element_rect(color = "black", fill = NA, size = 1)
      )
    fwrite(data, paste0(TEFileuserDir, "/geneTypePlotData.csv"))
    ggplot2::ggsave(filename = paste0(TEFileuserDir, "/geneTypePlot.png"), plot = plot, width = 9.6, height = 5.2, dpi = 250, bg = "#ffffff")
    ggplot2::ggsave(filename = paste0(TEFileuserDir, "/geneTypePlot.pdf"), plot = plot, width = 9.6, height = 5.2, bg = "transparent")
    return(plot)
  }
  #辅助函数55-rRNACountsPlot
  rRNACountsPlot <- function(countsData, allGeneInfo, inputnames, rpfnames) {
    colorTotal <- c("#70B48F", "#72C3A3","#6B7EB9", "#A5C2E2", "#6AB4C1")
    counts <- countsData
    df <- merge(
      countsData,
      allGeneInfo,
      by.x = "row.names",
      by.y = "ensembl_gene_id"
    )
    if(nrow(df) == 0){
      plot <- ggplot() + 
        theme_void() +
        annotate("text", x = 0.5, y = 0.5, label = "NULL", size = 15, color = "black") + 
        theme(plot.background = element_rect(fill = "#ffffff"))
      fwrite(df, paste0(TEFileuserDir, "/rRNACountsPlotData.csv"))
      ggsave(filename = paste0(TEFileuserDir, "/rRNACountsPlot.png"), plot = plot, width = 9.6, height = 5.2, dpi = 250, bg = "#ffffff")
      ggplot2::ggsave(filename = paste0(TEFileuserDir, "/rRNACountsPlot.pdf"), plot = plot, width = 9.6, height = 5.2, bg = "transparent")
      return(plot)
    }
    df$gene_biotype <- gsub(".*pseudogene", "Pseudogene", df$gene_biotype)
    df$gene_biotype <- gsub("TEC", "Unknown", df$gene_biotype)
    df$gene_biotype <- gsub("IG_.*", "IG", df$gene_biotype)
    df$gene_biotype <- gsub("TR_.*", "TR", df$gene_biotype)
    df$gene_biotype <- gsub("protein_coding", "Coding", df$gene_biotype)

    countsByType <- aggregate(
      df[, colnames(countsData)],
      by = list(df$gene_biotype),
      FUN = sum
    )
    colnames(countsByType)[1] = "Gene_Type"
    df <- cbind(Gene_Type = countsByType[, 1], sweep(countsByType[-1], 2, 0.01 * colSums(countsByType[-1]), "/"))
    df <- df[which(apply(df[, -1], 1, max) > 0.5), ]

    plotData <- reshape2::melt(df, id.vars = "Gene_Type")
    plot <- ggplot2::ggplot(plotData, ggplot2::aes(x = variable, y = value, fill = Gene_Type)) +
      ggplot2::geom_bar(stat = "identity", color = "black") +
      ggplot2::labs(x = NULL, y = "% Reads", title = "% Reads by Gene type")+
      ggplot2::scale_fill_manual(values = colorTotal[1:4])

    plot <- plot +
      ggplot2::geom_text(aes(label = sprintf("%.2f", value)),
                         vjust = 0.5, size = 4, color = "black", angle = 90, hjust = - 0.1) +
      ggplot2::scale_y_continuous(expand = c(0, 0), limits=c(0, max(plotData$value)+20)) +
      ggplot2::geom_bar(stat = "identity", width = 0.6) +
      ggplot2::theme(
        legend.position = "right",
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_text(
          color = "black",
          size = 14
        ),
        axis.text.x = ggplot2::element_text(
          angle = 60,
          size = 12,
          hjust = 1,
          vjust = 1
        ),
        axis.text.y = ggplot2::element_text(
          size = 15
        ),
        plot.title = ggplot2::element_text(
          color = "black",
          size = 15,
          face = "bold",
          hjust = .5
        ),
        panel.grid = ggplot2::element_blank(),
        axis.line.x = ggplot2::element_line(color = "black"),
        axis.line.y = ggplot2::element_line(color = "black"),
        panel.background = ggplot2::element_rect(fill = "transparent", color = NA),
        plot.background = ggplot2::element_rect(fill = "transparent", color = NA)
      )
    fwrite(plotData, paste0(TEFileuserDir, "/rRNACountsPlotData.csv"))
    ggplot2::ggsave(filename = paste0(TEFileuserDir, "/rRNACountsPlot.png"), plot = plot, width = 8, height = 4.8, dpi = 250, bg = "#ffffff")
    ggplot2::ggsave(filename = paste0(TEFileuserDir, "/rRNACountsPlot.pdf"), plot = plot, width = 8, height = 4.8, bg = "transparent")
    return(plot)
  }
  #辅助函数56-dataSummary
  dataSummary <- function(codonSeqs, transcriptId, geneId, geneName) {
    temp <- table(codonSeqs)
    res = data.table(transcript_id=rep(transcriptId,length(temp)), gene_id=rep(geneId,length(temp)), gene_name=rep(geneName,length(temp)), codon=toupper(names(temp)),freq=as.vector(temp))
    return(res)
  }
  # 辅助函数57-codonFirstDataMake
  codonFirstDataMake <- function(species){
    load(paste0("/public/wwwdb/ribotoolkit/annotation/",species,".gff.rda"))
    load(paste0("/public/wwwdb/ribotoolkit/annotation/",species,".txlens.rda"))
    txlensMax <- txlens[cds_len>0]
    txlensMax <- unique(txlensMax,by="gene_id")
    txlensMax[startsWith(gene_id,"ENS"),gene_id := sub("\\.\\d+$","",gene_id)]
    geneinfo <- setDT(as.data.frame(gff))
    setnames(geneinfo, old = grep("^transcript", names(geneinfo)[sapply(geneinfo, function(col) "protein_coding" %in% col)], value = TRUE), new = "transcript_type")
    geneinfo <- geneinfo[ type=="transcript" & transcript_type == "protein_coding"]
    geneinfo <- geneinfo[,.(transcript_id,gene_id,gene_name)]
    geneinfo[startsWith(gene_id,"ENS"),gene_id := sub("\\.\\d+$","",gene_id)]
    txlens[,c('tx_id','gene_id','nexon') := NULL]
    setnames(txlens,c('tx_name'),c('transcript_id'))
    transcript_seqs <- read.fasta(paste0('/public/wwwdb/ribotoolkit/mRNA/',species,'.txdb.fa'), seqtype = 'DNA', as.string = T)
    transcript_seqs <- data.table(transcript_id=names(transcript_seqs), seq=as.character(transcript_seqs))
    transcript_seqs <- merge(x=transcript_seqs,y=txlens[,c('transcript_id','cds_len','utr5_len','tx_len')], by='transcript_id', all.x=T)
    transcript_seqs <- transcript_seqs[geneinfo, on="transcript_id"]
    transcript_seqs <- transcript_seqs[cds_len%%3==0]
    transcript_seqs[,'cds' := mapply(function(seq,i,j) {substr(seq,i+1,i+j)}, seq, utr5_len, cds_len)]
    transcript_seqs[,'codon_seqs' := mapply(function(seq,i,j) {strsplit(gsub("([[:alnum:]]{3})", "\\1 ", substr(seq,i+1,i+j)), ' ')[[1]]}, seq, utr5_len, cds_len)]
    transcript_seqs[,c('seq','cds') := NULL]
    transcript_seqs <- transcript_seqs[transcript_id %in% txlensMax$tx_name]
    shinyEnv$transcriptSeqs <- transcript_seqs
    deg <- as.data.table(read.csv(paste0(TEFileuserDir,"/normalcount.csv"), check.names = FALSE))
    forcodon <- copy(deg)
    forcodon <- forcodon[,.(gene_id=GeneID, input1, input2, TE_A1, TE_A2)]
    deg <- deg[,.(gene_id=GeneID,gene_name=gene_name,logFC=log2FoldChange,pvalue_deg=pvalue,qvalue_deg=padj)]
    deg[,updown := "None"]
    deg[logFC >= log2(2) & pvalue_deg < 0.05, updown := "Up"]
    deg[logFC <= -log2(2) & pvalue_deg < 0.05, updown := "Down"]
    deg[,log2pvalue:=-log2(pvalue_deg)]
    deg <- deg[gene_name != ""]
    deg$updown <- factor(deg$updown, levels = c("Up","None","Down"))
    transcript_seqs_obj <- transcript_seqs[gene_name %in% deg$gene_name]
    cct <- mapply(dataSummary, transcript_seqs$codon_seqs, transcript_seqs$transcript_id, transcript_seqs$gene_id, transcript_seqs$gene_name, SIMPLIFY = FALSE)
    rl <- rbindlist(cct)
    rl <- rl[!codon %in% c("TAG","TAA","TGA")]
    rl[,sumv:=sum(freq),by='transcript_id']
    rl[,perc:=round(freq/sumv,5),by='transcript_id']
    rl[,freq2:=sum(freq),by='codon']
    rl[,total:=sum(freq)]
    rl[,per1k:=(freq*1000)/sumv]
    rl$gene_id <- toupper(rl$gene_id)
    fwrite(deg, paste0(TEFileuserDir,"/deg.csv"))
    fwrite(geneinfo, paste0(TEFileuserDir,"/geneinfo.csv"))
    fwrite(forcodon, paste0(TEFileuserDir,"/forcodon.csv"))
    fwrite(rl,paste0(TEFileuserDir,"/rl.csv"))
  }
  #辅助函数58-plotUsage1
  plotUsage1 <- function(obj) {
    res <- as.data.table(read.csv(paste0(TEFileuserDir, "/res1.csv"), check.names = FALSE))
    dfp =  res[codon==obj,]
    corTest1 <- cor.test(dfp$perc, dfp$input1, method = "pearson")
    corValue1 <- round(corTest1$estimate, 3)
    pvalue1 <- signif(corTest1$p.value, 2)
    x_offset1 <- 0.17 * (max(dfp$perc) - min(dfp$perc))
    y_offset1 <- 0.04 * (max(dfp$input1) - min(dfp$input1))
    codonPerc1 <- ggscatter(dfp,
                            x = 'perc',
                            y = 'input1',
                            size = 2,
                            alpha = 0.6,
                            add = 'reg.line',
                            add.params = list(color = "#00AFBB",
                                              fill = "lightgray", size = 1),
                            conf.int = TRUE) +
      xlab("CBI") +
      ylab("Translation efficiency") +
      ggtitle(paste0(obj, "-input1")) +
      geom_density2d() +
      geom_text(aes(x = max(dfp$perc)-x_offset1, y = max(dfp$input1) - y_offset1,
                    label = paste("R = ", corValue1, " P =", pvalue1)),
                size = 2.5) +
      theme(axis.title.x = element_text(size = 8),
            axis.title.y = element_text(size = 8),
            axis.text.x = element_text(size = 8),
            axis.text.y = element_text(size = 8),
            plot.title = element_text(size = 9, hjust = 0.5))
    ggsave(paste0(TEFileuserDir,"/",obj,".input1.perc.png"), width = 4, height = 3, plot = codonPerc1)
    ggsave(paste0(TEFileuserDir,"/",obj,".input1.perc.pdf"), width = 4, height = 3, plot = codonPerc1)
    corTest2 <- cor.test(dfp$perc, dfp$input2, method = "pearson")
    corValue2 <- round(corTest2$estimate, 3)
    pvalue2 <- signif(corTest2$p.value, 2)
    x_offset2 <- 0.17 * (max(dfp$perc) - min(dfp$perc))
    y_offset2 <- 0.04 * (max(dfp$input2) - min(dfp$input2))
    codonPerc2 <- ggscatter(dfp,
                            x = 'perc',
                            y = 'input2',
                            size = 2,
                            alpha = 0.6,
                            add = 'reg.line',
                            add.params = list(color = "#00AFBB",
                                              fill = "lightgray", size = 1),
                            conf.int = TRUE) +
      xlab("CBI") +
      ylab("Translation efficiency") +
      ggtitle(paste0(obj, "-input2")) +
      geom_density2d() +
      geom_text(aes(x = max(dfp$perc)-x_offset2, y = max(dfp$input2) - y_offset2,
                    label = paste("R = ", corValue2, " P =", pvalue2)),
                size = 2.5) +
      theme(axis.title.x = element_text(size = 8),
            axis.title.y = element_text(size = 8),
            axis.text.x = element_text(size = 8),
            axis.text.y = element_text(size = 8),
            plot.title = element_text(size = 9, hjust = 0.5))

    ggsave(paste0(TEFileuserDir,"/",obj,".input2.perc.png"), width = 4, height = 3, plot = codonPerc2)
    ggsave(paste0(TEFileuserDir,"/",obj,".input2.perc.pdf"), width = 4, height = 3, plot = codonPerc2)
  }
  #辅助函数59-codonPerc
  codonPerc <- function(){
    rl <- as.data.table(read.csv(paste0(TEFileuserDir,"/rl.csv"), check.names = FALSE))
    forcodon <- as.data.table(read.csv(paste0(TEFileuserDir,"/forcodon.csv"), check.names = FALSE))
    res <- merge(rl ,forcodon, by="gene_id",all.y=T)
    res <- res[!is.na(freq)]
    fwrite(res, paste0(TEFileuserDir, "/res1.csv"))
    req(file.exists(paste0(TEFileuserDir, "/res1.csv")))
    objcodons <- input$codonSelect
    parallel::mclapply(objcodons, plotUsage1, mc.cores = 6)
    df <- data.frame(
      Name = c("Alice"),
      Age = c(25)
    )
    fwrite(df, paste0(TEFileuserDir, "/codonPercComplete.csv"))
    shinyjs::hide("loader-overlay10")
    shinyjs::runjs('$(".RiboTE-working-btn-codon-input").addClass("RiboTE-working-hidden");')
  }
  #辅助函数60-plotUsage2
  plotUsage2 <- function(obj) {
    deg <- as.data.table(read.csv(paste0(TEFileuserDir,"/deg2.csv"), check.names = FALSE))
    rl <- as.data.table(read.csv(paste0(TEFileuserDir, "/rl2.csv"), check.names = FALSE))
    dfp =  rl[codon==obj,]
    dfp[,perc:=round(perc,3)]
    dfp <- merge(dfp,deg,all.y=T,by="gene_name")
    dfp <- dfp[!is.na(perc)]
    my_comparisons <- list(c("Up", "None"),c("Down", "None"))
    p <- ggboxplot(dfp,
                   x = 'updown',
                   y = 'perc',
                   color = 'updown',
                   palette = "jco") +
      ggtitle(paste0(obj, "-Specific")) +
      yscale("log2") +
      labs(x = "",
           y = "Codon frequency") +
      theme(axis.text.x = element_text(angle = 0,
                                       hjust = 0.5, vjust = 0.5, size = 8),
            legend.title = element_blank(),
            plot.title = element_text(hjust = 0.5, size = 9),
            axis.title.x = element_text(size = 8),
            axis.title.y = element_text(size = 8),
            axis.text.y = element_text(size = 8)) +
      stat_compare_means(comparisons = my_comparisons,
                         label = "p.signif")
    ggsave(paste0(TEFileuserDir,"/",obj,".specific.boxplot.png"), width = 5, height = 5, plot = p)
    ggsave(paste0(TEFileuserDir,"/",obj,".specific.boxplot.pdf"), width = 5, height = 5, plot = p)
  }
  #辅助函数61-codonSpecific
  codonSpecific <- function(){
    codonspecific(FALSE)
    deg <- as.data.table(read.csv(paste0(TEFileuserDir,"/deg.csv"), check.names = FALSE))
    rl <- as.data.table(read.csv(paste0(TEFileuserDir, "/rl.csv"), check.names = FALSE))
    deg[,gene_id:=NULL]
    msd <- rl[,.(meanvalue=mean(per1k),std=sd(per1k)),by=c("codon")]
    rl <- merge(rl,msd,by="codon")
    rl[,zscore := mapply(function(q,m,n) {(q-m)/n}, per1k, meanvalue, std)]
    rl[, pvalue := mapply(function(freq,sumv,freq2,total) phyper(freq, freq2, total-freq2, sumv, lower.tail=F), freq, sumv, freq2, total)]
    fwrite(rl, paste0(TEFileuserDir,"/rl2.csv"))
    fwrite(deg, paste0(TEFileuserDir,"/deg2.csv"))
    objcodons <- input$codonSelect
    parallel::mclapply(objcodons, plotUsage2, mc.cores = 1)
    df <- data.frame(
      Name = c("Alice"),
      Age = c(25)
    )
    fwrite(df, paste0(TEFileuserDir, "/codonPercComplete2.csv"))
    shinyjs::hide("loader-overlay10")
    shinyjs::runjs('$(".RiboTE-working-btn-codon-specific").addClass("RiboTE-working-hidden");')
    codonspecific(TRUE)
  }
  #辅助函数62-percPer1kDataMake
  percPer1kDataMake <- function(){
    rl <- as.data.table(read.csv(paste0(TEFileuserDir, "/rl2.csv"), check.names = FALSE))
    deg <- as.data.table(read.csv(paste0(TEFileuserDir,"/deg2.csv"), check.names = FALSE))
    objcodons <- input$codonSelect
    objCodonStat <- rl[codon %in% objcodons]
    objCodonStat2 <- objCodonStat[,sum(per1k),by=c("gene_name")]
    setnames(objCodonStat2,"V1","per1k")
    objCodonStat2 <- merge(objCodonStat2,deg,all.y=T,by="gene_name")
    objCodonStat2 <- objCodonStat2[!is.na(per1k)]
    objCodonStat2[,fcAndPvalue:=logFC*(-log2(pvalue_deg))]
    objCodonStat2_up <- objCodonStat2[updown=="Up"]
    objCodonStat2_down <- objCodonStat2[updown=="Down"]
    objCodonStat3 <- objCodonStat[,sum(perc),by=c("gene_name")]
    setnames(objCodonStat3,"V1","perc")
    objCodonStat3 <- merge(objCodonStat3,deg,all.y=T,by="gene_name")
    objCodonStat3 <- objCodonStat3[!is.na(perc)]
    objCodonStat3_up <- objCodonStat3[updown=="Up"]
    objCodonStat3_down <- objCodonStat3[updown=="Down"]
    fwrite(objCodonStat3_up, paste0(TEFileuserDir,"/objCodonStat3_up.csv"))
    fwrite(objCodonStat3_down, paste0(TEFileuserDir,"/objCodonStat3_down.csv"))
    fwrite(objCodonStat2_up, paste0(TEFileuserDir,"/objCodonStat2_up.csv"))
    fwrite(objCodonStat2_down, paste0(TEFileuserDir,"/objCodonStat2_down.csv"))
    fwrite(objCodonStat3, paste0(TEFileuserDir,"/objCodonStat3.csv"))
    fwrite(objCodonStat2, paste0(TEFileuserDir,"/objCodonStat2.csv"))
    unlink(paste0(TEFileuserDir, "/codonPercComplete3.csv"))
  }
  #辅助函数63-codonRatioData
  codonRatioData <- function(){
    rl <- as.data.table(read.csv(paste0(TEFileuserDir, "/rl2.csv"), check.names = FALSE))
    deg <- as.data.table(read.csv(paste0(TEFileuserDir,"/deg2.csv"), check.names = FALSE))
    rl <- merge(rl,deg,all.y=T,by="gene_name")
    rl <- rl[!is.na(codon)]
    fwrite(rl, paste0(TEFileuserDir, "/rl3.csv"))
    allcodons <- unique(rl$codon)
    allcodons <- allcodons[!allcodons %in% c("TAG","TAA","TGA")]
    res_up <- data.table()
    res_down <- data.table()
    for(i in allcodons) {
      objrl <- rl[pvalue<0.01 & codon == i]
      nd <- nrow(objrl[updown=="Down"])
      nu <- nrow(objrl[updown=="Up"])
      if(nd>0) {
        udRatio=nu/nd
        upratio=nu/nrow(objrl)
        res_up <- rbind(res_up,data.table(codon=i,udRatio=udRatio,upratio=upratio,nu=nu))
      }
      if(nu>0) {
        duRatio=nd/nu
        downratio=nd/nrow(objrl)
        res_down <- rbind(res_down,data.table(codon=i,duRatio=duRatio,downratio=downratio,nu=nd))
      }
    }
    objcodons <- input$codonSelect
    res_up[,type:="Other"]
    res_up[codon %in% objcodons,type:="Objective"]
    res_down[,type:="Other"]
    res_down[codon %in% objcodons,type:="Objective"]
    fwrite(res_down, paste0(TEFileuserDir, "/res_down.csv"))
    fwrite(res_up, paste0(TEFileuserDir, "/res_up.csv"))
    unlink(paste0(TEFileuserDir, "/codonPercComplete4.csv"))
    return(objcodons)
  }
  #辅助函数64-dendrogramData
  dendrogramData <- function(){
    rl <- as.data.table(read.csv(paste0(TEFileuserDir, "/rl2.csv"), check.names = FALSE))
    testt <- as.data.frame(dcast(rl, gene_id~codon,value.var="zscore",fun.aggregate=sum))
    rownames(testt) <- testt[,1]
    testt <- testt[,-1]
    objcodons <- input$codonSelect
    highlight <- objcodons
    fwrite(testt, paste0(TEFileuserDir, "/testt.csv"))
    unlink(paste0(TEFileuserDir, "/codonPercComplete5.csv"))
    unlink(paste0(TEFileuserDir, "/*.dendrogram.png"))

  }
  #辅助函数65-distributionData
  distributionData <- function(){
    rl <- as.data.table(read.csv(paste0(TEFileuserDir, "/rl3.csv"), check.names = FALSE))
    objcodons <- input$codonSelect
    objCodonStat <- rl[pvalue<0.01 & codon %in% objcodons]
    objCodonStat2 <- rl[codon %in% objcodons]
    objCodonStat <- unique(objCodonStat,by="gene_name")
    nd <- nrow(objCodonStat[updown=="Down"])
    nu <- nrow(objCodonStat[updown=="Up"])
    nonObjCodonStat <- rl[pvalue>=0.01 & (!codon %in% objcodons)]
    nonObjCodonStat <- unique(nonObjCodonStat,by="gene_name")
    tempForPlot <- objCodonStat2[,sum(per1k),by=c("gene_name")]
    objCodonStat2[,type:="other"]
    objCodonStat2[pvalue < 0.01,type:="enriched"]
    objCodonStat2[,fcAndPvalue:=logFC*(-log2(qvalue_deg))]

    objupdown <- input$distributionUpDownSelect
    if(objupdown == "Up") {
      objupratio <-nu/nd
      objudratio <-nu/nrow(objCodonStat)
      res <- data.table()
      for(i in seq(10000)) {
        temp <- nonObjCodonStat[sample(.N, nrow(objCodonStat), FALSE)]
        nd <- nrow(temp[updown=="Down"])
        nu <- nrow(temp[updown=="Up"])
        if(objupdown == "Up") {
          if(nd>0) {
            res <- rbind(res,data.table(rep=i,duRatio=nu/nd,udratio=nu/nrow(temp)))
          }
        }
      }
      obs_pval1 <- 2*mean(res$udratio>=objudratio)
      obs_pval2 <- 2*mean(res$duRatio>=objupratio)
      res2 <- data.table()
      for(i in seq(0,max(objCodonStat2$per1k-5),5)) {
        temp <- objCodonStat2[per1k>=i & per1k <i+5]
        nd <- nrow(temp[updown=="Down"])
        nu <- nrow(temp[updown=="Up"])
        if(objupdown == "Up") {
          if(nd>0) {
            res2 <- rbind(res2,data.table(rep=i,duRatio=nu/nd,udratio=nu/nrow(temp)))
          }
        }
      }
    }
    if(objupdown == "Down"){
      objupratio <-nd/nu
      objudratio <-nd/nrow(objCodonStat)
      res <- data.table()
      for(i in seq(10000)) {
        temp <- nonObjCodonStat[sample(.N, nrow(objCodonStat), FALSE)]
        nd <- nrow(temp[updown=="Down"])
        nu <- nrow(temp[updown=="Up"])
        if(objupdown == "Down") {
          if(nu>0) {
            res <- rbind(res,data.table(rep=i,duRatio=nd/nu,udratio=nd/nrow(temp)))
          }
        }
      }
      obs_pval1 <- 2*mean(res$udratio>=objudratio)
      obs_pval2 <- 2*mean(res$duRatio>=objupratio)
      res2 <- data.table()
      for(i in seq(0,max(objCodonStat2$per1k-5),5)) {
        temp <- objCodonStat2[per1k>=i & per1k <i+5]
        nd <- nrow(temp[updown=="Down"])
        nu <- nrow(temp[updown=="Up"])
        if(objupdown == "Down")  {
          if(nu>0) {
            res2 <- rbind(res2,data.table(rep=i,duRatio=nd/nu,udratio=nd/nrow(temp)))
          }
        }
      }
    }
    return(list(res, objupdown, obs_pval1, obs_pval2, objudratio, objupratio, res2))
  }
  #辅助函数66-xtailNormalCount
  xtailNormalCount <- function(input, rpf, condition, minCounts, nMinSamplesCount){
    countdata <- cbind(input, rpf)
    countdata <- countdata[ which( apply( cpm(DGEList(counts = countdata)), 1,
                                          function(y) sum(y >= minCounts)) >= nMinSamplesCount ) , ]
    sizes <- DESeq2::estimateSizeFactorsForMatrix(countdata)
    normalcount <- countdata / do.call(rbind, rep(list(sizes), nrow(countdata)))
    normalcount = setDT(normalcount,keep.rownames = TRUE)
    setnames(normalcount,"rn","GeneID")
    input <- input[apply(input, 1, function(row) all(row != 0)), ]
    rpf <- rpf[apply(rpf, 1, function(row) all(row != 0)), ]
    commonGenes <- intersect(rownames(input), rownames(rpf))
    input <- input[commonGenes, ]
    rpf <- rpf[commonGenes, ]
    results <- xtail(input, rpf, condition, threads = 5, bins = 1000)
    res <- resultsTable(results)
    res <- setDT(as.data.frame(res),keep.rownames=T)
    res <- res[,.(rn,log2FC_TE_final,pvalue_final,pvalue.adjust)]
    setnames(res,c("GeneID","log2FoldChange","pvalue","padj"))
    normalcount = merge(normalcount,res,by="GeneID",all.y=T)
    return(normalcount)
  }
  #辅助函数67-distributionData2
  distributionData2 <- function(){
    rl <- as.data.table(read.csv(paste0(TEFileuserDir, "/rl3.csv"), check.names = FALSE))
    objcodons <- input$codonSelect
    objCodonStat <- rl[pvalue<0.01 & codon %in% objcodons]
    objCodonStat2 <- rl[codon %in% objcodons]
    objCodonStat <- unique(objCodonStat,by="gene_name")
    shinyEnv$objCodonStat <- objCodonStat
    nd <- nrow(objCodonStat[updown=="Down"])
    nu <- nrow(objCodonStat[updown=="Up"])
    nonObjCodonStat <- rl[pvalue>=0.01 & (!codon %in% objcodons)]
    nonObjCodonStat <- unique(nonObjCodonStat,by="gene_name")
    tempForPlot <- objCodonStat2[,sum(per1k),by=c("gene_name")]
    objCodonStat2[,type:="other"]
    objCodonStat2[pvalue < 0.01,type:="enriched"]
    objCodonStat2[,fcAndPvalue:=logFC*(-log2(qvalue_deg))]
    objupdown <- input$codonZscoreUpDownSelect
    if(objupdown == "Up") {
      objupratio <-nu/nd
      objudratio <-nu/nrow(objCodonStat)
      res <- data.table()
      for(i in seq(10000)) {
        temp <- nonObjCodonStat[sample(.N, nrow(objCodonStat), FALSE)]
        nd <- nrow(temp[updown=="Down"])
        nu <- nrow(temp[updown=="Up"])
        if(objupdown == "Up") {
          if(nd>0) {
            res <- rbind(res,data.table(rep=i,duRatio=nu/nd,udratio=nu/nrow(temp)))
          }
        }
      }
      obs_pval1 <- 2*mean(res$udratio>=objudratio)
      obs_pval2 <- 2*mean(res$duRatio>=objupratio)
      res2 <- data.table()
      for(i in seq(0,max(objCodonStat2$per1k-5),5)) {
        temp <- objCodonStat2[per1k>=i & per1k <i+5]
        nd <- nrow(temp[updown=="Down"])
        nu <- nrow(temp[updown=="Up"])
        if(objupdown == "Up") {
          if(nd>0) {
            res2 <- rbind(res2,data.table(rep=i,duRatio=nu/nd,udratio=nu/nrow(temp)))
          }
        }
      }
    }
    if(objupdown == "Down"){
      objupratio <-nd/nu
      objudratio <-nd/nrow(objCodonStat)
      res <- data.table()
      for(i in seq(10000)) {
        temp <- nonObjCodonStat[sample(.N, nrow(objCodonStat), FALSE)]
        nd <- nrow(temp[updown=="Down"])
        nu <- nrow(temp[updown=="Up"])
        if(objupdown == "Down") {
          if(nu>0) {
            res <- rbind(res,data.table(rep=i,duRatio=nd/nu,udratio=nd/nrow(temp)))
          }
        }
      }
      obs_pval1 <- 2*mean(res$udratio>=objudratio)
      obs_pval2 <- 2*mean(res$duRatio>=objupratio)
      res2 <- data.table()
      for(i in seq(0,max(objCodonStat2$per1k-5),5)) {
        temp <- objCodonStat2[per1k>=i & per1k <i+5]
        nd <- nrow(temp[updown=="Down"])
        nu <- nrow(temp[updown=="Up"])
        if(objupdown == "Down")  {
          if(nu>0) {
            res2 <- rbind(res2,data.table(rep=i,duRatio=nd/nu,udratio=nd/nrow(temp)))
          }
        }
      }
    }
    return(list(res, objupdown, obs_pval1, obs_pval2, objudratio, objupratio, res2))
  }
  #辅助函数68-distributionData3
  distributionData3 <- function(){
    rl <- as.data.table(read.csv(paste0(TEFileuserDir, "/rl3.csv"), check.names = FALSE))
    objcodons <- input$codonSelect
    objCodonStat2 <- rl[codon %in% objcodons]
    objCodonStat2[,type:="other"]
    objCodonStat2[pvalue < 0.01,type:="enriched"]
    objCodonStat2[,fcAndPvalue:=logFC*(-log2(qvalue_deg))]
    return(objCodonStat2)
  }
  #辅助函数69-dataSummary2
  dataSummary2 <- function(codon_seqs, transcript_id, gene_id, gene_name, runLen) {
    codon_seqs <- unlist(codon_seqs)
    nl <- length(codon_seqs)
    temp <- data.table()
    for(i in seq(runLen)) {
      temp <- cbind(temp,codon_seqs[seq(i,nl-runLen+i,1)])
    }
    colheads <- paste0("v",seq(runLen))
    setnames(temp,colheads)
    temp[, codonRun := Reduce(function(...) paste(..., sep = ""), .SD), .SDcols = colheads]
    temp <- table(temp$codonRun)
    res = data.table(transcript_id=rep(transcript_id,length(temp)), gene_id=rep(gene_id,length(temp)), gene_name=rep(gene_name,length(temp)), codon=toupper(names(temp)),freq=as.vector(temp))
    return(res)
  }
  #辅助函数70-detectDelimiter
  detectDelimiter <- function(file_path) {
    preview <- readLines(file_path, n = 5)
    delimiters <- c("\t", ",", ";", " ")
    delimiterCount <- sapply(delimiters, function(delim) {
      sum(sapply(preview, function(line) {
        length(strsplit(line, delim)[[1]]) > 1
      }))
    })
    bestDelim <- delimiters[which.max(delimiterCount)]
    return(bestDelim)
  }
  #####基本数据加载----
  baseData <- function(){
    if(!is.null(input$geneMatrix) && input$demoData > 0){
      return(NULL)
    }
    if(!is.null(input$geneMatrix) && input$demoData == 0){
      countframe <- input$geneMatrix$datapath
      delimiter <- detectDelimiter(file_path = countframe)
      countframe <- read.table(countframe,header = T, sep = delimiter, check.names = FALSE)
      countframe <- geneMatrixPreprocessing(countframe)
      selectOrgId <- RiboTE_Org$id[RiboTE_Org$species == input$selectOrg]
      selectOrgAcronyms <- RiboTE_Org$acronyms[RiboTE_Org$species == input$selectOrg]
      species <- selectOrgAcronyms
      groupnames <- strsplit("Control#Case", "#")[[1]]
      fvalue <- input$fvalue
      pCutoff <- input$pCutoff
      pCutoffType <- input$pCutoff
      inputnames <- input$inputnames
      inputnames2 <- gsub("#", ",",inputnames)
      inputnames2 <- strsplit(inputnames2, ",")[[1]]
      inputnames2 <- trimws(inputnames2)
      rpfnames <- input$rpfnames
      rpfnames2 <- gsub("#", ",",rpfnames)
      rpfnames2 <- strsplit(rpfnames2, ",")[[1]]
      rpfnames2 <- trimws(rpfnames2)
      TEsampleName <- paste0("TE.", rpfnames2, ".", inputnames2)
      gff <- fread(paste0("/public/wwwdb/ribotoolkit/annotation/",species,".gff.txt"))
      gff <- gff[type=="gene"]
      setnames(gff,"gene_id","GeneID")
      gff[,GeneID := gsub("\\.\\d+$","",GeneID)]
      baseData <- list(countframe, species, inputnames2, rpfnames2, gff, fvalue, groupnames, pCutoff, pCutoffType, TEsampleName, inputnames, rpfnames, selectOrgId)
      allOrgData <- getAllOrgData()
      names(allOrgData)[1] <- "orgInfo"
      orgInfoAddData <- read.csv("/public/shiny/RiboTE/TEShinyData/orgInfo_add.csv")
      allOrgData$orgInfo <- rbind(orgInfoAddData, allOrgData$orgInfo)
      conversionInfo <- conversionInfoMake(geneMatrix = countframe, allOrgData = allOrgData, selectOrg = selectOrgId)
      write_json(baseData, paste0(TEFileuserDir,"/baseData.json"), pretty = TRUE)
      write_json(conversionInfo, paste0(TEFileuserDir,"/orgData.json"), pretty = TRUE)
      return(baseData)
    }
    if(is.null(input$geneMatrix) && input$demoData > 0){
      delimiter <- detectDelimiter(file_path = "/public/shiny/RiboTE/TEShinyData/all.count.txt")
      countframe <- read.table("/public/shiny/RiboTE/TEShinyData/all.count.txt",header = T, sep = delimiter, check.names = FALSE)
      countframe <- geneMatrixPreprocessing(countframe)
      species <- "hg38"
      groupnames <- strsplit("Control#Case", "#")[[1]]
      fvalue <- input$fvalue
      pCutoff <- input$pCutoff
      pCutoffType <- input$pCutoff
      inputnames <- "RNA.WT1,RNA.WT2#RNA.KO1,RNA.KO2"
      inputnames2 <- gsub("#", ",",inputnames)
      inputnames2 <- strsplit(inputnames2, ",")[[1]]
      inputnames2 <- trimws(inputnames2)
      rpfnames <- "RPF.WT1,RPF.WT2#RPF.KO1,RPF.KO2"
      rpfnames2 <- gsub("#", ",",rpfnames)
      rpfnames2 <- strsplit(rpfnames2, ",")[[1]]
      rpfnames2 <- trimws(rpfnames2)
      TEsampleName <- paste0("TE.", rpfnames2, ".", inputnames2)
      gff <- fread(paste0("/public/wwwdb/ribotoolkit/annotation/",species,".gff.txt"))
      gff <- gff[type=="gene"]
      setnames(gff,"gene_id","GeneID")
      gff[,GeneID := gsub("\\.\\d+$","",GeneID)]
      selectOrgId <- 99
      baseData <- list(countframe, species, inputnames2, rpfnames2, gff, fvalue, groupnames, pCutoff, pCutoffType, TEsampleName, inputnames, rpfnames, selectOrgId)
      allOrgData <- getAllOrgData()
      names(allOrgData)[1] <- "orgInfo"
      conversionInfo <- conversionInfoMake(geneMatrix = countframe, allOrgData = allOrgData, selectOrg = 99)
      write_json(baseData, paste0(TEFileuserDir,"/baseData.json"), pretty = TRUE)
      write_json(conversionInfo, paste0(TEFileuserDir,"/orgData.json"), pretty = TRUE)
      return(baseData)
    }
  }
  ####Riborex 计算----
  riborexNormalCount <- function(countframe, inputnames, rpfnames, gff, species, fvalue, groupnames, pCutoff, pCutoffType, TEtools, minCounts, nMinSamplesCount){
    input = countframe[,c("GeneID",inputnames)]
    input <- as.data.frame(input)
    rownames(input) <- input[,1]
    input <- input[,-1]
    rpf = countframe[,c("GeneID",rpfnames)]
    rpf <- as.data.frame(rpf)
    rownames(rpf) <- rpf[,1]
    rpf <- rpf[,-1]
    condition = c(rep("G1",ceiling(length(inputnames)/2)), rep("G2",floor(length(inputnames)/2)))
    if (TEtools == "riborex"){
      normalcount <- DESeq2Rex(input, rpf, condition, condition)
    }
    else{
      normalcount <- xtailNormalCount(input, rpf, condition, minCounts, nMinSamplesCount)
    }
    gff2 <- copy(gff)
    gff2$GeneID <- toupper(gff2$GeneID)
    gff2 <- as.data.table(gff2)
    if (!"gene_name" %in% colnames(gff2)) {
      gff2[, gene_name := "unknown"]
    }
    normalcount <- merge(gff2[ , c("GeneID","gene_name")], normalcount,by="GeneID", all.y=TRUE)
    normalcount[is.na(gene_name), gene_name := "unknown"]
    riborexResult <- list(input, condition, species, gff, inputnames, fvalue, normalcount, groupnames, rpfnames, pCutoff, pCutoffType)
    return(riborexResult)
  }
  rawCountMake <- function(countframe, inputnames, rpfnames, gff, species, fvalue, groupnames, pCutoff, pCutoffType){
    input = countframe[,c("GeneID",inputnames)]
    input <- as.data.frame(input)
    rownames(input) <- input[,1]
    input <- input[,-1]
    rpf = countframe[,c("GeneID",rpfnames)]
    rpf <- as.data.frame(rpf)
    rownames(rpf) <- rpf[,1]
    rpf <- rpf[,-1]
    condition = c(rep("G1",ceiling(length(inputnames)/2)), rep("G2",floor(length(inputnames)/2)))
    rawCountPre(input, rpf, condition, condition)
  }
  ####HeatmapData 计算----
  heatmapData <- function(){
    req(file.exists(paste0(TEFileuserDir,"/normalcount.csv")))
    runjs('$(".mask").show();')
    toggleClass(selector = ".RiboTE-working-btn-clustering", class = "RiboTE-working-hidden")
    shinyjs::show("loader-overlay3")
    normalcount <- read.csv(paste0(TEFileuserDir,"/normalcount.csv"),header = T,row.names = 1, check.names = FALSE)
    baseData <- fromJSON(paste0(TEFileuserDir,"/baseData.json"))
    inputnames <- baseData[[3]]
    rpfnames <- baseData[[4]]
    TEsampleName <- baseData[[10]]
    heatmapDataType <- input$heatmapDataType
    heatmapDataType <- switch(heatmapDataType,
                              "INPUT" = inputnames,
                              "RPF" = rpfnames,
                              "TE" = TEsampleName)
    heatmapData <- normalcount[, heatmapDataType]
    nGenesMax <- input$nGenes
    heatmapData <- heatmapData[order(-apply(heatmapData[, 1:dim(heatmapData)[2]], 1, sd)), ]
    geneCentering <- input$geneCentricity
    geneNormalize <- FALSE
    sampleCentering <- FALSE
    sampleNormalize <- FALSE
    if (nGenesMax > nrow(heatmapData)) {
      nGenesMax <- nrow(heatmapData)
    } else if (nGenesMax < 10) {
      nGenesMax <- 10
    }
    if (geneCentering) {
      heatmapData <- heatmapData[1:nGenesMax, ] - apply(heatmapData[1:nGenesMax, ], 1, mean)
    }
    if (geneNormalize) {
      heatmapData <- heatmapData / apply(heatmapData, 1, sd)
    }
    heatmapData <- scale(heatmapData, center = sampleCentering, scale = sampleNormalize)
    if (geneCentering) {
      heatmapDataTotal <- list(heatmapData,heatmapDataType)
      return(heatmapDataTotal)
    } else {
      heatmapData <- heatmapData[1:nGenesMax, ]
      heatmapDataTotal <- list(heatmapData,heatmapDataType)
      return(heatmapDataTotal)
    }
  }
  ####Heatmap-main Plot----
  heatmapPlot <- function(heatmapData, heatmapDataType){
    heatmapData <- as.data.frame(heatmapData)
    heatmapCutoff <- input$heatmapCutoff
    cutoff <- median(unlist(heatmapData)) + heatmapCutoff * sd(unlist(heatmapData))
    heatmapData[heatmapData > cutoff] <- cutoff
    cutoff <- median(unlist(heatmapData)) - heatmapCutoff * sd(unlist(heatmapData))
    heatmapData[heatmapData < cutoff] <- cutoff
    shinyEnv$heatmapData <- heatmapData
    heatmapColorSelect <- input$heatmapColorSeries
    heatmapColorSelect <- unlist(strsplit(heatmapColorSelect, "-"))
    if (min(heatmapData) < 0) {
      colSeries <- circlize::colorRamp2(c(min(heatmapData), 0, max(heatmapData)), heatmapColorSelect)
    } else {
      colSeries <- circlize::colorRamp2(c(min(heatmapData), median(heatmapData), max(heatmapData)),heatmapColorSelect)
    }
    groups <- heatmapDataType
    heatAnn <- NULL
    selectFactorsHeatmap <- "Names"
    if (!is.null(selectFactorsHeatmap)) {
      if (selectFactorsHeatmap != "All factors") {
        if (selectFactorsHeatmap == "Names") {
          groups <- heatmapDataType
        }
        groupsColors <- ggColorHue(length(unique(groups)))
        heatAnn <- ComplexHeatmap::HeatmapAnnotation(
          Group = groups,
          col = list(Group = setNames(groupsColors, unique(groups))),
          annotation_legend_param = list(
            Group = list(nrow = 1, title = NULL)
          ),
          show_annotation_name = list(Group = FALSE),
          show_legend = FALSE
        )
      }
    }
    heatFunction <- input$heatFunction
    distFuns <- distFunctions()
    dist_choices <- setNames(
      1:length(distFuns),
      names(distFuns)
    )
    distFunction <- input$distanceCalculation
    distFunction <- switch(distFunction,
                           "Pearson" = 1,
                           "Euclidean" = 2,
                           "Absolute_Pearson" = 3)
    p1 <- ComplexHeatmap::Heatmap(
      heatmapData,
      name = "Expression",
      col = colSeries,
      clustering_method_rows = heatFunction,
      clustering_method_columns = heatFunction,
      clustering_distance_rows = function(x) {
        distFuns[[as.numeric(distFunction)]](x)
      },
      clustering_distance_columns = function(x) {
        distFuns[[as.numeric(distFunction)]](x)
      },
      cluster_rows = TRUE,
      cluster_columns = TRUE,
      show_column_dend = F,
      show_row_dend = T,
      row_dend_side = "left",
      row_dend_width = grid::unit(1, "cm"),
      top_annotation = heatAnn,
      show_row_names = FALSE,
      show_column_names = FALSE,
      heatmap_legend_param = list(
        direction = "horizontal",
        legend_width = grid::unit(6, "cm"),
        title = "Color Key",
        title_position = "topcenter"
      )
    )
    p1 <- draw(p1, heatmap_legend_side = "bottom", annotation_legend_side = "bottom")
    shinyEnv$heatMain <- p1
    heatmapData <- cbind(Gene_ID = rownames(heatmapData), heatmapData)
    fwrite(heatmapData, paste0(TEFileuserDir, "/heatmap_data.csv"))

    CairoPNG(paste0(TEFileuserDir, "/heatmap_main.png"), width = 800, height = 500)
    draw(p1)
    dev.off()

    pdf(paste0(TEFileuserDir, "/heatmap_main.pdf"), width = 8, height = 5)
    draw(p1)
    dev.off()

    grid::grid.newpage()
    draw(p1)
    return(p1)
  }
  ####heatmap-sub Plot----
  heatsub <- function(enterGeneIDs){
    max_gene_ids <- 2000
    if(input$subHeatmapDisplayMode == "Select area"){
      lt <- InteractiveComplexHeatmap::getPositionFromBrush(input$heatmapBrush)
      pos1 <- lt[[1]]
      pos2 <- lt[[2]]
      pos <- InteractiveComplexHeatmap::selectArea(
        shinyEnv$heatMain,
        mark = FALSE,
        pos1 = pos1,
        pos2 = pos2,
        verbose = FALSE,
        ht_pos = shinyEnv$heatPosMain
      )
      columnIndex <- unlist(pos[1, "column_index"])
      row_index <- unlist(pos[1, "row_index"])
    }
    if(input$subHeatmapDisplayMode == "Base on GeneID"){
      row_index <- which(rownames(shinyEnv$heatmapData) %in% enterGeneIDs)
      reactiveEnv$numberGeneIDs <- length(row_index)
      if (length(row_index) == 0) {
        return(NULL)
      }
      columnIndex <- 1:ncol(shinyEnv$heatmapData)
    }
    subHeat <- subHeatAnn(
      data = shinyEnv$heatmapData,
      sample_info = NULL,
      select_factors_heatmap = "Names"
    )
    subAnn <- subHeat$heat_sub_ann[columnIndex]
    subGroups <- subHeat$groups[columnIndex]
    lgd <- subHeat$lgd
    groupColors <- subHeat$group_colors

    m <- shinyEnv$heatMain@ht_list[[1]]@matrix
    if (length(row_index) > max_gene_ids) {
      show_rows <- FALSE
    } else {
      show_rows <- TRUE
    }
    submap_data <- m[row_index, columnIndex, drop = FALSE]
    click_data <- submap_data
    ht_select <- ComplexHeatmap::Heatmap(
      m[row_index, columnIndex, drop = FALSE],
      col = shinyEnv$heatMain@ht_list[[1]]@matrix_color_mapping@col_fun,
      show_heatmap_legend = FALSE,
      cluster_rows = FALSE,
      cluster_columns = FALSE,
      show_row_names = show_rows,
      top_annotation = subAnn,
      name = "heat_1"
    )
    return(list(
      ht_select = ht_select,
      submap_data = submap_data,
      sub_groups = subGroups,
      lgd = lgd,
      group_colors = groupColors,
      click_data = click_data
    ))
  }
  ####SubheatmapClick Plot----
  clusterHeatClickInfo<- function(click, ht_sub, ht_sub_obj, ht_pos_sub, sub_groups, group_colors, click_data) {
    pos1 <- InteractiveComplexHeatmap::getPositionFromClick(click)
    pos <- InteractiveComplexHeatmap::selectPosition(
      ht_sub,
      mark = FALSE,
      pos = pos1,
      verbose = FALSE,
      ht_pos = ht_pos_sub
    )
    row_index <- pos[1, "row_index"]
    column_index <- pos[1, "column_index"]
    if (is.null(row_index)) {
      return(NULL)
    }
    cluster_meth <- 1
    if (cluster_meth == 1) {
      value <- click_data[row_index, column_index]
      col <- ComplexHeatmap::map_to_colors(ht_sub_obj@matrix_color_mapping, value)
      sample <- colnames(click_data)[column_index]
      gene <- rownames(click_data)[row_index]
    }
    group_name <- sub_groups[column_index]
    group_col <- group_colors[[group_name]]
    html <- GetoptLong::qq("
<div>
<pre>
GeneID: @{gene}  Expression: @{round(value, 2)} <span style='background-color:@{col};width=40px;'>    </span>
Sample: @{sample},  Group: @{group_name} <span style='background-color:@{group_col};width=40px;'>    </span>
</pre></div>")
    HTML(html)
  }
  ####TE Result----
  TEResult <- function(input, condition, species, gff, inputnames, fvalue, normalcount, groupnames, rpfnames, pCutoff, pCutoffType, volcanoGeneFinder, volcanoGene){
    normalcount <- as.data.table(normalcount)
    normalcount[,input1 := rowMeans(.SD, na.rm = TRUE),.SDcols = inputnames[condition=="G1"]]
    normalcount[,input2 := rowMeans(.SD, na.rm = TRUE),.SDcols = inputnames[condition=="G2"]]
    normalcount[,logInputFC := log2(input2/input1)]
    normalcount[,diffExp := "Non"]
    normalcount[logInputFC >= log2(fvalue),diffExp := "Up"]
    normalcount[logInputFC <= -log2(fvalue),diffExp := "Down"]
    normalcount$diffExp <- factor(normalcount$diffExp, levels = c("Up", "Non", "Down"))
    p1 <- ggscatter(normalcount,
                    x = 'input1',
                    y = 'input2',
                    color = 'diffExp',
                    palette = "jco",
                    size = 2,
                    alpha = 0.8) +
      xscale("log2") +
      yscale("log2") +
      scale_colour_manual(name = "",
                          values = c("red", "grey", "green"),
                          breaks = c("Up", "Non", "Down"),
                          labels = c("Up", "Non", "Down")) +
      xlab(paste0("RNA expression (", groupnames[1], ", log2)")) +
      ylab(paste0("RNA expression (", groupnames[2], ", log2)")) +
      border() +
      theme(legend.text = element_text(size = 16)) +
      guides(color = guide_legend(override.aes = list(size = 5)))
    shinyEnv$te_p1 <- p1
    ggplot2::ggsave(filename = paste0(TEFileuserDir, "/Input.scatter.png"), plot = p1, width = 8, height = 8, dpi = 100, bg = "#ffffff")
    ggplot2::ggsave(filename = paste0(TEFileuserDir, "/Input.scatter.pdf"), plot = p1, width = 8, height = 8, bg = "transparent")
    fwrite(normalcount, paste0(TEFileuserDir,"/inputScatterData.csv"))
    normalcount[,rpf1 := rowMeans(.SD, na.rm = TRUE),.SDcols = rpfnames[condition=="G1"]]
    normalcount[,rpf2 := rowMeans(.SD, na.rm = TRUE),.SDcols = rpfnames[condition=="G2"]]
    normalcount[,logRPFfc := log2(rpf2/rpf1)]
    p2 <- ggscatter(normalcount, x ='logInputFC', y = 'logRPFfc', col="grey", cor.coef = TRUE, cor.method = "pearson", size = 1.5, alpha = 0.8)+ xlab("log2 Fold change (INPUT)")+ylab("log2 Fold change(RPF)")+ border()
    shinyEnv$te_p2 <- p2
    ggplot2::ggsave(filename = paste0(TEFileuserDir, "/RPF.input.scatter.png"), plot = p2, width = 8, height = 8, dpi = 100, bg = "#ffffff")
    ggplot2::ggsave(filename = paste0(TEFileuserDir, "/RPF.input.scatter.pdf"), plot = p2, width = 8, height = 8, bg = "transparent")
    fwrite(normalcount, paste0(TEFileuserDir,"/rpfInputScatterData.csv"))
    TEsampleNames <- c()
    for (i in seq_len(length(inputnames))) {
      normalcount[, paste0('TE.',rpfnames[i],".",inputnames[i]) := eval(as.name(rpfnames[i]))/eval(as.name(inputnames[i]))]
      TEsampleNames <- c(TEsampleNames, paste0('TE.',rpfnames[i],".",inputnames[i]))
    }
    normalcount[,TE_A1 := round(rowMeans(.SD, na.rm = TRUE),3),.SDcols = TEsampleNames[condition=="G1"]]
    normalcount[,TE_A2 := round(rowMeans(.SD, na.rm = TRUE),3),.SDcols = TEsampleNames[condition=="G2"]]
    normalcount[,logTEfc := log2(TE_A2/TE_A1)]
    normalcount[,diffTE := "Non"]
    normalcount[logTEfc >= log2(fvalue),diffTE:="Up"]
    normalcount[logTEfc <= -log2(fvalue),diffTE:="Down"]
    normalcount$diffTE <- factor(normalcount$diffTE, levels = c("Up", "Non", "Down"))
    p3 <- ggscatter(normalcount,
                    x = 'TE_A1',
                    y = 'TE_A2',
                    color = 'diffTE',
                    palette = "jco",
                    size = 2,
                    alpha = 0.8) +
      xscale("log2") +
      yscale("log2") +
      scale_colour_manual(name = "",
                          values = c("red", "grey", "green"),
                          breaks = c("Up", "Non", "Down"),
                          labels = c("Up", "Non", "Down")) +
      xlab(paste0("Translation efficiency (", groupnames[1], ", log2)")) +
      ylab(paste0("Translation efficiency (", groupnames[2], ", log2)")) +
      border() +
      theme(legend.text = element_text(size = 16)) +
      guides(colour = guide_legend(override.aes = list(size = 5)))
    shinyEnv$te_p3 <- p3
    ggplot2::ggsave(filename = paste0(TEFileuserDir, "/TE.scatter.png"), plot = p3, width = 8, height = 8, dpi = 100, bg = "#ffffff")
    ggplot2::ggsave(filename = paste0(TEFileuserDir, "/TE.scatter.pdf"), plot = p3, width = 8, height = 8, bg = "transparent")
    fwrite(normalcount, paste0(TEFileuserDir,"/TEScatterData.csv"))
    normalcount$diffTE <- factor(normalcount$diffTE, levels = c("Up", "Non", "Down"))
    sp = ggscatter(normalcount, x = 'logInputFC' , y = 'log2FoldChange', color = "diffTE", palette = c("red", "gray", "green"), size = 1.2, alpha = 0.5)+
      xlab(paste0("log2 Fold Change (RPKM,", groupnames[2], "/",groupnames[1],")")) +ylab(paste0("log2 Fold Change (TE, ",groupnames[2],"/",groupnames[1],")"))+border() + xlim(-4,4) + ylim(-4,4)
    xplot <- ggdensity(normalcount, "logInputFC", fill = "diffTE", palette = c("red", "gray", "green"))+xlim(-4,4)
    yplot <- ggdensity(normalcount, "log2FoldChange", fill = "diffTE", palette = c("red", "gray", "green"))+xlim(-4,4)
    sp <- sp + rremove("legend")
    shinyEnv$te_p4_1 <- sp
    yplot <- yplot + clean_theme() + rremove("legend") + coord_flip()
    xplot <- xplot + clean_theme() + rremove("legend")
    shinyEnv$te_p4_2 <- xplot
    shinyEnv$te_p4_3 <- yplot
    p4 <- plot_grid(xplot, NULL, sp, yplot, ncol = 2, align = "hv", rel_widths = c(2, 1), rel_heights = c(1, 2))
    ggplot2::ggsave(filename = paste0(TEFileuserDir, "/TE_rpkm_fc.scatter.png"), plot = p4, width = 8, height = 8, dpi = 100, bg = "#ffffff")
    ggplot2::ggsave(filename = paste0(TEFileuserDir, "/TE_rpkm_fc.scatter.pdf"), plot = p4, width = 8, height = 8, bg = "transparent")
    fwrite(normalcount, paste0(TEFileuserDir,"/TERpkmFcScatterData.csv"))
    normalcount[, diffTE := "Non"]
    if(pCutoffType == "rawPvalue") {
      normalcount[logTEfc >= log2(fvalue) & pvalue < pCutoff, diffTE := "Up"]
      normalcount[logTEfc <= -log2(fvalue) & pvalue < pCutoff, diffTE := "Down"]
    } else {
      normalcount[logTEfc >= log2(fvalue) & padj < pCutoff, diffTE := "Up"]
      normalcount[logTEfc <= -log2(fvalue) & padj < pCutoff, diffTE := "Down"]
    }
    shinyEnv$normalcount <- normalcount
    telist <- normalcount[,.(GeneID, log2FoldChange, pvalue, padj, diffTE)]
    shinyEnv$te_telist <- telist
    xmax = max(abs(telist$log2FoldChange))+1
    if(pCutoffType == "rawPvalue") {
      p5 <- EnhancedVolcano(telist,
                            lab = telist$GeneID,
                            selectLab = "",
                            x = 'log2FoldChange',
                            y = 'pvalue',
                            title = "",
                            subtitle = "",
                            xlim = c(-xmax, xmax),
                            pCutoff = pCutoff) +
        theme(legend.text = element_text(size = 16)) +
        guides(colour = guide_legend(override.aes = list(size = 5)))
      shinyEnv$te_p5 <- p5
    }
    else {
      p5 <- EnhancedVolcano(telist,
                            lab = telist$GeneID,
                            selectLab = "",
                            x = 'log2FoldChange',
                            y = 'padj',
                            title = "",
                            subtitle = "",
                            xlim = c(-xmax, xmax),
                            pCutoff = pCutoff) +
        theme(legend.text = element_text(size = 16)) +
        guides(colour = guide_legend(override.aes = list(size = 5)))
      shinyEnv$te_p5 <- p5
    }
    ggplot2::ggsave(filename = paste0(TEFileuserDir, "/volcanoAB.png"), plot = p5, width = 8, height = 8, dpi = 100, bg = "#ffffff")
    ggplot2::ggsave(filename = paste0(TEFileuserDir, "/volcanoAB.pdf"), plot = p5, width = 8, height = 8, bg = "transparent")
    fwrite(telist, paste0(TEFileuserDir,"/volcanoABData.csv"))
    nn = nrow(telist[diffTE == "Non"])
    nu = nrow(telist[diffTE == "Up"])
    nd = nrow(telist[diffTE == "Down"])
    telist[diffTE == "Non", diffTEforLabel := paste0("Non (",nn,")")]
    telist[diffTE == "Up", diffTEforLabel := paste0("Up (",nu,")")]
    telist[diffTE == "Down", diffTEforLabel := paste0("Down (",nd,")")]
    diffTEforLabels <- c(paste0("Up (",nu,")"), paste0("Non (",nn,")"),paste0("Down (",nd,")"))
    telist$diffTEforLabel <- factor(telist$diffTEforLabel, levels = diffTEforLabels)
    if(pCutoffType == "rawPvalue") {
      telist[,pvalue := -log10(pvalue)]
      p6 <- ggscatter(telist,
                      x = 'log2FoldChange',
                      y = 'pvalue',
                      color = 'diffTEforLabel',
                      palette = c("red", "grey", "green"),
                      xlim = c(-xmax, xmax),
                      size = 2,
                      alpha = 0.8) +
        labs(x = "log2FoldChange", y = "-log10 P-value") +
        geom_vline(xintercept = c(-log2(fvalue), log2(fvalue)), linetype = "dashed", color = "gray") +
        geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "gray") +
        theme(legend.title = element_blank(),
              axis.text = element_text(size = 16),
              axis.title = element_text(size = 16),
              legend.text = element_text(size = 16)) +
        guides(colour = guide_legend(override.aes = list(size = 5)))
      shinyEnv$te_p6 <- p6
    }
    else {
      telist[,padj := -log10(padj)]
      p6 <- ggscatter(telist,
                      x = 'log2FoldChange',
                      y = 'padj',
                      color = 'diffTEforLabel',
                      palette = c("red", "grey", "green"),
                      xlim = c(-xmax, xmax),
                      size = 2,
                      alpha = 0.8) +
        labs(x = "log2FoldChange", y = "-log10 P-value") +
        geom_vline(xintercept = c(-log2(fvalue), log2(fvalue)), linetype = "dashed", color = "gray") +
        geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "gray") +
        theme(legend.title = element_blank(),
              axis.text = element_text(size = 16),
              axis.title = element_text(size = 16),
              legend.text = element_text(size = 16)) +
        guides(colour = guide_legend(override.aes = list(size = 5)))
      shinyEnv$te_p6 <- p6
    }
    ggplot2::ggsave(filename = paste0(TEFileuserDir, "/TE.volcano.scatter.png"), plot = p6, width = 8, height = 8, dpi = 100, bg = "#ffffff")
    ggplot2::ggsave(filename = paste0(TEFileuserDir, "/TE.volcano.scatter.pdf"), plot = p6, width = 8, height = 8, bg = "transparent")
    fwrite(telist, paste0(TEFileuserDir,"/TEVolcanoScatterData.csv"))
    normalcount <- as.data.frame(normalcount)
    WGCNAData <- normalcount[, c(1, grep("^TE\\.", names(normalcount)))]
    fwrite(WGCNAData,file = paste0(TEFileuserDir,"/WGCNAData.csv"))
    fwrite(normalcount,file = paste0(TEFileuserDir,"/normalcount.csv"))
    geneidMap <- setNames(gff$GeneID, toupper(gff$GeneID))
    normalcount$GeneID <- ifelse(
      toupper(normalcount$GeneID) %in% names(geneidMap),
      geneidMap[toupper(normalcount$GeneID)],
      normalcount$GeneID
    )
    fwrite(normalcount,file = paste0(TEFileuserDir,"/normalcountStandard.csv"))
  }
  TEtotal <- function(){
    req(file.exists(paste0(TEFileuserDir,"/baseData.json")))
    te(FALSE)
    unlink(paste0(TEFileuserDir,"/Input.scatter.png"))
    unlink(paste0(TEFileuserDir,"/RPF.input.scatter.png"))
    unlink(paste0(TEFileuserDir,"/TE.scatter.png"))
    unlink(paste0(TEFileuserDir,"/TE_rpkm_fc.scatter.png"))
    unlink(paste0(TEFileuserDir,"/volcanoAB.png"))
    unlink(paste0(TEFileuserDir,"/TE.volcano.scatter.png"))
    shinyjs::show("loader-overlay")
    baseData <- fromJSON(paste0(TEFileuserDir,"/baseData.json"))
    TEtools <- input$TEtools
    minCounts <- input$minCounts
    nMinSamplesCount <- input$nMinSamplesCount
    riborexResult <- riborexNormalCount(baseData[[1]], baseData[[3]], baseData[[4]], baseData[[5]], baseData[[2]], baseData[[6]], baseData[[7]], baseData[[8]], baseData[[9]], TEtools, minCounts, nMinSamplesCount)
    TEResult(riborexResult[[1]], riborexResult[[2]], riborexResult[[3]], riborexResult[[4]], riborexResult[[5]], riborexResult[[6]], riborexResult[[7]], riborexResult[[8]], riborexResult[[9]], riborexResult[[10]], riborexResult[[11]])
    te(TRUE)
  }
  observeEvent(input$volcanoGeneFinderBtn, {
    selectedGenes <- input$volcanoGeneFinder
    selectedGenes <- gsub(" ", "", selectedGenes)
    selectedGenes <- unlist(strsplit(selectedGenes, "[,，]"))
    telist2 <- shinyEnv$te_telist[GeneID %in% selectedGenes]
    if (nrow(telist2) == 0) {
      showModal(modalDialog(
        "Input genes are not present in the gene matrix. Please re-enter!!!",
        footer = tagList(
          modalButton("Close"),
        ),easyClose = TRUE,))
      return(NULL)
    }
    output$TEvolcano <- renderImage({
      shinyEnv$te_p6_hightlight <- shinyEnv$te_p6 +
        geom_text_repel(
          data = telist2,
          aes(label = GeneID),
          size = 3,
          box.padding = 3,
          point.padding = 0.5,
          force = 3,
          segment.color = 'grey50',
          max.overlaps = Inf
        )
      ggplot2::ggsave(filename = paste0(TEFileuserDir, "/TE.volcano.hightlight.scatter.png"), plot = shinyEnv$te_p6_hightlight, width = 8, height = 8, dpi = 250, bg = "#ffffff")
      ggplot2::ggsave(filename = paste0(TEFileuserDir, "/TE.volcano.hightlight.scatter.pdf"), plot = shinyEnv$te_p6_hightlight, width = 8, height = 8, bg = "transparent")
      return(
        list(
          src = paste0(TEFileuserDir,"/TE.volcano.hightlight.scatter.png"),
          contentType = "png",
          width = "600px",
          height = "600px",
          alt = " "
        )
      )
    })
    output$volcanoAB <- renderImage({
      shinyEnv$te_p5_hightlight <- shinyEnv$te_p5 +
        geom_text_repel(
          aes(x = log2FoldChange, y = -log10(padj), label = GeneID),
          data = shinyEnv$te_telist[GeneID %in% telist2$GeneID],
          size = 3,
          box.padding = 3,
          point.padding = 0.5,
          force = 3,
          segment.color = 'grey50',
          max.overlaps = Inf
        )
      ggplot2::ggsave(filename = paste0(TEFileuserDir, "/volcanoAB.hightlight.png"), plot = shinyEnv$te_p5_hightlight, width = 8, height = 8, dpi = 250, bg = "#ffffff")
      ggplot2::ggsave(filename = paste0(TEFileuserDir, "/volcanoAB.hightlight.pdf"), plot = shinyEnv$te_p5_hightlight, width = 8, height = 8, bg = "transparent")
      return(
        list(
          src = paste0(TEFileuserDir,"/volcanoAB.hightlight.png"),
          contentType = "png",
          width = "600px",
          height = "600px",
          alt = " "
        )
      )
    })
  })
  observeEvent(input$scatterGeneFinderBtn, {
    selectedGenes2 <- input$scatterGeneFinder
    selectedGenes2 <- gsub(" ", "", selectedGenes2)
    selectedGenes2 <- unlist(strsplit(selectedGenes2, "[,，]"))
    normalcount_hightlight <- shinyEnv$normalcount[GeneID %in% selectedGenes2]
    if (nrow(normalcount_hightlight) == 0) {
      showModal(modalDialog(
        "Input genes are not present in the gene matrix. Please re-enter!!!",
        footer = tagList(
          modalButton("Close"),
        ),easyClose = TRUE,))
      return(NULL)
    }
    output$inputScatter <- renderImage({
      shinyEnv$te_p1_hightight <- shinyEnv$te_p1 + geom_text_repel(
        data = normalcount_hightlight,
        aes(label = GeneID),
        size = 3,
        box.padding = 3,
        point.padding = 0.5,
        force = 3,
        segment.color = 'grey50',
        max.overlaps = Inf
      )
      ggplot2::ggsave(filename = paste0(TEFileuserDir, "/Input.scatter.hightlight.png"), plot = shinyEnv$te_p1_hightight, width = 8, height = 8, dpi = 250, bg = "#ffffff")
      ggplot2::ggsave(filename = paste0(TEFileuserDir, "/Input.scatter.hightlight.pdf"), plot = shinyEnv$te_p1_hightight, width = 8, height = 8, bg = "transparent")
      return(
        list(
          src = paste0(TEFileuserDir,"/Input.scatter.hightlight.png"),
          contentType = "png",
          width = "600px",
          height = "600px",
          alt = " "
        )
      )
    })
    output$rpfInputScatter <- renderImage({
      shinyEnv$te_p2_hightlight <- shinyEnv$te_p2 + geom_text_repel(
        data = normalcount_hightlight,
        aes(label = GeneID),
        size = 3,
        box.padding = 3,
        point.padding = 0.5,
        force = 3,
        segment.color = 'grey50',
        max.overlaps = Inf
      )
      ggplot2::ggsave(filename = paste0(TEFileuserDir, "/RPF.input.scatter.hightlight.png"), plot = shinyEnv$te_p2_hightlight, width = 8, height = 8, dpi = 250, bg = "#ffffff")
      ggplot2::ggsave(filename = paste0(TEFileuserDir, "/RPF.input.scatter.hightlight.pdf"), plot = shinyEnv$te_p2_hightlight, width = 8, height = 8, bg = "transparent")
      return(
        list(
          src = paste0(TEFileuserDir,"/RPF.input.scatter.hightlight.png"),
          contentType = "png",
          width = "600px",
          height = "600px",
          alt = " "
        )
      )
    })
    output$TEscatter <- renderImage({
      shinyEnv$shinyEnv$te_p3_hightlight <- shinyEnv$te_p3 + geom_text_repel(
        data = normalcount_hightlight,
        aes(label = GeneID),
        size = 3,
        box.padding = 3,
        point.padding = 0.5,
        force = 3,
        segment.color = 'grey50',
        max.overlaps = Inf
      )
      ggplot2::ggsave(filename = paste0(TEFileuserDir, "/TE.scatter.hightlight.png"), plot = shinyEnv$shinyEnv$te_p3_hightlight, width = 8, height = 8, dpi = 250, bg = "#ffffff")
      ggplot2::ggsave(filename = paste0(TEFileuserDir, "/TE.scatter.hightlight.pdf"), plot = shinyEnv$shinyEnv$te_p3_hightlight, width = 8, height = 8, bg = "transparent")
      return(
        list(
          src = paste0(TEFileuserDir,"/TE.scatter.hightlight.png"),
          contentType = "png",
          width = "600px",
          height = "600px",
          alt = " "
        )
      )
    })
    output$TErpkmFcScatter <- renderImage({
      shinyEnv$te_p4_1 <- shinyEnv$te_p4_1 + geom_text_repel(
        data = normalcount_hightlight,
        aes(label = GeneID),
        size = 3,
        box.padding = 3,
        point.padding = 0.5,
        force = 3,
        segment.color = 'grey50',
        max.overlaps = Inf
      )
      shinyEnv$te_p4_hightlight <- plot_grid(shinyEnv$te_p4_2, NULL, shinyEnv$te_p4_1, shinyEnv$te_p4_3, ncol = 2, align = "hv", rel_widths = c(2, 1), rel_heights = c(1, 2))
      ggplot2::ggsave(filename = paste0(TEFileuserDir, "/TE_rpkm_fc.scatter.hightlight.png"), plot = shinyEnv$te_p4_hightlight, width = 8, height = 8, dpi = 250, bg = "#ffffff")
      ggplot2::ggsave(filename = paste0(TEFileuserDir, "/TE_rpkm_fc.scatter.hightlight.pdf"), plot = shinyEnv$te_p4_hightlight, width = 8, height = 8, bg = "transparent")
      return(
        list(
          src = paste0(TEFileuserDir,"/TE_rpkm_fc.scatter.hightlight.png"),
          contentType = "png",
          width = "600px",
          height = "600px",
          alt = " "
        )
      )
    })
  })
  observeEvent(input$markerClearVolcano, {
    output$TEvolcano <- renderImage({
      req(file.exists(paste0(TEFileuserDir,"/TE.volcano.scatter.png")))
      if(file.exists(paste0(TEFileuserDir,"/TE.volcano.scatter.png"))){
        return(list(
          src = paste0(TEFileuserDir,"/TE.volcano.scatter.png"),
          contentType = "png",
          width = "600px",
          height = "600px",
          alt = " "
        ))
      }
    }, deleteFile=FALSE)
    output$volcanoAB <- renderImage({
      req(file.exists(paste0(TEFileuserDir,"/volcanoAB.png")))
      if(file.exists(paste0(TEFileuserDir,"/volcanoAB.png"))){
        return(list(
          src = paste0(TEFileuserDir,"/volcanoAB.png"),
          contentType = "png",
          width = "600px",
          height = "600px",
          alt = " "
        ))
      }
    }, deleteFile=FALSE)
  })
  observeEvent(input$markerClearScatter, {
    output$TEscatter <- renderImage({
      req(file.exists(paste0(TEFileuserDir,"/TE.scatter.png")))
      if(file.exists(paste0(TEFileuserDir,"/TE.scatter.png"))){
        list(
          src = paste0(TEFileuserDir,"/TE.scatter.png"),
          contentType = "png",
          width = "600px",
          height = "600px",
          alt = " "
        )}
    }, deleteFile=FALSE)
    output$TErpkmFcScatter <- renderImage({
      req(file.exists(paste0(TEFileuserDir,"/TE_rpkm_fc.scatter.png")))
      if(file.exists(paste0(TEFileuserDir,"/TE_rpkm_fc.scatter.png"))){
        list(
          src = paste0(TEFileuserDir,"/TE_rpkm_fc.scatter.png"),
          contentType = "png",
          width = "600px",
          height = "600px",
          alt = " "
        )
      }
    }, deleteFile=FALSE)
    output$inputScatter <- renderImage({
      req(file.exists(paste0(TEFileuserDir,"/Input.scatter.png")))
      if(file.exists(paste0(TEFileuserDir,"/Input.scatter.png"))){
        list(
          src = paste0(TEFileuserDir,"/Input.scatter.png"),
          contentType = "png",
          width = "600px",
          height = "600px",
          alt = " "
        )
      }
    }, deleteFile=FALSE)
    output$rpfInputScatter <- renderImage({
      req(file.exists(paste0(TEFileuserDir,"/RPF.input.scatter.png")))
      if(file.exists(paste0(TEFileuserDir,"/RPF.input.scatter.png"))){
        list(
          src = paste0(TEFileuserDir,"/RPF.input.scatter.png"),
          contentType = "png",
          width = "600px",
          height = "600px",
          alt = " "
        )
      }
    }, deleteFile=FALSE)
  })
  ####PCA Result----
  PCAResult <- function(){
    req(file.exists(paste0(TEFileuserDir,"/normalcount.csv")))
    toggleClass(selector = ".RiboTE-working-btn-pca", class = "RiboTE-working-hidden")
    runjs('$(".mask").show();')
    shinyjs::show("loader-overlay2")
    normalcount <- read.csv(paste0(TEFileuserDir,"/normalcount.csv"),header = T,row.names = 1, check.names = FALSE)
    normalcount <- as.data.table(normalcount)
    baseData <- fromJSON(paste0(TEFileuserDir,"/baseData.json"))
    inputnames <- baseData[[3]]
    rpfnames <- baseData[[4]]
    groupnames <- baseData[[7]]
    TEsampleName <- baseData[[10]]
    if(input$pcaData == "INPUT"){
      titleSuffix <- "RNA expression"
      data <- normalcount[,..inputnames]
      if(input$pcaMethod == "PCA"){
        title <- paste("PCA based on", titleSuffix)
        fileName <- "/input.pca.png"
      }
      if(input$pcaMethod == "MDS"){
        title <- paste("MDS based on", titleSuffix)
        fileName <- "/input.mds.png"
      }
      if(input$pcaMethod == "T-SNE"){
        title <- paste("T-SNE based on", titleSuffix)
        fileName <- "/input.t-sne.png"
      }
    }
    if(input$pcaData == "RPF"){
      titleSuffix <- "RPF expression"
      data <- normalcount[,..rpfnames]
      if(input$pcaMethod == "PCA"){
        title <- paste("PCA based on", titleSuffix)
        fileName <- "/rpf.pca.png"
      }
      if(input$pcaMethod == "MDS"){
        title <- paste("MDS based on", titleSuffix)
        fileName <- "/rpf.mds.png"
      }
      if(input$pcaMethod == "T-SNE"){
        title <- paste("T-SNE based on", titleSuffix)
        fileName <- "/rpf.t-sne.png"
      }
    }
    if(input$pcaData == "TE"){
      titleSuffix <- "TE"
      data <- normalcount[,..TEsampleName]
      if(input$pcaMethod == "PCA"){
        title <- paste("PCA based on", titleSuffix)
        fileName <- "/te.pca.png"
      }
      if(input$pcaMethod == "MDS"){
        title <- paste("MDS based on", titleSuffix)
        fileName <- "/te.mds.png"
      }
      if(input$pcaMethod == "T-SNE"){
        title <- paste("T-SNE based on", titleSuffix)
        fileName <- "/te.t-sne.png"
      }
    }
    if(input$pcaMethod == "PCA"){
      data <- data[apply(data, 1, function(row) all(is.finite(row))), ]
      pca <- prcomp(t(data))
      pca_data_perc <- round(100*pca$sdev^2/sum(pca$sdev^2),1)
      df_pca_data <- data.frame(PC1 = pca$x[,1], PC2 = pca$x[,2], sample = colnames(data), condition = c(rep(groupnames[1],ceiling(length(inputnames)/2)), rep(groupnames[2],floor(length(inputnames)/2))))
      p1 <- ggplot(df_pca_data, aes(PC1, PC2, color = sample, shape = condition)) +
        geom_point(size = 8) +
        labs(
          title = title,
          x = paste0("PC1 (", pca_data_perc[1] ,"%",")"),
          y = paste0("PC2 (", pca_data_perc[2] ,"%",")")
        ) + coord_fixed(ratio=1.) + theme(aspect.ratio=1) +
        theme(
          plot.title = element_text(hjust = 0.5),
          legend.key.size = unit(2, "lines"),
          legend.text = element_text(size = 15),
          axis.text.x = element_text(size = 20),
          axis.text.y = element_text(size = 20),
          axis.title.x = element_text(size = 20,margin = margin(t = 20, r = 0, b = 0, l = 0)),
          axis.title.y = element_text(size = 22)
        )
      ggplot2::ggsave(filename = paste0(TEFileuserDir,fileName), plot = p1, bg = 'white', width = 10, height = 8, dpi = 250)
      ggplot2::ggsave(filename = paste0(TEFileuserDir,sub("\\.png$", ".pdf", fileName)), plot = p1, bg = "transparent", width = 10, height = 8)
      fwrite(df_pca_data, paste0(TEFileuserDir, "/", tolower(input$pcaData), "_pca_plot", "_data.csv"))
      return(p1)
    }
    if(input$pcaMethod == "MDS"){
      data <- data[apply(data, 1, function(row) all(is.finite(row))), ]
      dist2 <- function(x, ...) as.dist(1-cor(t(x), method="pearson"))
      fit = cmdscale( dist2(t(data) ), eig=T, k=2)
      if (sum(fit$eig > 0) < 2) {
        showModal(modalDialog(
          "The data has significant variation in only one direction in two dimensions, with little or no significant variation in the other direction.",
          footer = tagList(
            modalButton("Close"),
          ),easyClose = TRUE,))
        return(NULL)
      }
      condition = c(rep(groupnames[1],ceiling(length(inputnames)/2)), rep(groupnames[2],floor(length(inputnames)/2)))
      pcaData = as.data.frame(fit$points[,1:2])
      pcaData = cbind(pcaData,colnames(data),condition)
      colnames(pcaData) = c("x1", "x2", "Sample","Condition")
      p1 <- ggplot(pcaData, aes(x1, x2, color=Sample, shape = Condition))
      p1 <- p1 + geom_point(size=8)
      p1 <- p1 + xlab("Dimension 1")
      p1 <- p1 + ylab("Dimension 2")
      p1 <- p1 + ggtitle(title) + coord_fixed(ratio=1.) +
        theme(plot.title = element_text(hjust = 0.5)) + theme(aspect.ratio=1) +
        theme(axis.text.x = element_text(size = 12),
              axis.text.y = element_text(size = 12),
              axis.title.x = element_text(size = 14),
              axis.title.y = element_text(size = 14) ) +
        theme(legend.text=element_text(size=10))
      ggplot2::ggsave(filename = paste0(TEFileuserDir,fileName), plot = p1, bg = 'white', width = 10, height = 8, dpi = 250)
      ggplot2::ggsave(filename = paste0(TEFileuserDir,sub("\\.png$", ".pdf", fileName)), plot = p1, bg = "transparent", width = 10, height = 8)
      fwrite(pcaData, paste0(TEFileuserDir, "/", tolower(input$pcaData), "_mds_plot",  "_data.csv"))
      return(p1)
    }
    if(input$pcaMethod == "T-SNE"){
      data <- data[apply(data, 1, function(row) all(is.finite(row))), ]
      tsne <- Rtsne(t(data), dims = 2, perplexity=1, verbose=FALSE, max_iter = 400)
      condition = c(rep(groupnames[1],ceiling(length(inputnames)/2)), rep(groupnames[2],floor(length(inputnames)/2)))
      pcaData = as.data.frame(tsne$Y)
      pcaData = cbind(pcaData,colnames(data),condition)
      colnames(pcaData) = c("x1", "x2", "Sample","Condition")
      p1 <- ggplot(pcaData, aes(x1, x2, color=Sample, shape = Condition))
      p1 <- p1 + geom_point(size=8)
      p1 <- p1 + xlab("Dimension 1")
      p1 <- p1 + ylab("Dimension 2")
      p1 <- p1 + ggtitle(title) + coord_fixed(ratio=1.) +
        theme(plot.title = element_text(hjust = 0.5)) + theme(aspect.ratio=1) +
        theme(axis.text.x = element_text(size = 12),
              axis.text.y = element_text(size = 12),
              axis.title.x = element_text(size = 14),
              axis.title.y = element_text(size = 14) ) +
        theme(legend.text=element_text(size=10))
      ggsave(paste0(TEFileuserDir,fileName), plot = p1, bg = 'white', width = 10, height = 8)
      ggplot2::ggsave(filename = paste0(TEFileuserDir,fileName), plot = p1, bg = 'white', width = 10, height = 8, dpi = 250)
      ggplot2::ggsave(filename = paste0(TEFileuserDir,sub("\\.png$", ".pdf", fileName)), plot = p1, bg = "transparent", width = 10, height = 8)
      fwrite(pcaData, paste0(TEFileuserDir, "/", tolower(input$pcaData), "_t-sne_plot_",  "data.csv"))
      return(p1)
    }
  }
  ####SignalP Result----
  SignalPResult <- function(){
    shinyjs::show("loader-overlay11")
    toggleClass(selector = ".RiboTE-working-btn-signalP", class = "RiboTE-working-hidden")
    normalcount <- read.csv(paste0(TEFileuserDir,"/normalcount.csv"), check.names = FALSE)
    normalcount <- as.data.table(normalcount)
    baseData <- fromJSON(paste0(TEFileuserDir,"/baseData.json"))
    species <- baseData[[2]]
    resultlist <- list()
    select_method <- input$SignalPMethod
    if(select_method == "All"){
      select_method <- c("signalP", "phobius", "tmhmm")
    }
    labels <- c()
    for(method in select_method) {
      if(file.exists(paste0("/public/shiny/RiboTE/TEShinyData/signal/",species,".pep.",method,".txt"))) {
        sig = fread(paste0("/public/shiny/RiboTE/TEShinyData/signal/",species,".pep.",method,".txt"),sep="\t", head=F)
        siggenes = toupper(unique(sig$V1))
        siggenes <- gsub("\\.\\d+$","",siggenes)
        normalcount[,sig:="Non"]
        normalcount[GeneID %in% siggenes, sig:="Yes"]
        updt <- as.data.table(table(normalcount[diffTE=="Up",sig]))
        updt[,tool:=method]
        updt[,total:=sum(N),by="tool"]
        updt[,type:="Up"]
        nondt <- as.data.table(table(normalcount[diffTE=="Non",sig]))
        nondt[,tool:=method]
        nondt[,total:=sum(N),by="tool"]
        nondt[,type:="None"]
        downdt <- as.data.table(table(normalcount[diffTE=="Down",sig]))
        downdt[,tool:=method]
        downdt[,total:=sum(N),by="tool"]
        downdt[,type:="Down"]
        telist <- rbindlist(list(updt,nondt,downdt))
        telist[,percent := round(N/total,5)]
        resultlist[[method]] = telist
        mat = matrix(c(telist[V1=="Yes" & type=="Up",N],telist[V1=="Yes"& type=="None",N],telist[V1=="Non" &type=="Up",N],telist[V1=="Non" & type=="None",N]), 2,2)
        un <- fisher.test(mat)$p.value
        un <- format(un, scientific = TRUE, digits = 5)
        mat = matrix(c(telist[V1=="Yes" & type=="Down",N],telist[V1=="Yes"&type=="None",N],telist[V1=="Non"&type=="Down",N],telist[V1=="Non"&type=="None",N]),2,2)
        dn <- fisher.test(mat)$p.value
        dn <- format(dn, scientific = TRUE, digits = 5)
        method <- switch(method,
                         "signalP" = "SignalP",
                         "phobius" = "Phobius",
                         "tmhmm" = "TMHMM")
        labels <- c(labels,paste0(method,": Up vs None pvalue = ",un,", Down vs None pvalue = ",dn))
      }
    }
    resultlist <- rbindlist(resultlist)
    if(nrow(resultlist) > 0) {
      label <- paste(labels,collapse="\n")
      resultlist <- resultlist[V1=="Yes"]
      ylim = max(resultlist$percent)+0.1
      p1 <- ggplot(resultlist, aes(x=tool, y=percent, fill=type)) +
        geom_bar(stat="identity", position=position_dodge(0.9), color="black", size=0.5, width = 0.8) +
        scale_fill_manual(values=c("#349F2B", "#CCCCCC", "#B2DF8A")) +
        ylim(0, ylim) +
        scale_y_continuous(limits=c(0, ylim), expand=c(0, 0)) +
        labs(y="Percentage of signal and membrane proteins", x="") +
        theme(
          legend.title=element_blank(),
          panel.background=element_blank(),
          panel.grid.major=element_blank(),
          legend.text=element_text(size=15),
          legend.key.size = unit(1.5, "lines"),
          panel.grid.minor=element_blank(),
          axis.line=element_line(color="black"),
          axis.text.x=element_text(size=15, color="black"),
          axis.text.y=element_text(size=15, color="black"),
          axis.title.y=element_text(margin=margin(r=10), size = 15)) +
        geom_text(aes(label=sprintf("%.2f%%", percent*100)), position=position_dodge(width=0.9), size = 5, vjust=-0.8, hjust=0.5, color="black") +
        annotate("text", x=0.5, y=ylim-0.01, label=label, color="black", size=5, hjust=0, vjust=1)
      fwrite(resultlist, paste0(TEFileuserDir, "/", input$SignalPMethod, "_plot_data.csv"))
      shinyEnv$signalPImagePath1 <- paste0(TEFileuserDir, "/", input$SignalPMethod, "_plot_data.csv")
      shinyEnv$signalPImagePath2 <- paste0(TEFileuserDir, "/", input$SignalPMethod, "_plot.png")
      shinyEnv$signalPImagePath3 <- paste0(TEFileuserDir, "/", input$SignalPMethod, "_plot.pdf")
      ggplot2::ggsave(filename = paste0(TEFileuserDir, "/", input$SignalPMethod, "_plot.png"), plot = p1, bg = 'white', width = 11, height = 7, dpi = 250)
      ggplot2::ggsave(filename = paste0(TEFileuserDir, "/", input$SignalPMethod, "_plot.pdf"), plot = p1, bg = "transparent", width = 11, height = 7)
      return(p1)
    }

  }
  ####GSEA Result----
  gaeaResult <- function(){
    req(file.exists(paste0(TEFileuserDir, "/orgData.json")))
    req(file.exists(paste0(TEFileuserDir, "/normalcount.csv")))
    runjs('$(".mask").show();')
    toggleClass(selector = ".RiboTE-working-btn-gsea", class = "RiboTE-working-hidden")
    shinyjs::show("loader-overlay5")
    riborexMatrix <- read.csv(paste0(TEFileuserDir, "/normalcount.csv"), check.names = FALSE)
    nPerm <- 20000
    fdrCutOffValue <- input$fdrCutOffValue
    absoluteFold <- input$absoluteFold
    geneSetSizeMin <- input$geneSetSizeMin
    geneSetSizeMax <- input$geneSetSizeMax
    geneSetSizeRange <- c(geneSetSizeMin, geneSetSizeMax)
    pathwayPValueCutOff <- input$pathwayPValueCutOff
    nPathwayShow <- input$nPathwayShow
    showPathwayId <- input$showPathwayId

    riborexMatrix <- as.data.frame(riborexMatrix)
    riborexMatrix <- riborexMatrix[, c("GeneID", "log2FoldChange", "padj")]
    rownames(riborexMatrix) <- riborexMatrix$GeneID
    gseaMatrix <- riborexMatrix[, -1]
    colnames(gseaMatrix) <- c("Fold", "FDR")
    gseaMatrix <- gseaMatrix[which(gseaMatrix$FDR < fdrCutOffValue), ]
    gseaFold <- gseaMatrix[, 1]
    names(gseaFold) <- rownames(gseaMatrix)
    if (absoluteFold) {
      gseaFold <- abs(gseaFold)
    }
    conversionInfo <- fromJSON(paste0(TEFileuserDir,"/orgData.json"))
    baseData <- fromJSON(paste0(TEFileuserDir,"/baseData.json"))
    converted <- conversionInfo[[1]]
    allGeneNames <- conversionInfo[[4]]
    pathwayDatabase <- "KEGG"
    selectOrg <- baseData[[13]]
    allOrgData <- conversionInfo[[6]]

    geneSets <- readGeneSets(converted, allGeneNames, pathwayDatabase, selectOrg, allOrgData, geneSetSizeRange)
    paths <- fgsea::fgsea(
      pathways = geneSets$geneLists,
      stats = gseaFold,
      minSize = geneSetSizeRange[1],
      maxSize = geneSetSizeRange[2],
      nPerm = nPerm,
      nproc = 6
    )

    paths <- paths[order(-abs(paths[, 5])), ]
    pathwayTable <- paths[, c(1, 5, 7, 3)]
    colnames(pathwayTable) <- c("Pathway", "NES", "Genes", "adj.Pval")

    pathwayTable <- pathwayTable[which(pathwayTable[, 4] <= pathwayPValueCutOff), , drop = FALSE]
    if (dim(pathwayTable)[1] > nPathwayShow) {
      pathwayTable <- pathwayTable[1:nPathwayShow, , drop = FALSE]
    }
    pathwayTable <- as.data.frame(pathwayTable)
    pathwayTable <- cbind(rep("CONTROL-TREATED", dim(pathwayTable)[1]), pathwayTable)
    pathwayTable[, 4] <- as.character(round(as.numeric(pathwayTable[, 4]), 4))
    pathwayTable$adj.Pval <- sprintf("%-2.1e", as.numeric(pathwayTable$adj.Pval))
    pathwayTable[, 1] <- as.character(pathwayTable[, 1])
    colnames(pathwayTable)[1] <- "Direction"
    colnames(pathwayTable)[2] <- paste("GSEA analysis:", gsub("-", " vs ", "CONTROL-TREATED"))
    pathwayTable[which(as.numeric(pathwayTable[, 3]) > 0), 1] <- "Up"
    pathwayTable[which(as.numeric(pathwayTable[, 3]) < 0), 1] <- "Down"

    pathwayTable <- pathwayTable[order(pathwayTable[, 1], as.numeric(pathwayTable$adj.Pval)), ]
    pathwayTable[duplicated(pathwayTable[, 1]), 1] <- ""
    pathwayTable[, 3] <- as.character(round(as.numeric(pathwayTable[, 3]), 4))
    pathwayCsvName <- colnames(pathwayTable)[2]
    nonHypertextName <- paste(pathwayCsvName, "Pathways")
    fwrite(pathwayTable, paste0(TEFileuserDir,"/pathwayTableFirst.csv"))
    if (ncol(pathwayTable) > 1) {
      ix <- match(pathwayTable[, 2], geneSets$pathwayInfo$description)
      if (!showPathwayId && selectOrg > 0) {
        pathwayTable[, 2] <- removePathwayId(pathwayTable[, 2], pathwayDatabase)
      }
      pathwayTable[nonHypertextName] <- pathwayTable[,2]
      pathwayTable[, 2] <- hyperText(
        pathwayTable[, 2],
        geneSets$pathwayInfo$memo[ix]
      )
      pathwayTable$URL <- NULL
      pathwayTable$URL <- geneSets$pathwayInfo$memo[ix]
      pathwayTable$Genes <- as.character(pathwayTable$Genes)
    }
    write_json(geneSets, paste0(TEFileuserDir,"/geneSets.json"))
    fwrite(pathwayTable, paste0(TEFileuserDir,"/pathwayTable.csv"))
    return(pathwayTable)
  }
  ####KEGG Result----
  observeEvent(input$gseaTabs, {
    if(input$gseaTabs == "KEGG Plot") {
      filePath <- file.path(TEFileuserDir, "pathwayTableFirst.csv")
      if (file.exists(filePath)) {
        pathwayTable <- read.csv(filePath, stringsAsFactors = FALSE, check.names = FALSE)
        keggPathways <- pathwayTable[[2]]
        output$keggPathwaySelect <- renderUI({
          selectInput("selectedPathway", "5.Select KEGG Pathway", choices = keggPathways, selected = keggPathways[1])
        })
      }
    }
  })
  keggPlot <- function(){
    req(input$selectedPathway)
    req(file.exists(paste0(TEFileuserDir, "/pathwayTable.csv")))
    req(file.exists(paste0(TEFileuserDir, "/geneSets.json")))
    runjs('$(".mask").show();')
    toggleClass(selector = ".RiboTE-working-btn-enrichment", class = "RiboTE-working-hidden")
    shinyjs::show("loader-overlay5")
    pathways <- read.csv(paste0(TEFileuserDir, "/pathwayTable.csv"), check.names = FALSE)
    colnames(pathways)[2] <- "Pathways"
    colnames(pathways)[4] <- "nGenes"
    pathways$adj_p_val <- as.numeric(pathways$adj.Pval)
    pathways <- subset(pathways, select = -c(adj.Pval))
    pathways$adj_p_val <- as.character(pathways$adj_p_val)
    if (nrow(pathways) > 1) {
      for (i in 2:nrow(pathways)) {
        if (nchar(pathways$Direction[i]) <= 1) {
          pathways$Direction[i] <- pathways$Direction[i - 1]
        }
      }
    }
    conversionInfo <- fromJSON(paste0(TEFileuserDir,"/orgData.json"))
    geneSets <- fromJSON(paste0(TEFileuserDir,"/geneSets.json"))
    baseData <- fromJSON(paste0(TEFileuserDir,"/baseData.json"))
    geneInfo <- conversionInfo[[2]]
    pathwayDatabase <- "KEGG"
    selectOrg <- baseData[[13]]
    converted <- conversionInfo[[1]]
    allOrgData <- conversionInfo[[6]]
    if (!"symbol" %in% colnames(geneInfo)) {
      geneInfo$symbol <- "null"
    }
    if (pathwayDatabase != "None" & selectOrg != "NEW") {
      if (sum(is.na(geneInfo$symbol)) / dim(geneInfo)[1] < 0.5) {
        probeToGene <- geneInfo[, c("ensembl_gene_id", "symbol")]
        probeToGene$symbol <- gsub(" ", "", probeToGene$symbol)
        ix <- which(
          is.na(probeToGene$symbol) |
            nchar(probeToGene$symbol) < 2 |
            toupper(probeToGene$symbol) == "NA" |
            toupper(probeToGene$symbol) == "0"
        )
        probeToGene[ix, 2] <- probeToGene[ix, 1]
      }
    }
    pathways$Genes <- vector(mode = "list", length = nrow(pathways))
    for (i in 1:nrow(pathways)) {
      ix <- which(names(geneSets) == pathways$Pathways[i])
      if (length(ix) != 0) {

        if (length(ix) > 1) {
          genes <- geneSets[[ix[[1]]]]
        } else {
          genes <- geneSets[[ix]]
        }
        if (!is.null(probeToGene)) {
          iy <- match(genes, probeToGene[, 1])
          genes <- probeToGene[iy, 2]
        }
        pathways$Genes[[i]] <- c(genes)
      }
    }
    showPathwayId <- TRUE
    if (!showPathwayId) {
      pathways$Pathways <- removePathwayId(
        strings = pathways$Pathways,
        pathwayDatabase = pathwayDatabase
      )
    }
    lowColor = "green"
    highColor = "red"
    outFile <- tempfile(fileext = ".png")
    png(outFile, width = 400, height = 300)
    frame()
    dev.off()
    blank <- list(
      src = outFile,
      contentType = "image/png",
      width = 600,
      height = 450,
      alt = "Not downloaded."
    )
    mypathview <- function(gene.data = NULL,
                           cpd.data = NULL,
                           pathway.id,
                           species = "hsa",
                           kegg.dir = ".",
                           cpd.idtype = "kegg",
                           gene.idtype = "entrez",
                           gene.annotpkg = NULL,
                           min.nnodes = 3,
                           kegg.native = TRUE,
                           map.null = TRUE,
                           expand.node = FALSE,
                           split.group = FALSE,
                           map.symbol = TRUE,
                           map.cpdname = TRUE,
                           node.sum = "sum",
                           discrete = list(gene = FALSE, cpd = FALSE),
                           limit = list(gene = 1, cpd = 1),
                           bins = list(gene = 10, cpd = 10),
                           both.dirs = list(gene = T, cpd = T),
                           trans.fun = list(gene = NULL, cpd = NULL),
                           low = list(gene = lowColor, cpd = "blue"),
                           mid = list(gene = "gray", cpd = "gray"),
                           high = list(gene = highColor, cpd = "yellow"),
                           na.col = "transparent",
                           ...) {
      dtypes <- !is.null(gene.data) + (!is.null(cpd.data))
      cond0 <- dtypes == 1 & is.numeric(limit) & length(limit) > 1
      if (cond0) {
        if (limit[1] != limit[2] & is.null(names(limit))) {
          limit <- list(gene = limit[1:2], cpd = limit[1:2])
        }
      }
      if (is.null(trans.fun)) {
        trans.fun <- list(gene = NULL, cpd = NULL)
      }
      arg.len2 <- c(
        "discrete", "limit", "bins", "both.dirs", "trans.fun",
        "low", "mid", "high"
      )
      for (arg in arg.len2) {
        obj1 <- eval(as.name(arg))
        if (length(obj1) == 1) {
          obj1 <- rep(obj1, 2)
        }
        if (length(obj1) > 2) {
          obj1 <- obj1[1:2]
        }
        obj1 <- as.list(obj1)
        ns <- names(obj1)
        if (length(ns) == 0 | !all(c("gene", "cpd") %in% ns)) {
          names(obj1) <- c("gene", "cpd")
        }
        assign(arg, obj1)
      }
      if (is.character(gene.data)) {
        gd.names <- gene.data
        gene.data <- rep(1, length(gene.data))
        names(gene.data) <- gd.names
        both.dirs$gene <- FALSE
        ng <- length(gene.data)
        nsamp.g <- 1
      } else if (!is.null(gene.data)) {
        if (length(dim(gene.data)) == 2) {
          gd.names <- rownames(gene.data)
          ng <- nrow(gene.data)
          nsamp.g <- 2
        } else if (is.numeric(gene.data) & is.null(dim(gene.data))) {
          gd.names <- names(gene.data)
          ng <- length(gene.data)
          nsamp.g <- 1
        } else {
          stop("wrong gene.data format!")
        }
      } else if (is.null(cpd.data)) {
        stop("gene.data and cpd.data are both NULL!")
      }
      gene.idtype <- toupper(gene.idtype)
      bods <- pathview::bods
      if (species != "ko") {
        species.data <- pathview::kegg.species.code(
          species,
          na.rm = T,
          code.only = FALSE
        )
      } else {
        species.data <- c(
          kegg.code = "ko",
          entrez.gnodes = "0",
          kegg.geneid = "K01488",
          ncbi.geneid = NA,
          ncbi.proteinid = NA,
          uniprot = NA
        )
        gene.idtype <- "KEGG"
        msg.fmt <- "Only KEGG ortholog gene ID is supported, make sure it looks like \"%s\"!"
        msg <- sprintf(msg.fmt, species.data["kegg.geneid"])
        message("Note: ", msg)
      }
      if (length(dim(species.data)) == 2) {
        message("Note: ", "More than two valide species!")
        species.data <- species.data[1, ]
      }
      species <- species.data["kegg.code"]
      entrez.gnodes <- species.data["entrez.gnodes"] == 1
      if (is.na(species.data["ncbi.geneid"])) {
        if (!is.na(species.data["kegg.geneid"])) {
          msg.fmt <- "Mapping via KEGG gene ID (not Entrez) is supported for this species,\nit looks like \"%s\"!"
          msg <- sprintf(msg.fmt, species.data["kegg.geneid"])
          message("Note: ", msg)
        } else {
          stop("This species is not annotated in KEGG!")
        }
      }
      if (is.null(gene.annotpkg)) {
        gene.annotpkg <- bods[match(species, bods[, 3]), 1]
      }
      if (
        length(grep("ENTREZ|KEGG|NCBIPROT|UNIPROT", gene.idtype)) < 1 & !is.null(gene.data)
      ) {
        if (is.na(gene.annotpkg)) {
          stop("No proper gene annotation package available!")
        }
        if (!gene.idtype %in% gene.idtype.bods[[species]]) {
          stop("Wrong input gene ID type!")
        }
        gene.idmap <- pathview::id2eg(
          gd.names,
          category = gene.idtype,
          pkg.name = gene.annotpkg,
          unique.map = F
        )
        gene.data <- pathview::mol.sum(gene.data, gene.idmap)
        gene.idtype <- "ENTREZ"
      }
      if (gene.idtype != "KEGG" & !entrez.gnodes & !is.null(gene.data)) {
        id.type <- gene.idtype
        if (id.type == "ENTREZ") {
          id.type <- "ENTREZID"
        }
        kid.map <- names(species.data)[-c(1:2)]
        kid.types <- names(kid.map) <- c(
          "KEGG", "ENTREZID", "NCBIPROT", "UNIPROT"
        )
        kid.map2 <- gsub("[.]", "-", kid.map)
        kid.map2["UNIPROT"] <- "up"
        if (is.na(kid.map[id.type])) {
          stop("Wrong input gene ID type for the species!")
        }
        message("Info: Getting gene ID data from KEGG...")
        gene.idmap <- KEGGREST::keggConv(kid.map2[id.type], species)
        message("Info: Done with data retrieval!")
        kegg.ids <- gsub(paste(species, ":", sep = ""), "", names(gene.idmap))
        in.ids <- gsub(paste0(kid.map2[id.type], ":"), "", gene.idmap)
        gene.idmap <- cbind(in.ids, kegg.ids)
        gene.data <- pathview::mol.sum(gene.data, gene.idmap)
        gene.idtype <- "KEGG"
      }
      if (is.character(cpd.data)) {
        cpdd.names <- cpd.data
        cpd.data <- rep(1, length(cpd.data))
        names(cpd.data) <- cpdd.names
        both.dirs$cpd <- FALSE
        ncpd <- length(cpd.data)
      } else if (!is.null(cpd.data)) {
        if (length(dim(cpd.data)) == 2) {
          cpdd.names <- rownames(cpd.data)
          ncpd <- nrow(cpd.data)
        } else if (is.numeric(cpd.data) & is.null(dim(cpd.data))) {
          cpdd.names <- names(cpd.data)
          ncpd <- length(cpd.data)
        } else {
          stop("wrong cpd.data format!")
        }
      }
      if (length(grep("kegg", cpd.idtype)) < 1 & !is.null(cpd.data)) {
        data(rn.list)
        cpd.types <- c(names(rn.list), "name")
        cpd.types <- tolower(cpd.types)
        cpd.types <- cpd.types[-grep("kegg", cpd.types)]
        if (!tolower(cpd.idtype) %in% cpd.types) {
          stop("Wrong input cpd ID type!")
        }
        cpd.idmap <- pathview::cpd2kegg(cpdd.names, in.type = cpd.idtype)
        cpd.data <- pathview::mol.sum(cpd.data, cpd.idmap)
      }
      warn.fmt <- "Parsing %s file failed, please check the file!"
      if (length(grep(species, pathway.id)) > 0) {
        pathway.name <- pathway.id
        pathway.id <- gsub(species, "", pathway.id)
      } else {
        pathway.name <- paste(species, pathway.id, sep = "")
      }
      kfiles <- list.files(path = kegg.dir, pattern = "[.]xml|[.]png")
      npath <- length(pathway.id)
      out.list <- list()
      tfiles.xml <- paste(pathway.name, "xml", sep = ".")
      tfiles.png <- paste(pathway.name, "png", sep = ".")
      if (kegg.native) {
        ttype <- c("xml", "png")
      } else {
        ttype <- "xml"
      }
      xml.file <- paste(kegg.dir, "/", tfiles.xml, sep = "")
      for (i in 1:npath) {
        if (kegg.native) {
          tfiles <- c(tfiles.xml[i], tfiles.png[i])
        } else {
          tfiles <- tfiles.xml[i]
        }
        if (!all(tfiles %in% kfiles)) {
          dstatus <- pathview::download.kegg(
            pathway.id = pathway.id[i],
            species = species,
            kegg.dir = kegg.dir,
            file.type = ttype
          )
          if (dstatus == "failed") {
            warn.fmt <- "Failed to download KEGG xml/png files, %s skipped!"
            warn.msg <- sprintf(warn.fmt, pathway.name[i])
            message("Warning: ", warn.msg)
            return(invisible(0))
          }
        }
        if (kegg.native) {
          node.data <- try(pathview::node.info(xml.file[i]), silent = T)
          if (class(node.data) == "try-error") {
            warn.msg <- sprintf(warn.fmt, xml.file[i])
            message("Warning: ", warn.msg)
            return(invisible(0))
          }
          node.type <- c("gene", "enzyme", "compound", "ortholog")
          sel.idx <- node.data$type %in% node.type
          nna.idx <- !is.na(
            node.data$x + node.data$y + node.data$width + node.data$height
          )
          sel.idx <- sel.idx & nna.idx
          if (sum(sel.idx) < min.nnodes) {
            warn.fmt <- "Number of mappable nodes is below %d, %s skipped!"
            warn.msg <- sprintf(warn.fmt, min.nnodes, pathway.name[i])
            message("Warning: ", warn.msg)
            return(invisible(0))
          }
          node.data <- lapply(node.data, "[", sel.idx)
        } else {
          gR1 <- try(
            pathview::parseKGML2Graph2(
              xml.file[i],
              genes = F,
              expand = expand.node,
              split.group = split.group
            ),
            silent = T
          )
          node.data <- try(
            pathview::node.info(gR1),
            silent = T
          )
          if (class(node.data) == "try-error") {
            warn.msg <- sprintf(warn.fmt, xml.file[i])
            message("Warning: ", warn.msg)
            return(invisible(0))
          }
        }
        if (species == "ko") {
          gene.node.type <- "ortholog"
        } else {
          gene.node.type <- "gene"
        }
        if ((
          !is.null(gene.data) | map.null) & sum(node.data$type == gene.node.type) > 1
        ) {
          plot.data.gene <- pathview::node.map(
            gene.data,
            node.data,
            node.types = gene.node.type,
            node.sum = node.sum,
            entrez.gnodes = entrez.gnodes
          )
          kng <- plot.data.gene$kegg.names
          kng.char <- gsub("[0-9]", "", unlist(kng))
          if (any(kng.char > "")) {
            entrez.gnodes <- FALSE
          }
          if (map.symbol & species != "ko" & entrez.gnodes) {
            if (is.na(gene.annotpkg)) {
              warn.fmt <- "No annotation package for the species %s, gene symbols not mapped!"
              warn.msg <- sprintf(warn.fmt, species)
              message("Warning: ", warn.msg)
            } else {

              plot.data.gene$labels <- NA
              plot.data.gene$labels <- pathview::eg2id(
                as.character(plot.data.gene$kegg.names),
                category = "SYMBOL",
                pkg.name = gene.annotpkg
              )[, 2]
              mapped.gnodes <- rownames(plot.data.gene)
              node.data$labels[mapped.gnodes] <- plot.data.gene$labels
            }
          }
          cols.ts.gene <- pathview::node.color(
            plot.data.gene,
            limit$gene,
            bins$gene,
            both.dirs = both.dirs$gene,
            trans.fun = trans.fun$gene,
            discrete = discrete$gene,
            low = low$gene,
            mid = mid$gene,
            high = high$gene,
            na.col = na.col
          )
        } else {
          plot.data.gene <- cols.ts.gene <- NULL
        }
        if ((
          !is.null(cpd.data) | map.null) & sum(node.data$type == "compound") > 1
        ) {
          plot.data.cpd <- pathview::node.map(
            cpd.data,
            node.data,
            node.types = "compound",
            node.sum = node.sum
          )
          if (map.cpdname & !kegg.native) {
            plot.data.cpd$labels <- pathview::cpdkegg2name(plot.data.cpd$labels)[, 2]
            mapped.cnodes <- rownames(plot.data.cpd)
            node.data$labels[mapped.cnodes] <- plot.data.cpd$labels
          }
          cols.ts.cpd <- pathview::node.color(
            plot.data.cpd,
            limit$cpd,
            bins$cpd,
            both.dirs = both.dirs$cpd,
            trans.fun = trans.fun$cpd,
            discrete = discrete$cpd,
            low = low$cpd,
            mid = mid$cpd,
            high = high$cpd,
            na.col = na.col
          )
        } else {
          plot.data.cpd <- cols.ts.cpd <- NULL
        }
        if (kegg.native) {
          pv.pars <- my.keggview.native(
            plot.data.gene = plot.data.gene,
            cols.ts.gene = cols.ts.gene,
            plot.data.cpd = plot.data.cpd,
            cols.ts.cpd = cols.ts.cpd,
            node.data = node.data,
            pathway.name = pathway.name[i],
            kegg.dir = kegg.dir,
            limit = limit,
            bins = bins,
            both.dirs = both.dirs,
            discrete = discrete,
            low = low,
            mid = mid,
            high = high,
            na.col = na.col,
            ...
          )
        } else {
          pv.pars <- pathview::keggview.graph(
            plot.data.gene = plot.data.gene,
            cols.ts.gene = cols.ts.gene,
            plot.data.cpd = plot.data.cpd,
            cols.ts.cpd = cols.ts.cpd,
            node.data = node.data,
            path.graph = gR1,
            pathway.name = pathway.name[i],
            map.cpdname = map.cpdname,
            split.group = split.group,
            limit = limit,
            bins = bins,
            both.dirs = both.dirs,
            discrete = discrete,
            low = low,
            mid = mid,
            high = high,
            na.col = na.col,
            ...
          )
        }
        plot.data.gene <- cbind(plot.data.gene, cols.ts.gene)
        if (!is.null(plot.data.gene)) {
          cnames <- colnames(plot.data.gene)[-(1:8)]
          nsamp <- length(cnames) / 2
          if (nsamp > 1) {
            cnames[(nsamp + 1):(2 * nsamp)] <- paste(
              cnames[(nsamp + 1):(2 * nsamp)], "col",
              sep = "."
            )
          } else {
            cnames[2] <- "mol.col"
          }
          colnames(plot.data.gene)[-(1:8)] <- cnames
        }
        plot.data.cpd <- cbind(plot.data.cpd, cols.ts.cpd)
        if (!is.null(plot.data.cpd)) {
          cnames <- colnames(plot.data.cpd)[-(1:8)]
          nsamp <- length(cnames) / 2
          if (nsamp > 1) {
            cnames[(nsamp + 1):(2 * nsamp)] <- paste(
              cnames[(nsamp + 1):(2 * nsamp)], "col",
              sep = "."
            )
          } else {
            cnames[2] <- "mol.col"
          }
          colnames(plot.data.cpd)[-(1:8)] <- cnames
        }
        out.list[[i]] <- list(
          plot.data.gene = plot.data.gene,
          plot.data.cpd = plot.data.cpd
        )
      }
      if (npath == 1) {
        out.list <- out.list[[1]]
      } else {
        names(out.list) <- pathway.name
      }
      return(invisible(out.list))
    }

    my.keggview.native <- function(plot.data.gene = NULL,
                                   plot.data.cpd = NULL,
                                   cols.ts.gene = NULL,
                                   cols.ts.cpd = NULL,
                                   node.data,
                                   pathway.name,
                                   out.suffix = "pathview",
                                   kegg.dir = ".",
                                   multi.state = TRUE,
                                   match.data = TRUE,
                                   same.layer = TRUE,
                                   res = 400,
                                   cex = 0.25,
                                   discrete = list(gene = FALSE, cpd = FALSE),
                                   limit = list(gene = 1, cpd = 1),
                                   bins = list(gene = 10, cpd = 10),
                                   both.dirs = list(gene = T, cpd = T),
                                   low = list(gene = "green", cpd = "blue"),
                                   mid = list(gene = "gray", cpd = "gray"),
                                   high = list(gene = "red", cpd = "yellow"),
                                   na.col = "transparent",
                                   new.signature = TRUE,
                                   plot.col.key = TRUE,
                                   key.align = "x",
                                   key.pos = "topright",
                                   ...) {
      img <- png::readPNG(
        paste(kegg.dir, "/", pathway.name, ".png", sep = "")
      )
      width <- ncol(img)
      height <- nrow(img)
      cols.ts.gene <- cbind(cols.ts.gene)
      cols.ts.cpd <- cbind(cols.ts.cpd)
      nc.gene <- max(ncol(cols.ts.gene), 0)
      nc.cpd <- max(ncol(cols.ts.cpd), 0)
      nplots <- max(nc.gene, nc.cpd)
      pn.suffix <- colnames(cols.ts.gene)
      if (length(pn.suffix) < nc.cpd) {
        pn.suffix <- colnames(cols.ts.cpd)
      }
      if (length(pn.suffix) < nplots) {
        pn.suffix <- 1:nplots
      }
      if (length(pn.suffix) == 1) {
        pn.suffix <- out.suffix
      } else {
        pn.suffix <- paste(out.suffix, pn.suffix, sep = ".")
      }
      na.col <- colorpanel2(1, low = na.col, high = na.col)
      if ((match.data | !multi.state) & nc.gene != nc.cpd) {
        if (nc.gene > nc.cpd & !is.null(cols.ts.cpd)) {
          na.mat <- matrix(na.col, ncol = nplots - nc.cpd, nrow = nrow(cols.ts.cpd))
          cols.ts.cpd <- cbind(cols.ts.cpd, na.mat)
        }
        if (nc.gene < nc.cpd & !is.null(cols.ts.gene)) {
          na.mat <- matrix(
            na.col,
            ncol = nplots - nc.gene,
            nrow = nrow(cols.ts.gene)
          )
          cols.ts.gene <- cbind(cols.ts.gene, na.mat)
        }
        nc.gene <- nc.cpd <- nplots
      }
      out.fmt <- "Working in directory %s"
      wdir <- getwd()
      out.msg <- sprintf(out.fmt, wdir)
      message("Info: ", out.msg)
      out.fmt <- "Writing image file %s"
      multi.state <- multi.state & nplots > 1
      if (multi.state) {
        nplots <- 1
        pn.suffix <- paste(out.suffix, "multi", sep = ".")
        if (nc.gene > 0) {
          cols.gene.plot <- cols.ts.gene
        }
        if (nc.cpd > 0) {
          cols.cpd.plot <- cols.ts.cpd
        }
      }
      for (np in 1:nplots) {
        img.file <- paste(
          kegg.dir,
          "/",
          pathway.name,
          ".",
          pn.suffix[np],
          ".png",
          sep = ""
        )
        out.msg <- sprintf(out.fmt, img.file)
        message("Info: ", out.msg)
        png(img.file, width = width, height = height, res = res)
        op <- par(mar = c(0, 0, 0, 0))
        plot(
          c(0, width),
          c(0, height),
          type = "n",
          xlab = "",
          ylab = "",
          xaxs = "i",
          yaxs = "i"
        )
        if (new.signature) {
          img[height - 4:25, 17:137, 1:3] <- 1
        }
        if (same.layer != T) {
          rasterImage(img, 0, 0, width, height, interpolate = F)
        }
        if (!is.null(cols.ts.gene) & nc.gene >= np) {
          if (!multi.state) {
            cols.gene.plot <- cols.ts.gene[, np]
          }
          if (same.layer != T) {
            render.kegg.node(
              plot.data.gene,
              cols.gene.plot,
              img,
              same.layer = same.layer,
              type = "gene",
              cex = cex
            )
          } else {
            plot.data.gene$width <- 46
            plot.data.gene$height <- 17
            img <- render.kegg.node(
              plot.data.gene,
              cols.gene.plot,
              img,
              same.layer = same.layer,
              type = "gene"
            )
          }
        }
        if (!is.null(cols.ts.cpd) & nc.cpd >= np) {
          if (!multi.state) {
            cols.cpd.plot <- cols.ts.cpd[, np]
          }
          if (same.layer != T) {
            render.kegg.node(
              plot.data.cpd,
              cols.cpd.plot,
              img,
              same.layer = same.layer,
              type = "compound",
              cex = cex
            )
          } else {
            img <- render.kegg.node(
              plot.data.cpd,
              cols.cpd.plot,
              img,
              same.layer = same.layer,
              type = "compound"
            )
          }
        }
        if (same.layer == T) {
          graphics::rasterImage(img, 0, 0, width, height, interpolate = F)
        }
        pv.pars <- list()
        pv.pars$gsizes <- c(width = width, height = height)
        pv.pars$nsizes <- c(46, 17)
        pv.pars$op <- op
        pv.pars$key.cex <- 2 * 72 / res
        pv.pars$key.lwd <- 1.2 * 72 / res
        pv.pars$sign.cex <- cex
        off.sets <- c(x = 0, y = 0)
        align <- "n"
        ucol.gene <- unique(as.vector(cols.ts.gene))
        na.col.gene <- ucol.gene %in% c(na.col, NA)
        if (plot.col.key & !is.null(cols.ts.gene) & !all(na.col.gene)) {
          off.sets <- pathview::col.key(
            limit = limit$gene,
            bins = bins$gene,
            both.dirs = both.dirs$gene,
            discrete = discrete$gene,
            graph.size = pv.pars$gsizes,
            node.size = pv.pars$nsizes,
            key.pos = key.pos,
            cex = pv.pars$key.cex,
            lwd = pv.pars$key.lwd,
            low = low$gene,
            mid = mid$gene,
            high = high$gene,
            align = "n"
          )
          align <- key.align
        }
        ucol.cpd <- unique(as.vector(cols.ts.cpd))
        na.col.cpd <- ucol.cpd %in% c(na.col, NA)
        if (plot.col.key & !is.null(cols.ts.cpd) & !all(na.col.cpd)) {
          off.sets <- pathview::col.key(
            limit = limit$cpd,
            bins = bins$cpd,
            both.dirs = both.dirs$cpd,
            discrete = discrete$cpd,
            graph.size = pv.pars$gsizes,
            node.size = pv.pars$nsizes,
            key.pos = key.pos,
            off.sets = off.sets,
            cex = pv.pars$key.cex,
            lwd = pv.pars$key.lwd,
            low = low$cpd,
            mid = mid$cpd,
            high = high$cpd,
            align = align
          )
        }
        if (new.signature) {
          pathview.stamp(x = 17, y = 20, on.kegg = T, cex = pv.pars$sign.cex)
        }
        par(pv.pars$op)
        dev.off()
      }
      return(invisible(pv.pars))
    }
    tmpfun <- get("keggview.native", envir = asNamespace("pathview"))
    environment(my.keggview.native) <- environment(tmpfun)
    attributes(my.keggview.native) <- attributes(tmpfun)
    riborexMatrix <- read.csv(paste0(TEFileuserDir, "/normalcount.csv"), check.names = FALSE)
    riborexMatrix <- as.data.frame(riborexMatrix)
    if(selectOrg == "5843334"){
      zea_may_id_mapping <- fread(paste0(TEFileuserDir, "/zea_may_mapping_data_frame.csv"))
      riborexMatrix <- riborexMatrix %>%
        left_join(zea_may_id_mapping, by = c("GeneID" = "id")) %>%
        mutate(new_GeneID = ifelse(is.na(ens), "null", ens)) %>%
        select(new_GeneID, everything()) %>%
        select(-ens, -idType) %>%
        mutate(GeneID = ifelse(new_GeneID == "null", NA, new_GeneID)) %>%
        select(-new_GeneID) %>%
        arrange(GeneID, desc(gene_name)) %>%  
        distinct(GeneID, .keep_all = TRUE) %>%
        filter(!is.na(GeneID))
    }
    riborexMatrix <- riborexMatrix[, c("GeneID", "log2FoldChange", "padj")]
    rownames(riborexMatrix) <- riborexMatrix$GeneID
    keggMatrix <- riborexMatrix[, -1]
    colnames(keggMatrix) <- c("Fold", "FDR")
    fold <- keggMatrix[, 1]
    names(fold) <- rownames(keggMatrix)
    species <- converted$species[1, 1]
    
    fold <- convertEnsemblToEntrez(
      query = fold,
      species = species,
      orgInfo = allOrgData$orgInfo,
      allOrgData = allOrgData
    )
    keggSpecies <- as.character(
      allOrgData$orgInfo$KEGG[which(allOrgData$orgInfo$ensembl_dataset == species)]
    )
    if(selectOrg == "5843334"){
      keggSpecies <- keggSpecies[[1]]
    }
    sigPathways <- input$selectedPathway
    pathId <- gsub(" .*", "", sigPathways)
    pathId <- gsub("Path:", "", pathId)
    randomString <- gsub(".*file", "", tempfile())
    tempFolder <- TEFileuserDir
    outFile <- paste(tempFolder, "/", pathId, ".", randomString, ".png", sep = "")
    shinyEnv$pathwayImagePath <- outFile
    pv.out <- mypathview(
      gene.data = fold,
      pathway.id = pathId,
      kegg.dir = tempFolder,
      out.suffix = randomString,
      species = keggSpecies,
      kegg.native = TRUE
    )
  }
  ####Enrichment Result----
  output$GeneOntology <- renderUI({
    if(te()){
      conversionInfo <- fromJSON(paste0(TEFileuserDir, "/orgData.json"))
      geneOntologyMethod <- conversionInfo[[5]]
      selectInput("selectedGeneOntology", "Select Gene Ontology Method", choices = geneOntologyMethod, selected = geneOntologyMethod[[5]])
    } else {
      return(NULL)
    }
  })
  enrichmentResult <- function(){
    req(file.exists(paste0(TEFileuserDir, "/orgData.json")))
    req(file.exists(paste0(TEFileuserDir, "/normalcount.csv")))
    runjs('$(".mask").show();')
    shinyjs::show("loader-overlay6")
    shinyjs::runjs('$(".RiboTE-working-btn-enrichment").removeClass("RiboTE-working-hidden");')
    conversionInfo <- fromJSON(paste0(TEFileuserDir,"/orgData.json"))
    baseData <- fromJSON(paste0(TEFileuserDir,"/baseData.json"))
    riborexMatrix <- read.csv(paste0(TEFileuserDir, "/normalcount.csv"), check.names = FALSE)
    riborexMatrix <- as.data.frame(riborexMatrix)
    ncol <- conversionInfo[[7]]
    normalcount <- riborexMatrix[, c("GeneID", colnames(riborexMatrix)[3:(ncol + 2)])]
    rownames(normalcount) <- normalcount$GeneID
    normalcount <- normalcount[, -1]
    riborexMatrix <- riborexMatrix[, c("GeneID", "log2FoldChange", "padj")]
    res <- riborexMatrix
    processedData <- normalcount
    geneLists <- pathwayDeg(conversionInfo, normalcount, res)
    geneInfo <- conversionInfo[[2]]
    allOrgData <- conversionInfo[[6]]
    selectOrg <- baseData[[13]]
    converted <- conversionInfo[[1]]
    gmtFile <- NULL
    plotGridLines <- FALSE
    ggplot2Theme <- "default"

    filteredBackground <- input$filteredBackground
    removeRedudant <- input$removeRedudant
    sortBy <- input$sortBy
    showPathwayId <- input$showPathwayId
    topPathway <- input$topPathways
    req(!is.null(input$selectedGeneOntology))
    geneOntology <- input$selectedGeneOntology

    pathwayTable <- function(geneLists, converted, allOrgData, selectOrg, gmtFile, geneInfo, processedData){
      req(!is.null(geneLists))
      pathwayInfo <- list()
      for (i in 1:length(geneLists)) {
        geneNamesQuery <- geneLists[[i]]
        req(!is.null(geneOntology))
        geneSets <- readPathwaySets(
          allGeneNamesQuery = geneNamesQuery,
          converted = converted,
          pathwayDatabase = geneOntology,
          selectOrg = selectOrg,
          gmtFile = gmtFile,
          allOrgData = allOrgData,
          geneInfo = geneInfo
        )
        pathwayInfo[[names(geneLists)[i]]] <- findOverlap(
          pathwayTable = geneSets$pathwayTable,
          querySet = geneSets$querySet,
          totalGenes = geneSets$totalGenes,
          processedData = processedData,
          geneInfo = geneInfo,
          pathwayDatabase = geneOntology,
          allOrgData = allOrgData,
          selectOrg = ifelse(is.null(gmtFile), selectOrg, "NEW"),
          useFilteredBackground = filteredBackground,
          reduced = removeRedudant,
          maxTerms = topPathway,
          sortByFold = (sortBy == "Fold")
        )
      }
      if(identical(pathwayInfo[[1]], data.frame("Enrichment" = "No significant enrichment found!")) &&
         identical(pathwayInfo[[2]], data.frame("Enrichment" = "No significant enrichment found!"))) {
        pathwayInfo$Up <- data.frame(Regulation = "Upregulated", pathwayInfo$Up)
        pathwayInfo$Down <- data.frame(Regulation = "Downregulated", pathwayInfo$Down)
        pathwayErrorData <- rbind(pathwayInfo$Up, pathwayInfo$Down)
        return(pathwayErrorData)
      }
      if (!showPathwayId && selectOrg > 0) {
        for (i in 1:length(pathwayInfo)) {
          pathwayInfo[[i]]$Pathway <- removePathwayId(
            strings = pathwayInfo[[i]]$Pathway,
            pathwayDatabase = geneOntology
          )
        }
      }
      return(pathwayInfo)
    }
    enrichmentDataframe <- function(geneLists, converted, allOrgData, selecOrg, gmtFile, geneInfo, processedData){
      pathwayTable <- pathwayTable(geneLists, converted, allOrgData, selecOrg, gmtFile, geneInfo, processedData)
      if(is.data.frame(pathwayTable)){
        return(pathwayTable)
      }
      resultsAll <- do.call(
        rbind,
        lapply(
          names(pathwayTable),
          function(x) {
            if (ncol(pathwayTable[[x]]) == 1) {
              return(NULL)
            }
            df1 <- dataFrameWithList(pathwayTable[[x]])
            df1$group <- x
            return(df1)
          }
        )
      )
      if (!is.null(resultsAll)) {
        if (ncol(resultsAll) > 1) {
          resultsAll <- resultsAll[
            ,
            c(
              "group",
              colnames(resultsAll)[1:(ncol(resultsAll) - 1)]
            )
          ]
        }
      }
      return(resultsAll)
    }
    showEnrichment <- function(geneLists, converted, allOrgData, selectOrg, gmtFile, geneInfo, processedData){
      enrichmentDataframe <- enrichmentDataframe(geneLists, converted, allOrgData, selectOrg, gmtFile, geneInfo, processedData)
      if(enrichmentDataframe[1,2] == "No significant enrichment found!" && enrichmentDataframe[2,2] == "No significant enrichment found!"){
        return(enrichmentDataframe)
      }
      res <- enrichmentDataframe
      colnames(res) <- gsub("\\.", " ", colnames(res))
      if (length(unique(res$group)) == 1) {
        res$group[duplicated(res$group)] <- ""
      } else {
        res$group[duplicated(res$group)] <- ""
      }
      res$FDR <- gsub("e-0", "e-", res$FDR)
      res$FDR <- gsub("e", "E", res$FDR)
      res$"nGenes" <- as.character(res$"nGenes")
      res$"Fold enriched" <- as.character(round(res$"Fold enriched", 1))

      res$"Pathway size" <- as.character(
        res$"Pathway size"
      )

      res$"Pathway" <- hyperText(
        res$"Pathway",
        res$URL
      )
      res <- subset(res, select = -Genes)
      res <- subset(res, select = -URL)
      colnames(res) <- gsub("Pathway size", "PathwaySize", colnames(res))
      colnames(res) <- gsub("Fold enriched", "Fold", colnames(res))
      colnames(res) <- gsub("FDR", "Adj.Pval", colnames(res))
      colnames(res) <- gsub("group", "Group.", colnames(res))
      res <- subset(res, select = -PathwaySize)

      colnames(res)[ncol(res)] <- "Pathway (Click for more info)"
      res <- subset(res, select = -nGenes)
      uniqueRes <- unique(res[[1]][!is.na(res[[1]]) & res[[1]] != ""])

      if (length(uniqueRes) == 1 && uniqueRes == "Down") {
        tempDataFrame <- data.frame(Group. = "Up", Adj.Pval = NA, Fold = NA, Pathway..Click.for.more.info. = "No significant enrichment found.")
        names(tempDataFrame) <- names(res)
        res <- rbind(res, tempDataFrame)
      } else if (length(uniqueRes) == 1 && uniqueRes == "Up"){
        tempDataFrame <- data.frame(Group. = "Down", Adj.Pval = NA, Fold = NA, Pathway..Click.for.more.info. = "No significant enrichment found.")
        names(tempDataFrame) <- names(res)
        res <- rbind(res, tempDataFrame)
      }
      return(res)
    }
    showEnrichment <- showEnrichment(geneLists, converted, allOrgData, selectOrg, gmtFile, geneInfo, processedData)
    fwrite(showEnrichment, paste0(TEFileuserDir, "/enrichment_table.csv"))
    return(showEnrichment)
  }
  ####WGCNA Result----
  WGCNAFirstData <- function(){
    req(file.exists(paste0(TEFileuserDir, "/WGCNAData.csv")))
    req(file.exists(paste0(TEFileuserDir, "/orgData.json")))
    runjs('$(".mask").show();')
    wgcnadata(FALSE)
    shinyjs::show("loader-overlay7")
    shinyjs::runjs('$(".RiboTE-working-btn-network").removeClass("RiboTE-working-hidden");')
    conversionInfo <- fromJSON(paste0(TEFileuserDir,"/orgData.json"))
    baseData <- fromJSON(paste0(TEFileuserDir,"/baseData.json"))
    WGCNAData <- read.csv(paste0(TEFileuserDir, "/WGCNAData.csv"),row.names = 1, check.names = FALSE)
    selectOrg <- baseData[[13]]
    if(selectOrg == "5843334"){
      zea_may_id_mapping <- fread(paste0(TEFileuserDir, "/zea_may_mapping_data_frame.csv"))
      WGCNAData <- WGCNAData %>%
        mutate(new_id = rownames(WGCNAData))
      WGCNAData <- WGCNAData %>%
        left_join(zea_may_id_mapping, by = c("new_id" = "id")) %>%
        mutate(ens_new_id = ifelse(is.na(ens), "null", ens)) %>%
        select(-new_id, -ens, -idType) %>%
        filter(ens_new_id != "null") %>%  
        distinct(ens_new_id, .keep_all = TRUE)
      rownames(WGCNAData) <- WGCNAData$ens_new_id
      WGCNAData <- WGCNAData %>% select(-ens_new_id)
      gff <- baseData[[5]]
      gff$GeneID <- toupper(gff$GeneID)
      gff <- gff %>%
        left_join(zea_may_id_mapping, by = c("GeneID" = "id")) %>%
        mutate(new_GeneID = ifelse(is.na(ens), "null", ens)) %>%
        select(-GeneID) %>%
        select(seqnames, start, end, width, strand, source, type, new_GeneID, everything()) %>%
        rename(GeneID = new_GeneID) %>%
        select(-ens, -idType) %>% 
        filter(GeneID != "null")
      conversionInfo[[2]] <- conversionInfo[[2]] %>%
        mutate(symbol = "null")
      conversionInfo[[2]] <- conversionInfo[[2]] %>%
        left_join(gff %>% select(GeneID, gene_name), by = c("ensembl_gene_id" = "GeneID")) %>%
        mutate(symbol = ifelse(!is.na(gene_name), gene_name, symbol)) %>%
        select(-gene_name)
      conversionInfo[[2]] <- conversionInfo[[2]] %>%
        filter(symbol != "null" & symbol != "")
    }
    nGenesNetwork <- input$nGenesNetwork
    softPower <- input$softPower
    minModuleSize <- input$minModuleSize
    edgeThreshold <- input$edgeThreshold
    topGenesNetwork <- input$topGenesNetwork
    selectOrg <- baseData[[13]]
    WGCNAData <- WGCNAData[!apply(WGCNAData, 1, function(row) any(is.infinite(row))), ]
    moduleList <- getWGCNAModules(WGCNA = WGCNA(WGCNAData, nGenesNetwork, softPower, minModuleSize))
    WGCNA <- list(moduleList, selectOrg, topGenesNetwork, conversionInfo, edgeThreshold, nGenesNetwork, WGCNAData, softPower, minModuleSize)
    wgcnadata(TRUE)
    write_json(WGCNA[[1]], paste0(TEFileuserDir, "/WGCNAmoduleList.json"))
    return(WGCNA)
  }
  output$moduleList <- renderUI({
    if(wgcnadata() && te()){
      req(file.exists(paste0(TEFileuserDir, "/WGCNAmoduleList.json")))
      moduleList <- fromJSON(paste0(TEFileuserDir, "/WGCNAmoduleList.json"))
      selectInput("moduleSelect", "4.Select a module", choices = moduleList, selected = input$moduleSelect)
    } else {
      return(NULL)
    }
  })
  
  
  
  WGCNAPlot <- function(WGCNAFirstData){
    req(input$moduleSelect)
    moduleList <- input$moduleSelect
    selectOrg <- WGCNAFirstData[[2]]
    topGenesNetwork <- WGCNAFirstData[[3]]
    conversionInfo <- WGCNAFirstData[[4]]
    edgeThreshold <- WGCNAFirstData[[5]]
    nGenesNetwork <- WGCNAFirstData[[6]]
    WGCNAData <- WGCNAFirstData[[7]]
    softPower <- WGCNAFirstData[[8]]
    minModuleSize <- WGCNAFirstData[[9]]
    network <- list(networkPlot = NULL)
    network$networkPlot <- getNetworkPlot(adjacencyMatrix = adjMatrix(moduleList, selectOrg, topGenesNetwork, conversionInfo, edgeThreshold, nGenesNetwork, WGCNAData, softPower, minModuleSize), edgeThreshold = edgeThreshold)
    moduleNetwork <- moduleNetwork(moduleList, network, WGCNAData, nGenesNetwork, softPower, minModuleSize)
    return(moduleNetwork)
  }
  ####codon Result----
  observeEvent(input$submitButton, {
    runjs("$('.notification_codon_data').hide();")
    shinyEnv$submitDone <- FALSE
    shinyEnv$submitDone2 <- FALSE
    shinyjs::show("loader-overlay10")
    shinyjs::runjs('$(".RiboTE-working-btn-codon-data").removeClass("RiboTE-working-hidden");')
    if(!file.exists(paste0(TEFileuserDir,"/normalcount.csv"))){
      showModal(modalDialog(
        "The files pertaining to translation efficiency are absent; consequently, it is imperative to execute the translation efficiency module.",
        footer = tagList(
          modalButton("Close"),
        ),easyClose = TRUE,))
      shinyjs::hide("loader-overlay10")
      return(NULL)
    }
    if(is.null(input$codonSelect)){
      showModal(modalDialog(
        "unselected codon!!!",
        footer = tagList(
          modalButton("Close"),
        ),easyClose = TRUE,))
      shinyjs::hide("loader-overlay10")
      return(NULL)
    }
    selectOrgAcronyms <- RiboTE_Org$acronyms[RiboTE_Org$species == input$selectOrg]
    codonFirstDataMake(species = selectOrgAcronyms)
    output$codonPreData <- renderDT({
      if(input$demoData == 0 && is.null(input$geneMatrix)){
        return(NULL)
      }
      req(file.exists(paste0(TEFileuserDir,"/deg.csv")))
      if(file.exists(paste0(TEFileuserDir,"/deg.csv"))){
        codonPreData <- read.csv(paste0(TEFileuserDir,"/deg.csv"), check.names = FALSE)
        datatable(codonPreData,
                  options = list(
                    dom = 'tip',
                    autoWidth = FALSE,
                    pageLength = 30,
                    columnDefs = list(
                      list(width = '100px', targets = "_all", className = 'dt-center')
                    ),
                    scrollX = TRUE
                  ),
                  rownames = FALSE,
                  class = 'hover stripe')
      }
    })
    shinyjs::hide("loader-overlay10")
    shinyjs::runjs('$(".RiboTE-working-btn-codon-data").addClass("RiboTE-working-hidden");')
  })
  updownBoxplot <- function(){
    cbidone(FALSE)
    selectOrgAcronyms <- RiboTE_Org$acronyms[RiboTE_Org$species == input$selectOrg]
    species <- selectOrgAcronyms
    deg <- as.data.table(read.csv(paste0(TEFileuserDir,"/deg.csv"), check.names = FALSE))
    geneinfo <- as.data.table(read.csv(paste0(TEFileuserDir,"/geneinfo.csv"), check.names = FALSE))

    tai <- fread(paste0("/public/wwwdb/ribotoolkit/cds/",species,".tai"))
    cbi <- fread(paste0("/public/wwwdb/ribotoolkit/cds/",species,".cds.m"))
    setnames(cbi,"title","transcript_id")
    cbi[,V4:=NULL]
    tai <- merge(tai,cbi,by="transcript_id")
    tai <- merge(tai,geneinfo[,.(transcript_id,gene_id)],by="transcript_id")
    tai$gene_id <- toupper(tai$gene_id)
    tai <- merge(tai,deg,by="gene_id")
    codonP1 <- ggboxplot(tai, x = 'updown', y = 'CBI',
                         color = 'updown', palette = "jco") +
      yscale("log2") +
      labs(x = "", y = "CBI") +
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
            legend.title = element_blank()) +
      stat_compare_means(aes(group = updown),
                         label = "p.signif", hide.ns = TRUE, vjust = -0.5, hjust = 0.5)
    codonP2 <- ggboxplot(tai, x = 'updown', y = 'tai',
                         color = 'updown', palette = "jco") +
      yscale("log2") +
      labs(x = "", y = "CBI") +
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
            legend.title = element_blank()) +
      stat_compare_means(aes(group = updown),
                         label = "p.signif", hide.ns = TRUE, vjust = -0.5, hjust = 0.5)
    ggsave(paste0(TEFileuserDir, "/codonP1.png"), plot = codonP1, width = 7, height = 5)
    ggsave(paste0(TEFileuserDir, "/codonP2.png"), plot = codonP2, width = 7, height = 5)
    ggsave(paste0(TEFileuserDir, "/codonP1.pdf"), plot = codonP1, width = 7, height = 5)
    ggsave(paste0(TEFileuserDir, "/codonP2.pdf"), plot = codonP2, width = 7, height = 5)
    fwrite(cbi, paste0(TEFileuserDir, "/cbi1.csv"))
    cbidone(TRUE)
  }
  cbiScatter <- function(){
    cbi <- as.data.table(read.csv(paste0(TEFileuserDir,"/cbi1.csv"), check.names = FALSE))
    geneinfo <- as.data.table(read.csv(paste0(TEFileuserDir,"/geneinfo.csv"), check.names = FALSE))
    forcodon <- as.data.table(read.csv(paste0(TEFileuserDir,"/forcodon.csv"), check.names = FALSE))
    cbi <- merge(cbi,geneinfo[,.(transcript_id,gene_id)],by="transcript_id")
    cbi$gene_id <- toupper(cbi$gene_id)
    cbi <- merge(cbi ,forcodon, by="gene_id",all.y=T)
    cbi <- cbi[!is.na(CBI)]
    if(input$cbiMothodSelect == "input"){
      codonP3 <- ggscatter(cbi,
                           x = 'CBI',
                           y = 'input1',
                           size = 2,
                           alpha = 0.6,
                           add = 'reg.line',
                           add.params = list(color = "#00AFBB", fill = "lightgray", size = 1.5),
                           conf.int = TRUE) +
        xlab("CBI") +
        ylab("Gene expression level") +
        geom_density2d() +
        stat_cor(method = 'pearson',
                 label.x = min(cbi$CBI) + 0.1,
                 label.y = max(cbi$input1) - 0.1)

      codonP4 <- ggscatter(cbi,
                           x = 'CBI',
                           y = 'input2',
                           size = 2,
                           alpha = 0.6,
                           add = 'reg.line',
                           add.params = list(color = "#00AFBB",
                                             fill = "lightgray", size = 1.5),
                           conf.int = TRUE) +
        xlab("CBI") +
        ylab("Gene expression level") +
        geom_density2d() +
        stat_cor(method = 'pearson',
                 label.x = min(cbi$CBI) + 0.1,
                 label.y = max(cbi$input2) - 0.1)
      ggsave(paste0(TEFileuserDir,"/codonP3.png"), plot = codonP3, width = 7, height = 5)
      ggsave(paste0(TEFileuserDir,"/codonP4.png"), plot = codonP4, width = 7, height = 5)
      ggsave(paste0(TEFileuserDir,"/codonP3.pdf"), plot = codonP3, width = 7, height = 5)
      ggsave(paste0(TEFileuserDir,"/codonP4.pdf"), plot = codonP4, width = 7, height = 5)
    }
    if(input$cbiMothodSelect == "TE"){
      cbiCleanINF <- cbi[is.finite(cbi$TE_A1), ]
      cbiCleanINF <- cbiCleanINF[is.finite(cbiCleanINF$TE_A2), ]
      codonP5 <- ggscatter(cbiCleanINF,
                           x = 'CBI',
                           y = 'TE_A1',
                           size = 2,
                           alpha = 0.6,
                           add = 'reg.line',
                           add.params = list(color = "#00AFBB",
                                             fill = "lightgray", size = 1),
                           conf.int = TRUE) +
        xlab("CBI") +
        ylab("Translation efficiency") +
        geom_density2d() +
        stat_cor(method = 'pearson',
                 label.x = min(cbiCleanINF$CBI) + 0.1,
                 label.y = max(cbiCleanINF$TE_A1) - 0.1)
      codonP6 <- ggscatter(cbiCleanINF,
                           x = 'CBI',
                           y = 'TE_A2',
                           size = 2,
                           alpha = 0.6,
                           add = 'reg.line',
                           add.params = list(color = "#00AFBB",
                                             fill = "lightgray", size = 1),
                           conf.int = TRUE) +
        xlab("CBI") +
        ylab("Translation efficiency") +
        geom_density2d() +
        stat_cor(method = 'pearson',
                 label.x = min(cbiCleanINF$CBI) + 0.1,
                 label.y = max(cbiCleanINF$TE_A2) - 0.1)
      ggsave(paste0(TEFileuserDir,"/codonP5.png"), plot = codonP5, width = 7, height = 5)
      ggsave(paste0(TEFileuserDir,"/codonP6.png"), plot = codonP6, width = 7, height = 5)
      ggsave(paste0(TEFileuserDir,"/codonP5.pdf"), plot = codonP5, width = 7, height = 5)
      ggsave(paste0(TEFileuserDir,"/codonP6.pdf"), plot = codonP6, width = 7, height = 5)
    }
  }
  percPer1kScatter <- function(){
    percper1k(FALSE)
    percPer1kDataMake()
    if(input$percPer1kUpDownSelect == "Up"){
      objCodonStat3_up <- read.csv(paste0(TEFileuserDir, "/objCodonStat3_up.csv"), check.names = FALSE)
      objCodonStat2_up <- read.csv(paste0(TEFileuserDir, "/objCodonStat2_up.csv"), check.names = FALSE)
      fwrite(objCodonStat3_up,"objCodonStat3_up1111.csv")
      p1 <- ggscatter(objCodonStat3_up,
                      x = "perc",
                      y = "logFC",
                      title = "",
                      size = 2,
                      alpha = 0.6,
                      add = "reg.line",
                      add.params = list(color = "#00AFBB",
                                        fill = "lightgray",size = 1),
                      conf.int = TRUE) +
        labs(x = "perc",
             y = "logFC") +
        stat_cor(method = "pearson",
                 label.x.npc = 0.7,
                 label.y.npc = 1)
      p2 <- ggscatter(objCodonStat2_up,
                      x = "per1k",
                      y = "logFC",
                      title = "",
                      size = 2,
                      alpha = 0.6,
                      add = "reg.line",
                      add.params = list(color = "#00AFBB",
                                        fill = "lightgray", size =1),
                      conf.int = TRUE) +
        labs(x = "per1k",
             y = "logFC") +
        stat_cor(method = "pearson",
                 label.x.npc = 0.7,
                 label.y.npc = 1)
      p3 <- ggscatter(objCodonStat2_up,
                      x = "per1k",
                      y = "fcAndPvalue",
                      title = "",
                      size = 2,
                      alpha = 0.6,
                      add = "reg.line",
                      add.params = list(color = "#00AFBB",
                                        fill = "lightgray", size =1),
                      conf.int = TRUE) +
        labs(x = "Z-scores",
             y = "Expression changes (-log2Pvalue x log2FC)") +
        stat_cor(method = "pearson",
                 label.x.npc = 0.7,
                 label.y.npc = 1)
      ggsave(paste0(TEFileuserDir,"/","scatter_plot_logFC_perc_up.png"), width = 7, height = 5, plot = p1)
      ggsave(paste0(TEFileuserDir,"/","scatter_plot_logFC_per1k_up.png"), width = 7, height = 5, plot = p2)
      ggsave(paste0(TEFileuserDir,"/","scatter_plot_zscore_per1k_up.png"), width = 7, height = 5, plot = p3)
      ggsave(paste0(TEFileuserDir,"/","scatter_plot_logFC_perc_up.pdf"), width = 7, height = 5, plot = p1)
      ggsave(paste0(TEFileuserDir,"/","scatter_plot_logFC_per1k_up.pdf"), width = 7, height = 5, plot = p2)
      ggsave(paste0(TEFileuserDir,"/","scatter_plot_zscore_per1k_up.pdf"), width = 7, height = 5, plot = p3)
      df <- data.frame(
        Name = c("Alice"),
        Age = c(25)
      )
      fwrite(df, paste0(TEFileuserDir, "/codonPercComplete3.csv"))
    }
    if(input$percPer1kUpDownSelect == "Down"){
      objCodonStat3_down <- read.csv(paste0(TEFileuserDir, "/objCodonStat3_down.csv"), check.names = FALSE)
      objCodonStat2_down <- read.csv(paste0(TEFileuserDir, "/objCodonStat2_down.csv"), check.names = FALSE)
      p1 <- ggscatter(objCodonStat3_down,
                      x = "perc",
                      y = "logFC",
                      title = "",
                      size = 2,
                      alpha = 0.6,
                      add = "reg.line",
                      add.params = list(color = "#00AFBB",
                                        fill = "lightgray", size = 1),
                      conf.int = TRUE) +
        labs(x = "perc",
             y = "logFC") +
        stat_cor(method = "pearson",
                 label.x.npc = 0.7,
                 label.y.npc = 0)
      p2 <- ggscatter(objCodonStat2_down,
                      x = "per1k",
                      y = "logFC",
                      title = "",
                      size = 2,
                      alpha = 0.6,
                      add = "reg.line",
                      add.params = list(color = "#00AFBB",
                                        fill = "lightgray", size = 1),
                      conf.int = TRUE) +
        labs(x = "per1k",
             y = "logFC") +
        stat_cor(method = "pearson",
                 label.x.npc = 0.7,
                 label.y.npc = 0)
      p3 <- ggscatter(objCodonStat2_down,
                      x = "per1k",
                      y = "fcAndPvalue",
                      title = "",
                      size = 2,
                      alpha = 0.6,
                      add = "reg.line",
                      add.params = list(color = "#00AFBB",
                                        fill = "lightgray",size = 1),
                      conf.int = TRUE) +
        labs(x = "Z-scores",
             y = "Expression changes (-log2Pvalue x log2FC)") +
        stat_cor(method = "pearson",
                 label.x.npc = 0.7,
                 label.y.npc = 0)
      ggsave(paste0(TEFileuserDir,"/","scatter_plot_logFC_perc_down.png"), width = 7, height = 5, plot = p1)
      ggsave(paste0(TEFileuserDir,"/","scatter_plot_logFC_per1k_down.png"), width = 7, height = 5, plot = p2)
      ggsave(paste0(TEFileuserDir,"/","scatter_plot_zscore_per1k_down.png"), width = 7, height = 5, plot = p3)
      ggsave(paste0(TEFileuserDir,"/","scatter_plot_logFC_perc_down.pdf"), width = 7, height = 5, plot = p1)
      ggsave(paste0(TEFileuserDir,"/","scatter_plot_logFC_per1k_down.pdf"), width = 7, height = 5, plot = p2)
      ggsave(paste0(TEFileuserDir,"/","scatter_plot_zscore_per1k_down.pdf"), width = 7, height = 5, plot = p3)
      df <- data.frame(
        Name = c("Alice"),
        Age = c(25)
      )
      fwrite(df, paste0(TEFileuserDir, "/codonPercComplete3.csv"))
    }
    if(input$percPer1kUpDownSelect == "Up and Down"){
      objCodonStat2 <- read.csv(paste0(TEFileuserDir, "/objCodonStat2.csv"), check.names = FALSE)
      p1 <- ggscatter(
        objCodonStat2,
        x = "per1k",
        y = "logFC",
        title = "",
        size = 2,
        alpha = 0.6,
        add = "reg.line",
        add.params = list(
          color = "#00AFBB",
          fill = "lightgray"
        ),
        conf.int = TRUE
      ) +
        labs(x = "per1k", y = "logFC") +
        stat_cor(
          method = "pearson",
          label.x.npc = 0.7,
          label.y.npc = 1
        )
      p2 <- ggscatter(
        objCodonStat2,
        x = "per1k",
        y = "fcAndPvalue",
        title = "",
        size = 2,
        alpha = 0.6,
        add = "reg.line",
        add.params = list(
          color = "#00AFBB",
          fill = "lightgray"
        ),
        conf.int = TRUE
      ) +
        labs(
          x = "Z-scores",
          y = "Expression changes (-log2Pvalue x log2FC)"
        ) +
        stat_cor(
          method = "pearson",
          label.x.npc = 0.7,
          label.y.npc = 1
        )
      ggsave(paste0(TEFileuserDir,"/","scatter_plot_logFC_per1k.png"), width = 7, height = 5, plot = p1)
      ggsave(paste0(TEFileuserDir,"/","scatter_plot_zscore_per1k.png"), width = 7, height = 5, plot = p2)
      ggsave(paste0(TEFileuserDir,"/","scatter_plot_logFC_per1k.pdf"), width = 7, height = 5, plot = p1)
      ggsave(paste0(TEFileuserDir,"/","scatter_plot_zscore_per1k.pdf"), width = 7, height = 5, plot = p2)
      df <- data.frame(
        Name = c("Alice"),
        Age = c(25)
      )
      fwrite(df, paste0(TEFileuserDir, "/codonPercComplete3.csv"))
    }
    percper1k(TRUE)
  }
  codonFrequencyBoxplot <- function(){
    objCodonStat3 <- read.csv(paste0(TEFileuserDir, "/objCodonStat3.csv"), check.names = FALSE)
    p <- ggboxplot(objCodonStat3,
                   x = 'updown',
                   y = 'perc',
                   color = 'updown',
                   palette = "jco") +
      yscale("log2") +
      labs(x = "",
           y = "Codon frequency") +
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = 8),
            legend.title = element_blank(),
            axis.title.x = element_text(size = 8),
            axis.title.y = element_text(size = 8),,
            axis.text.y = element_text(size = 8),) +
      stat_compare_means(aes(group = updown),
                         label = "p.signif",
                         hide.ns = TRUE,
                         vjust = -1,
                         hjust = 0.5)
    ggsave(paste0(TEFileuserDir,"/","perc.boxplot.png"), width = 7, height = 6, plot = p)
    ggsave(paste0(TEFileuserDir,"/","perc.boxplot.pdf"), width = 7, height = 6, plot = p)
  }
  codonHeatmapPlot <- function(){
    ht_opt$message = FALSE
    rl <- read.csv(paste0(TEFileuserDir,"/rl2.csv"), check.names = FALSE)
    testt <- as.data.frame(reshape2::dcast(rl, gene_name~codon, value.var="zscore", fun.aggregate=sum))
    rownames(testt) <- testt[,1]
    testt <- as.matrix(testt[,-1])
    pheatmap::pheatmap(testt,
                       scale = "row",
                       cluster_cols = TRUE,
                       color = colorRampPalette(colors = c("cyan", "black", "yellow"))(10),
                       cluster_rows = TRUE,
                       show_rownames = FALSE,
                       fontsize_col = 5,
                       use_raster = TRUE,
                       filename = paste0(TEFileuserDir, "/codon_zscore_heatmap.png"))
    pheatmap::pheatmap(testt,
                       scale = "row",
                       cluster_cols = TRUE,
                       color = colorRampPalette(colors = c("cyan", "black", "yellow"))(10),
                       cluster_rows = TRUE,
                       show_rownames = FALSE,
                       fontsize_col = 5,
                       use_raster = TRUE,
                       filename = paste0(TEFileuserDir, "/codon_zscore_heatmap.pdf"))
    shinyjs::hide("loader-overlay10")
    shinyjs::runjs('$(".RiboTE-working-btn-codon-heatmap").addClass("RiboTE-working-hidden");')

  }
  codoNatioPlot <- function(){
    codonratio(FALSE)
    objcodons <- codonRatioData()
    if(input$codoNatioUpDownSelect == "Down"){
      deg <- as.data.table(read.csv(paste0(TEFileuserDir,"/deg2.csv"), check.names = FALSE))
      res_down <- read.csv(paste0(TEFileuserDir, "/res_down.csv"), check.names = FALSE)
      genomeDuRatio <- nrow(deg[updown=="Down"])/nrow(deg[updown=="Up"])
      genomeDownratio <- nrow(deg[updown=="Down"])/nrow(deg)
      xlabel <- "log FC codon usage (TE down vs TE up)"
      ylabel <- "log FC codon usage (TE down vs Global average)"
      p1 <- ggscatter(res_down,
                      x = "downratio",
                      y = "duRatio",
                      color = "type",
                      palette = "jco",
                      size = 2,
                      alpha = 0.6,
                      label = "codon",
                      repel = TRUE,
                      label.select = objcodons,
                      show.legend = FALSE) +
        geom_hline(yintercept = genomeDuRatio,
                   linetype = "dashed",
                   color = "red") +
        geom_vline(xintercept = genomeDownratio,
                   linetype = "dashed",
                   color = "red") +
        xlab(paste0("% ", xlabel)) +
        ylab(ylabel)

      p2 <- ggscatter(res_down,
                      x = "nu",
                      y = "duRatio",
                      color = "type",
                      palette = "jco",
                      size = 2,
                      alpha = 0.6,
                      label = "codon",
                      repel = TRUE,
                      label.select = objcodons,
                      show.legend = FALSE) +
        geom_hline(yintercept = genomeDuRatio,
                   linetype = "dashed",
                   color = "red") +
        xlab(paste0("# ", xlabel)) +
        ylab(ylabel)
      ggsave(paste0(TEFileuserDir,"/","codon_ratio_stat_percent_down.png"), width = 7, height = 7, plot = p1)
      ggsave(paste0(TEFileuserDir,"/","codon_ratio_stat_number_down.png"), width = 7, height = 7, plot = p2)
      ggsave(paste0(TEFileuserDir,"/","codon_ratio_stat_percent_down.pdf"), width = 7, height = 7, plot = p1)
      ggsave(paste0(TEFileuserDir,"/","codon_ratio_stat_number_down.pdf"), width = 7, height = 7, plot = p2)
      df <- data.frame(
        Name = c("Alice"),
        Age = c(25)
      )
      fwrite(df, paste0(TEFileuserDir, "/codonPercComplete4.csv"))

    }
    if(input$codoNatioUpDownSelect == "Up"){
      deg <- as.data.table(read.csv(paste0(TEFileuserDir,"/deg2.csv"), check.names = FALSE))
      res_down <- read.csv(paste0(TEFileuserDir, "/res_down.csv"), check.names = FALSE)
      genomeUdRatio <- nrow(deg[updown=="Up"])/nrow(deg[updown=="Down"])
      genomeUpRatio <- nrow(deg[updown=="Up"])/nrow(deg)
      xlabel <- "log FC codon usage (TE up vs TE down)"
      ylabel <- "log FC codon usage (TE up vs Global average)"
      p1 <- ggscatter(res_down,
                      x = "downratio",
                      y = "duRatio",
                      color = "type",
                      palette = "jco",
                      size = 2,
                      alpha = 0.6,
                      label = "codon",
                      repel = TRUE,
                      label.select = objcodons,
                      show.legend = FALSE) +
        geom_hline(yintercept = genomeUdRatio,
                   linetype = "dashed",
                   color = "red") +
        geom_vline(xintercept = genomeUpRatio,
                   linetype = "dashed",
                   color = "red") +
        xlab(paste0("% ", xlabel)) +
        ylab(ylabel)

      p2 <- ggscatter(res_down,
                      x = "nu",
                      y = "duRatio",
                      color = "type",
                      palette = "jco",
                      size = 2,
                      alpha = 0.6,
                      label = "codon",
                      repel = TRUE,
                      label.select = objcodons,
                      show.legend = FALSE) +
        geom_hline(yintercept = genomeUdRatio,
                   linetype = "dashed",
                   color = "red") +
        xlab(paste0("# ", xlabel)) +
        ylab(ylabel)
      ggsave(paste0(TEFileuserDir,"/","codon_ratio_stat_percent_up.png"), width = 7, height = 7, plot = p1)
      ggsave(paste0(TEFileuserDir,"/","codon_ratio_stat_number_up.png"), width = 7, height = 7, plot = p2)
      ggsave(paste0(TEFileuserDir,"/","codon_ratio_stat_percent_up.pdf"), width = 7, height = 7, plot = p1)
      ggsave(paste0(TEFileuserDir,"/","codon_ratio_stat_number_up.pdf"), width = 7, height = 7, plot = p2)
      df <- data.frame(
        Name = c("Alice"),
        Age = c(25)
      )
      fwrite(df, paste0(TEFileuserDir, "/codonPercComplete4.csv"))
    }
    shinyjs::hide("loader-overlay10")
    shinyjs::runjs('$(".RiboTE-working-btn-codon-ratio").addClass("RiboTE-working-hidden");')
    codonratio(TRUE)
  }
  dendrogramPlot <- function(){
    unlink(list.files(TEFileuserDir, pattern = "\\.dendrogram\\.pdf$", full.names = TRUE))
    unlink(list.files(TEFileuserDir, pattern = "\\.dendrogram\\.png$", full.names = TRUE))
    dendrogramData()
    testt <- read.csv(file.path(TEFileuserDir, "testt.csv"), check.names = FALSE)
    testt <- t(testt)
    dendrogramDistMethod <- input$dendrogramDistMethod
    dendrogramHclustMethod <- input$dendrogramHclustMethod
    hc <- hclust(dist(as.matrix(testt), method=dendrogramDistMethod), method=dendrogramHclustMethod)
    dend <- as.dendrogram(hc)
    dend_data <- ggdendro::dendro_data(dend)
    labels_data <- ggdendro::label(dend_data)
    highlight_codon <- input$codonSelect
    labels_data$color <- ifelse(labels_data$label %in% highlight_codon, "red", "black")
    p <- ggplot(ggdendro::segment(dend_data)) +
      geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) +
      geom_text(data = labels_data, aes(x = x, y = 0, label = label, color = color),
                angle = 90, vjust = 0.5, hjust = 1.1) +
      scale_color_identity() +
      ylim(-60, NA)+
      ggtitle(paste0(dendrogramDistMethod,"-",dendrogramHclustMethod)) +
      geom_hline(yintercept = 0, color = "black") +
      theme_minimal() +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_line(),
            axis.line.y = element_line(color = "black"),
            plot.title = element_text(hjust = 0.5))
    ggsave(paste0(TEFileuserDir, "/", dendrogramDistMethod, ".", dendrogramHclustMethod, ".dendrogram.png"), plot = p, width = 9, height = 4.5, dpi = 250)
    ggsave(paste0(TEFileuserDir, "/", dendrogramDistMethod, ".", dendrogramHclustMethod, ".dendrogram.pdf"), plot = p, width = 9, height = 4.5, dpi = 250)
    df <- data.frame(
      Name = c("Alice"),
      Age = c(25)
    )
    fwrite(df, paste0(TEFileuserDir, "/codonPercComplete5.csv"))
    shinyjs::hide("loader-overlay10")
    shinyjs::runjs('$(".RiboTE-working-btn-codon-dendrogram").addClass("RiboTE-working-hidden");')
  }
  distributionPlot <- function(){
    distributionDataTotal <- distributionData()
    objupdown <- distributionDataTotal[[2]]
    res <- distributionDataTotal[[1]]
    obs_pval1 <- distributionDataTotal[[3]]
    obs_pval2 <- distributionDataTotal[[4]]
    objudratio <- distributionDataTotal[[5]]
    objupratio <- distributionDataTotal[[6]]
    res2 <- distributionDataTotal[[7]]
    if(objupdown == "Up"){
      xlabel <- "log FC codon usage (TE up vs TE down)"
      ylabel <- "log FC codon usage (TE up vs Global average)"
    }
    if(objupdown == "Down"){
      xlabel <- "log FC codon usage (TE down vs TE up)"
      ylabel <- "log FC codon usage (TE down vs Global average)"
    }
    png(paste0(TEFileuserDir,"/upratio_distribution_",objupdown,".png"))
    p1 <- ggplot(data = NULL, aes(x = res$udratio)) +
      geom_histogram(aes(y = after_stat(density)), bins = 500, fill = "black", na.rm = TRUE) +
      geom_vline(xintercept = objudratio, color = "blue", linewidth = 0.5) +
      xlim(c(0, 0.3)) +
      scale_y_continuous(expand = c(0, 0)) +
      xlab(paste0("%", xlabel)) +
      ylab("Percentage") +
      ggtitle(paste0("p-value: ", obs_pval1)) +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.text = element_text(color = "black")
      )
    p2 <- ggplot(data = NULL, aes(x = res$duRatio)) +
      geom_histogram(aes(y = after_stat(density)), bins = 300, fill = "black", color = "black") +
      geom_vline(xintercept = objupratio, color = "blue", linewidth = 0.5) +
      xlab(ylabel) +
      ylab("Percentage") +
      scale_y_continuous(expand = c(0, 0)) +
      ggtitle(paste0("pvalue", ": ", obs_pval2)) +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.text = element_text(color = "black")
      )
    ggsave(paste0(TEFileuserDir,"/upratio_distribution_",objupdown,".png"), plot = p1, width = 8, height = 5)
    ggsave(paste0(TEFileuserDir,"/dnratio_distribution_",objupdown,".png"), plot = p2, width = 8, height = 5)
    ggsave(paste0(TEFileuserDir,"/upratio_distribution_",objupdown,".pdf"), plot = p1, width = 8, height = 5)
    ggsave(paste0(TEFileuserDir,"/dnratio_distribution_",objupdown,".pdf"), plot = p2, width = 8, height = 5)
    df <- data.frame(
      Name = c("Alice"),
      Age = c(25)
    )
    fwrite(df, paste0(TEFileuserDir, "/codonPercComplete6.csv"))
    shinyjs::hide("loader-overlay10")
    shinyjs::runjs('$(".RiboTE-working-btn-codon-distribution").addClass("RiboTE-working-hidden");')
  }
  codonUpDownPlot <- function(){
    distributionDataTotal <- distributionData2()
    objupdown <- distributionDataTotal[[2]]
    res2 <- distributionDataTotal[[7]]
    if(objupdown == "Up"){
      xlabel <- "log FC codon usage (TE up vs TE down)"
      ylabel <- "log FC codon usage (TE up vs Global average)"
    }
    if(objupdown == "Down"){
      xlabel <- "log FC codon usage (TE down vs TE up)"
      ylabel <- "log FC codon usage (TE down vs Global average)"
    }
    p <- ggline(res2[rep <= 20],
                x = "rep",
                y = "duRatio",
                linetype = "dotted",
                shape = 16,
                size = 1) +
      ylab(ylabel) +
      xlab("#objective codon per 1000 codons") +
      theme(axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14))
    ggsave(paste0(TEFileuserDir, "/codon_updown_zscore_", objupdown, ".png"), plot = p, width = 4.8, height = 7, dpi = 250)
    ggsave(paste0(TEFileuserDir, "/codon_updown_zscore_", objupdown, ".pdf"), plot = p, width = 4.8, height = 7, dpi = 250)
    shinyjs::hide("loader-overlay10")
    shinyjs::runjs('$(".RiboTE-working-btn-codon-zscore").addClass("RiboTE-working-hidden");')
  }
  codonPer1kFcAndPvaluePlot <- function(){
    objCodonStat2 <- distributionData3()
    p <- ggscatter(objCodonStat2,
                   x = "per1k",
                   y = "fcAndPvalue",
                   color = "type",
                   size = 2,
                   alpha = 0.6,
                   add = "reg.line",
                   add.params = list(color = "#00AFBB", fill = "lightgray"),
                   conf.int = TRUE) +
      labs(x = "#objective codons per 1000 codons",
           y = "Expression changes (-log2Pvalue x log2FC)") +
      stat_cor(method = "pearson",
               label.x.npc = 0.6,
               label.y.npc = 1)
    ggsave(paste0(TEFileuserDir, "/codon_per1k_fcAndPvalue.png"), plot = p, width = 6, height = 6, dpi = 250)
    ggsave(paste0(TEFileuserDir, "/codon_per1k_fcAndPvalue.pdf"), plot = p, width = 6, height = 6, dpi = 250)
    shinyjs::hide("loader-overlay10")
    shinyjs::runjs('$(".RiboTE-working-btn-codon-Per1kFcAndPvalue").addClass("RiboTE-working-hidden");')
  }
  per1kZscorePlot <- function(){
    objcodon <- list()
    objcodons <- input$codonSelect
    objcodon[[1]] <- objcodons
    codonObjRun <- as.data.table(gtools::permutations(n=2,r=2,v=objcodons, repeats.allowed=T))
    codonObjRun[,run:=paste0(V1,V2)]
    objcodon[[2]] <- codonObjRun$run
    codonObjRun <- as.data.table(gtools::permutations(n=2,r=3,v=objcodons, repeats.allowed=T))
    codonObjRun[,run:=paste0(V1,V2,V3)]
    objcodon[[3]] <- codonObjRun$run
    codonnames <- c('single','double','triple')
    numcodon <- c(1,2,3)
    objupdown <- input$per1kZscoreUpDownSelect
    transcript_seqs <- shinyEnv$transcriptSeqs
    deg <- as.data.table(read.csv(paste0(TEFileuserDir, "/deg2.csv"), check.names = FALSE))
    transcript_seqs_obj <- transcript_seqs[gene_name %in% deg$gene_name]
    transcript_seqs_obj <- transcript_seqs_obj[cds_len > 9]
    for(i in seq(length(numcodon))) {
      codonObjRun <- objcodon[[i]]
      codonname <- codonnames[i]
      cct5 <- mapply(dataSummary2, transcript_seqs_obj$codon_seqs, transcript_seqs_obj$transcript_id, transcript_seqs_obj$gene_id, transcript_seqs_obj$gene_name, numcodon[i], SIMPLIFY = FALSE)
      rl5 <- rbindlist(cct5)
      rl5 <- merge(rl5,transcript_seqs_obj[,.(transcript_id,cds_len)],all.x=T,by="transcript_id")
      rl5[,codon_len:=cds_len/3]
      rl5[,per1k:=(freq*1000)/codon_len]
      msd <- rl5[,.(meanvalue=mean(per1k),std=sd(per1k)),by=c("codon")]
      rl5 <- merge(rl5,msd,by="codon")
      rl5[,zscore := mapply(function(q,m,n) {(q-m)/n}, per1k, meanvalue, std)]
      allData <- merge(rl5,deg,all.x=T,by="gene_name",allow.cartesian=TRUE)
      allData <- allData[!is.na(updown)]
      if(objupdown == "Up"){
        forDotPlot <- allData[updown==objupdown]
        forDotPlot[,type:="other"]
        forDotPlot[codon %in% codonObjRun,type:="Objective"]
        setorder(forDotPlot,by=-zscore)
        p1 <- ggscatter(forDotPlot,
                        x = "per1k",
                        y = "zscore",
                        colors = "type",
                        size = 2,
                        alpha = 0.6) +
          labs(x = "#Objective codon per 1000 codons",
               y = "Expression changes log2(fold change)") +
          stat_cor(method = "pearson",
                   label.x.npc = 0.1,
                   label.y.npc = 1)
        ggsave(paste0(TEFileuserDir,"/per1k_zscore_",objupdown,"_",codonname,".png"), plot = p1, width = 6, height = 6, dpi = 250)
        ggsave(paste0(TEFileuserDir,"/per1k_zscore_",objupdown,"_",codonname,".pdf"), plot = p1, width = 6, height = 6)
      }
      if(objupdown == "Down"){
        forDotPlot <- allData[updown==objupdown]
        forDotPlot[,type:="other"]
        forDotPlot[codon %in% codonObjRun,type:="Objective"]
        setorder(forDotPlot,by=-zscore)
        p2 <- ggscatter(forDotPlot,
                        x = "per1k",
                        y = "zscore",
                        colors = "type",
                        size = 2,
                        alpha = 0.6) +
          labs(x = "#Objective codon per 1000 codons",
               y = "Expression changes log2(fold change)") +
          stat_cor(method = "pearson",
                   label.x.npc = 0.1,
                   label.y.npc = 1)
        ggsave(paste0(TEFileuserDir,"/per1k_zscore_",objupdown,"_",codonname,".png"), plot = p2, width = 6, height = 6, dpi = 250)
        ggsave(paste0(TEFileuserDir,"/per1k_zscore_",objupdown,"_",codonname,".pdf"), plot = p2, width = 6, height = 6)
      }
    }
    df <- data.frame(
      Name = c("Alice"),
      Age = c(25)
    )
    fwrite(df, paste0(TEFileuserDir, "/codonPercComplete7.csv"))
    shinyjs::hide("loader-overlay10")
    shinyjs::runjs('$(".RiboTE-working-btn-codon-per1k-zscore").addClass("RiboTE-working-hidden");')
  }
  enrichedPlot <- function(){
    objcodon <- list()
    objcodons <- input$codonSelect
    objcodon[[1]] <- objcodons
    codonObjRun <- as.data.table(gtools::permutations(n=2,r=2,v=objcodons, repeats.allowed=T))
    codonObjRun[,run:=paste0(V1,V2)]
    objcodon[[2]] <- codonObjRun$run
    codonObjRun <- as.data.table(gtools::permutations(n=2,r=3,v=objcodons, repeats.allowed=T))
    codonObjRun[,run:=paste0(V1,V2,V3)]
    objcodon[[3]] <- codonObjRun$run
    codonnames <- c('single','double','triple')
    numcodon <- c(1,2,3)
    objupdown <- input$per1kZscoreUpDownSelect
    transcript_seqs <- shinyEnv$transcriptSeqs
    deg <- as.data.table(read.csv(paste0(TEFileuserDir, "/deg2.csv"), check.names = FALSE))
    transcript_seqs_obj <- transcript_seqs[gene_name %in% deg$gene_name]
    transcript_seqs_obj <- transcript_seqs_obj[cds_len > 9]
    for(i in seq(length(numcodon))) {
      codonObjRun <- objcodon[[i]]
      codonname <- codonnames[i]
      cct5 <- mapply(dataSummary2, transcript_seqs_obj$codon_seqs, transcript_seqs_obj$transcript_id, transcript_seqs_obj$gene_id, transcript_seqs_obj$gene_name, numcodon[i], SIMPLIFY = FALSE)
      rl5 <- rbindlist(cct5)
      rl5 <- merge(rl5,transcript_seqs_obj[,.(transcript_id,cds_len)],all.x=T,by="transcript_id")
      rl5[,codon_len:=cds_len/3]
      rl5[,per1k:=(freq*1000)/codon_len]
      msd <- rl5[,.(meanvalue=mean(per1k),std=sd(per1k)),by=c("codon")]
      rl5 <- merge(rl5,msd,by="codon")
      rl5[,zscore := mapply(function(q,m,n) {(q-m)/n}, per1k, meanvalue, std)]
      allData <- merge(rl5,deg,all.x=T,by="gene_name",allow.cartesian=TRUE)
      allData <- allData[!is.na(updown)]
      tempForPlot <- rl5[codon %in% codonObjRun]
      tempForPlot <- tempForPlot[,sum(per1k),by=c("gene_name")]
      tempForPlot <- merge(tempForPlot,deg,all.y=T,by="gene_name")
      tempForPlot <- tempForPlot[!is.na(V1)]
      tempForPlot[,type:="Other"]
      tempForPlot[updown=="Up",type:="Up"]
      tempForPlot[updown=="Down",type:="Down"]
      tempForPlot[,V1:=log2(V1)]
      if(input$enrichedDisplaySelect == "All"){
        p1 <- ggboxplot(tempForPlot, x = 'type', y = 'V1', color = 'type', palette = "jco") +
          labs(y = "#codon run per 1k codons") +
          stat_compare_means(aes(group = type), label = "p.signif", hide.ns = TRUE)
        ggsave(paste0(TEFileuserDir,"/enriched_all_", codonname, ".png"), plot = p1, width = 6, height = 6, dpi = 250)
        ggsave(paste0(TEFileuserDir,"/enriched_all_", codonname, ".pdf"), plot = p1, width = 6, height = 6)
      }
      if(input$enrichedDisplaySelect == "Obj"){
        objCodonStat <- shinyEnv$objCodonStat

        tempForPlot <- tempForPlot[gene_name %in% objCodonStat$gene_name]
        p2 <- ggboxplot(
          data = tempForPlot,
          x = 'type',
          y = 'V1',
          color = 'type',
          palette = "jco"
        ) +
          labs(y = "#codon run per 1k codons") +
          stat_compare_means(
            aes(group = type),
            label = "p.signif",
            hide.ns = TRUE
          )
        ggsave(paste0(TEFileuserDir,"/enriched_obj_", codonname, ".png"), plot = p2, width = 6, height = 6, dpi = 250)
        ggsave(paste0(TEFileuserDir,"/enriched_obj_", codonname, ".pdf"), plot = p2, width = 6, height = 6)
      }
    }
    df <- data.frame(
      Name = c("Alice"),
      Age = c(25)
    )
    fwrite(df, paste0(TEFileuserDir, "/codonPercComplete8.csv"))
    shinyjs::hide("loader-overlay10")
    shinyjs::runjs('$(".RiboTE-working-btn-codon-enriched").addClass("RiboTE-working-hidden");')
  }
  ####Load Data Data preprocess结果展示----
  observeEvent(input$demoData, {
    runjs("$('#RiboTE_TITLE').hide();")
    runjs("$('#RiboTE_TEXT').hide();")
    runjs("$('#RiboTE_CAROUSEL').hide();")
    shinyjs::show("loader-overlay8")
    output$userGeneMatrixStat <- DT::renderDT({
      if(!is.null(input$geneMatrix) && input$demoData > 0){
        showModal(modalDialog(
          "When loading example data, please do not select species and upload gene expression matrix files. Please refresh the page and click the button again.",
          footer = tagList(
            modalButton("Close"),
          ),easyClose = TRUE,))
        updateTextInput(session, "inputnames", value = "Please refresh")
        updateTextInput(session, "rpfnames", value = "Please refresh")
        updateSelectizeInput(session, "selectOrg", choices = c("Homo sapiens (hg38)", "Mus musculus (mm10)", "Oryza sativa (lRGSP 1.0)"), selected = NULL)
        unlink(paste0(TEFileuserDir,"/normalcount.csv"))
        shinyjs::hide("loader-overlay8")
        return(NULL)
      }
      if(is.null(input$geneMatrix) && input$demoData > 0){
        geneMatrix <- read.table("/public/shiny/RiboTE/TEShinyData/all.count.txt",header = T, sep = "\t", check.names = FALSE)
        updateTextInput(session, "inputnames", value = "RNA.WT1,RNA.WT2#RNA.KO1,RNA.KO2")
        updateTextInput(session, "rpfnames", value = "RPF.WT1,RPF.WT2#RPF.KO1,RPF.KO2")
        updateSelectizeInput(session, "selectOrg", choices = c("Homo sapiens (hg38)", "Mus musculus (mm10)", "Oryza sativa (lRGSP 1.0)"), selected = "Homo sapiens (hg38)")
        shinyjs::hide("loader-overlay8")
        shinyEnv$loadDataflag <- TRUE
        DT::datatable(geneMatrix,
                      options = list(
                        dom = 'tip',
                        autoWidth = FALSE,
                        pageLength = 30,
                        columnDefs = lapply(0:(ncol(geneMatrix) - 1), function(i) {
                          list(width = '100px', targets = i, className = 'dt-center')
                        }),

                        scrollX = TRUE
                      ),
                      rownames = FALSE,
                      class = 'hover stripe')

      }
    })
    output$preData <- DT::renderDT({
      datapreprocess(FALSE)
      toggleClass(selector = ".RiboTE-working-btn-data-preprocess", class = "RiboTE-working-hidden")
      shinyjs::show("loader-overlay9")
      runjs('$(".mask").show();')
      baseData <- baseData()
      req(!is.null(baseData))
      rawCountMake(baseData[[1]], baseData[[3]], baseData[[4]], baseData[[5]], baseData[[2]], baseData[[6]], baseData[[7]], baseData[[8]], baseData[[9]])
      req(file.exists(paste0(TEFileuserDir,"/combCntTbl.csv")))
      rawCount <- read.csv(paste0(TEFileuserDir,"/combCntTbl.csv"), header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
      shinyjs::hide("loader-overlay9")
      toggleClass(selector = ".RiboTE-working-btn-data-preprocess", class = "RiboTE-working-hidden")
      colnames(rawCount)[1] <- "GeneID"
      datapreprocess(TRUE)
      runjs('$(".mask").hide();')
      DT::datatable(rawCount,
                    options = list(
                      dom = 'tip',
                      autoWidth = FALSE,
                      pageLength = 30,
                      columnDefs = lapply(0:(ncol(rawCount) - 1), function(i) {
                        list(width = '100px', targets = i, className = 'dt-center')
                      }),

                      scrollX = TRUE
                    ),
                    rownames = FALSE,
                    class = 'hover stripe')
    })
    observeEvent(list(input$NAestimate, input$minCounts, input$nMinSamplesCount), {
      output$barPlot <- renderPlot({
        shinyjs::show("loader-overlay9")
        runjs('$(".mask").show();')
        toggleClass(selector = ".RiboTE-working-btn-data-preprocess", class = "RiboTE-working-hidden")
        baseData <- baseData()
        rawCountMake(baseData[[1]], baseData[[3]], baseData[[4]], baseData[[5]], 
                     baseData[[2]], baseData[[6]], baseData[[7]], baseData[[8]], 
                     baseData[[9]])
        req(file.exists(paste0(TEFileuserDir,"/combCntTbl.csv")))
        rawCount <- read.csv(paste0(TEFileuserDir,"/combCntTbl.csv"), row.names = 1, check.names = FALSE)
        baseData <- fromJSON(paste0(TEFileuserDir,"/baseData.json"))
        inputnames <- baseData[[11]]
        rpfnames <- baseData[[12]]
        p <- rawCountPlot(
          countsData = rawCount,
          type = "Raw",
          inputnames = inputnames,
          rpfnames = rpfnames
        )
        shinyjs::hide("loader-overlay9")
        runjs('$(".mask").hide();')
        toggleClass(selector = ".RiboTE-working-btn-data-preprocess", class = "RiboTE-working-hidden")
        return(p)
      })
    })
    observeEvent(list(input$NAestimate, input$minCounts, input$nMinSamplesCount), {
      output$qcPlot1 <- renderPlot({
        shinyjs::show("loader-overlay9")
        runjs('$(".mask").show();')
        toggleClass(selector = ".RiboTE-working-btn-data-preprocess", class = "RiboTE-working-hidden")
        baseData <- baseData()
        rawCountMake(baseData[[1]], baseData[[3]], baseData[[4]], baseData[[5]], 
                     baseData[[2]], baseData[[6]], baseData[[7]], baseData[[8]], 
                     baseData[[9]])
        req(file.exists(paste0(TEFileuserDir,"/qc1Table.csv")))
        qc1Table <- read.csv(paste0(TEFileuserDir,"/qc1Table.csv"), row.names = 1, check.names = FALSE)
        orgData <- fromJSON(paste0(TEFileuserDir,"/orgData.json"))
        allGeneInfo <- orgData[[2]]
        p <- geneTypePlot(countsData = qc1Table, allGeneInfo = allGeneInfo)
        return(p)
      })
      output$qcPlot2 <- renderPlot({
        req(file.exists(paste0(TEFileuserDir,"/combCntTbl.csv")))
        combCntTbl <- read.csv(paste0(TEFileuserDir,"/combCntTbl.csv"), row.names = 1, check.names = FALSE)
        baseData <- fromJSON(paste0(TEFileuserDir,"/baseData.json"))
        inputnames <- baseData[[11]]
        rpfnames <- baseData[[12]]
        orgData <- fromJSON(paste0(TEFileuserDir,"/orgData.json"))
        allGeneInfo <- orgData[[2]]
        p <- rRNACountsPlot(
          countsData = combCntTbl,
          allGeneInfo = allGeneInfo,
          inputnames = inputnames,
          rpfnames = rpfnames
        )
        shinyjs::hide("loader-overlay9")
        runjs('$(".mask").hide();')
        toggleClass(selector = ".RiboTE-working-btn-data-preprocess", class = "RiboTE-working-hidden")
        return(p)
      })
    })
  })
  observeEvent(input$goButton, {
    runjs("$('#RiboTE_TITLE').hide();")
    runjs("$('#RiboTE_TEXT').hide();")
    runjs("$('#RiboTE_CAROUSEL').hide();")
    unlink(list.files(paste0(TEFileuserDir, "/"), full.names = TRUE), recursive = TRUE)
    shinyjs::show("loader-overlay8")
    if(input$selectOrg == ""){
      showModal(modalDialog(
        "Species not selected!!!",
        footer = tagList(
          modalButton("Close"),
        ),easyClose = TRUE,))
      shinyjs::hide("loader-overlay8")
      return(NULL)
    }
    if(is.null(input$geneMatrix)){
      showModal(modalDialog(
        "Expression matrix not uploaded!!!",
        footer = tagList(
          modalButton("Close"),
        ),easyClose = TRUE,))
      shinyjs::hide("loader-overlay8")
      return(NULL)
    }
    if(nchar(input$inputnames) == 0){
      showModal(modalDialog(
        "RNA-seq samples not entered!!!",
        footer = tagList(
          modalButton("Close"),
        ),easyClose = TRUE,))
      shinyjs::hide("loader-overlay8")
      return(NULL)
    }
    if(nchar(input$rpfnames) == 0){
      showModal(modalDialog(
        "Ribo-seq samples not entered!!!",
        footer = tagList(
          modalButton("Close"),
        ),easyClose = TRUE,))
      shinyjs::hide("loader-overlay8")
      return(NULL)
    }
    inputnames <- input$inputnames
    inputnames <- gsub("#", ",",inputnames)
    inputnames <- strsplit(inputnames, ",")[[1]]
    rpfnames <- input$rpfnames
    rpfnames <- gsub("#", ",",rpfnames)
    rpfnames <- strsplit(rpfnames, ",")[[1]]
    if(length(inputnames) != length(rpfnames)){
      showModal(modalDialog(
        "RNA-seq is not the same as Ribo-seq sample size!!!",
        footer = tagList(
          modalButton("Close"),
        ),easyClose = TRUE,))
      shinyjs::hide("loader-overlay8")
      return(NULL)
    }
    output$userGeneMatrixStat <- DT::renderDT({
      datapreprocess(FALSE)
      userGeneMatrixPath <- input$geneMatrix$datapath
      delimiter <- detectDelimiter(file_path = userGeneMatrixPath)
      geneMatrix <- read.table(userGeneMatrixPath, header = T, sep = delimiter, check.names = FALSE)
      shinyjs::hide("loader-overlay8")
      shinyEnv$loadDataflag <- TRUE
      datapreprocess(TRUE)
      DT::datatable(geneMatrix,
                    options = list(
                      dom = 'tip',
                      autoWidth = FALSE,
                      pageLength = 30,
                      columnDefs = lapply(0:(ncol(geneMatrix) - 1), function(i) {
                        list(width = '100px', targets = i, className = 'dt-center')
                      }),

                      scrollX = TRUE
                    ),
                    rownames = FALSE,
                    class = 'hover stripe')
    })
    output$preData <- DT::renderDT({
      shinyjs::show("loader-overlay9")
      toggleClass(selector = ".RiboTE-working-btn-data-preprocess", class = "RiboTE-working-hidden")
      runjs('$(".mask").show();')
      baseData <- baseData()
      req(!is.null(baseData))
      rawCountMake(baseData[[1]], baseData[[3]], baseData[[4]], baseData[[5]], baseData[[2]], baseData[[6]], baseData[[7]], baseData[[8]], baseData[[9]])
      req(file.exists(paste0(TEFileuserDir,"/combCntTbl.csv")))
      rawCount <- read.csv(paste0(TEFileuserDir,"/combCntTbl.csv"), check.names = FALSE)
      shinyjs::hide("loader-overlay9")
      colnames(rawCount)[1] <- "GeneID"
      runjs('$(".mask").hide();')
      toggleClass(selector = ".RiboTE-working-btn-data-preprocess", class = "RiboTE-working-hidden")
      return(DT::datatable(rawCount,
                           options = list(
                             dom = 'tip',
                             autoWidth = FALSE,
                             pageLength = 30,
                             columnDefs = lapply(0:(ncol(rawCount) - 1), function(i) {
                               list(width = '100px', targets = i, className = 'dt-center')
                             }),

                             scrollX = TRUE
                           ),
                           rownames = FALSE,
                           class = 'hover stripe'))
    })
    
    observeEvent(list(input$NAestimate, input$minCounts, input$nMinSamplesCount),{
      output$barPlot <- renderPlot({
        req(file.exists(paste0(TEFileuserDir,"/combCntTbl.csv")))
        shinyjs::show("loader-overlay9")
        toggleClass(selector = ".RiboTE-working-btn-data-preprocess", class = "RiboTE-working-hidden")
        runjs('$(".mask").show();')
        baseData <- baseData()
        rawCountMake(baseData[[1]], baseData[[3]], baseData[[4]], baseData[[5]], 
                     baseData[[2]], baseData[[6]], baseData[[7]], baseData[[8]], 
                     baseData[[9]])
        rawCount <- read.csv(paste0(TEFileuserDir,"/combCntTbl.csv"), row.names = 1, check.names = FALSE)
        baseData <- fromJSON(paste0(TEFileuserDir,"/baseData.json"))
        inputnames <- baseData[[11]]
        rpfnames <- baseData[[12]]
        p <- rawCountPlot(
          countsData = rawCount,
          type = "Raw",
          inputnames = inputnames,
          rpfnames = rpfnames
        )
        shinyjs::hide("loader-overlay9")
        toggleClass(selector = ".RiboTE-working-btn-data-preprocess", class = "RiboTE-working-hidden")
        runjs('$(".mask").hide();')
        return(p)
      })
    })
    observeEvent(list(input$NAestimate, input$minCounts, input$nMinSamplesCount),{
      output$qcPlot1 <- renderPlot({
        req(file.exists(paste0(TEFileuserDir,"/qc1Table.csv")))
        shinyjs::show("loader-overlay9")
        runjs('$(".mask").show();')
        toggleClass(selector = ".RiboTE-working-btn-data-preprocess", class = "RiboTE-working-hidden")
        baseData <- baseData()
        rawCountMake(baseData[[1]], baseData[[3]], baseData[[4]], baseData[[5]], 
                     baseData[[2]], baseData[[6]], baseData[[7]], baseData[[8]], 
                     baseData[[9]])
        qc1Table <- read.csv(paste0(TEFileuserDir,"/qc1Table.csv"), row.names = 1, check.names = FALSE)
        orgData <- fromJSON(paste0(TEFileuserDir,"/orgData.json"))
        allGeneInfo <- orgData[[2]]
        p <- geneTypePlot(countsData = qc1Table, allGeneInfo = allGeneInfo)
        shinyjs::hide("loader-overlay9")
        return(p)
      })
      output$qcPlot2 <- renderPlot({
        req(file.exists(paste0(TEFileuserDir,"/combCntTbl.csv")))
        combCntTbl <- read.csv(paste0(TEFileuserDir,"/combCntTbl.csv"), row.names = 1, check.names = FALSE)
        baseData <- fromJSON(paste0(TEFileuserDir,"/baseData.json"))
        inputnames <- baseData[[11]]
        rpfnames <- baseData[[12]]
        orgData <- fromJSON(paste0(TEFileuserDir,"/orgData.json"))
        allGeneInfo <- orgData[[2]]
        p <- rRNACountsPlot(
          countsData = combCntTbl,
          allGeneInfo = allGeneInfo,
          inputnames = inputnames,
          rpfnames = rpfnames
        )
        shinyjs::hide("loader-overlay9")
        runjs('$(".mask").hide();')
        toggleClass(selector = ".RiboTE-working-btn-data-preprocess", class = "RiboTE-working-hidden")
        return(p)
      })
    })
  })
  ####Data preprocess结果下载----
  output$ExportBtn1 <- downloadHandler(
    filename = function() {
      return("preprocess_data.csv")
    },
    content = function(file) {
      filepath <- paste0(TEFileuserDir, "/combCntTbl.csv")
      if (file.exists(filepath)) {
        file.copy(filepath, file)
      } else {
        return(NULL)
      }
    }
  )
  output$ExportBtn2 <- downloadHandler(
    filename = function() {
      return("raw_count_plot_data.csv")
    },
    content = function(file) {
      filepath <- paste0(TEFileuserDir, "/rawCountPlotData.csv")
      if (file.exists(filepath)) {
        file.copy(filepath, file)
      } else {
        return(NULL)
      }
    }
  )
  output$ExportBtn3 <- downloadHandler(
    filename = function() {
      return("raw_count_plot.pdf")
    },
    content = function(file) {
      filepath <- paste0(TEFileuserDir, "/rawCountPlot.pdf")
      if (file.exists(filepath)) {
        file.copy(filepath, file)
      } else {
        return(NULL)
      }
    }
  )
  output$ExportBtn4 <- downloadHandler(
    filename = function() {
      return("raw_count_plot.png")
    },
    content = function(file) {
      filepath <- paste0(TEFileuserDir, "/rawCountPlot.png")
      if (file.exists(filepath)) {
        file.copy(filepath, file)
      } else {
        return(NULL)
      }
    }
  )
  output$ExportBtn5 <- downloadHandler(
    filename = function() {
      return("quality_control_plot_data.zip")
    },
    content = function(file) {
      files_to_zip <- c(
        paste0(TEFileuserDir, "/rRNACountsPlotData.csv"),
        paste0(TEFileuserDir, "/geneTypePlotData.csv")
      )
      if (all(file.exists(files_to_zip))) {
        zip::zipr(file, files_to_zip)
      } else {
        return(NULL)
      }
    }
  )
  output$ExportBtn6 <- downloadHandler(
    filename = function() {
      return("quality_control_plot_pdf.zip")
    },
    content = function(file) {
      files_to_zip <- c(
        paste0(TEFileuserDir, "/rRNACountsPlot.pdf"),
        paste0(TEFileuserDir, "/geneTypePlot.pdf")
      )
      if (all(file.exists(files_to_zip))) {
        zip::zipr(file, files_to_zip)
      } else {
        return(NULL)
      }
    }
  )
  output$ExportBtn7 <- downloadHandler(
    filename = function() {
      return("quality_control_plot_png.zip")
    },
    content = function(file) {
      files_to_zip <- c(
        paste0(TEFileuserDir, "/rRNACountsPlot.png"),
        paste0(TEFileuserDir, "/geneTypePlot.png")
      )
      if (all(file.exists(files_to_zip))) {
        zip::zipr(file, files_to_zip)
      } else {
        return(NULL)
      }
    }
  )

  ####TE 数据展示----
  observeEvent(list(input$TEtools, input$fvalue, input$pCutoffType, input$pCutoff), {
    output$proTotaldata <- renderDT({
      if(input$demoData >= 0 && is.null(input$geneMatrix) && !datapreprocess()){
        return(NULL)
      }
      if(datapreprocess()){
        shinyjs::show("loader-overlay")
        runjs('$(".mask").show();')
        toggleClass(selector = ".RiboTE-working-btn-translation-efficiency", class = "RiboTE-working-hidden")
        baseData()
        req(file.exists(paste0(TEFileuserDir,"/baseData.json")))
        TEtotal()
        if(file.exists(paste0(TEFileuserDir,"/normalcount.csv"))){
          normalcount <- read.csv(paste0(TEFileuserDir,"/normalcount.csv"), check.names = FALSE)
          shinyjs::hide("loader-overlay")
          runjs('$(".mask").hide();')
          toggleClass(selector = ".RiboTE-working-btn-translation-efficiency", class = "RiboTE-working-hidden")
          datatable(normalcount,
                    options = list(
                      dom = 'tip',
                      autoWidth = TRUE,
                      pageLength = 30,
                      columnDefs = lapply(0:(ncol(normalcount) - 1), function(i) {
                        list(width = '100px', targets = i, className = 'dt-center')
                      }),
                      scrollX = TRUE
                    ),
                    rownames = FALSE,
                    class = 'hover stripe')}
      }
    })
  })
  observeEvent(list(input$TEtools, input$fvalue, input$pCutoffType, input$pCutoff), {
    output$TEvolcano <- renderImage({
      if(input$demoData >= 0 && is.null(input$geneMatrix) && !datapreprocess()){
        return(list(
          src = "img/RiboTE_null_image.png",
          contentType = "image/png",
          width = "1px",
          height = "1px"
        ))
      }
      if(datapreprocess()){
        shinyjs::show("loader-overlay")
        runjs('$(".mask").show();')
        toggleClass(selector = ".RiboTE-working-btn-translation-efficiency", class = "RiboTE-working-hidden")
        baseData()
        req(file.exists(paste0(TEFileuserDir,"/baseData.json")))
        TEtotal()
        req(file.exists(paste0(TEFileuserDir,"/TE.volcano.scatter.png")))
        if(file.exists(paste0(TEFileuserDir,"/TE.volcano.scatter.png"))){
          list(
            src = paste0(TEFileuserDir,"/TE.volcano.scatter.png"),
            contentType = "png",
            width = "600px",
            height = "600px",
            alt = " "
          )
        }
      } else {
        return(list(
          src = "img/RiboTE_null_image.png",
          contentType = "image/png",
          width = "1px",
          height = "1px"
        ))
      }
      
    }, deleteFile=FALSE)
    output$volcanoAB <- renderImage({
      if(input$demoData >= 0 && is.null(input$geneMatrix) && !datapreprocess()){
        return(list(
          src = "img/RiboTE_null_image.png",
          contentType = "image/png",
          width = "1px",
          height = "1px"
        ))
      }
      if(datapreprocess()){
        req(file.exists(paste0(TEFileuserDir,"/volcanoAB.png")))
        if(file.exists(paste0(TEFileuserDir,"/volcanoAB.png"))){
          shinyjs::hide("loader-overlay")
          runjs('$(".mask").hide();')
          toggleClass(selector = ".RiboTE-working-btn-translation-efficiency", class = "RiboTE-working-hidden")
          list(
            src = paste0(TEFileuserDir,"/volcanoAB.png"),
            contentType = "png",
            width = "600px",
            height = "600px",
            alt = " "
          )
        }
      }else {
        return(list(
          src = "img/RiboTE_null_image.png",
          contentType = "image/png",
          width = "1px",
          height = "1px"
        ))
      }
      
    }, deleteFile=FALSE)
  })
  observeEvent(list(input$TEtools, input$fvalue, input$pCutoffType, input$pCutoff), {
    output$TEscatter <- renderImage({
      if(input$demoData >= 0 && is.null(input$geneMatrix) && !datapreprocess()){
        return(list(
          src = "img/RiboTE_null_image.png",
          contentType = "image/png",
          width = "1px",
          height = "1px"
        ))
      }
      if(datapreprocess()){
        shinyjs::show("loader-overlay")
        runjs('$(".mask").show();')
        toggleClass(selector = ".RiboTE-working-btn-translation-efficiency", class = "RiboTE-working-hidden")
        baseData()
        req(file.exists(paste0(TEFileuserDir,"/baseData.json")))
        TEtotal()
        req(file.exists(paste0(TEFileuserDir,"/TE.scatter.png")))
        if(file.exists(paste0(TEFileuserDir,"/TE.scatter.png"))){
          shinyjs::show("loader-overlay")
          list(
            src = paste0(TEFileuserDir,"/TE.scatter.png"),
            contentType = "png",
            width = "600px",
            height = "600px",
            alt = " "
          )}
      }else {
        return(list(
          src = "img/RiboTE_null_image.png",
          contentType = "image/png",
          width = "1px",
          height = "1px"
        ))
      }
      
    }, deleteFile=FALSE)
    output$TErpkmFcScatter <- renderImage({
      if(input$demoData >= 0 && is.null(input$geneMatrix) && !datapreprocess()){
        return(list(
          src = "img/RiboTE_null_image.png",
          contentType = "image/png",
          width = "1px",
          height = "1px"
        ))
      }
      if(datapreprocess()){
        req(file.exists(paste0(TEFileuserDir,"/TE_rpkm_fc.scatter.png")))
        if(file.exists(paste0(TEFileuserDir,"/TE_rpkm_fc.scatter.png"))){
          list(
            src = paste0(TEFileuserDir,"/TE_rpkm_fc.scatter.png"),
            contentType = "png",
            width = "600px",
            height = "600px",
            alt = " "
          )
        }
      }else {
        return(list(
          src = "img/RiboTE_null_image.png",
          contentType = "image/png",
          width = "1px",
          height = "1px"
        ))
      }
      
    }, deleteFile=FALSE)
    output$inputScatter <- renderImage({
      if(input$demoData >= 0 && is.null(input$geneMatrix) && !datapreprocess()){
        return(list(
          src = "img/RiboTE_null_image.png",
          contentType = "image/png",
          width = "1px",
          height = "1px"
        ))
      }
      if(datapreprocess()){
        req(file.exists(paste0(TEFileuserDir,"/Input.scatter.png")))
        if(file.exists(paste0(TEFileuserDir,"/Input.scatter.png"))){
          list(
            src = paste0(TEFileuserDir,"/Input.scatter.png"),
            contentType = "png",
            width = "600px",
            height = "600px",
            alt = " "
          )
        }
      }else {
        return(list(
          src = "img/RiboTE_null_image.png",
          contentType = "image/png",
          width = "1px",
          height = "1px"
        ))
      }
      
    }, deleteFile=FALSE)
    output$rpfInputScatter <- renderImage({
      if(input$demoData >= 0 && is.null(input$geneMatrix) && !datapreprocess()){
        return(list(
          src = "img/RiboTE_null_image.png",
          contentType = "image/png",
          width = "1px",
          height = "1px"
        ))
      }
      if(datapreprocess()){
        req(file.exists(paste0(TEFileuserDir,"/RPF.input.scatter.png")))
        if(file.exists(paste0(TEFileuserDir,"/RPF.input.scatter.png"))){
          shinyjs::hide("loader-overlay")
          toggleClass(selector = ".RiboTE-working-btn-translation-efficiency", class = "RiboTE-working-hidden")
          runjs('$(".mask").hide();')
          list(
            src = paste0(TEFileuserDir,"/RPF.input.scatter.png"),
            contentType = "png",
            width = "600px",
            height = "600px",
            alt = " "
          )
        }
      } else {
        return(list(
          src = "img/RiboTE_null_image.png",
          contentType = "image/png",
          width = "1px",
          height = "1px"
        ))
      }
      
    }, deleteFile=FALSE)
  })
  ####TE 数据下载----
  output$ExportBtn8 <- downloadHandler(
    filename = function() {
      return("translation_efficiency_normalcount.csv")
    },
    content = function(file) {
      filepath <- paste0(TEFileuserDir, "/normalcount.csv")
      if (file.exists(filepath)) {
        file.copy(filepath, file)
      } else {
        return(NULL)
      }
    }
  )
  output$ExportBtn8_geneid_standard <- downloadHandler(
    filename = function() {
      return("translation_efficiency_normalcount_geneid_standard.csv")
    },
    content = function(file) {
      filepath <- paste0(TEFileuserDir, "/normalcountStandard.csv")
      if (file.exists(filepath)) {
        file.copy(filepath, file)
      } else {
        return(NULL)
      }
    }
  )
  output$ExportBtn9 <- downloadHandler(
    filename = function() {
      return("translation_efficiency_volcano_plot_data.zip")
    },
    content = function(file) {
      files_to_zip <- c(
        paste0(TEFileuserDir, "/TEVolcanoScatterData.csv"),
        paste0(TEFileuserDir, "/volcanoABData.csv")
      )
      if (all(file.exists(files_to_zip))) {
        zip::zipr(file, files_to_zip)
      } else {
        return(NULL)
      }
    }
  )
  output$ExportBtn10 <- downloadHandler(
    filename = function() {
      return("translation_efficiency_volcano_plot_pdf.zip")
    },
    content = function(file) {
      files_to_zip <- c(
        paste0(TEFileuserDir, "/TE.volcano.scatter.pdf"),
        paste0(TEFileuserDir, "/volcanoAB.pdf")
      )
      if (all(file.exists(files_to_zip))) {
        zip::zipr(file, files_to_zip)
      } else {
        return(NULL)
      }
    }
  )
  output$ExportBtn11 <- downloadHandler(
    filename = function() {
      return("translation_efficiency_volcano_plot_png.zip")
    },
    content = function(file) {
      files_to_zip <- c(
        paste0(TEFileuserDir, "/TE.volcano.scatter.png"),
        paste0(TEFileuserDir, "/volcanoAB.png")
      )
      if (all(file.exists(files_to_zip))) {
        zip::zipr(file, files_to_zip)
      } else {
        return(NULL)
      }
    }
  )

  output$ExportBtn12 <- downloadHandler(
    filename = function() {
      return("translation_efficiency_scatter_plot_data.zip")
    },
    content = function(file) {
      files_to_zip <- c(
        paste0(TEFileuserDir, "/inputScatterData.csv"),
        paste0(TEFileuserDir, "/rpfInputScatterData.csv"),
        paste0(TEFileuserDir, "/TEScatterData.csv"),
        paste0(TEFileuserDir, "/TERpkmFcScatterData.csv")
      )
      if (all(file.exists(files_to_zip))) {
        zip::zipr(file, files_to_zip)
      } else {
        return(NULL)
      }
    }
  )
  output$ExportBtn13 <- downloadHandler(
    filename = function() {
      return("translation_efficiency_scatter_plot_pdf.zip")
    },
    content = function(file) {
      files_to_zip <- c(
        paste0(TEFileuserDir, "/Input.scatter.pdf"),
        paste0(TEFileuserDir, "/RPF.input.scatter.pdf"),
        paste0(TEFileuserDir, "/TE.scatter.pdf"),
        paste0(TEFileuserDir, "/TE_rpkm_fc.scatter.pdf")
      )
      if (all(file.exists(files_to_zip))) {
        zip::zipr(file, files_to_zip)
      } else {
        return(NULL)
      }
    }
  )
  output$ExportBtn14 <- downloadHandler(
    filename = function() {
      return("translation_efficiency_volcano_plot_png.zip")
    },
    content = function(file) {
      files_to_zip <- c(
        paste0(TEFileuserDir, "/Input.scatter.png"),
        paste0(TEFileuserDir, "/RPF.input.scatter.png"),
        paste0(TEFileuserDir, "/TE.scatter.png"),
        paste0(TEFileuserDir, "/TE_rpkm_fc.scatter.png")
      )
      if (all(file.exists(files_to_zip))) {
        zip::zipr(file, files_to_zip)
      } else {
        return(NULL)
      }
    }
  )
  ####PCA 数据展示----
  output$pcaPlot <- renderPlot({
    if(input$demoData >= 0 && is.null(input$geneMatrix) && !te()){
      return(NULL)
    }
    if(te()){
      pcaPlot <- PCAResult()
      shinyjs::hide("loader-overlay2")
      runjs('$(".mask").hide();')
      toggleClass(selector = ".RiboTE-working-btn-pca", class = "RiboTE-working-hidden")
      return(pcaPlot)
    }
  }, execOnResize = FALSE)
  ####PCA 数据下载----
  output$ExportBtn15 <- downloadHandler(
    filename = function() {
      return(paste0(tolower(input$pcaData), "_", tolower(input$pcaMethod), "_plot_","_data.csv"))
    },
    content = function(file) {
      filepath <- paste0(TEFileuserDir, "/", tolower(input$pcaData), "_", tolower(input$pcaMethod), "_plot_data.csv")
      if (file.exists(filepath)) {
        file.copy(filepath, file)
      } else {
        return(NULL)
      }
    }
  )
  output$ExportBtn16 <- downloadHandler(
    filename = function() {
      return(paste0(tolower(input$pcaData), "_", tolower(input$pcaMethod), ".pdf"))
    },
    content = function(file) {
      filepath <- paste0(TEFileuserDir, "/", tolower(input$pcaData), ".", tolower(input$pcaMethod), ".pdf")
      if (file.exists(filepath)) {
        file.copy(filepath, file)
      } else {
        return(NULL)
      }
    }
  )
  output$ExportBtn17 <- downloadHandler(
    filename = function() {
      return(paste0(tolower(input$pcaData), "_", tolower(input$pcaMethod), ".png"))
    },
    content = function(file) {
      filepath <- paste0(TEFileuserDir, "/", tolower(input$pcaData), ".", tolower(input$pcaMethod), ".png")
      if (file.exists(filepath)) {
        file.copy(filepath, file)
      } else {
        return(NULL)
      }
    }
  )
  ####Heatmap 数据展示----
  output$heatmapPlot <- renderPlot({
    if(input$demoData >= 0 && is.null(input$geneMatrix) && !te()){
      return(list(
        src = "img/RiboTE_null_image.png",
        contentType = "image/png",
        width = "1px",
        height = "1px"
      ))
    }
    if(te()){
      heatmapDataTotal <- heatmapData()
      heatmapData <- heatmapDataTotal[[1]]
      heatmapDataType <- heatmapDataTotal[[2]]
      heatmapPlot <- heatmapPlot(heatmapData, heatmapDataType)
      shinyEnv$heatPosMain <- InteractiveComplexHeatmap::htPositionsOnDevice(shinyEnv$heatMain)
      shinyjs::hide("loader-overlay3")
      runjs('$(".mask").hide();')
      toggleClass(selector = ".RiboTE-working-btn-clustering", class = "RiboTE-working-hidden")
      return(heatmapPlot)
    }
  })
  output$subHeatmap <- renderPlot({
    if (is.null(input$heatmapBrush) && input$subHeatmapDisplayMode == "Base on GeneID") {
      session$resetBrush("heatmapBrush")
      enterGeneIDs <- unlist(strsplit(input$enterGeneIDs, "[,，]"))
      enterGeneIDs <- trimws(enterGeneIDs)
      submapReturn <- heatsub(enterGeneIDs)
      if (is.null(submapReturn)) {
        plot.new()
        text(0.5, 0.5, "The requested gene was not found in the heatmap", cex = 1)
        return()
      }
      shinyEnv$ht_sub_obj <- submapReturn$ht_select
      shinyEnv$submap_data <- submapReturn$submap_data
      shinyEnv$sub_groups <- submapReturn$sub_groups
      shinyEnv$group_colors <- submapReturn$group_colors
      shinyEnv$click_data <- submapReturn$click_data
      shinyEnv$heatmapSub <- ComplexHeatmap::draw(
        shinyEnv$ht_sub_obj,
        annotation_legend_list = submapReturn$lgd,
        annotation_legend_side = "top"
      )
      heightSubHeatmap <- heightSubHeatmap()
      CairoPNG(paste0(TEFileuserDir, "/heatmap_sub.png"), height = heightSubHeatmap * 2, width = 500, res = 120)
      draw(shinyEnv$heatmapSub)
      dev.off()
      pdf(paste0(TEFileuserDir, "/heatmap_sub.pdf"), width = 5, height = heightSubHeatmap * 0.02)
      draw(shinyEnv$heatmapSub)
      dev.off()
      grid::grid.newpage()
      draw(shinyEnv$heatmapSub)
      shinyEnv$ht_pos_sub <- InteractiveComplexHeatmap::htPositionsOnDevice(shinyEnv$heatmapSub)
      return(shinyEnv$heatmapSub)
    }

    if (!is.null(input$heatmapBrush)) {
      if (input$subHeatmapDisplayMode == "Base on GeneID") {
        session$resetBrush("heatmapBrush")
        enterGeneIDs <- unlist(strsplit(input$enterGeneIDs, "[,，]"))
        enterGeneIDs <- trimws(enterGeneIDs)
        submapReturn <- heatsub(enterGeneIDs)
      }
      if (input$subHeatmapDisplayMode == "Select area") {
        submapReturn <- heatsub(NULL)
      }
      if (is.null(submapReturn)) {
        plot.new()
        text(0.5, 0.5, "The requested gene was not found in the heatmap", cex = 1)
        return()
      }
      shinyEnv$ht_sub_obj <- submapReturn$ht_select
      shinyEnv$submap_data <- submapReturn$submap_data
      shinyEnv$sub_groups <- submapReturn$sub_groups
      shinyEnv$group_colors <- submapReturn$group_colors
      shinyEnv$click_data <- submapReturn$click_data
      shinyEnv$heatmapSub <- ComplexHeatmap::draw(
        shinyEnv$ht_sub_obj,
        annotation_legend_list = submapReturn$lgd,
        annotation_legend_side = "top",
        padding = grid::unit(c(2, 2, 2, 10), "mm")
      )
      heightSubHeatmap <- heightSubHeatmap()
      CairoPNG(paste0(TEFileuserDir, "/heatmap_sub.png"), height = heightSubHeatmap * 2, width = 500, res = 120)
      draw(shinyEnv$heatmapSub)
      dev.off()
      pdf(paste0(TEFileuserDir, "/heatmap_sub.pdf"), width = 5, height = heightSubHeatmap * 0.02)
      draw(shinyEnv$heatmapSub)
      dev.off()
      grid::grid.newpage()
      draw(shinyEnv$heatmapSub)
      shinyEnv$ht_pos_sub <- InteractiveComplexHeatmap::htPositionsOnDevice(shinyEnv$heatmapSub)
      return(shinyEnv$heatmapSub)
    }
  }, height = reactive(heightSubHeatmap()), width = 500)
  output$htClickContent <- renderUI({
    if(!is.null(input$heatmapBrush) && !is.null(input$htClick)){
      return(clusterHeatClickInfo(
        click = input$htClick,
        ht_sub = shinyEnv$heatmapSub,
        ht_sub_obj = shinyEnv$ht_sub_obj,
        ht_pos_sub = shinyEnv$ht_pos_sub,
        sub_groups = shinyEnv$sub_groups,
        group_colors = shinyEnv$group_colors,
        click_data = shinyEnv$click_data
      ))
    }
    if(is.null(input$heatmapBrush) && input$subHeatmapDisplayMode == "Base on GeneID" && !is.null(input$htClick)){
      return(clusterHeatClickInfo(
        click = input$htClick,
        ht_sub = shinyEnv$heatmapSub,
        ht_sub_obj = shinyEnv$ht_sub_obj,
        ht_pos_sub = shinyEnv$ht_pos_sub,
        sub_groups = shinyEnv$sub_groups,
        group_colors = shinyEnv$group_colors,
        click_data = shinyEnv$click_data
      ))
    }
  })
  ####Heatmap 数据下载----
  output$ExportBtn18 <- downloadHandler(
    filename = function() {
      return("heatmap_data.csv")
    },
    content = function(file) {
      filepath <- paste0(TEFileuserDir, "/heatmap_data.csv")
      if (file.exists(filepath)) {
        file.copy(filepath, file)
      } else {
        return(NULL)
      }
    }
  )
  output$ExportBtn19 <- downloadHandler(
    filename = function() {
      if(file.exists(paste0(TEFileuserDir, "/heatmap_sub.pdf"))){
        return("heatmap_main_sub.zip")
      } else {
        return("heatmap_main.pdf")
      }
    },
    content = function(file) {
      if(file.exists(paste0(TEFileuserDir, "/heatmap_sub.pdf"))){
        files_to_zip <- c(
          paste0(TEFileuserDir, "/heatmap_main.pdf"),
          paste0(TEFileuserDir, "/heatmap_sub.pdf")
        )
        if (all(file.exists(files_to_zip))) {
          zip::zipr(file, files_to_zip)
        }
      } else if(!file.exists(paste0(TEFileuserDir, "/heatmap_sub.pdf"))){
        filepath <- paste0(TEFileuserDir, "/heatmap_main.pdf")
        if (file.exists(filepath)) {
          file.copy(filepath, file)
        }
      } else {
        return(NULL)
      }
    }
  )
  output$ExportBtn20 <- downloadHandler(
    filename = function() {
      if(file.exists(paste0(TEFileuserDir, "/heatmap_sub.png"))){
        return("heatmap_main_sub.zip")
      } else {
        return("heatmap_main.png")
      }
    },
    content = function(file) {
      if(file.exists(paste0(TEFileuserDir, "/heatmap_sub.png"))){
        files_to_zip <- c(
          paste0(TEFileuserDir, "/heatmap_main.png"),
          paste0(TEFileuserDir, "/heatmap_sub.png")
        )
        if (all(file.exists(files_to_zip))) {
          zip::zipr(file, files_to_zip)
        }
      } else if(!file.exists(paste0(TEFileuserDir, "/heatmap_sub.png"))){
        filepath <- paste0(TEFileuserDir, "/heatmap_main.png")
        if (file.exists(filepath)) {
          file.copy(filepath, file)
        }
      } else {
        return(NULL)
      }
    }
  )
  ####SignalP 数据展示----
  output$SignalPPlot <- renderPlot({
    if(input$demoData >= 0 && is.null(input$geneMatrix) && !te()){
      return(NULL)
    }
    if(te()){
      req(file.exists(paste0(TEFileuserDir,"/normalcount.csv")))
      signalPPlot <- SignalPResult()
      shinyjs::hide("loader-overlay11")
      toggleClass(selector = ".RiboTE-working-btn-signalP", class = "RiboTE-working-hidden")
      return(signalPPlot)
    }
  }, execOnResize = FALSE)
  ####SignalP 数据下载----
  output$ExportBtn28 <- downloadHandler(
    filename = function() {
      return(paste0("signalP_plot_", input$SignalPMethod, "_data", ".csv"))
    },
    content = function(file) {
      filepath <- shinyEnv$signalPImagePath1
      if (file.exists(filepath)) {
        file.copy(filepath, file)
      } else {
        return(NULL)
      }
    }
  )
  output$ExportBtn29 <- downloadHandler(
    filename = function() {
      return(paste0("signalP_plot_", input$SignalPMethod, ".pdf"))
    },
    content = function(file) {
      filepath <- shinyEnv$signalPImagePath3
      if (file.exists(filepath)) {
        file.copy(filepath, file)
      } else {
        return(NULL)
      }
    }
  )
  output$ExportBtn30 <- downloadHandler(
    filename = function() {
      return(paste0("signalP_plot_", input$SignalPMethod, ".png"))
    },
    content = function(file) {
      filepath <- shinyEnv$signalPImagePath2
      if (file.exists(filepath)) {
        file.copy(filepath, file)
      } else {
        return(NULL)
      }
    }
  )
  ####GSEA 数据展示----
  output$gseaTable <- renderTable({
    if(input$demoData >= 0 && is.null(input$geneMatrix) &&!te()){
      return(NULL)
    }
    if(te()){
      gaeaResult <- gaeaResult()
      shinyjs::hide("loader-overlay5")
      runjs('$(".mask").hide();')
      toggleClass(selector = ".RiboTE-working-btn-gsea", class = "RiboTE-working-hidden")
      return(gaeaResult[, c(1:5)])
    }
  }, digits = -1, spacing = "s", striped = TRUE, bordered = TRUE, width = "auto", hover = TRUE, sanitize.text.function = function(x) x)


  ####GSEA 数据下载----
  output$ExportBtn21 <- downloadHandler(
    filename = function() {
      return("pathway_table.csv")
    },
    content = function(file) {
      filepath <- paste0(TEFileuserDir, "/pathwayTable.csv")
      if (file.exists(filepath)) {
        file.copy(filepath, file)
      } else {
        return(NULL)
      }
    }
  )

  ####KEGG 数据展示----
  output$keggImage <- renderImage({
    if(input$demoData >= 0 && is.null(input$geneMatrix) && !te()){
      return(list(
        src = "img/RiboTE_null_image.png",
        contentType = "image/png",
        width = "1px",
        height = "1px"
      ))
    }
    if(te()){
      keggPlot()
      shinyjs::hide("loader-overlay5")
      runjs('$(".mask").hide();')
      toggleClass(selector = ".RiboTE-working-btn-enrichment", class = "RiboTE-working-hidden")
      list(
        src = shinyEnv$pathwayImagePath,
        contentType = "image/png",
        width = "950px",
        height = "780px",
        alt = "KEGG pathway image."
      )
    }
  }, deleteFile = FALSE)
  ####KEGG 数据下载----
  output$ExportBtn23 <- downloadHandler(
    filename = function() {
      return("pathway.png")
    },
    content = function(file) {
      filepath <- shinyEnv$pathwayImagePath
      if (file.exists(filepath)) {
        file.copy(filepath, file)
      } else {
        return(NULL)
      }
    }
  )
  ####Enrichment 数据展示----
  output$enrichmentTable <- renderTable({
    if(input$demoData >= 0 && is.null(input$geneMatrix) && !te()){
      return(NULL)
    }
    if(te()){
      enrichmentResult <- enrichmentResult()
      shinyjs::hide("loader-overlay6")
      runjs('$(".mask").hide();')
      runjs('$(".RiboTE-working-btn-enrichment").addClass("RiboTE-working-hidden");')
      return(enrichmentResult)
    }
  }, digits = -1, spacing = "s", striped = TRUE, bordered = TRUE, width = "auto", hover = TRUE, sanitize.text.function = function(x) x)
  ####Enrichment 数据下载----
  output$ExportBtn24 <- downloadHandler(
    filename = function() {
      return("enrichment_table.csv")
    },
    content = function(file) {
      filepath <- paste0(TEFileuserDir, "/enrichment_table.csv")
      if (file.exists(filepath)) {
        file.copy(filepath, file)
      } else {
        return(NULL)
      }
    }
  )
  ####Network 数据展示----
  output$networkPlot <- renderPlot({
    if(input$demoData >= 0 && is.null(input$geneMatrix) && !te()){
      return(NULL)
    }
    if(te()){
      WGCNAFirstData <- WGCNAFirstData()
      networkPlot <- WGCNAPlot(WGCNAFirstData)
      shinyjs::hide("loader-overlay7")
      runjs('$(".mask").hide();')
      shinyjs::runjs('$(".RiboTE-working-btn-network").addClass("RiboTE-working-hidden");')
      return(networkPlot)
    }
  },execOnResize = FALSE)
  ####Network 数据下载----
  output$ExportBtn25 <- downloadHandler(
    filename = function() {
      return("network_data.rds")
    },
    content = function(file) {
      filepath <- paste0(TEFileuserDir, "/network_data.rds")
      if (file.exists(filepath)) {
        file.copy(filepath, file)
      } else {
        return(NULL)
      }
    }
  )
  output$ExportBtn26 <- downloadHandler(
    filename = function() {
      return("network_plot.pdf")
    },
    content = function(file) {
      filepath <- paste0(TEFileuserDir, "/network_plot.pdf")
      if (file.exists(filepath)) {
        file.copy(filepath, file)
      } else {
        return(NULL)
      }
    }
  )
  output$ExportBtn27 <- downloadHandler(
    filename = function() {
      return("network_plot.png")
    },
    content = function(file) {
      filepath <- paste0(TEFileuserDir, "/network_plot.png")
      if (file.exists(filepath)) {
        file.copy(filepath, file)
      } else {
        return(NULL)
      }
    }
  )
  ####codon 数据展示----
  #deg rl -> codon data preprocess --CBI TAI
  output$cbiUpDownPlot <- renderImage({
    if((input$demoData >= 0 && input$submitButton == 0 && is.null(input$geneMatrix))) {
      return(list(
        src = "img/RiboTE_null_image.png",
        contentType = "image/png",
        width = "1px",
        height = "1px"
      ))
    }
    req(file.exists(paste0(TEFileuserDir,"/deg.csv")))
    shinyjs::show("loader-overlay10")
    shinyjs::runjs('$(".RiboTE-working-btn-codon-cbi-tai").removeClass("RiboTE-working-hidden");')
    runjs("$('.notification_codon_data_front').hide();")
    updownBoxplot()
    if(file.exists(paste0(TEFileuserDir,"/codonP2.png"))){
      return(list(
        src = paste0(TEFileuserDir,"/codonP2.png"),
        contentType = "png",
        width = "700px",
        height = "500px",
        alt = " "
      ))
    }
  }, deleteFile = FALSE)
  output$taiUpDownPlot <- renderImage({
    if((input$demoData >= 0 && input$submitButton == 0 && is.null(input$geneMatrix))) {
      return(list(
        src = "img/RiboTE_null_image.png",
        contentType = "image/png",
        width = "1px",
        height = "1px"
      ))
    }
    req(file.exists(paste0(TEFileuserDir,"/codonP1.png")))
    if(file.exists(paste0(TEFileuserDir,"/codonP1.png"))){
      shinyjs::hide("loader-overlay10")
      shinyjs::runjs('$(".RiboTE-working-btn-codon-cbi-tai").addClass("RiboTE-working-hidden");')
      return(list(
        src = paste0(TEFileuserDir,"/codonP1.png"),
        contentType = "png",
        width = "700px",
        height = "500px",
        alt = " "
      ))
    }
  }, deleteFile = FALSE)
  #cbi1 <- CBI TAI --CBI
  output$cbiInputTE1Plot <- renderImage({
    if(input$demoData >= 0 && input$submitButton >= 0 && is.null(input$geneMatrix) && !cbidone()) {
      return(list(
        src = "img/RiboTE_null_image.png",
        contentType = "image/png",
        width = "1px",
        height = "1px"
      ))
    }
    if(cbidone()){
      shinyjs::show("loader-overlay10")
      shinyjs::runjs('$(".RiboTE-working-btn-codon-cbi").removeClass("RiboTE-working-hidden");')
      runjs("$('.notification_cbi_tai').hide();")
      cbiScatter()
      if(input$cbiMothodSelect == "input"){
        cbiInputTE1Path1 <- paste0(TEFileuserDir,"/codonP3.png")
      }
      if(input$cbiMothodSelect == "TE"){
        cbiInputTE1Path1 <- paste0(TEFileuserDir,"/codonP5.png")
      }
      return(list(
        src = cbiInputTE1Path1,
        contentType = "png",
        width = "700px",
        height = "500px",
        alt = " "
      ))
    }
  }, deleteFile = FALSE)
  output$cbiInputTE2Plot <- renderImage({
    if(input$demoData >= 0 && input$submitButton >= 0 && is.null(input$geneMatrix) && !cbidone()) {
      return(list(
        src = "img/RiboTE_null_image.png",
        contentType = "image/png",
        width = "1px",
        height = "1px"
      ))
    }
    if(cbidone()){
      req(file.exists(paste0(TEFileuserDir,"/codonP4.png")))
      shinyjs::hide("loader-overlay10")
      shinyjs::runjs('$(".RiboTE-working-btn-codon-cbi").addClass("RiboTE-working-hidden");')
      if(input$cbiMothodSelect == "input"){
        cbiInputTE1Path2 <- paste0(TEFileuserDir,"/codonP4.png")
      }
      if(input$cbiMothodSelect == "TE"){
        cbiInputTE1Path2 <- paste0(TEFileuserDir,"/codonP6.png")
      }
      return(list(
        src = cbiInputTE1Path2,
        contentType = "png",
        width = "700px",
        height = "500px",
        alt = " "
      ))
    }

  }, deleteFile = FALSE)
  #--condon Input
  output$codonPerc <- renderUI({
    req(input$codonSelect)
    codonImagePanels <- lapply(input$codonSelect, function(codon) {
      tagList(
        fluidRow(
          column(6, imageOutput(paste0("plot1_", codon))),
          column(6, imageOutput(paste0("plot2_", codon)))
        )
      )
    })
    do.call(tagList, codonImagePanels)
  })
  observeEvent(input$codonTabs, {
    if (input$codonTabs == "codon Input" && !shinyEnv$submitDone) {
      req(file.exists(paste0(TEFileuserDir,"/rl.csv")))
      req(file.exists(paste0(TEFileuserDir,"/forcodon.csv")))
      shinyjs::show("loader-overlay10")
      shinyjs::runjs('$(".RiboTE-working-btn-codon-input").removeClass("RiboTE-working-hidden");')
      runjs("$('.notification_codon_data_front').hide();")
      codonPerc()
      req(file.exists(paste0(TEFileuserDir,"/codonPercComplete.csv")))

      lapply(input$codonSelect, function(codon) {
        output[[paste0("plot1_", codon)]] <- renderImage({
          if((input$demoData == 0 && is.null(input$geneMatrix))) {
            return(list(
              src = "img/RiboTE_null_image.png",
              contentType = "image/png",
              width = "1px",
              height = "1px"
            ))
          }
          req(file.exists(paste0(TEFileuserDir, "/", codon, ".input1.perc.png")))
          list(src = file.path(TEFileuserDir, paste0(codon, ".input1.perc.png")),
               contentType = 'png',
               width = 500,
               height = 400,
               alt = " ")
        }, deleteFile = FALSE)

        output[[paste0("plot2_", codon)]] <- renderImage({
          if((input$demoData == 0 && is.null(input$geneMatrix))) {
            return(list(
              src = "img/RiboTE_null_image.png",
              contentType = "image/png",
              width = "1px",
              height = "1px"
            ))
          }
          req(file.exists(paste0(TEFileuserDir, "/", codon, ".input2.perc.png")))
          list(src = file.path(TEFileuserDir, paste0(codon, ".input2.perc.png")),
               contentType = 'png',
               width = 500,
               height = 400,
               alt = " ")
        }, deleteFile = FALSE)
      })
      shinyEnv$submitDone <- TRUE
    }
  })
  #--codon Specific
  output$codonSpecific <- renderUI({
    req(input$codonSelect)
    codonGroups <- split(input$codonSelect, ceiling(seq_along(input$codonSelect) / 2))
    codonImagePanels2 <- lapply(codonGroups, function(codonPair) {
      tagList(
        fluidRow(
          if (length(codonPair) >= 1) {
            column(6, imageOutput(paste0("plot3_", codonPair[1]), width = 500, height = 500))
          },
          if (length(codonPair) == 2) {
            column(6, imageOutput(paste0("plot3_", codonPair[2]), width = 500, height = 500))
          }
        )
      )
    })
    do.call(tagList, codonImagePanels2)
  })
  observeEvent(input$codonTabs, {
    if (input$codonTabs == "codon Specific" && !shinyEnv$analysisDone2) {
      req(file.exists(paste0(TEFileuserDir,"/rl.csv")))
      req(file.exists(paste0(TEFileuserDir,"/deg.csv")))
      shinyjs::show("loader-overlay10")
      shinyjs::runjs('$(".RiboTE-working-btn-codon-specific").removeClass("RiboTE-working-hidden");')
      runjs("$('.notification_codon_data_front').hide();")
      codonSpecific()
      req(file.exists(paste0(TEFileuserDir,"/codonPercComplete2.csv")))
      lapply(input$codonSelect, function(codon) {
        output[[paste0("plot3_", codon)]] <- renderImage({
          if((input$demoData == 0 && is.null(input$geneMatrix))) {
            return(list(
              src = "img/RiboTE_null_image.png",
              contentType = "image/png",
              width = "1px",
              height = "1px"
            ))
          }
          req(file.exists(paste0(TEFileuserDir, "/", codon, ".specific.boxplot.png")))
          list(src = file.path(TEFileuserDir, paste0(codon, ".specific.boxplot.png")),
               contentType = 'png',
               width = 500,
               height = 500,
               alt = " ")
        }, deleteFile = FALSE)
      })
      shinyEnv$analysisDone2 <- TRUE
    }
  })

  #deg2 rl2 <- codon Specific --Perc Per1K
  output$logFCPercPlot <- renderImage({
    if(input$demoData >= 0 && input$submitButton >= 0 && is.null(input$geneMatrix) && !codonspecific()) {
      return(list(
        src = "img/RiboTE_null_image.png",
        contentType = "image/png",
        width = "1px",
        height = "1px"
      ))
    }
    if(codonspecific()){
      shinyjs::show("loader-overlay10")
      shinyjs::runjs('$(".RiboTE-working-btn-codon-perc-per1k").removeClass("RiboTE-working-hidden");')
      runjs("$('.notification_codon_specific').hide();")
      percPer1kScatter()
      if(file.exists(paste0(TEFileuserDir,"/codonPercComplete3.csv"))){
        if(input$percPer1kUpDownSelect == "Up"){
          logFCPercPlotPath <- paste0(TEFileuserDir,"/scatter_plot_logFC_perc_up.png")
        }
        if(input$percPer1kUpDownSelect == "Down"){
          logFCPercPlotPath <- paste0(TEFileuserDir,"/","scatter_plot_logFC_perc_down.png")
        }
        if(input$percPer1kUpDownSelect == "Up and Down"){
          logFCPercPlotPath <- paste0(TEFileuserDir,"/","scatter_plot_logFC_per1k.png")
        }
        return(list(
          src = logFCPercPlotPath,
          contentType = "png",
          width = "700px",
          height = "500px",
          alt = " "
        ))
      }
    }
  }, deleteFile = FALSE)
  output$logFCPer1kPlot <- renderImage({
    if(input$demoData >= 0 && input$submitButton >= 0 && is.null(input$geneMatrix) && !codonspecific()) {
      return(list(
        src = "img/RiboTE_null_image.png",
        contentType = "image/png",
        width = "1px",
        height = "1px"
      ))
    }
    if(codonspecific()){
      req(file.exists(paste0(TEFileuserDir,"/codonPercComplete3.csv")))
      if(file.exists(paste0(TEFileuserDir,"/codonPercComplete3.csv"))){
        if(input$percPer1kUpDownSelect == "Up"){
          logFCPer1kPlotPath <- paste0(TEFileuserDir,"/scatter_plot_logFC_per1k_up.png")
        }
        if(input$percPer1kUpDownSelect == "Down"){
          logFCPer1kPlotPath <- paste0(TEFileuserDir,"/","scatter_plot_logFC_per1k_down.png")
        }
        if(input$percPer1kUpDownSelect == "Up and Down"){
          logFCPer1kPlotPath <- paste0(TEFileuserDir,"/","scatter_plot_zscore_per1k.png")
        }
        return(list(
          src = logFCPer1kPlotPath,
          contentType = "png",
          width = "700px",
          height = "500px",
          alt = " "
        ))
      }
    }
  }, deleteFile = FALSE)
  output$ZscorePer1kPlot <- renderImage({
    if(input$demoData >= 0 && input$submitButton >= 0 && is.null(input$geneMatrix) && !codonspecific()) {
      return(list(
        src = "img/RiboTE_null_image.png",
        contentType = "image/png",
        width = "1px",
        height = "1px"
      ))
    }
    if(codonspecific()){
      req(file.exists(paste0(TEFileuserDir,"/codonPercComplete3.csv")))
      if(file.exists(paste0(TEFileuserDir,"/codonPercComplete3.csv"))){
        if(input$percPer1kUpDownSelect == "Up"){
          ZscorePer1kPlotPath <- paste0(TEFileuserDir,"/scatter_plot_zscore_per1k_up.png")
        }
        if(input$percPer1kUpDownSelect == "Down"){
          ZscorePer1kPlotPath <- paste0(TEFileuserDir,"/","scatter_plot_zscore_per1k_down.png")
        }
        if(input$percPer1kUpDownSelect == "Up and Down"){
          ZscorePer1kPlotPath <- "www/img/RiboTE_null_image.png"
          shinyjs::hide("loader-overlay10")
          shinyjs::runjs('$(".RiboTE-working-btn-codon-perc-per1k").addClass("RiboTE-working-hidden");')
          return(list(
            src = ZscorePer1kPlotPath,
            contentType = "image/png",
            width = "1px",
            height = "1px",
            alt = " "
          ))
        }
        shinyjs::hide("loader-overlay10")
        shinyjs::runjs('$(".RiboTE-working-btn-codon-perc-per1k").addClass("RiboTE-working-hidden");')
        return(list(
          src = ZscorePer1kPlotPath,
          contentType = "png",
          width = "700px",
          height = "500px",
          alt = " "
        ))
      }
    }
  }, deleteFile = FALSE)
  #--codon Frequency <- Perc Per1K
  output$codonFrequency <- renderImage({
    if(input$demoData >= 0 && input$submitButton >= 0 && is.null(input$geneMatrix) && !codonspecific() && !percper1k()) {
      return(list(
        src = "img/RiboTE_null_image.png",
        contentType = "image/png",
        width = "1px",
        height = "1px"
      ))
    }
    if(codonspecific() && percper1k()){
      req(file.exists(paste0(TEFileuserDir,"/objCodonStat3.csv")))
      shinyjs::show("loader-overlay10")
      shinyjs::runjs('$(".RiboTE-working-btn-codon-frequency").removeClass("RiboTE-working-hidden");')
      runjs("$('.notification_codon_perc_per1k').hide();")
      codonFrequencyBoxplot()
      req(file.exists(paste0(TEFileuserDir,"/perc.boxplot.png")))
      shinyjs::hide("loader-overlay10")
      shinyjs::runjs('$(".RiboTE-working-btn-codon-frequency").addClass("RiboTE-working-hidden");')
      return(list(
        src = paste0(TEFileuserDir,"/perc.boxplot.png"),
        contentType = "png",
        width = "700px",
        height = "600px",
        alt = " "
      ))
    }else{
      return(list(
        src = "img/RiboTE_null_image.png",
        contentType = "image/png",
        width = "1px",
        height = "1px"
      ))
    }
  }, deleteFile = FALSE)
  #--Heatmap
  output$codonHeatmap <- renderImage({
    if(input$demoData >= 0 && input$submitButton >= 0 && is.null(input$geneMatrix) && !codonspecific()) {
      return(list(
        src = "img/RiboTE_null_image.png",
        contentType = "image/png",
        width = "1px",
        height = "1px"
      ))
    }
    if(codonspecific()){
      req(file.exists(paste0(TEFileuserDir,"/rl2.csv")))
      shinyjs::show("loader-overlay10")
      shinyjs::runjs('$(".RiboTE-working-btn-codon-heatmap").removeClass("RiboTE-working-hidden");')
      runjs("$('.notification_codon_specific').hide();")
      codonHeatmapPlot()
      req(file.exists(paste0(TEFileuserDir,"/codon_zscore_heatmap.png")))
      return(list(
        src = paste0(TEFileuserDir,"/codon_zscore_heatmap.png"),
        contentType = "png",
        width = "750px",
        height = "750px",
        alt = " "
      ))
    }
  }, deleteFile = FALSE)
  #--codon Ratio
  output$codonRatioUpDown1 <- renderImage({
    if(input$demoData >= 0 && input$submitButton >= 0 && is.null(input$geneMatrix) && !codonspecific()) {
      return(list(
        src = "img/RiboTE_null_image.png",
        contentType = "image/png",
        width = "1px",
        height = "1px"
      ))
    }
    if(codonspecific()){
      req(file.exists(paste0(TEFileuserDir,"/deg2.csv")))
      req(file.exists(paste0(TEFileuserDir,"/rl2.csv")))
      shinyjs::show("loader-overlay10")
      shinyjs::runjs('$(".RiboTE-working-btn-codon-ratio").removeClass("RiboTE-working-hidden");')
      runjs("$('.notification_codon_specific').hide();")
      codoNatioPlot()
      if(file.exists(paste0(TEFileuserDir,"/codonPercComplete4.csv"))){
        if(input$codoNatioUpDownSelect == "Up"){
          codoNatioPlotPath1 <- paste0(TEFileuserDir,"/codon_ratio_stat_percent_up.png")
          return(list(
            src = codoNatioPlotPath1,
            contentType = "png",
            width = "600px",
            height = "600px",
            alt = " "
          ))
        }
        if(input$codoNatioUpDownSelect == "Down"){
          codoNatioPlotPath2 <- paste0(TEFileuserDir,"/","codon_ratio_stat_percent_down.png")
          return(list(
            src = codoNatioPlotPath2,
            contentType = "png",
            width = "600px",
            height = "600px",
            alt = " "
          ))
        }
      }
    }
  }, deleteFile = FALSE)
  output$codonRatioUpDown2 <- renderImage({
    if(input$demoData >= 0 && input$submitButton >= 0 && is.null(input$geneMatrix) && !codonspecific()) {
      return(list(
        src = "img/RiboTE_null_image.png",
        contentType = "image/png",
        width = "1px",
        height = "1px"
      ))
    }
    if(codonspecific()){
      if(file.exists(paste0(TEFileuserDir,"/codonPercComplete4.csv"))){
        if(input$codoNatioUpDownSelect == "Up"){
          codoNatioPlotPath1 <- paste0(TEFileuserDir,"/codon_ratio_stat_number_up.png")
          return(list(
            src = codoNatioPlotPath1,
            contentType = "png",
            width = "600px",
            height = "600px",
            alt = " "
          ))
        }
        if(input$codoNatioUpDownSelect == "Down"){
          codoNatioPlotPath2 <- paste0(TEFileuserDir,"/","codon_ratio_stat_number_down.png")
          return(list(
            src = codoNatioPlotPath2,
            contentType = "png",
            width = "600px",
            height = "600px",
            alt = " "
          ))
        }
      }
    }
  }, deleteFile = FALSE)
  #--dendrogram
  output$dendrogramPlot <- renderImage({
    if(input$demoData >= 0 && input$submitButton >= 0 && is.null(input$geneMatrix) && !codonspecific()) {
      return(list(
        src = "img/RiboTE_null_image.png",
        contentType = "image/png",
        width = "1px",
        height = "1px"
      ))
    }
    if(codonspecific()){
      req(file.exists(paste0(TEFileuserDir,"/rl2.csv")))
      shinyjs::show("loader-overlay10")
      shinyjs::runjs('$(".RiboTE-working-btn-codon-dendrogram").removeClass("RiboTE-working-hidden");')
      runjs("$('.notification_codon_specific').hide();")
      dendrogramPlot()
      req(length(list.files(TEFileuserDir, pattern = "\\.dendrogram\\.png$", full.names = TRUE)) > 0)
      req(file.exists(paste0(TEFileuserDir,"/codonPercComplete5.csv")))
      dendrogramPlotPath <- list.files(TEFileuserDir, pattern = "\\.dendrogram\\.png$", full.names = TRUE)
      return(list(
        src = dendrogramPlotPath[[1]],
        contentType = "png",
        width = "900px",
        height = "450px",
        alt = " "
      ))
    }
  }, deleteFile = FALSE)

  #deg2 rl3 <- Codon Ratio --distribution
  output$distributionUpDown1 <- renderImage({
    if(input$demoData >= 0 && input$submitButton >= 0 && is.null(input$geneMatrix) && !codonratio()) {
      return(list(
        src = "img/RiboTE_null_image.png",
        contentType = "image/png",
        width = "1px",
        height = "1px"
      ))
    }
    if(codonratio()){
      req(file.exists(paste0(TEFileuserDir,"/rl3.csv")))
      shinyjs::show("loader-overlay10")
      shinyjs::runjs('$(".RiboTE-working-btn-codon-distribution").removeClass("RiboTE-working-hidden");')
      runjs("$('.notification_codon_ratio').hide();")
      distributionPlot()
      if(input$distributionUpDownSelect == "Up"){
        req(file.exists(paste0(TEFileuserDir,"/upratio_distribution_Up",".png")))
        distributionPlotPath <- paste0(TEFileuserDir,"/upratio_distribution_Up",".png")
      }
      if(input$distributionUpDownSelect == "Down"){
        req(file.exists(paste0(TEFileuserDir,"/upratio_distribution_Down",".png")))
        distributionPlotPath <- paste0(TEFileuserDir,"/upratio_distribution_Down",".png")
      }
      return(list(
        src = distributionPlotPath,
        contentType = "png",
        width = "800px",
        height = "500px",
        alt = " "
      ))
    }
  }, deleteFile = FALSE)
  output$distributionUpDown2 <- renderImage({
    if(input$demoData >= 0 && input$submitButton >= 0 && is.null(input$geneMatrix) && !codonratio()) {
      return(list(
        src = "img/RiboTE_null_image.png",
        contentType = "image/png",
        width = "1px",
        height = "1px"
      ))
    }
    if(codonratio()){
      req(file.exists(paste0(TEFileuserDir,"/codonPercComplete6.csv")))
      if(input$distributionUpDownSelect == "Up"){
        req(file.exists(paste0(TEFileuserDir,"/dnratio_distribution_Up",".png")))
        distributionPlotPath <- paste0(TEFileuserDir,"/dnratio_distribution_Up",".png")
      }
      if(input$distributionUpDownSelect == "Down"){
        req(file.exists(paste0(TEFileuserDir,"/dnratio_distribution_Down",".png")))
        distributionPlotPath <- paste0(TEFileuserDir,"/dnratio_distribution_Down",".png")
      }
      return(list(
        src = distributionPlotPath,
        contentType = "png",
        width = "800px",
        height = "500px",
        alt = " "
      ))
    }
  }, deleteFile = FALSE)
  #--codon Zscore
  output$codonZscoreUpDown <- renderImage({
    if(input$demoData >= 0 && input$submitButton >= 0 && is.null(input$geneMatrix) && !codonratio()) {
      return(list(
        src = "img/RiboTE_null_image.png",
        contentType = "image/png",
        width = "1px",
        height = "1px"
      ))
    }
    if(codonratio()){
      req(file.exists(paste0(TEFileuserDir,"/rl3.csv")))
      shinyjs::show("loader-overlay10")
      shinyjs::runjs('$(".RiboTE-working-btn-codon-zscore").removeClass("RiboTE-working-hidden");')
      runjs("$('.notification_codon_ratio').hide();")
      codonUpDownPlot()
      if(input$codonZscoreUpDownSelect == "Up"){
        req(file.exists(paste0(TEFileuserDir,"/codon_updown_zscore_Up",".png")))
        codonZscorePlotPath <- paste0(TEFileuserDir,"/codon_updown_zscore_Up",".png")
      }
      if(input$codonZscoreUpDownSelect == "Down"){
        req(file.exists(paste0(TEFileuserDir,"/codon_updown_zscore_Down",".png")))
        codonZscorePlotPath <- paste0(TEFileuserDir,"/codon_updown_zscore_Down",".png")
      }
      return(list(
        src = codonZscorePlotPath,
        contentType = "png",
        width = "480px",
        height = "700px",
        alt = " "
      ))
    }

  }, deleteFile = FALSE)
  #--codonPer1kFcAndPvalue
  output$codonPer1kFcAndPvaluePlot <- renderImage({
    if(input$demoData >= 0 && input$submitButton >= 0 && is.null(input$geneMatrix) && !codonratio()) {
      return(list(
        src = "img/RiboTE_null_image.png",
        contentType = "image/png",
        width = "1px",
        height = "1px"
      ))
    }
    if(codonratio()){
      req(file.exists(paste0(TEFileuserDir,"/rl3.csv")))
      shinyjs::show("loader-overlay10")
      shinyjs::runjs('$(".RiboTE-working-btn-codon-Per1kFcAndPvalue").removeClass("RiboTE-working-hidden");')
      runjs("$('.notification_codon_ratio').hide();")
      codonPer1kFcAndPvaluePlot()
      req(file.exists(paste0(TEFileuserDir,"/codon_per1k_fcAndPvalue.png")))
      codonPer1kFcAndPvaluePlotPath <- paste0(TEFileuserDir,"/codon_per1k_fcAndPvalue.png")
      return(list(
        src = codonPer1kFcAndPvaluePlotPath,
        contentType = "png",
        width = "600px",
        height = "600px",
        alt = " "
      ))
    }
  }, deleteFile = FALSE)
  #--per1kZscore
  output$per1kZscoreUpDownPlot1 <- renderImage({
    if(input$demoData >= 0 && input$submitButton >= 0 && is.null(input$geneMatrix) && !codonspecific()) {
      return(list(
        src = "img/RiboTE_null_image.png",
        contentType = "image/png",
        width = "1px",
        height = "1px"
      ))
    }
    if(codonspecific()){
      req(file.exists(paste0(TEFileuserDir,"/deg2.csv")))
      shinyjs::show("loader-overlay10")
      shinyjs::runjs('$(".RiboTE-working-btn-codon-per1k-zscore").removeClass("RiboTE-working-hidden");')
      runjs("$('.notification_codon_ratio').hide();")
      per1kZscorePlot()
      req(file.exists(paste0(TEFileuserDir,"/codonPercComplete7.csv")))
      if(input$per1kZscoreUpDownSelect == "Up"){
        per1kZscorePlotPath <- paste0(TEFileuserDir,"/per1k_zscore_Up_single.png")
      }
      if(input$per1kZscoreUpDownSelect == "Down"){
        per1kZscorePlotPath <- paste0(TEFileuserDir,"/per1k_zscore_Down_single.png")
      }
      return(list(
        src = per1kZscorePlotPath,
        contentType = "png",
        width = "600px",
        height = "600px",
        alt = " "
      ))
    }
  }, deleteFile = FALSE)
  output$per1kZscoreUpDownPlot2 <- renderImage({
    if(input$demoData >= 0 && input$submitButton >= 0 && is.null(input$geneMatrix) && !codonspecific()) {
      return(list(
        src = "img/RiboTE_null_image.png",
        contentType = "image/png",
        width = "1px",
        height = "1px"
      ))
    }
    if(codonspecific()){
      req(file.exists(paste0(TEFileuserDir,"/codonPercComplete7.csv")))
      if(input$per1kZscoreUpDownSelect == "Up"){
        per1kZscorePlotPath <- paste0(TEFileuserDir,"/per1k_zscore_Up_double.png")
      }
      if(input$per1kZscoreUpDownSelect == "Down"){
        per1kZscorePlotPath <- paste0(TEFileuserDir,"/per1k_zscore_Down_double.png")
      }
      return(list(
        src = per1kZscorePlotPath,
        contentType = "png",
        width = "600px",
        height = "600px",
        alt = " "
      ))
    }

  }, deleteFile = FALSE)
  output$per1kZscoreUpDownPlot3 <- renderImage({
    if(input$demoData >= 0 && input$submitButton >= 0 && is.null(input$geneMatrix) && !codonspecific()) {
      return(list(
        src = "img/RiboTE_null_image.png",
        contentType = "image/png",
        width = "1px",
        height = "1px"
      ))
    }
    if(codonspecific()){
      req(file.exists(paste0(TEFileuserDir,"/codonPercComplete7.csv")))
      if(input$per1kZscoreUpDownSelect == "Up"){
        per1kZscorePlotPath <- paste0(TEFileuserDir,"/per1k_zscore_Up_triple.png")
      }
      if(input$per1kZscoreUpDownSelect == "Down"){
        per1kZscorePlotPath <- paste0(TEFileuserDir,"/per1k_zscore_Down_triple.png")
      }
      return(list(
        src = per1kZscorePlotPath,
        contentType = "png",
        width = "600px",
        height = "600px",
        alt = " "
      ))
    }

  }, deleteFile = FALSE)
  #--enriched
  output$enrichedPlot1 <- renderImage({
    if(input$demoData >= 0 && input$submitButton >= 0 && is.null(input$geneMatrix) && !codonspecific()) {
      return(list(
        src = "img/RiboTE_null_image.png",
        contentType = "image/png",
        width = "1px",
        height = "1px"
      ))
    }
    if(codonspecific()){
      req(file.exists(paste0(TEFileuserDir,"/deg2.csv")))
      shinyjs::show("loader-overlay10")
      shinyjs::runjs('$(".RiboTE-working-btn-codon-enriched").removeClass("RiboTE-working-hidden");')
      runjs("$('.notification_codon_ratio').hide();")
      enrichedPlot()
      req(file.exists(paste0(TEFileuserDir,"/codonPercComplete8.csv")))
      if(input$enrichedDisplaySelect == "All"){
        enrichedPlotPath <- paste0(TEFileuserDir,"/enriched_all_single.png")
      }
      if(input$enrichedDisplaySelect == "Obj"){
        enrichedPlotPath <- paste0(TEFileuserDir,"/enriched_obj_single.png")
      }
      return(list(
        src = enrichedPlotPath,
        contentType = "png",
        width = "600px",
        height = "600px",
        alt = " "
      ))
    }

  }, deleteFile = FALSE)
  output$enrichedPlot2 <- renderImage({
    if(input$demoData >= 0 && input$submitButton >= 0 && is.null(input$geneMatrix) && !codonspecific()) {
      return(list(
        src = "img/RiboTE_null_image.png",
        contentType = "image/png",
        width = "1px",
        height = "1px"
      ))
    }
    if(codonspecific()){
      req(file.exists(paste0(TEFileuserDir,"/codonPercComplete8.csv")))
      if(input$enrichedDisplaySelect == "All"){
        enrichedPlotPath <- paste0(TEFileuserDir,"/enriched_all_double.png")
      }
      if(input$enrichedDisplaySelect == "Obj"){
        enrichedPlotPath <- paste0(TEFileuserDir,"/enriched_obj_double.png")
      }
      return(list(
        src = enrichedPlotPath,
        contentType = "png",
        width = "600px",
        height = "600px",
        alt = " "
      ))
    }
  }, deleteFile = FALSE)
  output$enrichedPlot3 <- renderImage({
    if(input$demoData >= 0 && input$submitButton >= 0 && is.null(input$geneMatrix) && !codonspecific()) {
      return(list(
        src = "img/RiboTE_null_image.png",
        contentType = "image/png",
        width = "1px",
        height = "1px"
      ))
    }
    if(codonspecific()){
      req(file.exists(paste0(TEFileuserDir,"/codonPercComplete8.csv")))
      if(input$enrichedDisplaySelect == "All"){
        enrichedPlotPath <- paste0(TEFileuserDir,"/enriched_all_triple.png")
      }
      if(input$enrichedDisplaySelect == "Obj"){
        enrichedPlotPath <- paste0(TEFileuserDir,"/enriched_obj_triple.png")
      }
      return(list(
        src = enrichedPlotPath,
        contentType = "png",
        width = "600px",
        height = "600px",
        alt = " "
      ))
    }
  }, deleteFile = FALSE)
  ####codon 数据下载----
  output$ExportBtn31 <- downloadHandler(
    filename = function() {
      return("codon_preprocess_data.csv")
    },
    content = function(file) {
      filepath <- paste0(TEFileuserDir,"/deg.csv")
      if (file.exists(filepath)) {
        file.copy(filepath, file)
      } else {
        return(NULL)
      }
    }
  )
  output$ExportBtn33 <- downloadHandler(
    filename = function() {
      return("codon_specific_pdf.zip")
    },
    content = function(file) {
      filepath <- list.files(TEFileuserDir, pattern = "specific\\.boxplot\\.pdf$", full.names = TRUE)
      if (length(filepath) > 0) {
        zip::zipr(file, filepath)
      } else {
        return(NULL)
      }
    }
  )
  output$ExportBtn34 <- downloadHandler(
    filename = function() {
      return("codon_specific_png.zip")
    },
    content = function(file) {
      filepath <- list.files(TEFileuserDir, pattern = "specific\\.boxplot\\.png$", full.names = TRUE)
      if (length(filepath) > 0) {
        zip::zipr(file, filepath)
      } else {
        return(NULL)
      }
    }
  )
  output$ExportBtn35 <- downloadHandler(
    filename = function() {
      if(input$codoNatioUpDownSelect == "Up"){
        return("codon_ratio_stat_up_pdf.zip")
      }
      if(input$codoNatioUpDownSelect == "Down"){
        return("codon_ratio_stat_down_pdf.zip")
      }
    },
    content = function(file) {
      if(input$codoNatioUpDownSelect == "Up"){
        files_to_zip <- c(
          paste0(TEFileuserDir, "/codon_ratio_stat_percent_up.pdf"),
          paste0(TEFileuserDir, "/codon_ratio_stat_number_up.pdf")
        )
        if (all(file.exists(files_to_zip))) {
          zip::zipr(file, files_to_zip)
        } else {
          return(NULL)
        }
      }
      if(input$codoNatioUpDownSelect == "Down"){
        files_to_zip <- c(
          paste0(TEFileuserDir, "/codon_ratio_stat_percent_down.pdf"),
          paste0(TEFileuserDir, "/codon_ratio_stat_number_down.pdf")
        )
        if (all(file.exists(files_to_zip))) {
          zip::zipr(file, files_to_zip)
        } else {
          return(NULL)
        }
      }

    }
  )
  output$ExportBtn36 <- downloadHandler(
    filename = function() {
      if(input$codoNatioUpDownSelect == "Up"){
        return("codon_ratio_stat_up_png.zip")
      }
      if(input$codoNatioUpDownSelect == "Down"){
        return("codon_ratio_stat_down_png.zip")
      }
    },
    content = function(file) {
      if(input$codoNatioUpDownSelect == "Up"){
        files_to_zip <- c(
          paste0(TEFileuserDir, "/codon_ratio_stat_percent_up.png"),
          paste0(TEFileuserDir, "/codon_ratio_stat_number_up.png")
        )
        if (all(file.exists(files_to_zip))) {
          zip::zipr(file, files_to_zip)
        } else {
          return(NULL)
        }
      }
      if(input$codoNatioUpDownSelect == "Down"){
        files_to_zip <- c(
          paste0(TEFileuserDir, "/codon_ratio_stat_percent_down.png"),
          paste0(TEFileuserDir, "/codon_ratio_stat_number_down.png")
        )
        if (all(file.exists(files_to_zip))) {
          zip::zipr(file, files_to_zip)
        } else {
          return(NULL)
        }
      }
    }
  )
  output$ExportBtn37 <- downloadHandler(
    filename = function() {
      return("cbi_tai_pdf.zip")
    },
    content = function(file) {
      files_to_zip <- c(
        paste0(TEFileuserDir, "/codonP1.pdf"),
        paste0(TEFileuserDir, "/codonP2.pdf")
      )
      if (all(file.exists(files_to_zip))) {
        zip::zipr(file, files_to_zip)
      } else {
        return(NULL)
      }
    }
  )
  output$ExportBtn38 <- downloadHandler(
    filename = function() {
      return("cbi_tai_png.zip")
    },
    content = function(file) {
      files_to_zip <- c(
        paste0(TEFileuserDir, "/codonP1.png"),
        paste0(TEFileuserDir, "/codonP2.png")
      )
      if (all(file.exists(files_to_zip))) {
        zip::zipr(file, files_to_zip)
      } else {
        return(NULL)
      }
    }
  )
  output$ExportBtn39 <- downloadHandler(
    filename = function() {
      if(input$cbiMothodSelect == "input"){
        return("cbi_input_pdf.zip")
      }
      if(input$cbiMothodSelect == "TE"){
        return("cbi_te_pdf.zip")
      }
    },
    content = function(file) {
      if(input$cbiMothodSelect == "input"){
        files_to_zip <- c(
          paste0(TEFileuserDir, "/codonP3.pdf"),
          paste0(TEFileuserDir, "/codonP4.pdf")
        )
        if (all(file.exists(files_to_zip))) {
          zip::zipr(file, files_to_zip)
        } else {
          return(NULL)
        }
      }
      if(input$cbiMothodSelect == "TE"){}
      files_to_zip <- c(
        paste0(TEFileuserDir, "/codonP5.pdf"),
        paste0(TEFileuserDir, "/codonP6.pdf")
      )
      if (all(file.exists(files_to_zip))) {
        zip::zipr(file, files_to_zip)
      } else {
        return(NULL)
      }
    }
  )
  output$ExportBtn40 <- downloadHandler(
    filename = function() {
      if(input$cbiMothodSelect == "input"){
        return("cbi_input_png.zip")
      }
      if(input$cbiMothodSelect == "TE"){
        return("cbi_te_png.zip")
      }
    },
    content = function(file) {
      if(input$cbiMothodSelect == "input"){
        files_to_zip <- c(
          paste0(TEFileuserDir, "/codonP3.png"),
          paste0(TEFileuserDir, "/codonP4.png")
        )
        if (all(file.exists(files_to_zip))) {
          zip::zipr(file, files_to_zip)
        } else {
          return(NULL)
        }
      }
      if(input$cbiMothodSelect == "TE"){}
      files_to_zip <- c(
        paste0(TEFileuserDir, "/codonP5.png"),
        paste0(TEFileuserDir, "/codonP6.png")
      )
      if (all(file.exists(files_to_zip))) {
        zip::zipr(file, files_to_zip)
      } else {
        return(NULL)
      }
    }
  )
  output$ExportBtn41 <- downloadHandler(
    filename = function() {
      return("codon_input_pdf.zip")
    },
    content = function(file) {
      filepath <- list.files(TEFileuserDir, pattern = "perc\\.pdf$", full.names = TRUE)
      if (length(filepath) > 0) {
        zip::zipr(file, filepath)
      } else {
        return(NULL)
      }
    }
  )
  output$ExportBtn42 <- downloadHandler(
    filename = function() {
      return("codon_input_png.zip")
    },
    content = function(file) {
      filepath <- list.files(TEFileuserDir, pattern = "perc\\.png$", full.names = TRUE)
      if (length(filepath) > 0) {
        zip::zipr(file, filepath)
      } else {
        return(NULL)
      }
    }
  )
  output$ExportBtn43 <- downloadHandler(
    filename = function() {
      if(input$percPer1kUpDownSelect == "Up"){
        return("perc_per1k_up_pdf.zip")
      }
      if(input$percPer1kUpDownSelect == "Down"){
        return("perc_per1k_down_pdf.zip")
      }
      if(input$percPer1kUpDownSelect == "Up and Down"){
        return("perc_per1k_up_down_pdf.zip")
      }
    },
    content = function(file) {
      if(input$percPer1kUpDownSelect == "Up"){
        files_to_zip <- c(
          paste0(TEFileuserDir, "/scatter_plot_logFC_perc_up.pdf"),
          paste0(TEFileuserDir, "/scatter_plot_logFC_per1k_up.pdf"),
          paste0(TEFileuserDir, "/scatter_plot_zscore_per1k_up.pdf")
        )
        if (all(file.exists(files_to_zip))) {
          zip::zipr(file, files_to_zip)
        } else {
          return(NULL)
        }
      }
      if(input$percPer1kUpDownSelect == "Down"){
        files_to_zip <- c(
          paste0(TEFileuserDir, "/scatter_plot_logFC_perc_down.pdf"),
          paste0(TEFileuserDir, "/scatter_plot_logFC_per1k_down.pdf"),
          paste0(TEFileuserDir, "/scatter_plot_zscore_per1k_down.pdf")
        )
        if (all(file.exists(files_to_zip))) {
          zip::zipr(file, files_to_zip)
        } else {
          return(NULL)
        }
      }
      if(input$percPer1kUpDownSelect == "Up and Down"){
        files_to_zip <- c(
          paste0(TEFileuserDir, "/scatter_plot_logFC_per1k.pdf"),
          paste0(TEFileuserDir, "/scatter_plot_zscore_per1k.pdf"),
        )
        if (all(file.exists(files_to_zip))) {
          zip::zipr(file, files_to_zip)
        } else {
          return(NULL)
        }
      }

    }
  )
  output$ExportBtn44 <- downloadHandler(
    filename = function() {
      if(input$percPer1kUpDownSelect == "Up"){
        return("perc_per1k_up_png.zip")
      }
      if(input$percPer1kUpDownSelect == "Down"){
        return("perc_per1k_down_png.zip")
      }
      if(input$percPer1kUpDownSelect == "Up and Down"){
        return("perc_per1k_up_down_png.zip")
      }
    },
    content = function(file) {
      if(input$percPer1kUpDownSelect == "Up"){
        files_to_zip <- c(
          paste0(TEFileuserDir, "/scatter_plot_logFC_perc_up.png"),
          paste0(TEFileuserDir, "/scatter_plot_logFC_per1k_up.png"),
          paste0(TEFileuserDir, "/scatter_plot_zscore_per1k_up.png")
        )
        if (all(file.exists(files_to_zip))) {
          zip::zipr(file, files_to_zip)
        } else {
          return(NULL)
        }
      }
      if(input$percPer1kUpDownSelect == "Down"){
        files_to_zip <- c(
          paste0(TEFileuserDir, "/scatter_plot_logFC_perc_down.png"),
          paste0(TEFileuserDir, "/scatter_plot_logFC_per1k_down.png"),
          paste0(TEFileuserDir, "/scatter_plot_zscore_per1k_down.png")
        )
        if (all(file.exists(files_to_zip))) {
          zip::zipr(file, files_to_zip)
        } else {
          return(NULL)
        }
      }
      if(input$percPer1kUpDownSelect == "Up and Down"){
        files_to_zip <- c(
          paste0(TEFileuserDir, "/scatter_plot_logFC_per1k.png"),
          paste0(TEFileuserDir, "/scatter_plot_zscore_per1k.png"),
        )
        if (all(file.exists(files_to_zip))) {
          zip::zipr(file, files_to_zip)
        } else {
          return(NULL)
        }
      }

    }
  )
  output$ExportBtn45 <- downloadHandler(
    filename = function() {
      return("codon_frequency.pdf")
    },
    content = function(file) {
      filepath <- paste0(TEFileuserDir, "/perc.boxplot.pdf")
      if (file.exists(filepath)) {
        file.copy(filepath, file)
      } else {
        return(NULL)
      }
    }
  )
  output$ExportBtn46 <- downloadHandler(
    filename = function() {
      return("codon_frequency.png")
    },
    content = function(file) {
      filepath <- paste0(TEFileuserDir, "/perc.boxplot.png")
      if (file.exists(filepath)) {
        file.copy(filepath, file)
      } else {
        return(NULL)
      }
    }
  )
  output$ExportBtn47 <- downloadHandler(
    filename = function() {
      return(paste0(input$dendrogramDistMethod, ".", input$dendrogramHclustMethod, ".dendrogram.pdf"))
    },
    content = function(file) {
      filepath <- list.files(TEFileuserDir, pattern = "\\.dendrogram\\.pdf$", full.names = TRUE)
      if (file.exists(filepath)) {
        file.copy(filepath, file)
      } else {
        return(NULL)
      }
    }
  )
  output$ExportBtn48 <- downloadHandler(
    filename = function() {
      return(paste0(input$dendrogramDistMethod, ".", input$dendrogramHclustMethod, ".dendrogram.png"))
    },
    content = function(file) {
      filepath <- list.files(TEFileuserDir, pattern = "\\.dendrogram\\.png$", full.names = TRUE)
      if (file.exists(filepath)) {
        file.copy(filepath, file)
      } else {
        return(NULL)
      }
    }
  )
  output$ExportBtn49 <- downloadHandler(
    filename = function() {
      if(input$distributionUpDownSelect == "Up"){
        return("distribution_up_pdf.zip")
      }
      if(input$distributionUpDownSelect == "Down"){
        return("distribution_down_pdf.zip")
      }
    },
    content = function(file) {
      if(input$distributionUpDownSelect == "Up"){
        files_to_zip <- c(
          paste0(TEFileuserDir, "/upratio_distribution_Up.pdf"),
          paste0(TEFileuserDir, "/dnratio_distribution_Up.pdf")
        )
        if (all(file.exists(files_to_zip))) {
          zip::zipr(file, files_to_zip)
        } else {
          return(NULL)
        }
      }
      if(input$distributionUpDownSelect == "Down"){}
      files_to_zip <- c(
        paste0(TEFileuserDir, "/upratio_distribution_Down.pdf"),
        paste0(TEFileuserDir, "/dnratio_distribution_Down.pdf")
      )
      if (all(file.exists(files_to_zip))) {
        zip::zipr(file, files_to_zip)
      } else {
        return(NULL)
      }
    }
  )
  output$ExportBtn50 <- downloadHandler(
    filename = function() {
      if(input$distributionUpDownSelect == "Up"){
        return("distribution_up_png.zip")
      }
      if(input$distributionUpDownSelect == "Down"){
        return("distribution_down_png.zip")
      }
    },
    content = function(file) {
      if(input$distributionUpDownSelect == "Up"){
        files_to_zip <- c(
          paste0(TEFileuserDir, "/upratio_distribution_Up.png"),
          paste0(TEFileuserDir, "/dnratio_distribution_Up.png")
        )
        if (all(file.exists(files_to_zip))) {
          zip::zipr(file, files_to_zip)
        } else {
          return(NULL)
        }
      }
      if(input$distributionUpDownSelect == "Down"){}
      files_to_zip <- c(
        paste0(TEFileuserDir, "/upratio_distribution_Down.png"),
        paste0(TEFileuserDir, "/dnratio_distribution_Down.png")
      )
      if (all(file.exists(files_to_zip))) {
        zip::zipr(file, files_to_zip)
      } else {
        return(NULL)
      }
    }
  )
  output$ExportBtn51 <- downloadHandler(
    filename = function() {
      if(input$codonZscoreUpDownSelect == "Up"){
        return("codon_updown_zscore_up.pdf")
      }
      if(input$codonZscoreUpDownSelect == "Down"){
        return("codon_updown_zscore_down.pdf")
      }
    },
    content = function(file) {
      if(input$codonZscoreUpDownSelect == "Up"){
        filepath <- paste0(TEFileuserDir,"/codon_updown_zscore_Up.pdf")
        if (file.exists(filepath)) {
          file.copy(filepath, file)
        } else {
          return(NULL)
        }
      }
      if(input$codonZscoreUpDownSelect == "Down"){
        filepath <- paste0(TEFileuserDir,"/codon_updown_zscore_Down.pdf")
        if (file.exists(filepath)) {
          file.copy(filepath, file)
        } else {
          return(NULL)
        }
      }

    }
  )
  output$ExportBtn52 <- downloadHandler(
    filename = function() {
      if(input$codonZscoreUpDownSelect == "Up"){
        return("codon_updown_zscore_up.png")
      }
      if(input$codonZscoreUpDownSelect == "Down"){
        return("codon_updown_zscore_down.png")
      }
    },
    content = function(file) {
      if(input$codonZscoreUpDownSelect == "Up"){
        filepath <- paste0(TEFileuserDir,"/codon_updown_zscore_Up.png")
        if (file.exists(filepath)) {
          file.copy(filepath, file)
        } else {
          return(NULL)
        }
      }
      if(input$codonZscoreUpDownSelect == "Down"){
        filepath <- paste0(TEFileuserDir,"/codon_updown_zscore_Down.png")
        if (file.exists(filepath)) {
          file.copy(filepath, file)
        } else {
          return(NULL)
        }
      }

    }
  )
  output$ExportBtn53 <- downloadHandler(
    filename = function() {
      return("codon_per1k_fcAndPvalue.pdf")
    },
    content = function(file) {
      filepath <- paste0(TEFileuserDir,"/codon_per1k_fcAndPvalue.pdf")
      if (file.exists(filepath)) {
        file.copy(filepath, file)
      } else {
        return(NULL)
      }
    }
  )
  output$ExportBtn54 <- downloadHandler(
    filename = function() {
      return("codon_per1k_fcAndPvalue.png")
    },
    content = function(file) {
      filepath <- paste0(TEFileuserDir,"/codon_per1k_fcAndPvalue.png")
      if (file.exists(filepath)) {
        file.copy(filepath, file)
      } else {
        return(NULL)
      }
    }
  )
  output$ExportBtn55 <- downloadHandler(
    filename = function() {
      if(input$per1kZscoreUpDownSelect == "Up"){
        return("per1k_zscore_up_pdf.zip")
      }
      if(input$per1kZscoreUpDownSelect == "Down"){
        return("per1k_zscore_down_pdf.zip")
      }
    },
    content = function(file) {
      if(input$codoNatioUpDownSelect == "Up"){
        files_to_zip <- c(
          paste0(TEFileuserDir, "/per1k_zscore_Up_single.pdf"),
          paste0(TEFileuserDir, "/per1k_zscore_Up_double.pdf"),
          paste0(TEFileuserDir, "/per1k_zscore_Up_triple.pdf")
        )
        if (all(file.exists(files_to_zip))) {
          zip::zipr(file, files_to_zip)
        } else {
          return(NULL)
        }
      }
      if(input$codoNatioUpDownSelect == "Down"){
        files_to_zip <- c(
          paste0(TEFileuserDir, "/per1k_zscore_Down_single.pdf"),
          paste0(TEFileuserDir, "/per1k_zscore_Down_double.pdf"),
          paste0(TEFileuserDir, "/per1k_zscore_Down_triple.pdf")
        )
        if (all(file.exists(files_to_zip))) {
          zip::zipr(file, files_to_zip)
        } else {
          return(NULL)
        }
      }

    }
  )
  output$ExportBtn56 <- downloadHandler(
    filename = function() {
      if(input$per1kZscoreUpDownSelect == "Up"){
        return("per1k_zscore_up_png.zip")
      }
      if(input$per1kZscoreUpDownSelect == "Down"){
        return("per1k_zscore_down_png.zip")
      }
    },
    content = function(file) {
      if(input$codoNatioUpDownSelect == "Up"){
        files_to_zip <- c(
          paste0(TEFileuserDir, "/per1k_zscore_Up_single.png"),
          paste0(TEFileuserDir, "/per1k_zscore_Up_double.png"),
          paste0(TEFileuserDir, "/per1k_zscore_Up_triple.png")
        )
        if (all(file.exists(files_to_zip))) {
          zip::zipr(file, files_to_zip)
        } else {
          return(NULL)
        }
      }
      if(input$codoNatioUpDownSelect == "Down"){
        files_to_zip <- c(
          paste0(TEFileuserDir, "/per1k_zscore_Down_single.png"),
          paste0(TEFileuserDir, "/per1k_zscore_Down_double.png"),
          paste0(TEFileuserDir, "/per1k_zscore_Down_triple.png")
        )
        if (all(file.exists(files_to_zip))) {
          zip::zipr(file, files_to_zip)
        } else {
          return(NULL)
        }
      }

    }
  )
  output$ExportBtn57 <- downloadHandler(
    filename = function() {
      if(input$enrichedDisplaySelect == "All"){
        return("enriched_all_pdf.zip")
      }
      if(input$enrichedDisplaySelect == "Obj"){
        return("enriched_codon_pdf.zip")
      }
    },
    content = function(file) {
      if(input$enrichedDisplaySelect == "All"){
        files_to_zip <- c(
          paste0(TEFileuserDir, "/enriched_all_single.pdf"),
          paste0(TEFileuserDir, "/enriched_all_double.pdf"),
          paste0(TEFileuserDir, "/enriched_all_triple.pdf")
        )
        if (all(file.exists(files_to_zip))) {
          zip::zipr(file, files_to_zip)
        } else {
          return(NULL)
        }
      }
      if(input$enrichedDisplaySelect == "Obj"){
        files_to_zip <- c(
          paste0(TEFileuserDir, "/enriched_obj_single.pdf"),
          paste0(TEFileuserDir, "/enriched_obj_double.pdf"),
          paste0(TEFileuserDir, "/enriched_obj_triple.pdf")
        )
        if (all(file.exists(files_to_zip))) {
          zip::zipr(file, files_to_zip)
        } else {
          return(NULL)
        }
      }

    }
  )
  output$ExportBtn58 <- downloadHandler(
    filename = function() {
      if(input$enrichedDisplaySelect == "All"){
        return("enriched_all_png.zip")
      }
      if(input$enrichedDisplaySelect == "Obj"){
        return("enriched_codon_png.zip")
      }
    },
    content = function(file) {
      if(input$enrichedDisplaySelect == "All"){
        files_to_zip <- c(
          paste0(TEFileuserDir, "/enriched_all_single.png"),
          paste0(TEFileuserDir, "/enriched_all_double.png"),
          paste0(TEFileuserDir, "/enriched_all_triple.png")
        )
        if (all(file.exists(files_to_zip))) {
          zip::zipr(file, files_to_zip)
        } else {
          return(NULL)
        }
      }
      if(input$enrichedDisplaySelect == "Obj"){
        files_to_zip <- c(
          paste0(TEFileuserDir, "/enriched_obj_single.png"),
          paste0(TEFileuserDir, "/enriched_obj_double.png"),
          paste0(TEFileuserDir, "/enriched_obj_triple.png")
        )
        if (all(file.exists(files_to_zip))) {
          zip::zipr(file, files_to_zip)
        } else {
          return(NULL)
        }
      }

    }
  )
  output$ExportBtn59 <- downloadHandler(
    filename = function() {
      return("codon_zscore_heatmap.pdf")
    },
    content = function(file) {
      filepath <- paste0(TEFileuserDir,"/codon_zscore_heatmap.pdf")
      if (file.exists(filepath)) {
        file.copy(filepath, file)
      } else {
        return(NULL)
      }
    }
  )
  output$ExportBtn60 <- downloadHandler(
    filename = function() {
      return("codon_zscore_heatmap.png")
    },
    content = function(file) {
      filepath <- paste0(TEFileuserDir,"/codon_zscore_heatmap.png")
      if (file.exists(filepath)) {
        file.copy(filepath, file)
      } else {
        return(NULL)
      }
    }
  )

})











