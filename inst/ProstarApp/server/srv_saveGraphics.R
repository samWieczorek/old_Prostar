

output$test <- renderUI({
    
    shinyjs::disable("downloadReport")
})



######---------------------------------------------------------------------
observeEvent(input$generateReport,{
    if (is.null(input$generateReport)) {return (NULL)}
    input$chooseDatasetToExport
    rv$dataset
    input$whichGO2Save
    if (is.null(input$chooseDatasetToExport)){return(NULL)}
    
    
    withProgress(message = 'Calculation in progress',
                 detail = '', value = 0, {
                     
                     
                     ## Init the Rmd file for the report
                     src <- normalizePath('Rmd_sources/report.Rmd')
                     filename <- paste(tempdir(), sessionID, 'report.Rmd',sep="/")
                     file.copy(src, filename, overwrite = TRUE)
                     
                     
                     for (iDat in input$chooseDatasetToExport){
                         rv$iDat <- iDat
                         dname <- unlist(strsplit(iDat, " - "))[1]
                         type <- unlist(strsplit(iDat, " - "))[2]
                         incProgress(1/length(input$chooseDatasetToExport), detail = paste("Computing plot for", iDat))
                         
                         switch(dname, 
                                Original={
                                    createPNG_DescriptiveStatistics()
                                    txt2Rmd <- readLines("Rmd_sources/descriptiveStats_Rmd.Rmd")
                                    filename <- paste(tempdir(), sessionID, 'report.Rmd',sep="/")
                                    write(txt2Rmd, file = filename,append = TRUE, sep = "\n")
                                },
                                Filtered={
                                    #createPNG_Filtering()
                                    txt2Rmd <- readLines("Rmd_sources/filtering_Rmd.Rmd")
                                    filename <- paste(tempdir(), sessionID, 'report.Rmd',sep="/")
                                    write(txt2Rmd, file = filename,append = TRUE, sep = "\n") 
                                },
                                Normalized={
                                    createPNG_Normalization()
                                    txt2Rmd <- readLines("Rmd_sources/normalization_Rmd.Rmd")
                                    filename <- paste(tempdir(), sessionID, 'report.Rmd',sep="/")
                                    write(txt2Rmd, file = filename,append = TRUE, sep = "\n")
                                },
                                Imputed={
                                    createPNG_Imputation()
                                    txt2Rmd <- readLines("Rmd_sources/imputation_Rmd.Rmd")
                                    filename <- paste(tempdir(), sessionID, 'report.Rmd',sep="/")
                                    write(txt2Rmd, file = filename,append = TRUE, sep = "\n")
                                },
                                Aggregated={
                                    createPNG_Aggregation()
                                    txt2Rmd <- readLines("Rmd_sources/aggregation_Rmd.Rmd")
                                    filename <- paste(tempdir(), sessionID, 'report.Rmd',sep="/")
                                    write(txt2Rmd, file = filename,append = TRUE, sep = "\n")
                                },
                                DiffAnalysis={
                                    createPNG_DifferentialAnalysis()
                                    txt2Rmd <- readLines("Rmd_sources/anaDiff_Rmd.Rmd")
                                    filename <- paste(tempdir(), sessionID, 'report.Rmd',sep="/")
                                    write(txt2Rmd, file = filename,append = TRUE, sep = "\n")
                                },
                                GOAnalysis={
                                    switch(input$whichGO2Save,
                                           Both={
                                               createPNG_Enrichment()
                                               createPNG_GroupGO()
                                               txt2Rmd <- readLines("Rmd_sources/GO_Classification_Rmd.Rmd")
                                               filename <- paste(tempdir(), sessionID, 'report.Rmd',sep="/")
                                               write(txt2Rmd, file = filename,append = TRUE, sep = "\n")
                                               
                                               txt2Rmd <- readLines("Rmd_sources/GO_Enrichment_Rmd.Rmd")
                                               filename <- paste(tempdir(), sessionID, 'report.Rmd',sep="/")
                                               write(txt2Rmd, file = filename,append = TRUE, sep = "\n")
                                           },
                                           Classification={
                                               createPNG_GroupGO()
                                               txt2Rmd <- readLines("Rmd_sources/GO_Classification_Rmd.Rmd")
                                               filename <- paste(tempdir(), sessionID, 'report.Rmd',sep="/")
                                               write(txt2Rmd, file = filename,append = TRUE, sep = "\n")
                                           },
                                           Enrichment={
                                               createPNG_Enrichment()
                                               txt2Rmd <- readLines("Rmd_sources/GO_Enrichment_Rmd.Rmd")
                                               filename <- paste(tempdir(), sessionID, 'report.Rmd',sep="/")
                                               write(txt2Rmd, file = filename,append = TRUE, sep = "\n")
                                           }
                                    )
                                }
                         )
                         
                     }
                 })
    shinyjs::enable("downloadReport")
    
})



func_density <- function(obj, tempplot){
    if (is.null(tempplot)) {
        tempplot <- wrapper.densityPlotD_HC(obj)
    }
    createPNGFromWidget(tempplot, "temp_density.html", gGraphicsFilenames$densityPlot)
}

func_boxplot <- function(obj, leg){
    plotPNG(function(){if (!is.null(leg)) {
        wrapper.boxPlotD(obj,  leg)
    } else {
        wrapper.boxPlotD(obj)}
    },
    filename=paste(tempdir(), sessionID, gGraphicsFilenames$boxplot, sep="/"),
    width = pngWidth,
    height=pngHeight,
    res=resolution)
}


func_violinPlot <- function(obj, leg){
     plotPNG(function(){if (!is.null(leg)) {
        wrapper.violinPlotD(obj,  leg)
    } else {
        wrapper.violinPlotD(obj)}
    },
    filename=paste(tempdir(), sessionID, gGraphicsFilenames$violinplot, sep="/"),
    width = pngWidth,
    height=pngHeight,
    res=resolution)
}

func_varDist <- function(obj, plot){
    if (is.null(plot)) {
        tempplot <- wrapper.CVDistD_HC(obj)
    } else{ 
        tempplot <- plot
    }
    createPNGFromWidget(tempplot, "temp_varDist.html", gGraphicsFilenames$varDist)
}

func_corrMatrix <- function(obj, plot){
     if (is.null(plot)) {
        tempplot <- wrapper.corrMatrixD_HC(obj) 
        }else {
        tempplot <- plot
        }
    createPNGFromWidget(tempplot, "temp_corrMatrix.html", gGraphicsFilenames$corrMatrix)
}

func_mvHisto_1 <- function(obj, plot){
     if (is.null(plot)) {
        tempplot <- wrapper.mvPerLinesHistoPerCondition_HC(obj)
     } else{ tempplot <- plot
     }
    createPNGFromWidget(tempplot, "temp_mvHisto_1.html", gGraphicsFilenames$histo_missvalues_per_lines_per_conditions_DS)
}


func_mvHisto_2 <- function(obj, plot){
    
     if (is.null(plot)) {
        tempplot <- wrapper.mvPerLinesHisto_HC(obj)
        } else{
        tempplot <- plot
        }
    createPNGFromWidget(tempplot, "temp_mvHisto_2.html", gGraphicsFilenames$histo_missvalues_per_lines_DS)
}

func_mvHisto_3 <- function(obj, plot){
    if (is.null(plot)) {
        tempplot <- wrapper.mvHisto_HC(obj)
    } else{ tempplot <- plot}
    createPNGFromWidget(tempplot, "temp_mvHisto_3.html", gGraphicsFilenames$histoMV_Image_DS)
}


###-------------------------------------------------
func_heatmap <- function(obj, distance, link){
    print("In heatmpa function")
    plotPNG(function(){
        if (is.null (distance) && is.null(link)) {
            wrapper.heatmapD(obj)
        } else {
            wrapper.heatmapD(obj,distance, link, TRUE)
        }},
        filename=paste(tempdir(), sessionID, gGraphicsFilenames$heatmap, sep="/"),
        width = 1200,
        height=800,
        res=150)
}


createPNGFromWidget <- function(tempplot, tmp_filename, png_filename){
    htmlwidgets::saveWidget(widget = tempplot, file = paste(tempdir(), sessionID, tmp_filename, sep="/"))
    webshot::webshot(url = paste(tempdir(), sessionID, tmp_filename, sep="/"),
                     file = paste(tempdir(), sessionID, png_filename, sep="/"),
                     delay = 3,
                     zoom = zoomWebshot)
}


###--------------------------------------------------------------------------
createPNG_DescriptiveStatistics <- reactive({
    
    obj <- rv$dataset[[rv$iDat]]
    
    list_f <- list(func_corrMatrix,
                   func_varDist,
                   func_violinPlot,
                   func_boxplot,
                   func_density,
                   func_mvHisto_3,
                   func_mvHisto_2,
                   func_mvHisto_1)             
    
    list_params <- list(list(obj,rv$tempplot$corrMatrix),
                        list(obj,rv$tempplot$varDist),
                        list(obj,rv$PlotParams$legDS_Violinplot),
                        list(obj,rv$PlotParams$legDS),
                        list(obj,rv$tempplot$Density),
                        list(obj,rv$tempplot$mvHisto_HC),
                        list(obj,rv$tempplot$mvHisto_perLines_HC),
                        list(obj,rv$tempplot$histo_missvalues_per_lines_per_conditions))
    
 
    
    if (nrow(obj) < limitHeatmap){
        list_f <- append(list_f,list(func_heatmap) ) 
        list_params <- c(list_params, list(list(obj,rv$PlotParams$HeatmapDistance,rv$PlotParams$HeatmapLinkage)))
    }
    
   
    
     require(doParallel)
    #registerDoParallel(detectCores()-1)  
     #foreach(i=1:length(list_f)) %dopar% {
    foreach(i=1:length(list_f)) %do% {
        do.call(list_f[[i]],list_params[[i]])
    }
    
    
    
})





###--------------------------------------------------------------------------
# createPNG_Filtering <- reactive({
#     obj <- rv$dataset[[rv$iDat]]
#     
#     if (!is.null(rv$nbContaminantsDeleted) && !is.null(rv$nbReverseDeleted)) {
#         tempplot <-proportionConRev_HC(rv$nbContaminantsDeleted, rv$nbReverseDeleted, nrow(obj))
#         createPNGFromWidget(tempplot, "tempplot_proportionConRev_HC.html", gGraphicsFilenames$propContRev)
#     }
# })




###--------------------------------------------------------------------------
createPNG_Normalization <- reactive({
    obj1 <- rv$dataset[[(which(names(rv$dataset)==rv$iDat) - 1)]]
    obj2 <- rv$dataset[[rv$iDat]]
    
    
    plotPNG(function(){wrapper.compareNormalizationD(obj1, obj2)}, 
            filename=paste(tempdir(), sessionID, gGraphicsFilenames$compareNorm, sep="/"), 
            width = 1200, 
            height=800,
            res=150)
    
    
    if (is.null(rv$tempplot$Density)) {
        tempplot <- wrapper.densityPlotD_HC(obj2)}
    else{ tempplot <- rv$tempplot$Density}
    createPNGFromWidget(tempplot, "tempplot_densityPlotAfterNorm.html", gGraphicsFilenames$densityPlotAfterNorm)
   
    
    
    plotPNG(function(){if (!is.null(rv$PlotParams$legDS)) {
        wrapper.boxPlotD(obj2,  rv$PlotParams$legDS)
    } else {
        wrapper.boxPlotD(obj2)}
    },
    filename=paste(tempdir(), sessionID, gGraphicsFilenames$boxplotAfterNorm, sep="/"),
    width = 1200,
    height=800,
    res=150)
    
    
    
    
})


###--------------------------------------------------------------------------
createPNG_Imputation <- reactive({
    obj <- rv$dataset[[(which(names(rv$dataset)==rv$iDat) - 1)]]
    obj <- rv$dataset[[rv$iDat]]
    
    tempplot <- wrapper.hc_mvTypePlot2(obj)
    createPNGFromWidget(tempplot, "tempplot_MVtypePlot.html", gGraphicsFilenames$MVtypePlot)
    
    
})

###--------------------------------------------------------------------------
createPNG_Aggregation <- reactive({
    obj <- rv$dataset[[rv$iDat]]
    
    
})

###--------------------------------------------------------------------------
createPNG_DifferentialAnalysis <- reactive({
    obj <- rv$dataset[[rv$iDat]]
 
    allCompNames <- obj@experimentData@other$Params[["anaDiff"]]$AllPairwiseCompNames
    data <- as.data.frame(fData(obj)[,allCompNames$FC])
    colnames(data) <- allCompNames$FC
    th_logFC <-  obj@experimentData@other$Params[["anaDiff"]]$th_logFC
    th_pval <-  obj@experimentData@other$Params[["anaDiff"]]$th_pval
    cond <- c(obj@experimentData@other$Params[["anaDiff"]]$condition1,
              obj@experimentData@other$Params[["anaDiff"]]$condition2)

    tempplot <- hc_FC_DensityPlot(data,th_logFC)
    createPNGFromWidget(tempplot, "tempplot_logFC_Distribution.html", gGraphicsFilenames$logFC_Distribution)
    
    df <- data.frame(x=fData(obj)$FC,
                     y = -log10(fData(obj)$P_Value))
    
    
    ## Plot for distribution of FC
    tempplot <- diffAnaVolcanoplot_rCharts(df,
                                           threshold_logFC = th_logFC,
                                           threshold_pVal = th_pval,
                                           conditions = cond)
    createPNGFromWidget(tempplot, "tempplot_Volcano.html", gGraphicsFilenames$volcanoPlot_3)
    
    # plotPNG(function(){calibrationPlot()},
    #         filename=paste(tempdir(), sessionID, gGraphicsFilenames$calibrationPlot, sep="/"),
    #         width = pngWidth,
    #         height=pngHeight,
    #         res=resolution)
    
    
    plotPNG(function(){
        t <- fData(obj)$P_Value
        t <- t[which(abs(fData(obj)$FC) >= th_logFC)]
        wrapperCalibrationPlot(t, "ALL")},
        filename=paste(tempdir(), sessionID, gGraphicsFilenames$calibrationPlotAll, sep="/"),
        width = 1200,
        height=800,
        res=150)
})


###--------------------------------------------------------------------------

createPNG_Enrichment <- reactive({
    obj <- rv$dataset[[rv$iDat]]
    
    
    tempplot <- barplotEnrichGO_HC(rv$enrichGO_data)
    createPNGFromWidget(tempplot, "tempplot_barplotEnrichGO.html", gGraphicsFilenames$GOEnrichBarplot)
    
    
    tempplot <- scatterplotEnrichGO_HC(rv$enrichGO_data)
    createPNGFromWidget(tempplot, "tempplot_scatterplotEnrichGO.html", gGraphicsFilenames$GOEnrichDotplot)
    
    
})


###--------------------------------------------------------------------------
createPNG_GroupGO <- reactive({
    
    l <- length(rv$groupGO_data)
    pngNames <- c(gGraphicsFilenames$GOClassification_img1,
                  gGraphicsFilenames$GOClassification_img2,
                  gGraphicsFilenames$GOClassification_img3)
    
    require(doParallel)
    registerDoParallel(detectCores()-1 )
    foreach(i=1:l) %dopar% {
        tempplot <- barplotGroupGO_HC(rv$groupGO_data[[i]]$ggo_res, 
                                      title = paste("Groups at level ",  rv$groupGO_data[[i]]$level))
        file <- paste("tempplot_barplotGroupGO", i, ".html", sep="")
        createPNGFromWidget(tempplot, file, pngNames[i])
   
    }
    
})



######-----------------------------------------------------------------
output$downloadReport <- downloadHandler(
    input$reportFilename,
    filename = function() {
        paste(input$reportFilename, sep = '.', switch(
            input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
        ))
    },
    
    content = function(file) {
        filename <- paste(tempdir(), sessionID, 'report.Rmd',sep="/")
        print(tempdir())
        library(rmarkdown)
        #paramRmd <- list(current.obj=rv$current.obj)
        out <- render(filename, 
                      output_file = file,
                      params = list(listOverview = list(pourcentageNA = rv$pourcentageNA,
                                                        nEntities = dim(Biobase::exprs(rv$current.obj))[1],
                                                        typeOfData = rv$typeOfDataset,
                                                        nNAlines = rv$nb.empty.lines,
                                                        nSamples = dim(Biobase::exprs(rv$current.obj))[2]),
                                    
                                    listFiltering= rv$current.obj@experimentData@other$Params[["Filtering"]],
                                    listNormalization = rv$current.obj@experimentData@other$Params[["Norm"]],
                                    listImputation = rv$current.obj@experimentData@other$Params[["Imputation"]],
                                    listAggregation = list(method = input$aggregationMethod,
                                                           df=rv$AggregProtStats),
                                    
                                    listAnaDiff = rv$current.obj@experimentData@other$Params[["anaDiff"]],
                                    
                                    listGOClassifAnalysis = list(ontology = input$Ontology, 
                                                                 organism = input$Organism,
                                                                 universe = input$universe,
                                                                 group1 = (length(rv$groupGO_data)>=1),
                                                                 group2 = (length(rv$groupGO_data)>=2),
                                                                 group3 = (length(rv$groupGO_data)==3),
                                                                 df1 = if (length(rv$groupGO_data)>=1){
                                                                     dat <- rv$groupGO_data[[1]]$ggo_res@result
                                                                     dat <- dat[order(dat$Count, decreasing=TRUE),]
                                                                     dat[1:5,c("Description", "Count", "GeneRatio")]},
                                                                 df2 = if (length(rv$groupGO_data)>=2){
                                                                     dat <- rv$groupGO_data[[2]]$ggo_res@result
                                                                     dat <- dat[order(dat$Count, decreasing=TRUE),]
                                                                     dat[1:5,c("Description", "Count", "GeneRatio")]},
                                                                 df3 = if (length(rv$groupGO_data)==3){
                                                                     dat <- rv$groupGO_data[[3]]$ggo_res@result
                                                                     dat <- dat[order(dat$Count, decreasing=TRUE),]
                                                                     dat[1:5,c("Description", "Count", "GeneRatio")]}
                                    ),
                                    listGOEnrichmentAnalysis = list(ontology = input$Ontology, 
                                                                    organism = input$Organism,
                                                                    universe = input$universe,
                                                                    pval = input$pvalueCutoff,
                                                                    df = if (!is.null(rv$enrichGO_data)){
                                                                        dat <- rv$enrichGO_data@result
                                                                        dat <- dat[order(dat$pvalue, decreasing=FALSE),]
                                                                        dat[seq(1:10),c("Description", "GeneRatio", "p.adjust", "Count")]}
                                    )
                      ),
                      switch(
                          input$format,
                          PDF = pdf_document(), 
                          HTML = html_document(), 
                          Word = word_document()
                      ))
        
    }
)

