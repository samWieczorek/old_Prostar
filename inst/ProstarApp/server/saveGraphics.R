
 
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
                   createPNG_Filtering()
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
               DiffAnalysis.Limma={
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



 func_density <- reactive({
    
   obj <- rv$dataset[[rv$iDat]]
     if (is.null(rv$tempplot$Density)) {
         tempplot <- wrapper.densityPlotD_HC(obj)}
     else{ tempplot <- rv$tempplot$Density}
     htmlwidgets::saveWidget(widget = tempplot, file = paste(tempdir(), sessionID, "tempplot.html", sep="/"))
     webshot::webshot(url = paste(tempdir(), sessionID, "tempplot.html", sep="/"),
                      file = paste(tempdir(), sessionID, gGraphicsFilenames$densityPlot, sep="/"),
                      delay = 3,
                      zoom = zoomWebshot)
 })
 
 func_boxplot <- reactive({
 
   obj <- rv$dataset[[rv$iDat]]
     plotPNG(function(){if (!is.null(rv$PlotParams$legDS)) {
         wrapper.boxPlotD(obj,  rv$PlotParams$legDS)
     } else {
         wrapper.boxPlotD(obj)}
     },
     filename=paste(tempdir(), sessionID, gGraphicsFilenames$boxplot, sep="/"),
     width = pngWidth,
     height=pngHeight,
     res=resolution)
 })
 
 
 func_violinPlot <- reactive({

   obj <- rv$dataset[[rv$iDat]]
     plotPNG(function(){if (!is.null(rv$PlotParams$legDS_Violinplot)) {
         wrapper.violinPlotD(obj,  rv$PlotParams$legDS_Violinplot)
     } else {
         wrapper.violinPlotD(obj)}
     },
     filename=paste(tempdir(), sessionID, gGraphicsFilenames$violinplot, sep="/"),
     width = pngWidth,
     height=pngHeight,
     res=resolution)
 })
 
 func_varDist <- reactive({

   obj <- rv$dataset[[rv$iDat]]
     if (is.null(rv$tempplot$varDist)) {tempplot <- wrapper.CVDistD_HC(obj)}
     else{ tempplot <- rv$tempplot$varDist}
     htmlwidgets::saveWidget(widget = tempplot, file = paste(tempdir(), sessionID, "tempplot.html", sep="/"))
     webshot::webshot(url = paste(tempdir(), sessionID, "tempplot.html", sep="/"),
                      file = paste(tempdir(), sessionID, gGraphicsFilenames$varDist, sep="/"),
                      delay = 3,
                      zoom = zoomWebshot)
 })
 
 func_corrMatrix <- reactive({

   obj <- rv$dataset[[rv$iDat]]
     if (is.null(rv$tempplot$corrMatrix)) {
         tempplot <- wrapper.corrMatrixD_HC(obj) }
     else {
         tempplot <- rv$tempplot$corrMatrix
     }
     htmlwidgets::saveWidget(widget = tempplot, file = paste(tempdir(), sessionID, "tempplot.html", sep="/"))
     webshot::webshot(url = paste(tempdir(), sessionID, "tempplot.html", sep="/"),
                      file = paste(tempdir(), sessionID, gGraphicsFilenames$corrMatrix, sep="/"),
                      delay = 3,
                      zoom = zoomWebshot)
 })
 
 func_mvHisto_1 <- reactive({

   obj <- rv$dataset[[rv$iDat]]
     ##last plot of descriptive statistics
     if (is.null(rv$tempplot$mvHisto_perLines_HC)) {
         tempplot <- wrapper.mvPerLinesHistoPerCondition_HC(obj)}
     else{ tempplot <- rv$tempplot$histo_missvalues_per_lines_per_conditions}
     htmlwidgets::saveWidget(widget = tempplot, file = paste(tempdir(), sessionID, "tempplot.html", sep="/"))
     webshot::webshot(url = paste(tempdir(), sessionID, "tempplot.html", sep="/"),
                      file = paste(tempdir(), sessionID, gGraphicsFilenames$histo_missvalues_per_lines_per_conditions_DS, sep="/"),
                      delay = 3,
                      zoom = zoomWebshot)
 })
 
 
 func_mvHisto_2 <- reactive({

   obj <- rv$dataset[[rv$iDat]]
     ##second plot of descriptive statistics
     if (is.null(rv$tempplot$mvHisto_perLines_HC)) {
         tempplot <- wrapper.mvPerLinesHisto_HC(obj)}
     else{
         tempplot <- rv$tempplot$mvHisto_perLines_HC}
     htmlwidgets::saveWidget(widget = tempplot, file = paste(tempdir(), sessionID, "tempplot.html", sep="/"))
     webshot::webshot(url = paste(tempdir(), sessionID, "tempplot.html", sep="/"),
                      file = paste(tempdir(), sessionID, gGraphicsFilenames$histo_missvalues_per_lines_DS, sep="/"),
                      delay = 3,
                      zoom = zoomWebshot)
 })
 
 func_mvHisto_3 <- reactive({

     obj <- rv$dataset[[rv$iDat]]
     # first plot of descriptive statistics
     if (is.null(rv$tempplot$mvHisto_HC)) {
         tempplot <- wrapper.mvHisto_HC(obj)}
     else{ tempplot <- rv$tempplot$mvHisto_HC}
     htmlwidgets::saveWidget(widget = tempplot, file = paste(tempdir(), sessionID, "tempplot.html", sep="/"))
     webshot::webshot(url = paste(tempdir(), sessionID, "tempplot.html", sep="/"),
                      file = paste(tempdir(), sessionID, gGraphicsFilenames$histoMV_Image_DS, sep="/"),
                      delay = 3,
                      zoom = zoomWebshot)
 })
 
 func_heatmap <- reactive({
   
   obj <- rv$dataset[[rv$iDat]]
     plotPNG(function(){
         if (is.null (rv$PlotParams$HeatmapDistance) && 
             is.null(rv$PlotParams$HeatmapLinkage)) {
             wrapper.heatmapD(obj)
         }
         else {
             wrapper.heatmapD(obj,
                              rv$PlotParams$HeatmapDistance, 
                              rv$PlotParams$HeatmapLinkage,
                              TRUE)
         }},
         filename=paste(tempdir(), sessionID, gGraphicsFilenames$heatmap, sep="/"),
         width = pngWidth,
         height=pngHeight,
         res=resolution)
 })

###--------------------------------------------------------------------------
createPNG_DescriptiveStatistics <- reactive({
    obj <- rv$dataset[[rv$iDat]]

    
    # func_heatmap()
    #                func_mvHisto_3()
    #                func_mvHisto_2()
    #                func_mvHisto_1()
    #                func_corrMatrix()
    #                func_varDist()
    #                func_violinPlot()
    #                func_boxplot()
    #                func_density()
    # 
                   
      list_f <- list(func_heatmap,
                     func_corrMatrix,
                     func_varDist,
                     func_violinPlot,
                     func_boxplot,
                     func_density,
                     func_mvHisto_3,
                     func_mvHisto_2,
                     func_mvHisto_1)             
     
      require(doParallel)
      registerDoParallel()  
      foreach(i=1:length(list_f)) %dopar% {
         list_f[[i]]()
     }
     
    
    
})





###--------------------------------------------------------------------------
# createPNG_BeforeFiltering <- reactive({
#     ##last plot of descriptive statistics
#     tempplot <- histo_missvalues_per_lines_per_conditions_DS()
#     htmlwidgets::saveWidget(widget = tempplot, file = paste(tempdir(), sessionID, "tempplot.html", sep="/"))
#     webshot::webshot(url = paste(tempdir(), sessionID, "tempplot.html", sep="/"), 
#                      file = paste(tempdir(), sessionID, gGraphicsFilenames$histo_missvalues_per_lines_per_conditions_DS_BeforeFiltering, sep="/"),
#                      zoom = zoomWebshot)
#     
#     
#     ##second plot of descriptive statistics
#     tempplot <- histo_missvalues_per_lines_DS()
#     htmlwidgets::saveWidget(widget = tempplot, file = paste(tempdir(), sessionID, "tempplot.html", sep="/"))
#     webshot::webshot(url = paste(tempdir(), sessionID, "tempplot.html", sep="/"), 
#                      file = paste(tempdir(), sessionID, gGraphicsFilenames$histo_missvalues_per_lines_DS_BeforeFiltering, sep="/"),
#                      zoom = zoomWebshot)
#     
#     # first plot of descriptive statistics
#     tempplot <- histoMV_Image_DS()
#     htmlwidgets::saveWidget(widget = tempplot, file = paste(tempdir(), sessionID, "tempplot.html", sep="/"))
#     webshot::webshot(url = paste(tempdir(), sessionID, "tempplot.html", sep="/"), 
#                      file = paste(tempdir(), sessionID, gGraphicsFilenames$histoMV_Image_DS_BeforeFiltering, sep="/"),
#                      zoom = zoomWebshot)
#     
# })



###--------------------------------------------------------------------------
createPNG_Filtering <- reactive({
  obj <- rv$dataset[[rv$iDat]]
  
    if (!is.null(rv$nbContaminantsDeleted) && !is.null(rv$nbReverseDeleted)) {
        tempplot <-proportionConRev_HC(rv$nbContaminantsDeleted, rv$nbReverseDeleted, nrow(obj))
    htmlwidgets::saveWidget(widget = tempplot, file = paste(tempdir(), sessionID, "tempplot.html", sep="/"))
    webshot::webshot(url = paste(tempdir(), sessionID, "tempplot.html", sep="/"), 
                     file = paste(tempdir(), sessionID, gGraphicsFilenames$propContRev, sep="/"),
                     delay = 3,
                     zoom = zoomWebshot)
    }
})




###--------------------------------------------------------------------------
createPNG_Normalization <- reactive({
    obj1 <- rv$dataset[[(which(names(rv$dataset)==rv$iDat) - 1)]]
    obj2 <- rv$dataset[[rv$iDat]]
      
      
    plotPNG(function(){wrapper.compareNormalizationD(obj1, obj2)}, 
            filename=paste(tempdir(), sessionID, gGraphicsFilenames$compareNorm, sep="/"), 
            width = pngWidth, 
            height=pngHeight,
            res=resolution)
    
    
    if (is.null(rv$tempplot$Density)) {
        tempplot <- wrapper.densityPlotD_HC(obj2)}
    else{ tempplot <- rv$tempplot$Density}
    htmlwidgets::saveWidget(widget = tempplot, file = paste(tempdir(), sessionID, "tempplot.html", sep="/"))
    webshot::webshot(url = paste(tempdir(), sessionID, "tempplot.html", sep="/"),
                     file = paste(tempdir(), sessionID, gGraphicsFilenames$densityPlotAfterNorm, sep="/"),
                     delay = 1,
                     zoom = zoomWebshot)
    
    
    plotPNG(function(){if (!is.null(rv$PlotParams$legDS)) {
        wrapper.boxPlotD(obj2,  rv$PlotParams$legDS)
    } else {
        wrapper.boxPlotD(obj2)}
    },
    filename=paste(tempdir(), sessionID, gGraphicsFilenames$boxplotAfterNorm, sep="/"),
    width = pngWidth,
    height=pngHeight,
    res=resolution)
    
    
    
    
})


###--------------------------------------------------------------------------
createPNG_Imputation <- reactive({
  obj <- rv$dataset[[(which(names(rv$dataset)==rv$iDat) - 1)]]
  
  plotPNG(function(){wrapper.mvTypePlot(obj)},
            filename=paste(tempdir(), sessionID, gGraphicsFilenames$MVtypePlot, sep="/"),
            width = pngWidth,
            height=pngHeight,
            res=resolution)

})

###--------------------------------------------------------------------------
createPNG_Aggregation <- reactive({
  obj <- rv$dataset[[rv$iDat]]
  
  
})

###--------------------------------------------------------------------------
createPNG_DifferentialAnalysis <- reactive({
  obj <- rv$dataset[[rv$iDat]]
  
    df <- data.frame(x=fData(obj)$logFC,
                     y = -log10(fData(obj)$P_Value))
    logFC <- obj@experimentData@other$threshold_logFC
    pval <-obj@experimentData@other$threshold_p_value
    cond <-c(obj@experimentData@other$condition1,
             obj@experimentData@other$condition2)
    
    tempplot <- diffAnaVolcanoplot_rCharts(df,
                                           threshold_logFC = logFC,
                                           threshold_pVal = pval,
                                           conditions = cond    )
                                           
    htmlwidgets::saveWidget(widget = tempplot, file = paste(tempdir(), sessionID, "tempplot.html", sep="/"))
    webshot::webshot(url = paste(tempdir(), sessionID, "tempplot.html", sep="/"), 
                     file = paste(tempdir(), sessionID, gGraphicsFilenames$volcanoPlot_3, sep="/"),
                     delay = 10
                     ,zoom = zoomWebshot
    )
    
    
    # plotPNG(function(){calibrationPlot()},
    #         filename=paste(tempdir(), sessionID, gGraphicsFilenames$calibrationPlot, sep="/"),
    #         width = pngWidth,
    #         height=pngHeight,
    #         res=resolution)


    plotPNG(function(){
        t <- rv$resAnaDiff$P_Value
        t <- t[which(abs(rv$resAnaDiff$logFC) >= rv$seuilLogFC)]
        wrapperCalibrationPlot(t, "ALL")},
            filename=paste(tempdir(), sessionID, gGraphicsFilenames$calibrationPlotAll, sep="/"),
            width = pngWidth,
            height=pngHeight,
            res=resolution)
})


###--------------------------------------------------------------------------

createPNG_Enrichment <- reactive({
  obj <- rv$dataset[[rv$iDat]]
  
  
    tempplot <- barplotEnrichGO_HC(rv$enrichGO_data)
    htmlwidgets::saveWidget(widget = tempplot, file = paste(tempdir(), sessionID, "tempplot.html", sep="/"))
    webshot::webshot(url = paste(tempdir(), sessionID, "tempplot.html", sep="/"), 
                     file = paste(tempdir(), sessionID, gGraphicsFilenames$GOEnrichBarplot, sep="/"),
                     delay = 5
                     ,zoom = zoomWebshot
    )
    
    tempplot <- scatterplotEnrichGO_HC(rv$enrichGO_data)
    htmlwidgets::saveWidget(widget = tempplot, file = paste(tempdir(), sessionID, "tempplot.html", sep="/"))
    webshot::webshot(url = paste(tempdir(), sessionID, "tempplot.html", sep="/"), 
                     file = paste(tempdir(), sessionID, gGraphicsFilenames$GOEnrichDotplot, sep="/"),
                     delay = 5
                     ,zoom = zoomWebshot
    )
    
})


###--------------------------------------------------------------------------
createPNG_GroupGO <- reactive({
    #input$GO_level
    
    l <- length(rv$groupGO_data)
    if (l == 1){
        
        tempplot <- barplotGroupGO_HC(rv$groupGO_data[[1]]$ggo_res, 
                                      title = paste("Groups at level ",  rv$groupGO_data[[1]]$level))
        htmlwidgets::saveWidget(widget = tempplot, file = paste(tempdir(), sessionID, "tempplot.html", sep="/"))
        webshot::webshot(url = paste(tempdir(), sessionID, "tempplot.html", sep="/"), 
                         file = paste(tempdir(), sessionID, gGraphicsFilenames$GOClassification_img1, sep="/"),
                         delay = 5
                         ,zoom = zoomWebshot
        )}
    
    if (l == 2){
        
        tempplot <- barplotGroupGO_HC(rv$groupGO_data[[2]]$ggo_res, 
                                      title = paste("Groups at level ",  rv$groupGO_data[[2]]$level))
        htmlwidgets::saveWidget(widget = tempplot, file = paste(tempdir(), sessionID, "tempplot.html", sep="/"))
        webshot::webshot(url = paste(tempdir(), sessionID, "tempplot.html", sep="/"), 
                         file = paste(tempdir(), sessionID, gGraphicsFilenames$GOClassification_img2, sep="/"),
                         delay = 5
                         ,zoom = zoomWebshot
        )
    }
    
    if (l== 3){

        tempplot <- tempplot <- barplotGroupGO_HC(rv$groupGO_data[[3]]$ggo_res, 
                                                  title = paste("Groups at level ", rv$groupGO_data[[3]]$level))
        
        htmlwidgets::saveWidget(widget = tempplot, file = paste(tempdir(), sessionID, "tempplot.html", sep="/"))
        webshot::webshot(url = paste(tempdir(), sessionID, "tempplot.html", sep="/"), 
                         file = paste(tempdir(), sessionID, gGraphicsFilenames$GOClassification_img3, sep="/"),
                         delay = 5
                         ,zoom = zoomWebshot
        )
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
        #src <- normalizePath('report.Rmd')
        filename <- paste(tempdir(), sessionID, 'report.Rmd',sep="/")
        
        #file.copy(src, filename, overwrite = TRUE)
        library(rmarkdown)
        #paramRmd <- list(current.obj=rv$current.obj)
        out <- render(filename, 
                      output_file = file,
                      params = list(listOverview = list(pourcentageNA = rv$pourcentageNA,
                                                        nEntities = dim(Biobase::exprs(rv$current.obj))[1],
                                                        typeOfData = rv$typeOfDataset,
                                                        nNAlines = rv$nb.empty.lines,
                                                        nSamples = dim(Biobase::exprs(rv$current.obj))[2]),
                                    
                                    listFiltering= list(filter = input$ChooseFilters,
                                                        seuilNA = as.integer(input$seuilNA),
                                                        nbReverseDeleted = rv$nbReverseDeleted,
                                                        nbContaminantsDeleted = rv$nbContaminantsDeleted,
                                                        stringBasedFiltered = (!is.null(rv$nbContaminantsDeleted) && !is.null(rv$nbReverseDeleted))
                                                        ),
                                    
                                    listNormalization = list(method=input$normalization.method,
                                                             type = input$normalization.type,
                                                             quantile=input$normalization.quantile,
                                                             quantileOther = input$normalization.quantileOther,
                                                             scaling = input$normalization.variance.reduction),
                                    
                                    listImputation = list(algorithm = input$missing.value.algorithm,
                                                          basicAlgo = input$missing.value.basic.algorithm,
                                                          detQuantile_quantile = input$detQuant_quantile,
                                                          detQuantile_factor = input$detQuant_factor,
                                                          imp4p_withLapala = input$imp4p_withLapala,
                                                          OnlyLAPALA_qmin = input$OnlyLAPALA_qmin,
                                                          OnlyLAPALA_distrib = input$OnlyLAPALA_distrib,
                                                          imp4pLAPALA_distrib = input$imp4pLAPALA_distrib),
                                    
                                    listAggregation = list(method = input$aggregationMethod,
                                                           df=rv$AggregProtStats),
                                    listAnaDiff = list(condition1 = input$condition1, 
                                                       condition2 = input$condition2,
                                                       calibrationMethod = input$calibrationMethod,
                                                       numericValCalibration = input$numericValCalibration,
                                                       seuilPValue = rv$seuilPVal,
                                                       seuilLogFC = rv$seuilLogFC,
                                                       method = input$diffAnaMethod,
                                                       fdr = round(100*rv$fdr, digits=2),
                                                       nbSelected = rv$nbSelected_Step3),
                                    listGOClassifAnalysis = list(ontology = input$Ontology, 
                                                       organism = input$Organism,
                                                       universe = input$universe,
                                                       group1 = (length(rv$groupGO_data)==1),
                                                       group2 = (length(rv$groupGO_data)==2),
                                                       group3 = (length(rv$groupGO_data)==3),
                                                       df1 = if (length(rv$groupGO_data)==1){
                                                                    dat <- rv$groupGO_data[[1]]$ggo_res@result
                                                                    dat <- dat[order(dat$Count, decreasing=TRUE),]
                                                                    dat[1:5,c("Description", "Count", "GeneRatio")]},
                                                       df2 = if (length(rv$groupGO_data)==2){
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

