# 
 initRmd <- reactive({
     
     ## Init the Rmd file for the report
     src <- normalizePath('Rmd_sources/report.Rmd')
     filename <- paste(tempdir(), sessionID, 'report.Rmd',sep="/")
     file.copy(src, filename, overwrite = TRUE)
     
 })

 
 output$test <- renderUI({
     
     shinyjs::disable("downloadReport")
 })
 
observeEvent(input$generateReport,{
    if (is.null(input$generateReport)) {return (NULL)}
    input$chooseDatasetToExport
    rv$dataset
    input$whichGO2Save
    if (is.null(input$chooseDatasetToExport)){return(NULL)}
    
     ## Init the Rmd file for the report
    src <- normalizePath('Rmd_sources/report.Rmd')
    filename <- paste(tempdir(), sessionID, 'report.Rmd',sep="/")
    file.copy(src, filename, overwrite = TRUE)
    
    
     if (paste("Original", rv$typeOfDataset, sep=" - ") %in% input$chooseDatasetToExport) {
         createPNG_DescriptiveStatistics()

         txt2Rmd <- readLines("Rmd_sources/descriptiveStats_Rmd.Rmd")
         filename <- paste(tempdir(), sessionID, 'report.Rmd',sep="/")
         write(txt2Rmd, file = filename,append = TRUE, sep = "\n")
     }

    if (paste("Filtered", rv$typeOfDataset, sep=" - ") %in% input$chooseDatasetToExport) {
        createPNG_Filtering()
        txt2Rmd <- readLines("Rmd_sources/filtering_Rmd.Rmd")
        filename <- paste(tempdir(), sessionID, 'report.Rmd',sep="/")
        write(txt2Rmd, file = filename,append = TRUE, sep = "\n")
    }
    
    if (paste("Normalized", rv$typeOfDataset, sep=" - ") %in% input$chooseDatasetToExport) {
        createPNG_Normalization()
        txt2Rmd <- readLines("Rmd_sources/normalization_Rmd.Rmd")
        filename <- paste(tempdir(), sessionID, 'report.Rmd',sep="/")
        write(txt2Rmd, file = filename,append = TRUE, sep = "\n")
    }
    
    # if (paste("Imputed", rv$typeOfDataset, sep=" - ") %in% listDatasets) {
    #     txt2Rmd <- readLines("Rmd_sources/imputation_Rmd.Rmd")
    #     filename <- paste(tempdir(), sessionID, 'report.Rmd',sep="/")
    #     write(txt2Rmd, file = filename,append = TRUE, sep = "\n")
    # }
    
    nameOfDataset <- paste("DiffAnalysis.Limma", rv$typeOfDataset, sep=" - ")
    if (nameOfDataset %in% input$chooseDatasetToExport) {
        createPNG_DifferentialAnalysis()
        txt2Rmd <- readLines("Rmd_sources/anaDiff_Rmd.Rmd")
        filename <- paste(tempdir(), sessionID, 'report.Rmd',sep="/")
        write(txt2Rmd, file = filename,append = TRUE, sep = "\n")
    }
    
    
    
    if (paste("GOAnalysis", rv$typeOfDataset, sep=" - ") %in% input$chooseDatasetToExport) {
        if  (input$whichGO2Save == "Both"){
            createPNG_Enrichment()
            createPNG_GroupGO()
            txt2Rmd <- readLines("Rmd_sources/GO_Classification_Rmd.Rmd")
            filename <- paste(tempdir(), sessionID, 'report.Rmd',sep="/")
            write(txt2Rmd, file = filename,append = TRUE, sep = "\n")
            
            txt2Rmd <- readLines("Rmd_sources/GO_Enrichment_Rmd.Rmd")
            filename <- paste(tempdir(), sessionID, 'report.Rmd',sep="/")
            write(txt2Rmd, file = filename,append = TRUE, sep = "\n")
        }
        else if  (input$whichGO2Save == "Classification"){
            createPNG_GroupGO()
            txt2Rmd <- readLines("Rmd_sources/GO_Classification_Rmd.Rmd")
            filename <- paste(tempdir(), sessionID, 'report.Rmd',sep="/")
            write(txt2Rmd, file = filename,append = TRUE, sep = "\n")
        }
        else if (input$whichGO2Save == "Enrichment"){
            createPNG_Enrichment()
            txt2Rmd <- readLines("Rmd_sources/GO_Enrichment_Rmd.Rmd")
            filename <- paste(tempdir(), sessionID, 'report.Rmd',sep="/")
            write(txt2Rmd, file = filename,append = TRUE, sep = "\n")
        }
        
    }
    
    shinyjs::enable("downloadReport")
    
})





###--------------------------------------------------------------------------
createPNG_DescriptiveStatistics <- reactive({
    dname <- paste("Original", rv$typeOfDataset, sep=" - ")
    obj <- rv$dataset[[dname]]
  
  if (is.null(rv$tempplot$Density)) {
    tempplot <- wrapper.densityPlotD_HC(obj)}
      else{ tempplot <- rv$tempplot$Density}
  htmlwidgets::saveWidget(widget = tempplot, file = paste(tempdir(), sessionID, "tempplot.html", sep="/"))
  webshot::webshot(url = paste(tempdir(), sessionID, "tempplot.html", sep="/"),
                   file = paste(tempdir(), sessionID, gGraphicsFilenames$densityPlot, sep="/"),
                   delay = 1,
                   zoom = zoomWebshot)


  plotPNG(function(){if (!is.null(rv$PlotParams$legDS)) {
    wrapper.boxPlotD(obj,  rv$PlotParams$legDS)
    } else {
    wrapper.boxPlotD(obj)}
      },
          filename=paste(tempdir(), sessionID, gGraphicsFilenames$boxplot, sep="/"),
          width = pngWidth,
          height=pngHeight,
          res=resolution)


  plotPNG(function(){if (!is.null(rv$PlotParams$legDS_Violinplot)) {
      wrapper.violinPlotD(obj,  rv$PlotParams$legDS_Violinplot)
  } else {
      wrapper.violinPlotD(obj)}
    },
    filename=paste(tempdir(), sessionID, gGraphicsFilenames$violinplot, sep="/"),
    width = pngWidth,
    height=pngHeight,
    res=resolution)



  if (is.null(rv$tempplot$varDist)) {tempplot <- wrapper.CVDistD_HC(obj)}
  else{ tempplot <- rv$tempplot$varDist}
        htmlwidgets::saveWidget(widget = tempplot, file = paste(tempdir(), sessionID, "tempplot.html", sep="/"))
    webshot::webshot(url = paste(tempdir(), sessionID, "tempplot.html", sep="/"),
                     file = paste(tempdir(), sessionID, gGraphicsFilenames$varDist, sep="/"),
                     delay = 1,
                     zoom = zoomWebshot)



    if (is.null(rv$tempplot$corrMatrix)) {
    tempplot <- wrapper.corrMatrixD_HC(obj) }
    else {
      tempplot <- rv$tempplot$corrMatrix
    }
        htmlwidgets::saveWidget(widget = tempplot, file = paste(tempdir(), sessionID, "tempplot.html", sep="/"))
    webshot::webshot(url = paste(tempdir(), sessionID, "tempplot.html", sep="/"),
                     file = paste(tempdir(), sessionID, gGraphicsFilenames$corrMatrix, sep="/"),
                     zoom = zoomWebshot)


    #png(paste(tempdir(), sessionID, gGraphicsFilenames$heatmap, sep="/"))
    #heatmap()
    #dev.off()


    ##last plot of descriptive statistics
    if (is.null(rv$tempplot$mvHisto_perLines_HC)) {
      tempplot <- wrapper.mvPerLinesHistoPerCondition_HC(obj)}
    else{ tempplot <- rv$tempplot$histo_missvalues_per_lines_per_conditions}
        htmlwidgets::saveWidget(widget = tempplot, file = paste(tempdir(), sessionID, "tempplot.html", sep="/"))
    webshot::webshot(url = paste(tempdir(), sessionID, "tempplot.html", sep="/"),
                     file = paste(tempdir(), sessionID, gGraphicsFilenames$histo_missvalues_per_lines_per_conditions_DS, sep="/"),
                     zoom = zoomWebshot)


    ##second plot of descriptive statistics
    if (is.null(rv$tempplot$mvHisto_perLines_HC)) {
      tempplot <- wrapper.mvPerLinesHisto_HC(obj)}
    else{
      tempplot <- rv$tempplot$mvHisto_perLines_HC}
    htmlwidgets::saveWidget(widget = tempplot, file = paste(tempdir(), sessionID, "tempplot.html", sep="/"))
    webshot::webshot(url = paste(tempdir(), sessionID, "tempplot.html", sep="/"),
                     file = paste(tempdir(), sessionID, gGraphicsFilenames$histo_missvalues_per_lines_DS, sep="/"),
                     zoom = zoomWebshot)


    # first plot of descriptive statistics
    if (is.null(rv$tempplot$mvHisto_HC)) {
      tempplot <- wrapper.mvHisto_HC(obj)}
    else{ tempplot <- rv$tempplot$mvHisto_HC}
    htmlwidgets::saveWidget(widget = tempplot, file = paste(tempdir(), sessionID, "tempplot.html", sep="/"))
    webshot::webshot(url = paste(tempdir(), sessionID, "tempplot.html", sep="/"),
                     file = paste(tempdir(), sessionID, gGraphicsFilenames$histoMV_Image_DS, sep="/"),
                     zoom = zoomWebshot)



  # plotPNG(function(){heatmap()},
  #           filename=paste(tempdir(), sessionID, gGraphicsFilenames$heatmap, sep="/"),
  #           width = pngWidth,
  #           height=pngHeight,
  #           res=resolution)
  #
    
    
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
  dname <- paste("Filtered", rv$typeOfDataset, sep=" - ")
  obj <- rv$dataset[[dname]]
  
    tempplot <-proportionConRev_HC(rv$nbContaminantsDeleted, rv$nbReverseDeleted, nrow(obj))
    htmlwidgets::saveWidget(widget = tempplot, file = paste(tempdir(), sessionID, "tempplot.html", sep="/"))
    webshot::webshot(url = paste(tempdir(), sessionID, "tempplot.html", sep="/"), 
                     file = paste(tempdir(), sessionID, gGraphicsFilenames$propContRev, sep="/"),
                     zoom = zoomWebshot)
})



###--------------------------------------------------------------------------
createPNG_Normalization <- reactive({
  dname <- paste("Normalized", rv$typeOfDataset, sep=" - ")
  
    obj1 <- rv$dataset[[(which(names(rv$dataset)==dname) - 1)]]
    obj2 <- rv$dataset[[dname]]
      
      
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
createPNG_BeforeImputation <- reactive({
    #png(paste(tempdir(), sessionID, gGraphicsFilenames$imageNA_BeforeImputation, sep="/"))
    #showImageNA()
    #dev.off()
    
    #png(paste(tempdir(), sessionID, gGraphicsFilenames$MVtypePlot_BeforeImputation, sep="/"))
    #viewNAbyMean()
    #dev.off()
})

createPNG_AfterImputation <- reactive({
    #png(paste(tempdir(), sessionID, gGraphicsFilenames$imageNA_AfterImputation, sep="/"))
    #showImageNA()
    #dev.off()
    
    #png(paste(tempdir(), sessionID, gGraphicsFilenames$MVtypePlot_AfterImputation, sep="/"))
    #viewNAbyMean()
    #dev.off()
})

###--------------------------------------------------------------------------
createPNG_Aggregation <- reactive({
  dname <- paste("Aggregated", rv$typeOfDataset, sep=" - ")
  obj <- rv$dataset[[dname]]
  
  
})

###--------------------------------------------------------------------------
createPNG_DifferentialAnalysis <- reactive({
  nameOfDataset <- paste("DiffAnalysis.Limma", rv$typeOfDataset, sep=" - ")
  obj <- rv$dataset[[nameOfDataset]]
  
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
  nameOfDataset <- paste("GOAnalysis", rv$typeOfDataset, sep=" - ")
  obj <- rv$dataset[[nameOfDataset]]
  
  
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
###--------------------------------------------------------------------------
createPNG_GOAnalysis<- reactive({
    input$whichGO2Save
    
    
    if (input$whichGO2Save == "Both"){
        createPNG_GroupGO()
        createPNG_Enrichment()
    }
    else if (input$whichGO2Save == "Classification"){ createPNG_GroupGO()}
    else if (input$whichGO2Save == "Enrichment"){createPNG_Enrichment()}
    
    
    
    #png(paste(tempdir(), sessionID, gGraphicsFilenames$compareNorm, sep="/"))
    #viewComparisonNorm()
    #dev.off()
    
    # plotPNG(function(){GOplotGroup()}, 
    #        filename=paste(tempdir(), sessionID, gGraphicsFilenames$GOClassification, sep="/"), 
    #        width = pngWidth, 
    #        height=pngHeight,
    #        res=resolution)
    
    
    
    
    # plotPNG(function(){GObarplotEnrich()}, 
    #         filename=paste(tempdir(), sessionID, gGraphicsFilenames$GOEnrichBarplot, sep="/"), 
    #         width = pngWidth, 
    #         height=pngHeight,
    #         res=resolution)
    # 
    # 
    # plotPNG(function(){GOdotplotEnrich()}, 
    #         filename=paste(tempdir(), sessionID, gGraphicsFilenames$GOEnrichDotplot, sep="/"), 
    #         width = pngWidth, 
    #         height=pngHeight,
    #         res=resolution)
    
    
    
})
