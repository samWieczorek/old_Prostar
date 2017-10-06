
initRmd <- reactive({
    
    ## Init the Rmd file for the report
    src <- normalizePath('Rmd_sources/report.Rmd')
    filename <- paste(tempdir(), sessionID, 'report.Rmd',sep="/")
    file.copy(src, filename, overwrite = TRUE)
    
    #createPNG_DescriptiveStatistics()
    
     # if (xxxx) {
    #     txt2Rmd <- readLines("Rmd_sources/filtering_Rmd.Rmd")
    #     filename <- paste(tempdir(), sessionID, 'report.Rmd',sep="/")
    #     write(txt2Rmd, file = filename,append = TRUE, sep = "\n")
    #     
    # }
    
})

completeRmd <- reactive({
    rv$dataset
    input$whichGO2Save
    
    ## Init the Rmd file for the report
    src <- normalizePath('Rmd_sources/report.Rmd')
    filename <- paste(tempdir(), sessionID, 'report.Rmd',sep="/")
    file.copy(src, filename, overwrite = TRUE)
    
    #createPNG_DescriptiveStatistics()
    
    
    listDatasets <- names(rv$dataset)
    print(listDatasets)
    
    # if (paste("Original", rv$typeOfDataset, sep=" - ") %in% listDatasets) {
    #     txt2Rmd <- readLines("Rmd_sources/descriptiveStats_Rmd.Rmd")
    #     filename <- paste(tempdir(), sessionID, 'report.Rmd',sep="/")
    #     write(txt2Rmd, file = filename,append = TRUE, sep = "\n")
    # }

    if (paste("Filtered", rv$typeOfDataset, sep=" - ") %in% listDatasets) {
        txt2Rmd <- readLines("Rmd_sources/filtering_Rmd.Rmd")
        filename <- paste(tempdir(), sessionID, 'report.Rmd',sep="/")
        write(txt2Rmd, file = filename,append = TRUE, sep = "\n")
    }
    
    if (paste("Normalized", rv$typeOfDataset, sep=" - ") %in% listDatasets) {
        txt2Rmd <- readLines("Rmd_sources/normalization_Rmd.Rmd")
        filename <- paste(tempdir(), sessionID, 'report.Rmd',sep="/")
        write(txt2Rmd, file = filename,append = TRUE, sep = "\n")
    }
    
    if (paste("Imputed", rv$typeOfDataset, sep=" - ") %in% listDatasets) {
        txt2Rmd <- readLines("Rmd_sources/imputation_Rmd.Rmd")
        filename <- paste(tempdir(), sessionID, 'report.Rmd',sep="/")
        write(txt2Rmd, file = filename,append = TRUE, sep = "\n")
    }
    
    if (paste("DiffAnalysis.Limma", rv$typeOfDataset, sep=" - ") %in% listDatasets) {
        txt2Rmd <- readLines("Rmd_sources/anaDiff_Rmd.Rmd")
        filename <- paste(tempdir(), sessionID, 'report.Rmd',sep="/")
        write(txt2Rmd, file = filename,append = TRUE, sep = "\n")
    }
    
    
    
    if (paste("GOAnalysis", rv$typeOfDataset, sep=" - ") %in% listDatasets) {
        if  (input$whichGO2Save == "Both"){
            txt2Rmd <- readLines("Rmd_sources/GO_Classification_Rmd.Rmd")
            filename <- paste(tempdir(), sessionID, 'report.Rmd',sep="/")
            write(txt2Rmd, file = filename,append = TRUE, sep = "\n")
            
            txt2Rmd <- readLines("Rmd_sources/GO_Enrichment_Rmd.Rmd")
            filename <- paste(tempdir(), sessionID, 'report.Rmd',sep="/")
            write(txt2Rmd, file = filename,append = TRUE, sep = "\n")
        }
        else if  (input$whichGO2Save == "Classification"){
            txt2Rmd <- readLines("Rmd_sources/GO_Classification_Rmd.Rmd")
            filename <- paste(tempdir(), sessionID, 'report.Rmd',sep="/")
            write(txt2Rmd, file = filename,append = TRUE, sep = "\n")
        }
        else if (input$whichGO2Save == "Enrichment"){
            txt2Rmd <- readLines("Rmd_sources/GO_Enrichment_Rmd.Rmd")
            filename <- paste(tempdir(), sessionID, 'report.Rmd',sep="/")
            write(txt2Rmd, file = filename,append = TRUE, sep = "\n")
        }
        
    }
    
})





###--------------------------------------------------------------------------
createPNG_DescriptiveStatistics <- reactive({
    
    
    tempplot <- Densityplot_DS()
    htmlwidgets::saveWidget(widget = tempplot, file = paste(tempdir(), sessionID, "tempplot.html", sep="/"))
    webshot::webshot(url = paste(tempdir(), sessionID, "tempplot.html", sep="/"),
                     file = paste(tempdir(), sessionID, gGraphicsFilenames$densityPlot, sep="/"),
                     delay = 1,
                     zoom = zoomWebshot)
    
    
    tempplot <-viewDistCV()
        htmlwidgets::saveWidget(widget = tempplot, file = paste(tempdir(), sessionID, "tempplot.html", sep="/"))
    webshot::webshot(url = paste(tempdir(), sessionID, "tempplot.html", sep="/"),
                     file = paste(tempdir(), sessionID, gGraphicsFilenames$varDist, sep="/"),
                     delay = 1,
                     zoom = zoomWebshot)

    
    tempplot <-corrMatrix()
        htmlwidgets::saveWidget(widget = tempplot, file = paste(tempdir(), sessionID, "tempplot.html", sep="/"))
    webshot::webshot(url = paste(tempdir(), sessionID, "tempplot.html", sep="/"),
                     file = paste(tempdir(), sessionID, gGraphicsFilenames$corrMatrix, sep="/"),
                     zoom = zoomWebshot)

    
    #png(paste(tempdir(), sessionID, gGraphicsFilenames$heatmap, sep="/"))
    #heatmap()
    #dev.off()
    
   
    ##last plot of descriptive statistics
    tempplot <-histo_missvalues_per_lines_per_conditions_DS()
        htmlwidgets::saveWidget(widget = tempplot, file = paste(tempdir(), sessionID, "tempplot.html", sep="/"))
    webshot::webshot(url = paste(tempdir(), sessionID, "tempplot.html", sep="/"),
                     file = paste(tempdir(), sessionID, gGraphicsFilenames$histo_missvalues_per_lines_per_conditions_DS, sep="/"),
                     zoom = zoomWebshot)
    
    
    ##second plot of descriptive statistics
    tempplot <-histo_missvalues_per_lines_DS()
        htmlwidgets::saveWidget(widget = tempplot, file = paste(tempdir(), sessionID, "tempplot.html", sep="/"))
    webshot::webshot(url = paste(tempdir(), sessionID, "tempplot.html", sep="/"),
                     file = paste(tempdir(), sessionID, gGraphicsFilenames$histo_missvalues_per_lines_DS, sep="/"),
                     zoom = zoomWebshot)
    
    
    # first plot of descriptive statistics
    tempplot <-histoMV_Image_DS()
        htmlwidgets::saveWidget(widget = tempplot, file = paste(tempdir(), sessionID, "tempplot.html", sep="/"))
    webshot::webshot(url = paste(tempdir(), sessionID, "tempplot.html", sep="/"),
                     file = paste(tempdir(), sessionID, gGraphicsFilenames$histoMV_Image_DS, sep="/"),
                     zoom = zoomWebshot)
    
    
    plotPNG(function(){boxPlot()}, 
            filename=paste(tempdir(), sessionID, gGraphicsFilenames$boxplot, sep="/"), 
            width = pngWidth, 
            height=pngHeight,
            res=resolution)
    
    
    plotPNG(function(){violinPlot2()}, 
            filename=paste(tempdir(), sessionID, gGraphicsFilenames$violinplot, sep="/"), 
            width = pngWidth, 
            height=pngHeight,
            res=resolution)
    
    plotPNG(function(){heatmap()}, 
            filename=paste(tempdir(), sessionID, gGraphicsFilenames$heatmap, sep="/"), 
            width = pngWidth, 
            height=pngHeight,
            res=resolution)
    
    
    
})





###--------------------------------------------------------------------------
createPNG_BeforeFiltering <- reactive({
    ##last plot of descriptive statistics
    tempplot <- histo_missvalues_per_lines_per_conditions_DS()
    htmlwidgets::saveWidget(widget = tempplot, file = paste(tempdir(), sessionID, "tempplot.html", sep="/"))
    webshot::webshot(url = paste(tempdir(), sessionID, "tempplot.html", sep="/"), 
                     file = paste(tempdir(), sessionID, gGraphicsFilenames$histo_missvalues_per_lines_per_conditions_DS_BeforeFiltering, sep="/"),
                     zoom = zoomWebshot)
    
    
    ##second plot of descriptive statistics
    tempplot <- histo_missvalues_per_lines_DS()
    htmlwidgets::saveWidget(widget = tempplot, file = paste(tempdir(), sessionID, "tempplot.html", sep="/"))
    webshot::webshot(url = paste(tempdir(), sessionID, "tempplot.html", sep="/"), 
                     file = paste(tempdir(), sessionID, gGraphicsFilenames$histo_missvalues_per_lines_DS_BeforeFiltering, sep="/"),
                     zoom = zoomWebshot)
    
    # first plot of descriptive statistics
    tempplot <- histoMV_Image_DS()
    htmlwidgets::saveWidget(widget = tempplot, file = paste(tempdir(), sessionID, "tempplot.html", sep="/"))
    webshot::webshot(url = paste(tempdir(), sessionID, "tempplot.html", sep="/"), 
                     file = paste(tempdir(), sessionID, gGraphicsFilenames$histoMV_Image_DS_BeforeFiltering, sep="/"),
                     zoom = zoomWebshot)
    
})



###--------------------------------------------------------------------------
createPNG_Filtering <- reactive({
    
    tempplot <-GlobalPieChart()
    htmlwidgets::saveWidget(widget = tempplot, file = paste(tempdir(), sessionID, "tempplot.html", sep="/"))
    webshot::webshot(url = paste(tempdir(), sessionID, "tempplot.html", sep="/"), 
                     file = paste(tempdir(), sessionID, gGraphicsFilenames$propContRev, sep="/"),
                     zoom = zoomWebshot)
})

###--------------------------------------------------------------------------
createPNG_Normalization <- reactive({
    
    #png(paste(tempdir(), sessionID, gGraphicsFilenames$compareNorm, sep="/"))
    #viewComparisonNorm()
    #dev.off()
    
    plotPNG(function(){viewComparisonNorm()}, 
            filename=paste(tempdir(), sessionID, gGraphicsFilenames$compareNorm, sep="/"), 
            width = pngWidth, 
            height=pngHeight,
            res=resolution)
    
})


###--------------------------------------------------------------------------
createPNG_BeforeNormalization <- reactive({
    
    tempplot <- viewDensityplotNorm()
    htmlwidgets::saveWidget(widget = tempplot, file = paste(tempdir(), sessionID, "tempplot.html", sep="/"))
    webshot::webshot(url = paste(tempdir(), sessionID, "tempplot.html", sep="/"), 
                     file = paste(tempdir(), sessionID, gGraphicsFilenames$densityPlotBeforeNorm, sep="/"),
                     delay = 1,
                     zoom = zoomWebshot)
    
    
    # png(paste(tempdir(), sessionID, gGraphicsFilenames$boxplotBeforeNorm, sep="/"))
    # viewBoxPlotNorm()
    # dev.off()
    # 
    plotPNG(function(){viewBoxPlotNorm()}, 
            filename=paste(tempdir(), sessionID, gGraphicsFilenames$boxplotBeforeNorm, sep="/"), 
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
    
})

###--------------------------------------------------------------------------
createPNG_DifferentialAnalysis <- reactive({
    tempplot <- volcanoplot_rCharts_Step3()
    htmlwidgets::saveWidget(widget = tempplot, file = paste(tempdir(), sessionID, "tempplot.html", sep="/"))
    webshot::webshot(url = paste(tempdir(), sessionID, "tempplot.html", sep="/"), 
                     file = paste(tempdir(), sessionID, gGraphicsFilenames$volcanoPlot_3, sep="/"),
                     delay = 5
                     ,zoom = zoomWebshot
    )
    
    # png(paste(tempdir(), sessionID, gGraphicsFilenames$calibrationPlot, sep="/"))
    # calibrationPlot()
    # Sys.sleep(5)
    # dev.off()
    # 
    plotPNG(function(){calibrationPlot()}, 
            filename=paste(tempdir(), sessionID, gGraphicsFilenames$calibrationPlot, sep="/"), 
            width = pngWidth, 
            height=pngHeight,
            res=resolution)
    
    # png(paste(tempdir(), sessionID, gGraphicsFilenames$calibrationPlotAll, sep="/"))
    # calibrationPlotAll()
    # dev.off()
    # 
    plotPNG(function(){calibrationPlotAll()}, 
            filename=paste(tempdir(), sessionID, gGraphicsFilenames$calibrationPlotAll, sep="/"), 
            width = pngWidth, 
            height=pngHeight,
            res=resolution)
})



createPNG_Enrichment <- reactive({
    
    tempplot <- GObarplotEnrich()
    htmlwidgets::saveWidget(widget = tempplot, file = paste(tempdir(), sessionID, "tempplot.html", sep="/"))
    webshot::webshot(url = paste(tempdir(), sessionID, "tempplot.html", sep="/"), 
                     file = paste(tempdir(), sessionID, gGraphicsFilenames$GOEnrichBarplot, sep="/"),
                     delay = 5
                     ,zoom = zoomWebshot
    )
    
    tempplot <- GOdotplotEnrich()
    htmlwidgets::saveWidget(widget = tempplot, file = paste(tempdir(), sessionID, "tempplot.html", sep="/"))
    webshot::webshot(url = paste(tempdir(), sessionID, "tempplot.html", sep="/"), 
                     file = paste(tempdir(), sessionID, gGraphicsFilenames$GOEnrichDotplot, sep="/"),
                     delay = 5
                     ,zoom = zoomWebshot
    )
    
})


createPNG_GroupGO <- reactive({
    input$GO_level
    
    if (length(input$GO_level) == 1){
        
        tempplot <- GOplotGroup_level2()
        htmlwidgets::saveWidget(widget = tempplot, file = paste(tempdir(), sessionID, "tempplot.html", sep="/"))
        webshot::webshot(url = paste(tempdir(), sessionID, "tempplot.html", sep="/"), 
                         file = paste(tempdir(), sessionID, gGraphicsFilenames$GOClassification_img1, sep="/"),
                         delay = 5
                         ,zoom = zoomWebshot
        )}
    
    if (length(input$GO_level) == 2){
        
        tempplot <- GOplotGroup_level3()
        htmlwidgets::saveWidget(widget = tempplot, file = paste(tempdir(), sessionID, "tempplot.html", sep="/"))
        webshot::webshot(url = paste(tempdir(), sessionID, "tempplot.html", sep="/"), 
                         file = paste(tempdir(), sessionID, gGraphicsFilenames$GOClassification_img2, sep="/"),
                         delay = 5
                         ,zoom = zoomWebshot
        )
    }
    
    if (length(input$GO_level) == 3){
        
        
        tempplot <- GOplotGroup_level4()
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
    print("ecriture des fichiers image")
    
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
