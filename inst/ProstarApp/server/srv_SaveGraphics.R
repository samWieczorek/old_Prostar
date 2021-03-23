

output$test2 <- renderUI({
  shinyjs::disable("downloadReport")
})


GetTreeSelected <- function(toto){
  tmp <- unlist(shinyTree::get_selected(toto))
  toExclude <- c('MV plots', 'PCA plots')
  tmp <- setdiff(tmp, toExclude)
  return(tmp)
}



toto <- reactive({
  rv$dataset
  
  ## Init the Rmd file for the report
  src <- file.path("Rmd_sources", "report.Rmd")
  rv$outfile <- tempfile(pattern = "report",fileext='.Rmd')
  file.create(rv$outfile)
  file.copy(src, rv$outfile, overwrite = TRUE)
  
  # if (!is.null(rv$current.obj)){
  #     txt2Rmd <- readLines("Rmd_sources/summary.Rmd")
  #     write(txt2Rmd, file = rv$outfile, append = TRUE, sep = "\n")
  # }            
  #    
  ntotal <- 0
  for (i in 1:length(names(rv$dataset))){
    widgetName <- paste0('plotsFor.',names(rv$dataset)[i])
    widgetName <- gsub(".", "_", widgetName, fixed=TRUE)
    
    ll_plots <- GetTreeSelected(input[[widgetName]])
    ntotal <- ntotal + length(ll_plots)
  }
  
  withProgress(message = '',detail = '', value = 0, {
    
    
    ### for each available dataset, create the selected plots
    for (i in 1:length(names(rv$dataset))){
      print(paste0("Dataset:",names(rv$dataset)[i]))
      widgetName <- paste0('plotsFor.',names(rv$dataset)[i])
      widgetName <- gsub(".", "_", widgetName, fixed=TRUE)
      write(paste0("# ",gsub(".", "_", names(rv$dataset)[i], fixed=TRUE),"
                   " ), file = rv$outfile,append = TRUE, sep = "\n")
      
      ll_plots <-  GetTreeSelected(input[[widgetName]])
      for (plot.name in ll_plots) {
        
        obj <- rv$dataset[[names(rv$dataset)[i]]]
        pattern <- paste0(names(rv$dataset)[i],".", plot.name)
        pattern <- gsub(".", "_", pattern, fixed=TRUE)
        
        incProgress(1/ntotal, detail = paste0('Building ',pattern))
        params <- NULL
        #params <- GetParamsForPlot()
        #pngfile <- createPNG_BoxplotHC(obj,params,basename = pattern)
        print(paste0("createPNG_",plot.name))
        pngfile <- do.call(paste0("createPNG_",plot.name),
                           list(obj=obj,params=params,basename = pattern))
        
        addPNG_to_Rmd(pngfile, paste0("fig_",pattern), rv$outfile)
      }
    }  ## END for each dataset
    
    
    ##
    # for (xxxx){
    #   
    # }
  })
  
  
})
  
 

addPNG_to_Rmd <- function(pngfilename, tagRmd, outfile){
  
  
  txt <- paste0("
```{r ",tagRmd,",out.width = '75%', echo=FALSE}
  filename <-  normalizePath(paste(tempdir(), '",pngfilename,"', sep='/'), winslash='/')
  if (file.exists(filename)){knitr::include_graphics(filename) }

 ```
")

  write(txt, file = outfile,append = TRUE, sep = "\n")

}




createPNG_PCA_Var <- function(obj,params=NULL,basename = NULL){
  if (is.null(basename) || is.null(obj) || is.null(rv$res.pca)){return(NULL)}
  #req(c(basename, obj,rv$res.pca  ))
  
  pngfile <- paste0(basename, ".png")
  fullname <- normalizePath(paste(tempdir(), pngfile, sep="/"))
 # if (!file.exists(pngfile)){
    png (fullname)
    print(plotPCA_Var(rv$res.pca, rv$PCA_axes))
    dev.off ()
    
    #width = pngWidth,
    #height=pngHeight,
    #res=resolution
    
  #}
  return(pngfile)
}



createPNG_PCA_Ind <- function(obj,params=NULL,basename = NULL){
  if (is.null(basename) || is.null(obj) || is.null(rv$res.pca)){return(NULL)}
  #if (is.null(basename)||is.null(obj)){return(NULL)}
  pngfile <- paste0(basename, ".png")
  fullname <- normalizePath( paste(tempdir(), pngfile, sep="/"))
  #if (!file.exists(pngfile)){
    png (fullname)
    print(plotPCA_Ind(rv$res.pca, rv$PCA_axes))
    dev.off ()
  #}
  return(pngfile)
}




###----------------------------------------------------------
createPNG_PCA_Eigen <- function(obj,params=NULL,basename = NULL){
  if (is.null(basename)||is.null(obj)){return(NULL)}
  pngfile <- paste0(basename, ".png")
  if (!file.exists(pngfile)){
    tempplot <- DAPAR::plotPCA_Eigen_hc(rv$res.pca)
    createPNGFromWidget(tempplot,basename) 
  }
  return(pngfile)
}


###----------------------------------------------------------
createPNG_BoxplotHC <- function(obj,params=NULL,basename = NULL){
  if (is.null(basename)||is.null(obj)){return(NULL)}
  pngfile <- paste0(basename, ".png")
  if (!file.exists(pngfile)){
    tempplot <- DAPAR::boxPlotD_HC(obj,
                                   pal=rv$PlotParams$paletteForConditions
                                   )
    createPNGFromWidget(tempplot,basename) 
  }
  return(pngfile)
}




createPNG_boxplot <- function(obj,params=NULL,basename = NULL){
  if (is.null(basename)||is.null(obj)){return(NULL)}
  pngfile <- paste0(basename, ".png")
  if (!file.exists(pngfile)){
    tempplot <- DAPAR::boxPlotD_HC(obj, 
                                   legend = rv$PlotParams$legendForSamples, 
                                   pal=rv$PlotParams$paletteForConditions)
    createPNGFromWidget(tempplot,basename) 
  }
  return(pngfile)
}

###----------------------------------------------------------
createPNG_densityplot <- function(obj,params=NULL,basename = NULL){
  if (is.null(basename)||is.null(obj)){return(NULL)}
  
  pngfile <- paste0(basename, ".png")
  
  if (!file.exists(pngfile)){
    tempplot <- DAPAR::densityPlotD_HC(obj, 
                                       legend = rv$PlotParams$legendForSamples, 
                                       pal=rv$PlotParams$paletteForConditions)
    createPNGFromWidget(tempplot, basename)
    }
  return(pngfile)
}


createPNG_CVDistr <- function(obj,params=NULL,basename = NULL){
  req(c(basename,obj))
  
  pngfile <- paste0(basename, ".png")
  
  if (!file.exists(pngfile)){
    tempplot <- wrapper.CVDistD_HC(obj,
                                   pal=rv$PlotParams$paletteForConditions)
    createPNGFromWidget(tempplot, basename)
    }
  return(pngfile)
}


createPNG_violinplot <- function(obj,params=NULL,basename = NULL){
  if (is.null(basename)||is.null(obj)){return(NULL)}
  
  pngfile <- paste0(basename, ".png")
  fullname <- normalizePath(paste(tempdir(), pngfile, sep="/"))
  #if (!file.exists(pngfile)){
    plotPNG(function(){
      DAPAR::violinPlotD(obj, 
                         legend = rv$PlotParams$legendForSamples,
                         pal=rv$PlotParams$paletteForConditions)
    },
    filename=fullname
    #width = pngWidth,
    #height=pngHeight,
    #res=resolution
    )
   # }
  return(pngfile)
}


createPNG_corrMatrix <- function(obj,params=NULL,basename = NULL){
  if (is.null(basename)||is.null(obj)){return(NULL)}
  
  pngfile <- paste0(basename, ".png")
   
  if (!file.exists(pngfile)){
    tempplot <- wrapper.corrMatrixD_HC(obj)
    createPNGFromWidget(tempplot, basename)
    }
  return(pngfile)
}


createPNG_MVPlot1 <- function(obj,params=NULL,basename = NULL){
  if (is.null(basename)||is.null(obj)){return(NULL)}
  
  pngfile <- paste0(basename, ".png")
  
  if (!file.exists(pngfile)){
    tempplot <- wrapper.mvPerLinesHistoPerCondition_HC(obj)
    createPNGFromWidget(tempplot, basename)
    }
  return(pngfile)
}


createPNG_MVPlot2 <- function(obj,params=NULL,basename = NULL){
  if (is.null(basename)||is.null(obj)){return(NULL)}
  
  pngfile <- paste0(basename, ".png")
    if (!file.exists(pngfile)){
    tempplot <- wrapper.mvPerLinesHisto_HC(obj)
    createPNGFromWidget(tempplot, basename)
    }
  return(pngfile)
}

createPNG_MVPlot3 <- function(obj,
                              params=NULL,
                              basename = NULL){
  if (is.null(basename)||is.null(obj)){return(NULL)}
  
  pngfile <- paste0(basename, ".png")
   pattern = NULL
  if (!file.exists(pngfile)){
    tempplot <- metacellHisto_HC(obj, pattern)
    createPNGFromWidget(tempplot, basename)
    }
  return(pngfile)
}


createPNG_logFCDistr <- function(obj,
                                 params=NULL,
                                 basename = NULL){
  if (is.null(basename)||is.null(obj)){return(NULL)}
  
  pngfile <- paste0(basename, ".png")
  pattern <- NULL
  if (!file.exists(pngfile)){
    tempplot <- metacellHisto_HC(obj, pattern)
    createPNGFromWidget(tempplot, basename)
  }
  return(pngfile)
}


createPNG_heatmap <- function(obj,params=NULL,basename = NULL){
  if (is.null(basename)||is.null(obj)){return(NULL)}
  
  pngfile <- paste0(basename, ".png")
  fullname <- normalizePath(paste(tempdir(), pngfile, sep="/"))
  
  if (!file.exists(pngfile)){
    plotPNG(function(){
      wrapper.heatmapD(obj)
    },
    filename=fullname,
    width = pngWidth,
    height=pngHeight,
    res=resolution)
  }
  return(pngfile)
}


###--------------------------------------------------------------------------
createPNG_Normalization <- reactive({
    obj1 <- rv$dataset[[(which(names(rv$dataset)==rv$iDat) - 1)]]
    obj2 <- rv$dataset[[rv$iDat]]
    rv$PlotParams$paletteForConditions
    
    plotPNG(function(){wrapper.compareNormalizationD(obj1, 
                                                     obj2, 
                                                     pal=rv$PlotParams$paletteForConditions)}, 
            filename=paste(tempdir(), gGraphicsFilenames$compareNorm, sep="/"), 
            width = 1200, 
            height=800,
            res=150)
    
    
    if (is.null(rv$tempplot$Density)) {
        tempplot <- DAPAR::densityPlotD_HC(obj2, 
                                           pal=rv$PlotParams$paletteForConditions)}
    else{ tempplot <- rv$tempplot$Density}
    createPNGFromWidget(tempplot, "tempplot_densityPlotAfterNorm.html", gGraphicsFilenames$densityPlotAfterNorm)
   
    
    if (is.null(rv$tempplot$boxplot)) {
      tempplot <- DAPAR::boxPlotD_HC(obj2,
                                     rv$PlotParams$legendForSamples,
                                     pal=rv$PlotParams$paletteForConditions)
    } else{ tempplot <- rv$tempplot$boxplot}
    
    createPNGFromWidget(tempplot, "temp_boxplot.html", gGraphicsFilenames$boxplotAfterNorm)

})


###--------------------------------------------------------------------------
createPNG_Imputation <- reactive({
obj <- rv$dataset[[(which(names(rv$dataset)==rv$iDat) - 1)]]
obj <- rv$dataset[[rv$iDat]]
    
tempplot <- wrapper.hc_mvTypePlot2(obj)
createPNGFromWidget(tempplot, "tempplot_MVtypePlot.html", gGraphicsFilenames$MVtypePlot)
})



###--------------------------------------------------------------------------
createPNG_Aggregation <- reactive({ obj <- rv$dataset[[rv$iDat]] })

###--------------------------------------------------------------------------
createPNG_DifferentialAnalysis <- reactive({
 th_logFC <- rv$current.obj@experimentData@other$Params[["HypothesisTest"]]$th_logFC

createPNGFromWidget(rv$tempplot$volcano, "tempplot_Volcano.html", gGraphicsFilenames$volcanoPlot)
    
plotPNG(function(){
        t <- rv$resAnaDiff$P_Value
        t <- t[which(abs(rv$resAnaDiff$logFC) >= th_logFC)]
        wrapperCalibrationPlot(t, "ALL")},
        filename=paste(tempdir(), gGraphicsFilenames$calibrationPlotAll, sep="/"),
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
pngNames <- c(gGraphicsFilenames$GOClassificationImg1,
              gGraphicsFilenames$GOClassificationImg2,
              gGraphicsFilenames$GOClassificationImg3)
    
require(doParallel)
registerDoParallel(detectCores()-1 )
foreach(i=1:l) %dopar% {
        tempplot <- barplotGroupGO_HC(rv$groupGO_data[[i]]$ggo_res, 
                                      title = paste("Groups at level ",  rv$groupGO_data[[i]]$level))
        file <- paste("tempplot_barplotGroupGO", i, ".html", sep="")
        createPNGFromWidget(tempplot, file, pngNames[i])
    }
})




