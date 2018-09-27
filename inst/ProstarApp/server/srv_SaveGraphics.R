

output$test2 <- renderUI({
  shinyjs::disable("downloadReport")
})



######---------------------------------------------------------------------
observeEvent(input$generateReport,{
 # req(input$chooseDatasetToExport)
  rv$dataset
  #input$whichGO2Save
  
  #nSteps <- 1 + length(input$chooseDatasetToExport)+ (Get_ParamValue('params.anaDiff','Condition1') != "")
  #print(nSteps)
  #addResourcePath('Rmd_sources', normalizePath('Rmd_sources'))
  #withProgress(message = 'Calculation in progress',detail = '', value = 0, {
                 
  ## Init the Rmd file for the report
  src <- file.path("Rmd_sources", "report.Rmd")
  rv$outfile <- tempfile(pattern = "report",fileext='.Rmd')
  file.create(rv$outfile)
  file.copy(src, rv$outfile, overwrite = TRUE)
                 
  if (!is.null(rv$current.obj)){
      txt2Rmd <- readLines("Rmd_sources/summary.Rmd")
      write(txt2Rmd, file = rv$outfile, append = TRUE, sep = "\n")
  }            
     
  ntotal <- 0
  for (i in 1:length(names(rv$dataset))){
    widgetName <- paste0('plotsFor.',names(rv$dataset)[i])
    widgetName <- gsub(".", "_", widgetName, fixed=TRUE)
    ll_plots <- unlist(shinyTree::get_selected(input[[widgetName]]))
    ntotal <- ntotal + length(ll_plots)
  }
  
  withProgress(message = '',detail = '', value = 0, {
    ### for each available dataset, create the selected plots
    for (i in 1:length(names(rv$dataset))){
      ll_plots <- unlist(shinyTree::get_selected(input[[widgetName]]))
         for (plot.name in ll_plots) {
          
            obj <- rv$dataset[[names(rv$dataset)[i]]]
            pattern <- paste0(names(rv$dataset)[i],".", plot.name)
            incProgress(1/ntotal, detail = paste0('Building ',pattern))
            params <- NULL
            #params <- GetParamsForPlot()
            #pngfile <- createPNG_BoxplotHC(obj,params,basename = pattern)
            print(paste0("createPNG_",plot.name))
            pngfile <- do.call(paste0("createPNG_",plot.name),
                            list(obj=obj,params=params,basename = pattern))
            
            addPNG_to_Rmd(pngfile, paste0("fig_",pattern), rv$outfile)
        
          # future(do.call(paste0("createPNG_",plot.name),
          #                list(obj=obj,params=params,basename = pattern))) %...>%
          #   addPNG_to_Rmd(paste0("fig_",pattern), rv$outfile)
        }
    }
  })         
           
                   ## Insert differential analysis
                   # if (Get_ParamValue('params.anaDiff','Condition1') != "") {
                   #   createPNG_DifferentialAnalysis()
                   #  txt2Rmd <- readLines("Rmd_sources/anaDiff_Rmd.Rmd")
                   #   write(txt2Rmd, file = rv$outfile,append = TRUE, sep = "\n")
                   #   #incProgress(1/nSteps, detail = paste("Computing differential analysis"))
                   #   
                   # }
                 
                 
                   ## Insert GO analysis
                   # if(!is.null){
                   #   switch(input$whichGO2Save,
                   #        Both={
                   #          createPNG_Enrichment()
                   #          createPNG_GroupGO()
                   #          txt2Rmd <- readLines("Rmd_sources/GO_Classification_Rmd.Rmd")
                   #          filename <- paste(tempdir(), 'report.Rmd',sep="/")
                   #          write(txt2Rmd, file = filename,append = TRUE, sep = "\n")
                   #          
                   #          txt2Rmd <- readLines("Rmd_sources/GO_Enrichment_Rmd.Rmd")
                   #          filename <- paste(tempdir(), 'report.Rmd',sep="/")
                   #          write(txt2Rmd, file = filename,append = TRUE, sep = "\n")
                   #        },
                   #        Classification={
                   #          createPNG_GroupGO()
                   #          txt2Rmd <- readLines("Rmd_sources/GO_Classification_Rmd.Rmd")
                   #          filename <- paste(tempdir(), 'report.Rmd',sep="/")
                   #          write(txt2Rmd, file = filename,append = TRUE, sep = "\n")
                   #        },
                   #        Enrichment={
                   #          createPNG_Enrichment()
                   #          txt2Rmd <- readLines("Rmd_sources/GO_Enrichment_Rmd.Rmd")
                   #          filename <- paste(tempdir(), 'report.Rmd',sep="/")
                   #          write(txt2Rmd, file = filename,append = TRUE, sep = "\n")
                   #        },
                   #        default=NULL
                   # )
                   # }
               
                 
         # })

  shinyjs::enable("downloadReport")
  #setwd(wd)
})


addPNG_to_Rmd <- function(pngfilename, tagRmd, outfile){
  txt <- paste0("```{r ",tagRmd,",out.width = \"75%\", echo=FALSE}
  filename <-  normalizePath(paste(tempdir(), '",pngfilename,"', sep=\"/\"))
                if (file.exists(filename)){knitr::include_graphics(filename) }
                ```")
  write(txt, file = outfile,append = TRUE, sep = "\n")
}




createPNG_PCA_Var <- function(obj,params=NULL,basename = NULL){
 # if (is.null(basename)||is.null(obj)){return(NULL)}
  req(c(basename, obj,rv$res.pca  ))
  
  pngfile <- paste0(basename, ".png")
  fullname <- normalizePath(paste(tempdir(), pngfile, sep="/"))
  if (!file.exists(pngfile)){
    png (fullname)
    print(plot.pca.var(rv$res.pca, rv$PCA_axes))
    dev.off ()
    
    #width = pngWidth,
    #height=pngHeight,
    #res=resolution
    
  }
  return(pngfile)
}



createPNG_PCA_Ind <- function(obj,params=NULL,basename = NULL){
  if (is.null(basename) || is.null(obj) || is.null(rv$res.pca)){return(NULL)}
  #if (is.null(basename)||is.null(obj)){return(NULL)}
  print(rv$res.pca)
  print(rv$PCA_axes)
  pngfile <- paste0(basename, ".png")
  fullname <- normalizePath(paste(tempdir(), pngfile, sep="/"))
  if (!file.exists(pngfile)){
    png (fullname)
    print(plot.pca.ind(rv$res.pca, rv$PCA_axes))
    dev.off ()
  }
  return(pngfile)
}




###----------------------------------------------------------
createPNG_PCA_Eigen <- function(obj,params=NULL,basename = NULL){
  if (is.null(basename)||is.null(obj)){return(NULL)}
  pngfile <- paste0(basename, ".png")
  if (!file.exists(pngfile)){
    tempplot <- DAPAR::plot.pca.eigen.hc(rv$res.pca)
    createPNGFromWidget(tempplot,basename) 
  }
  return(pngfile)
}


###----------------------------------------------------------
createPNG_BoxplotHC <- function(obj,params=NULL,basename = NULL){
  if (is.null(basename)||is.null(obj)){return(NULL)}
  pngfile <- paste0(basename, ".png")
  if (!file.exists(pngfile)){
    tempplot <- DAPAR::boxPlotD_HC(obj, palette=rv$PlotParams$paletteConditions)
    createPNGFromWidget(tempplot,basename) 
  }
  return(pngfile)
}




createPNG_boxplot <- function(obj,params=NULL,basename = NULL){
  if (is.null(basename)||is.null(obj)){return(NULL)}
  pngfile <- paste0(basename, ".png")
  if (!file.exists(pngfile)){
    tempplot <- DAPAR::boxPlotD_HC(obj, legend = rv$PlotParams$legendForSamples, palette=rv$PlotParams$paletteConditions)
    createPNGFromWidget(tempplot,basename) 
  }
  return(pngfile)
}

###----------------------------------------------------------
createPNG_densityplot <- function(obj,params=NULL,basename = NULL){
  if (is.null(basename)||is.null(obj)){return(NULL)}
  
  pngfile <- paste0(basename, ".png")
  
  if (!file.exists(pngfile)){
    tempplot <- DAPAR::densityPlotD_HC(obj, legend = rv$PlotParams$legendForSamples, palette=rv$PlotParams$paletteConditions)
    createPNGFromWidget(tempplot, basename)
    }
  return(pngfile)
}


createPNG_CVDistr <- function(obj,params=NULL,basename = NULL){
  if (is.null(basename)||is.null(obj)){return(NULL)}
  
  pngfile <- paste0(basename, ".png")
  
  if (!file.exists(pngfile)){
    tempplot <- wrapper.CVDistD_HC(obj,palette=rv$PlotParams$paletteConditions)
    createPNGFromWidget(tempplot, basename)
    }
  return(pngfile)
}


createPNG_violinplot <- function(obj,params=NULL,basename = NULL){
  if (is.null(basename)||is.null(obj)){return(NULL)}
  
  pngfile <- paste0(basename, ".png")
  fullname <- normalizePath(paste(tempdir(), pngfile, sep="/"))
  if (!file.exists(pngfile)){
    plotPNG(function(){
      DAPAR::violinPlotD(obj, legend = rv$PlotParams$legendForSamples, palette=rv$PlotParams$paletteConditions)
    },
    filename=fullname
    #width = pngWidth,
    #height=pngHeight,
    #res=resolution
    )
    }
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

createPNG_MVPlot3 <- function(obj,params=NULL,basename = NULL){
  if (is.null(basename)||is.null(obj)){return(NULL)}
  
  pngfile <- paste0(basename, ".png")
   
  if (!file.exists(pngfile)){
    tempplot <- wrapper.mvHisto_HC(obj)
    createPNGFromWidget(tempplot, basename)
    }
  return(pngfile)
}


createPNG_logFCDistr <- function(obj,params=NULL,basename = NULL){
  if (is.null(basename)||is.null(obj)){return(NULL)}
  
  pngfile <- paste0(basename, ".png")
  
  if (!file.exists(pngfile)){
    tempplot <- wrapper.mvHisto_HC(obj)
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
    rv$PlotParams$paletteConditions
    
    plotPNG(function(){wrapper.compareNormalizationD(obj1, obj2, palette=rv$PlotParams$paletteConditions)}, 
            filename=paste(tempdir(), gGraphicsFilenames$compareNorm, sep="/"), 
            width = 1200, 
            height=800,
            res=150)
    
    
    if (is.null(rv$tempplot$Density)) {
        tempplot <- DAPAR::densityPlotD_HC(obj2, palette=rv$PlotParams$paletteConditions)}
    else{ tempplot <- rv$tempplot$Density}
    createPNGFromWidget(tempplot, "tempplot_densityPlotAfterNorm.html", gGraphicsFilenames$densityPlotAfterNorm)
   
    
    if (is.null(rv$tempplot$boxplot)) {
      tempplot <- DAPAR::boxPlotD_HC(obj2,rv$PlotParams$legendForSamples, palette=rv$PlotParams$paletteConditions)
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



######-----------------------------------------------------------------
output$downloadReport <- downloadHandler(
    input$reportFilename,
    filename = function() {
        paste(input$reportFilename, sep = '.', switch(
            input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
        ))
    },
    
    content = function(file) {
        filename <- rv$outfile
        print(filename)
        library(rmarkdown)
        #paramRmd <- list(current.obj=rv$current.obj)
        out <- rmarkdown::render(rv$outfile, 
                      output_file = file,
                      switch(
                          input$format,
                          PDF = pdf_document(), 
                          HTML = html_document(), 
                          Word = word_document()
                      )
                  ) # END render
        #file.rename(out, file)
    }
)

