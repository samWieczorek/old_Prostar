######
modulePlotsUI <- function(id){
  ns <- NS(id)
  
    uiOutput(ns("plotModule"))

}




####-----------------------------------------------------------####

modulePlots <- function(input, output, session, dataIn, llPlots){
  ns <- session$ns
  
  
  source(file.path(".", "modules/Plots/corrMatrixPlots.R"),  local = TRUE)$value
  source(file.path(".", "modules/Plots/mvPlots.R"),  local = TRUE)$value
   source(file.path(".", "modules/Plots/varDistPlots.R"),  local = TRUE)$value
  source(file.path(".", "modules/Plots/pcaPlots.R"),  local = TRUE)$value
  source(file.path(".", "modules/Plots/intdistribPlots.R"),  local = TRUE)$value
  source(file.path(".", "modules/Plots/heatmapPlots.R"),  local = TRUE)$value
  
  .width <- 50
  .height <- 50
  
  
  
  
  output$plotModule <- renderUI({
    req(dataIn())
    
    panelheight = 60*length(llPlots())
      absolutePanel(
        id  = "#AbsolutePanelPlots",
        style= "text-align: center; color: grey; border-width:0px;",
        top = 350, right = 50, width = "70px",height = paste0(as.character(panelheight), "px"),
        draggable = TRUE,fixed = TRUE,
        cursor = "default",
        tags$head(tags$style(".modal-dialog{ width:100%}")),
        tags$head(tags$style(".modal-body{ min-height:50%}")),
        actionButton(ns('plotBtn'), 'Plots', "data-toggle"='collapse', "data-target"=paste0('#',ns('plotDiv')), 
                   style='color: white;background-color: lightgrey',
                   class = actionBtnClass),
        tags$div(
          id = ns('plotDiv'),  
          class="collapse", 
          style='background-color: white',
          uiOutput(ns('createVignettes'))
          )
    )

  })
  
  
  
  output$createVignettes <- renderUI({

    ll <- list(NULL)
    
    vHeight <- 60
    vWidth <- 50
    
    for (i in 1:length(llPlots())) {
      switch(llPlots()[i],
             quantiTable=ll[[i]] <- tags$div( style="display:inline-block;",imageOutput(ns("plotquantiTablesmall"), height='60', width='50')),
             intensity=ll[[i]] <- tags$div( style="display:inline-block;",imageOutput(ns("plotintensitysmall"), height='60', width='50')),
             pca = ll[[i]] <- tags$div( style="display:inline-block;",imageOutput(ns("plotpcasmall"), height='60', width='50')),
             varDist = ll[[i]] <- tags$div( style="display:inline-block;",imageOutput(ns("plotvarDistsmall"), height='60', width='50')),
             corrMatrix=ll[[i]] <- tags$div( style="display:inline-block;",imageOutput(ns("plotcorrMatrixsmall"), height='60', width='50')),
             heatmap = ll[[i]] <- tags$div( style="display:inline-block;",imageOutput(ns("plotheatmapsmall"), height='60', width='50')),
             mv = ll[[i]] <- tags$div( style="display:inline-block;",imageOutput(ns("plotmvsmall"), height='60', width='50'))
      )
    }
    
    
    for (i in 1:length(llPlots())) {
      n <- i + length(llPlots())
      switch(llPlots()[i],
           quantiTable=
             ll[[n]] <- shinyBS::bsModal("modalquantiTable", "Data explorer", ns("plotquantiTablesmall"), size = "large",plotOutput(ns("plotquantiTablelarge"))),
           intensity=
             ll[[n]] <- shinyBS::bsModal("modalintensity", "Intensities distribution", ns("plotintensitysmall"), size = "large",plotOutput(ns("plotintensitylarge"))),
           pca = 
             ll[[n]] <- shinyBS::bsModal("modalpca", "PCA", ns("plotpcasmall"), size = "large",uiOutput(ns("plotpcalarge"))),
           varDist = 
             ll[[n]] <- shinyBS::bsModal("modalvarDist", "Variance distribution", ns("plotvarDistsmall"), size = "large",uiOutput(ns("plotvarDistlarge"))),
           corrMatrix=
             ll[[n]] <- shinyBS::bsModal("modalcorrMatrix", "Correlation matrix", ns("plotcorrMatrixsmall"), size = "large", uiOutput(ns("plotcorrMatrixlarge"))),
           heatmap = 
             ll[[n]] <- shinyBS::bsModal("modalheatmap", "Heatmap", ns("plotheatmapsmall"), size = "large",uiOutput(ns("plotheatmaplarge"))),
           mv = 
             ll[[n]] <- shinyBS::bsModal(ns("modalmv"), "Missing values statistics", ns("plotmvsmall"), size = "large",uiOutput(ns("plotmvlarge")))
           )
    }
    
    # jqui_resizable(paste0("#",ns("modalmv")," .modal-content"), options = list(minHeight = 100, maxHeight = 300,
    #                                                          minWidth = 200, maxWidth = 400))
    # jqui_resizable("#modalintensity .modal-content", options = list(handles = 'e'))
    # jqui_resizable("#modalquantiTable .modal-content", options = list(handles = 'e'))
    # jqui_resizable("#modalpca .modal-content", options = list(handles = 'e'))
    # jqui_resizable("#modalvarDist .modal-content", options = list(handles = 'e'))
    # jqui_resizable("#modalcorrMatrix .modal-content", options = list(handles = 'e'))
    # jqui_resizable("#modalheatmap .modal-content", options = list(handles = 'e'))
    
   
   ll 
    
  })
  
  
  output$plotquantiTablesmall <- renderImage({
    filename <- normalizePath(file.path('./images','desc_quantiData.png'))
    list(src = filename,
         width = .width,
         height = .height)
  }, deleteFile = FALSE)
  
  

  
 
 
  
  
  
  ###### definition of large plots
  
  
  output$plotquantiTablelarge <- renderPlot({
    boxplot(iris, main = "Data 2")
  })

 
  
 
  

  
  

  return(NULL)
}