######
modulePlotsUI <- function(id){
  ns <- NS(id)
  
    uiOutput(ns("plotModule"))

}




####-----------------------------------------------------------####

modulePlots <- function(input, output, session, dataIn, llPlots){
  ns <- session$ns
  
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
        #tags$head(tags$style(".modal-dialog{ width:100%}")),
        #tags$head(tags$style(".modal-body{ min-height:50%}")),
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
             ll[[n]] <- shinyBS::bsModal("modalpca", "PCA", ns("plotpcasmall"), size = "large",plotOutput(ns("plotpcalarge"))),
           varDist = 
             ll[[n]] <- shinyBS::bsModal("modalvarDist", "Variance distribution", ns("plotvarDistsmall"), size = "large",plotOutput(ns("plotvarDistlarge"))),
           corrMatrix=
             ll[[n]] <- shinyBS::bsModal("modalcorrMatrix", "Correlation matrix", ns("plotcorrMatrixsmall"), size = "large", uiOutput(ns("plotcorrMatrixlarge"))),
           heatmap = 
             ll[[n]] <- shinyBS::bsModal("modalheatmap", "Heatmap", ns("plotheatmapsmall"), size = "large",plotOutput(ns("plotheatmaplarge"))),
           mv = 
             ll[[n]] <- shinyBS::bsModal(ns("modalmv"), "Missing values statistics", ns("plotmvsmall"), size = "large",plotOutput(ns("plo.mvlarge")))
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
  
  output$plotintensitysmall <- renderImage({
    filename <- normalizePath(file.path('./images','desc_intdistrib.png'))
    list(src = filename,
         width = .width,
         height = .height)
  }, deleteFile = FALSE)


  output$plotcorrMatrixsmall <- renderImage({
    filename <- normalizePath(file.path('./images','desc_corrmatrix.png'))
    list(src = filename,
         width = .width,
         height = .height)
  }, deleteFile = FALSE)
  
  output$plotmvsmall <- renderImage({
    filename <- normalizePath(file.path('./images','desc_mv.png'))
    list(src = filename,
         width = .width,
         height = .height)
  }, deleteFile = FALSE)
  
 
  output$plotheatmapsmall <- renderImage({
    filename <- normalizePath(file.path('./images','desc_heatmap.png'))
    list(src = filename,
         width = .width,
         height = .height)
  }, deleteFile = FALSE)

  
  output$plotpcasmall <- renderImage({
    filename <- normalizePath(file.path('./images','desc_pca.png'))
    list(src = filename,
         width = .width,
         height = .height)
  }, deleteFile = FALSE)
  
  output$plotvarDistsmall <- renderImage({
    filename <- normalizePath(file.path('./images','desc_varDist.jpg'))
    list(src = filename,
         width = .width,
         height = .height)
  }, deleteFile = FALSE)
  
  
  
  ###### definition of large plots
  
  
  output$plotquantiTablelarge <- renderPlot({
    boxplot(iris, main = "Data 2")
  })

  output$plotintensitylarge <- renderPlot({
    hist(rnorm(8), main = "Data 1")
   })
  
  
  output$plotmvlarge <- renderPlot({
    hist(rnorm(100), main = "Data 3")
  })
  
  
  output$plotheatmaplarge <- renderPlot({
    boxplot(iris, main = "Plot 4 large")
  })
  
  output$plotpcalarge <- renderPlot({
    hist(rnorm(8), main = "Plot 5 large")
  })
  
  
  output$plotvarDistlarge <- renderPlot({
    hist(rnorm(100), main = "Plot 6 large")
  })
  
  
  output$plotcorrMatrixlarge <- renderUI({
      tagList(
        tags$br(),tags$br(),
        tags$div(
          tags$div(style="display:inline-block; vertical-align: middle;",
                   tags$p("Plot options")
          ),
          
          tags$div(style="display:inline-block; vertical-align: middle;",
                   
                   tags$div(
                     tags$div(style="display:inline-block; vertical-align: top;",
                              shinyWidgets::dropdownButton(
                                tags$div(
                                  tags$div(style="display:inline-block; vertical-align: bottom;",
                                           sliderInput(ns("expGradientRate"),
                                                       "Tune to modify the color gradient",
                                                       min = 0,max = 1,value = defaultGradientRate,step=0.01),
                                           tooltip="Plots parameters",
                                           style = "material-circle", icon = icon("gear"), status = optionsBtnClass
                                           
                                  )
                                ),
                                tooltip="Plots parameters",
                                style = "material-circle", icon = icon("gear"), status = optionsBtnClass
                              ))
                   )
                   
          )
        ),
        highchartOutput(ns("corrMatrix"),width = plotWidth,height = plotHeight) %>% withSpinner(type=spinnerType)
      )
    })


  
  corrMatrix <- reactive({
    
    req(dataIn())
    input$expGradientRate
    
    gradient <- NULL
    if (is.null(input$expGradientRate)){gradient <- defaultGradientRate}
    else{
      gradient <- input$expGradientRate}
    isolate({
      rv$tempplot$corrMatrix <- wrapper.corrMatrixD_HC(dataIn(),gradient)
      rv$tempplot$corrMatrix
    })
    
  })
  
  
  output$corrMatrix <- renderHighchart({
    corrMatrix()
  }) 
  

  return(NULL)
}