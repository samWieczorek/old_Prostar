pcaPlotsUI <- function(id) {
  ns <- NS(id)
  
  
  tagList(
    uiOutput(ns("WarningNA_PCA")),
    uiOutput(ns("pcaOptions")),
    
    fluidRow(
      column(width=6,  imageOutput(ns("pcaPlotVar"))),
      column(width=6,  imageOutput(ns("pcaPlotInd")))
    ),
    fluidRow(
      column(width=6,  highchartOutput(ns("pcaPlotEigen"))),
      column(width=6,  mod_staticDT_ui(ns("PCAvarCoord")))
    )
  )
}




#------------------------------------------------------------
pcaPlots <- function(input, output, session, data) {
  ns <- session$ns
  
  rv.pca <- reactiveValues(
    PCA_axes =NULL,
    res.pca = NULL,
    PCA_varScale = NULL
  )
  
  mod_staticDT_server("PCAvarCoord",
                      data=reactive({
                        if (!is.null(rv.pca$res.pca))
                          round(rv.pca$res.pca$var$coord, digits=7)
                        }), 
                      filename = 'PCAVarCoords')
  
  output$pcaOptions <- renderUI({
    req(data())
    m <- match.metacell(DAPAR::GetMetacell(rv$current.obj), 
                        pattern="missing",
                        level = DAPAR::GetTypeofData(rv$current.obj)
    )
    tagList(
      
      if (length(which(m)) > 0)
      {
        tags$p("Warning: As your dataset contains missing values, the PCA cannot be computed.
               Please impute them first.")
      }
      else{
        
        tags$div(
          
          
          tags$div( style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                    numericInput(ns('pca.axe1'), "Dimension 1", min=1, max=Compute_PCA_dim(),value=1,width='100px')
          ),
          tags$div( style="display:inline-block; vertical-align: middle;",
                    numericInput(ns('pca.axe2'), "Dimension 2", min=1, max=Compute_PCA_dim(),value=2,width='100px')
          ),
          
          tags$div( style="display:inline-block; vertical-align: middle; padding-right: 20px;",
                    checkboxInput(ns('varScale_PCA'), "Variance scaling", value=rv.pca$PCA_varScale))
        )
        
      }
      
    )
    
})
  
  observeEvent(c(input$pca.axe1,input$pca.axe2),{
    rv.pca$PCA_axes <- c(input$pca.axe1,input$pca.axe2)
    })
  
  observeEvent(input$varScale_PCA,{
    rv.pca$PCA_varScale <- input$varScale_PCA
    rv.pca$res.pca <- wrapper.pca(data(), rv.pca$PCA_varScale, ncp=Compute_PCA_dim())
  })
  
  observeEvent(data(), {
    rv.pca$res.pca <- wrapper.pca(data(), rv.pca$PCA_varScale, ncp=Compute_PCA_dim())
  })
  
  
  
  
  Compute_PCA_dim <- reactive({
    nmax <- 12 # ncp should not be greater than... 
    # pour info, ncp = nombre de composantes ou de dimensions dans les r?sultats de l'ACP
    
    y <- exprs(data())
    nprot <- dim(y)[1]
    n <- dim(y)[2] # If too big, take the number of conditions.
    
    if (n > nmax){
      n <- length(unique(pData(data())$Condition))
    }
    
    ncp <- min(n, nmax)
    ncp
  })
  
  
  
  output$pcaPlotVar <- renderImage({
    
    req(rv.pca$PCA_axes)
    req(rv.pca$res.pca)
    outfile <- tempfile(fileext='.png')
    
    # Generate a png
    png(outfile)
    plotPCA_Var(rv.pca$res.pca, rv.pca$PCA_axes)
    dev.off()
    
    # Return a list
    list(src = outfile,
         alt = "This is alternate text")
  }, deleteFile = TRUE)
  
  output$pcaPlotInd <- renderImage({
    req(rv.pca$PCA_axes)
    req(rv.pca$res.pca)
    
    outfile <- tempfile(fileext='.png')
    
    # Generate a png
    png(outfile)
    plotPCA_Ind(rv.pca$res.pca, rv.pca$PCA_axes)
    dev.off()
    
    # Return a list
    list(src = outfile,
         alt = "This is alternate text")
  }, deleteFile = TRUE)
  
  
  output$pcaPlotEigen <- renderHighchart({
    req(rv.pca$res.pca)
    plotPCA_Eigen_hc(rv.pca$res.pca)
  })
  

  
  
}
