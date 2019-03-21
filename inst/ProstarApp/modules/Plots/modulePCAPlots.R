pcaPlotsUI <- function(id) {
  ns <- NS(id)
  
  
  tagList(
    uiOutput(ns("WarningNA_PCA")),
    uiOutput(ns("pcaOptions")),
    
    fluidRow(
      column(width=6,  plotOutput(ns("pcaPlotVar"))),
      column(width=6,  plotOutput(ns("pcaPlotInd")))
    ),
    fluidRow(
      column(width=6,  highchartOutput(ns("pcaPlotEigen"))),
      column(width=6,  moduleStaticDataTableUI(ns("PCAvarCoord")))
    )
  )
}




#------------------------------------------------------------
pcaPlots <- function(input, output, session, data) {
  
  
  rv.pca <- reactiveValues(
    PCA_axes =NULL,
    res.pca = NULL,
    PCA_varScale = NULL
  )
  
  callModule(moduleStaticDataTable,"PCAvarCoord", table2show=reactive({if (!is.null(rv.pca$res.pca)) round(rv.pca$res.pca$var$coord, digits=7)}), showRownames=TRUE)
  
  
  
  observeEvent(c(input$pca.axe1,input$pca.axe2),{
    rv$PCA_axes <- c(input$pca.axe1,input$pca.axe2)})
  
  observeEvent(input$varScale_PCA,{
    rv.pca$PCA_varScale <- input$varScale_PCA
    rv.pca$res.pca <- wrapper.pca(data(), rv.pca$PCA_varScale, ncp=Compute_PCA_nbDimensions())
  })
  
  observeEvent(data(), {
    rv.pca$res.pca <- wrapper.pca(data(), rv.pca$PCA_varScale, ncp=Compute_PCA_nbDimensions())
  })
  
  
  
  
  Compute_PCA_nbDimensions <- reactive({
    nmax <- 12 # ncp should not be greater than... 
    # pour info, ncp = nombre de composantes ou de dimensions dans les r?sultats de l'ACP
    
    y <- exprs(data())
    nprot <- dim(y)[1]
    n <- dim(y)[2] # If too big, take the number of conditions.
    
    if (n > nmax){
      n <- length(unique(Biobase::pData(data())$Condition))
    }
    
    
    ncp <- min(n, nmax)
    ncp
  })
  
  
  
  
  output$pcaPlotVar <- renderPlot({
    req(rv.pca$PCA_axes)
    req(rv.pca$res.pca)
    
    plotPCA_Var(rv.pca$res.pca, rv.pca$PCA_axes)
    
  })
  
  output$pcaPlotInd <- renderPlot({
    req(rv.pca$PCA_axes)
    req(rv.pca$res.pca)
    
    plotPCA_Ind(rv.pca$res.pca, rv.pca$PCA_axes)
    
  })
  
  
  output$pcaPlotEigen <- renderHighchart({
    req(rv$res.pca)
    plotPCA_Eigen_hc(rv.pca$res.pca)
  })
  
  output$pcaOptions <- renderUI({
    req(data())
    
    tagList(
      
      if (length(which(is.na(Biobase::exprs(data())))) > 0)
      {
        tags$p("Warning: As your dataset contains missing values, the PCA cannot be computed.
               Please impute them first")
      }
      else{
        
        tags$div(
          
          
          tags$div( style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                    numericInput(ns('pca.axe1'), "Dimension 1", min=1, max=Compute_PCA_nbDimensions(),value=1,width='100px')
          ),
          tags$div( style="display:inline-block; vertical-align: middle;",
                    numericInput(ns('pca.axe2'), "Dimension 2", min=1, max=Compute_PCA_nbDimensions(),value=2,width='100px')
          ),
          
          tags$div( style="display:inline-block; vertical-align: middle; padding-right: 20px;",
                    checkboxInput(ns('varScale_PCA'), "Variance scaling", value=rv.pca$PCA_varScale))
        )
        
      }
      
    )
    
})
  
  
}
