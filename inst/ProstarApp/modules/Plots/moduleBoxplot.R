moduleBoxplotUI <- function(id) {
  ns <- NS(id)
  tagList(
    highchartOutput(ns("BoxPlot")),
    plotOutput(ns("viewViolinPlot")),
    selectInput(ns("choosePlot"), "Choose plot", choices=c( "violinplot"="violinplot","boxplot"="boxplot"), width='100px')
  )
}




#------------------------------------------------------------
moduleBoxplot <- function(input, output, session) {
  
  observeEvent(input$choosePlot, {
    switch(input$choosePlot,
           boxplot={
             shinyjs::hide('viewViolinPlot')
             shinyjs::show('BoxPlot')
           },
           violinplot={
             shinyjs::hide('BoxPlot')
             shinyjs::show('viewViolinPlot')
           }
    )
  })
  
  
  output$BoxPlot <- renderHighchart({
    req(GetCurrentMSnSet())
    rv$current.obj.name
    #rv$PlotParams$paletteConditions
    rv$PlotParams$legendForSamples
    tmp <- NULL
    isolate({
      pattern <- paste0(GetCurrentObjName(),".boxplot")
      tmp <- DAPAR::boxPlotD_HC(GetCurrentMSnSet(), rv$PlotParams$legendForSamples, palette=rv$settings()$examplePalette)
      #future(createPNGFromWidget(tmp,pattern))
      
      
    })
    tmp
  })
  
  
  output$viewViolinPlot <- renderPlot({
    
    req(GetCurrentMSnSet())
    rv$PlotParams$legendForSamples
    rv$settings()$examplePalette
    tmp <- NULL
    isolate({
      pattern <- paste0(GetCurrentObjName(),".violinplot")
      tmp <- DAPAR::violinPlotD(GetCurrentMSnSet(), rv$PlotParams$legendForSamples, palette=rv$settings()$examplePalette)
      #future(createPNGFromWidget(tmp,pattern))
    })
    tmp
  }) 
  
}
