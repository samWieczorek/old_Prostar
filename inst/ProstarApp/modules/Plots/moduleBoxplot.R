moduleBoxplotUI <- function(id) {
  ns <- NS(id)
  tagList(
    highchartOutput(ns("BoxPlot")),
    plotOutput(ns("viewViolinPlot")),
    selectInput(ns("choosePlot"), "Choose plot", choices=c( "violinplot"="violinplot","boxplot"="boxplot"), width='100px')
  )
}




#------------------------------------------------------------
moduleBoxplot <- function(input, output, session, dataIn) {
  ns <- session$ns
  
  
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
    req(dataIn()$obj)
    #rv$PlotParams$paletteConditions
    rv.prostar$settings()$legendForSamples
    name <- dataIn()$name
    tmp <- NULL
    print("rererererere")
    isolate({
      pattern <- paste0(name,".boxplot")
      tmp <- DAPAR::boxPlotD_HC(dataIn()$obj, rv.prostar$settings()$legendForSamples, palette=rv.prostar$settings()$examplePalette)
      #future(createPNGFromWidget(tmp,pattern))
      
      
    })
    tmp
  })
  
  
  output$viewViolinPlot <- renderPlot({
    
    req(dataIn())
    rv.prostar$settings()$legendForSamples
    rv.prostar$settings()$examplePalette
    tmp <- NULL
    isolate({
      pattern <- paste0(dataIn()$name,".violinplot")
      tmp <- DAPAR::violinPlotD(dataIn()$obj, rv.prostar$settings()$legendForSamples, palette=rv.prostar$settings()$examplePalette)
      #future(createPNGFromWidget(tmp,pattern))
    })
    tmp
  }) 
  
}
