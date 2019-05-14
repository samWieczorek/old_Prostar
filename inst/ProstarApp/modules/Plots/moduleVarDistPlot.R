moduleVarDistPlotUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    helpText("Display the condition-wise distributions of the log-intensity CV (Coefficient of Variation) 
               of the protein/peptides."),
    helpText("For better visualization, it is possible to zoom in by click-and-drag."),
    highchartOutput(ns("viewDistCV"),width = plotWidth, height = plotHeight) %>% withSpinner(type=spinnerType)
  )

}



moduleVarDistPlot <- function(input, output, session, dataIn) {
  ns <- session$ns
  
  viewDistCV <- reactive({
    
    req(dataIn())
    rv.prostar$settings()$examplePalette
    
    isolate({
      rv.prostar$tempplot$varDist <- wrapper.CVDistD_HC(dataIn()$obj, rv.prostar$settings()$examplePalette)
    })
    rv.prostar$tempplot$varDist
  })
  
  
  output$viewDistCV <- renderHighchart({
    viewDistCV()
    
  })
  
  
  
}