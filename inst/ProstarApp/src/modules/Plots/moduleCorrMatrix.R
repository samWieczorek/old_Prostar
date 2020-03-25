


moduleCorrMatrixUI <- function(id) {
  ns <- NS(id)
  tagList(
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
    highchartOutput(ns("corrMatrix"),width = plotWidth,height = plotHeight) 
  )
}

#------------------------------------------------------------
moduleCorrMatrix <- function(input, output, session, dataIn) {
  ns <- session$ns
  
  
  corrMatrix <- reactive({
    
    req(dataIn())
    input$expGradientRate
    
    gradient <- NULL
    if (is.null(input$expGradientRate)){gradient <- rv.prostar$settings()$corrMatrixGradient}
    else{
      gradient <- input$expGradientRate}
    isolate({
      pattern <- paste0(dataIn()$name,".corrMatrix")
      tmp <- wrapper.corrMatrixD_HC(dataIn()$obj,gradient)
      
    })
    tmp
  })
  
  
  output$corrMatrix <- renderHighchart({
    corrMatrix()
  }) 
  
}
