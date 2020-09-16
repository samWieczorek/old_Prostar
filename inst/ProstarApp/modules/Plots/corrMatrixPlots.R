

output$plotcorrMatrixsmall <- renderImage({
  filename <- normalizePath(file.path('./images','desc_corrmatrix.png'))
  list(src = filename,
       width = .width,
       height = .height)
}, deleteFile = FALSE)



output$plotcorrMatrixlarge <- renderUI({
  tagList(
    tags$br(),tags$br(),
    tags$div(
      tags$div(style="display:inline-block; vertical-align: middle;",
               tags$p("Plot options")
      ),
      
      tags$div(style="display:inline-block; vertical-align: middle;",
               
               tags$div(
                 tags$div(style="display:inline-block; vertical-align: top; materiel-circle;",
                          shinyWidgets::dropdownButton(
                            tags$div(
                              tags$div(style="display:inline-block; vertical-align: bottom;",
                                       checkboxInput(ns("showValues"),
                                                     'Show values?',
                                                     value = TRUE),
                                       sliderInput(ns("expGradientRate"),
                                                   "Tune to modify the color gradient",
                                                   min = 0,max = 2,value = defaultGradientRate,step=0.01),
                                       tooltip="Plots parameters"
                                       
                              )
                            ),
                            tooltip="Plots parameters",
                            icon = icon("gear"), status = optionsBtnClass
                          ))
               )
               
      )
    ),
    withProgress(message = 'Building plot...',detail = '', value = 1, {
      highchartOutput(ns("corrMatrix"),width = plotWidth,height = plotHeight)
    })
  )
})


corrMatrix <- reactive({
  
  req(dataIn())
  input$expGradientRate
  input$showValues
  
  gradient <- NULL
  if (is.null(input$expGradientRate)){gradient <- defaultGradientRate}
  else{
    gradient <- input$expGradientRate}
  isolate({
    rv$tempplot$corrMatrix <- wrapper.corrMatrixD_HC(dataIn(),gradient,input$showValues)
    rv$tempplot$corrMatrix
  })
  
})

output$corrMatrix <- renderHighchart({
  corrMatrix()
}) 
