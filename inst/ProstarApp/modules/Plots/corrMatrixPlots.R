

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
    withProgress(message = 'Building plot...',detail = '', value = 1, {
      highchartOutput(ns("corrMatrix"),width = plotWidth,height = plotHeight)
    })
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
