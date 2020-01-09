output$plotvarDistsmall <- renderImage({
  filename <- normalizePath(file.path('./images','desc_varDist.jpg'))
  list(src = filename,
       width = .width,
       height = .height)
}, deleteFile = FALSE)





output$plotvarDistlarge <- renderUI({
  tagList(
    helpText("Display the condition-wise distributions of the log-intensity CV (Coefficient of Variation) 
             of the protein/peptides."),
    helpText("For better visualization, it is possible to zoom in by click-and-drag."),
    withProgress(message = '',detail = '', value = 1, {
      highchartOutput(ns("viewDistCV"),width = plotWidth, height = plotHeight)
    })
    )
})




viewDistCV <- reactive({
  
  req(dataIn())
  rv$PlotParams$paletteConditions
  
  isolate({
    rv$tempplot$varDist <- wrapper.CVDistD_HC(dataIn(),rv$PlotParams$paletteConditions)
    })
  rv$tempplot$varDist
  
  
})


output$viewDistCV <- renderHighchart({
  viewDistCV()
  
})

