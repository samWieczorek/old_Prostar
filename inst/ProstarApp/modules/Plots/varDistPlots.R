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
    highchartOutput(ns("viewDistCV"),width = plotWidth, height = plotHeight) %>% withSpinner(type=spinnerType)
  )
})




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

