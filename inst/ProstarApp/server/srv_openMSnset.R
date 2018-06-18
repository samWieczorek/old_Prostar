output$openMSnsetScreen <- renderUI({
  
  tagList(
    fileInput("file", "Open a MSnset file", multiple = FALSE),
    moduleDatasetOverviewUI("overview_openMSnset"),
    uiOutput("infoAboutAggregationTool")
  )
})

