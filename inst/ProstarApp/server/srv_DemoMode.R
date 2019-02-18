
output$chooseDataset <- renderUI({
  
  if(require("DAPARdata", lib.loc=DAPARdata.loc)){
    print("DAPARdata is loaded correctly")
    selectInput("demoDataset",
                "Demo dataset",
                choices = utils::data(package="DAPARdata")$results[,"Item"],
                width='200px'    )
  } else {
    print("trying to install DAPARdata")
    BiocManager::install("DAPARdata")
    if(require(DAPARdata)){
      print("DAPARdata installed and loaded")
      selectInput("demoDataset",
                  "Demo dataset",
                  choices = utils::data(package='DAPARdata')$results[,"Item"],
                  width='200px'   )
    } else {
      stop("Could not install the package DAPARdata")
    }
  }
  
  
})


observeEvent(input$loadDemoDataset,{
  
  ntotal <- 4
  withProgress(message = '',detail = '', value = 0, {
    
  ClearMemory()
  #ClearUI()
  incProgress(1/ntotal, detail = 'Clear memory ')
  utils::data(list = input$demoDataset)
  rv$current.obj <- get(input$demoDataset)
  incProgress(1/ntotal, detail = 'Load dataset ')
  
  rv$current.obj.name <- input$demoDataset
  rv$indexNA <- which(is.na(rv$current.obj))
  
  rv$current.obj <- addOriginOfValue(rv$current.obj)
  l.params <- list(filename = input$demoDataset)
  incProgress(1/ntotal, detail = 'Configure dataset')
  
  loadObjectInMemoryFromConverter()
  incProgress(1/ntotal, detail = 'Load memory ')
  
  })
})
