

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



output$linktoDemoPdf <- renderUI({
  req(input$demoDataset)
  
  file<- paste(system.file(package = "DAPARdata"),"/doc/",
               input$demoDataset,".pdf", sep="")
  cmd <- paste("cp ",file," www/.", sep="")
    system(cmd)
  filename <-paste0(input$demoDataset,".pdf", sep="")
  tags$p("Dataset documentation ",
  tags$a(href=filename, target='_blank', "(pdf)"))
 })


callModule(moduleStaticDataTable,"overview_DemoMode", table2show=reactive({GetDatasetOverview()}))


output$infoAboutDemoDataset <- renderUI({
  req(rv$current.obj)
  
  #isolate({ 
    NA.count <- length(which(is.na(Biobase::exprs(rv$current.obj))))
  
  nb.empty.lines <- sum(apply(is.na(as.matrix(exprs(rv$current.obj))), 1, all))
  
  tagList(
    tags$h3("Info"),
    if (rv$typeOfDataset == "protein"){
      tags$p("Note: the aggregation tool
             has been disabled because the dataset contains 
             protein quantitative data.")
    },
    
    if (NA.count > 0){
      tags$p("As your dataset contains missing values, you should 
             impute them prior to proceed to the differential analysis.")
    },
    if (nb.empty.lines > 0){
      tags$p("As your dataset contains lines with no values, you 
             should remove them with the filter tool
             prior to proceed to the analysis of the data.")
    },

    tags$div(
      tags$div( style="display:inline-block; vertical-align: top;",
                moduleStaticDataTableUI("overview_DemoMode")
      )
    )
    
      )
    #})
  
  
    })




observeEvent(input$loadDemoDataset,{
  
  ntotal <- 4
  withProgress(message = '',detail = '', value = 0, {
    
  ClearMemory()
  ClearUI()
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
