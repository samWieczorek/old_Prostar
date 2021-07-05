mod_staticDT_server("overview_DemoMode",
                    data = reactive({GetDatasetOverview()}),
                    filename = 'Demomode_Overview')


output$chooseDataset <- renderUI({
 
  if(!require("DAPARdata", lib.loc=DAPARdata.loc)){
    print("Installing DAPARdata")
    BiocManager::install("DAPARdata")
  }
  if(!require(DAPARdata))
    stop("Could not install the package DAPARdata")
  
  
    print("DAPARdata is loaded correctly")
    if(require(DAPARdata)){
      print("DAPARdata installed and loaded")
      selectInput("demoDataset",
                  "Demo dataset",
                  choices = c("None" = "None", utils::data(package="DAPARdata")$results[,"Item"]),
                  width='200px')
    }

})


output$linktoDemoPdf <- renderUI({
  req(input$demoDataset)
  if (input$demoDataset == "None"){return(NULL)}
  
  file<- paste(system.file(package = "DAPARdata"),"/doc/",
               input$demoDataset,".pdf", sep="")
  cmd <- paste("cp ",file," www/.", sep="")
    system(cmd)
  filename <-paste0(input$demoDataset,".pdf", sep="")
  tags$p("Dataset documentation ",
  tags$a(href=filename, target='_blank', paste0("(",filename,")")))
 })




output$infoAboutDemoDataset <- renderUI({
  req(rv$current.obj)
  
  isolate({ 
   
    m <- match.metacell(DAPAR::GetMetacell(rv$current.obj), 
                        pattern="missing", 
                        level = 'peptide')
    NA.count <- length(which(m))
  
  nb.empty.lines <- sum(apply(m, 1, all))
  
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
                mod_staticDT_ui("overview_DemoMode")
      )
    )
    
    
      )
    })
  
  
    })



observeEvent(input$loadDemoDataset,{

  req(input$demoDataset != "None")
  
  isolate({
    ntotal <- 4
   
  withProgress(message = '',detail = '', value = 0, {
    
    ClearUI()
    ClearMemory()
   incProgress(1/ntotal, detail = 'Clear memory ')
  utils::data(list = input$demoDataset)
  rv$current.obj <- get(input$demoDataset)
  incProgress(1/ntotal, detail = 'Load dataset ')
  
  rv$current.obj.name <- input$demoDataset
  rv$indexNA <- which(is.na(rv$current.obj))
  
  #rv$current.obj <- addOriginOfValue(rv$current.obj)
  l.params <- list(filename = input$demoDataset)
  incProgress(1/ntotal, detail = 'Configure dataset')
  
  loadObjectInMemoryFromConverter()
  incProgress(1/ntotal, detail = 'Load memory ')
  })
  })
  # shinyjs::disable("loadDemoDataset")
  # shinyjs::disable("chooseDataset")
  # shinyjs::disable("linktoDemoPdf")
})




