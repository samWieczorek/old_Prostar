callModule(moduleDatasetOverview,"overview_DemoMode")


output$chooseDataset <- renderUI({
  
  if(require("DAPARdata")){
    print("DAPARdata is loaded correctly")
    selectInput("demoDataset",
                "Demo dataset",
                choices = utils::data(package="DAPARdata")$results[,"Item"]
    )
  } else {
    print("trying to install DAPARdata")
    install.packages("DAPARdata")
    if(require(DAPARdata)){
      print("DAPARdata installed and loaded")
      selectInput("demoDataset",
                  "Demo dataset",
                  choices = utils::data(package='DAPARdata')$results[,"Item"])
    } else {
      stop("Could not install the package DAPARdata")
    }
  }
  
  
})




output$optionsDemomode <- renderUI({
  
  req(input$demoDataset)
  tagList(
    checkboxInput("showDemoDatasetPDF", "Show PDF documentation", value=FALSE),
    actionButton("loadDemoDataset", "Load demo dataset")
  )
})





output$showDatasetDoc <- renderUI({
  req(input$demoDataset)
  req(input$showDemoDatasetPDF)
  
  file<- paste(system.file(package = "DAPARdata"),"/doc/",
               input$demoDataset,".pdf", sep="")
  cmd <- paste("cp ",file," www/.", sep="")
  system(cmd)
  tags$iframe(src=paste(input$demoDataset,".pdf", sep=""), 
              width="900", height="700")
  
})





output$infoAboutDemoDataset <- renderUI({
  rv$current.obj
  rv$typeOfDataset
  if (is.null(rv$current.obj)) {return(NULL)    }
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
             impute them prior to proceed",br()," 
             to the differential analysis.")
    },
    if (nb.empty.lines > 0){
      tags$p("As your dataset contains lines with no values, you 
             should remove them with the filter",br()," tool
             prior to proceed to the analysis of the data.")
    }
    
      )
    })