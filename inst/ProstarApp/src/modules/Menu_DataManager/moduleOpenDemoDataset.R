#source(file.path("./src", "modules/Menu_DataManager/funcs.R"),  local = TRUE)$value


moduleOpenDemoDatasetUI  <- function(id){
  ns <- NS(id)
  
  
  tagList(
    moduleChoosePipelineUI(ns("choosePipe")),
    
    tags$div(
      tags$div( style="display:inline-block; vertical-align: middle; padding-right: 20px;",
                uiOutput(ns("chooseDemoDataset"))
      ),
      tags$div( style="display:inline-block; vertical-align: middle; padding-right: 20px;",
                p(""),
                actionButton(ns("loadDemoDataset"), "Load demo dataset",class = actionBtnClass)
      ),
      tags$div( style="display:inline-block; vertical-align: middle;",
                p(""),
                uiOutput(ns("linktoDemoPdf"))
      )
    ),
     
    moduleInfoDatasetUI(ns("infoAboutMSnset")),
    div( style="display:inline-block; vertical-align: top;",
         moduleStaticDataTableUI(ns("overview_DemoMode"))
    )
  )
}



moduleOpenDemoDataset  <- function(input, output, session){
  ns <- session$ns
  
  rv.openDemo <- reactiveValues(
    dataOut = NULL,
    pipe = NULL
  )
  
  
  rv.openDemo$pipe <- callModule(moduleChoosePipeline, "choosePipe", reactive({G_path_to_pipeline_conf}))
  
  callModule(moduleStaticDataTable,"overview_DemoMode",
             table2show=reactive({req(rv.openDemo$dataOut)
                                GetDatasetOverview2(rv.openDemo$dataOut[['original']])}))
  
  callModule(moduleInfoDataset, "infoAboutMSnset",
              obj = reactive({req(rv.openDemo$dataOut)
                rv.openDemo$dataOut[['original']]}))
    


  ### function for demo mode
  output$chooseDemoDataset <- renderUI({
    
    if(require("DAPARdata", lib.loc=DAPARdata.loc)){
      print("DAPARdata is loaded correctly")
      selectInput(ns("demoDataset"),
                  "Demo dataset",
                  choices = utils::data(package="DAPARdata")$results[,"Item"],
                  width='200px')
    } else {
      print("Trying to install DAPARdata")
      BiocManager::install("DAPARdata")
      if(require(DAPARdata)){
        print("DAPARdata installed and loaded")
        selectInput(ns("demoDataset"),
                    "Demo dataset",
                    choices = utils::data(package='DAPARdata')$results[,"Item"],
                    width='200px'   )
      } else {
        stop("Could not install the package DAPARdata")
      }
    }
   
  })
  
  
  
  
  
  
  observeEvent(req(input$loadDemoDataset), {
    
    
    nSteps <- length(def.progress.loadDataset)
     withProgress(message = '',detail = '', value = 0, {
      #ClearMemory()
      #ClearUI()
      incProgress(1/nSteps, detail = def.progress.loadDataset[1])
      utils::data(list = input$demoDataset)
      data <- get(input$demoDataset)
      if (class(data)[1]!="MSnSet") {
        shinyjs::info("Warning : this file is not a MSnSet file ! 
                      Please choose another one.")
        return(NULL)
      }
      proteinID <- data@experimentData@other$proteinId
      typeOfData <- data@experimentData@other$typeOfData
      ll.pipeline <- rv.openDemo$pipe
      switch(typeOfData,
             protein = {
                   rv.openDemo$dataOut <- PipelineProtein(analysis= input$demoDataset, 
                                                              pipelineType = name(ll.pipeline), 
                                                              dataType ='protein',
                                                              processes=unlist(ll.pipeline), 
                                                              experiments=list(original=data), 
                                                              colData=Biobase::pData(data))
                  },
             peptide = {
                   rv.openDemo$dataOut <- PipelinePeptide(analysis= input$demoDataset, 
                                                          pipelineType = name(ll.pipeline), 
                                                          dataType ='peptide',
                                                          processes=unlist(ll.pipeline),
                                                          proteinID = proteinID,
                                                          experiments=list(original=data), 
                                                          colData=Biobase::pData(data))
                 }
          ) # end swith

  }) # End withProgress
     
  }) # End observeEvent

  
  output$linktoDemoPdf <- renderUI({
    req(input$demoDataset)
    
    file<- paste(system.file(package = "DAPARdata"),"/doc/",
                 input$demoDataset,".pdf", sep="")
    cmd <- paste("cp ",file," www/.", sep="")
    system(cmd)
    filename <-paste0(input$demoDataset,".pdf", sep="")
    p("Dataset documentation ",a(href=filename, target='_blank', "(pdf)"))
    
  })
  
  
  
  
  return(reactive({rv.openDemo$dataOut}))
  
}