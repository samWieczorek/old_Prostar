setwd("~/Github/Prostar/inst/ProstarApp/")

# library(MultiAssayExperiment)
# library(shiny)
# library(Prostar)
# library(DAPAR)
# library(htmlwidgets)
# 
# source(file.path("./src", "modules/Misc/moduleStaticDataTable.R"),  local = TRUE)$value
# source(file.path("~/Github/DAPAR/R/", "AllClasses.R"),  local = TRUE)$value
# source(file.path("~/Github/DAPAR/R/", "protein-pipeline-class.R"),  local = TRUE)$value
# source(file.path("~/Github/DAPAR/R/", "peptide-pipeline-class.R"),  local = TRUE)$value
# source(file.path("~/Github/DAPAR/R/", "pipeline-template-class.R"),  local = TRUE)$value
# source(file.path("~/Github/DAPAR/R/", "utils.R"),  local = TRUE)$value
# source(file.path("~/Github/Prostar/inst/ProstarApp/","global.R"), local = TRUE)$value
# source(file.path("./src", "commonFunc.R"),  local = TRUE)$value
# source(file.path("./src", "modules/Menu_DataManager/moduleInfoDataset.R"),  local = TRUE)$value
# 
# pipeline.def <- ReadPipelineConfig("src/modules/process/pipeline.conf")

##########################################################################################


ui <- fluidPage(
  
  tabPanel("Open MSnset file",
           value = "openMSnsetTab",
           tagList(
             fileInput("file", "Open a MSnset file", multiple = FALSE),
             actionButton("loadMSnset", "Load MSnset",class = actionBtnClass),
             p("Once the 'Load' button (above) clicked, all importing functions ('Open MSnset', 'Demo data' and 'Convert data') will be disabled 
    (because successive dataset loading can make Prostar unstable). To work on another dataset, use first the 'Reload Prostar' functionality from 
    the 'Dataset manager' menu: it will make Prostar restart with a fresh R session where import functions are enabled.")
           ),
           moduleInfoDatasetUI("infoAboutMSnset"),
           div( style="display:inline-block; vertical-align: top;",
                moduleStaticDataTableUI("overview_openMSnset")
           )
           
  )
)


server <- function(input, output, session) {
  
  callModule(moduleStaticDataTable,"overview_openMSnset", 
             table2show=reactive({
               req(rv.openMSnSet$dataOut)
               GetDatasetOverview2(dataset(rv.openMSnSet$dataOut, 'original'))}))
  
  callModule(moduleInfoDataset, "infoAboutMSnset",
             obj = reactive({
               req(rv.openMSnSet$dataOut)
               dataset(rv.openMSnSet$dataOut, 'original') }))
  
  
  rv.openMSnSet <- reactiveValues(
    current.obj = NULL,
    dataOut = NULL
  )
  
  observeEvent( input$file, {
    print("input$file")
    print(input$file)
    print(input$file$name)
    #######################################################
    print( (strsplit(unlist(input$file$name), "\\."))[1] )
    # extension : "Exp1_R25_HypTest_prot.msnset" -> "msnset"
    
    
  })
  
  
  observeEvent( input$loadMSnset,ignoreInit =TRUE,{ 
    
    
    req(input$file)
    
    data <- readRDS(input$file$datapath)
    
    
    
    if(!(class(data)[1] == "MultiAssayExperiment")) {
      
      
      if (class(data)[1]=="MSnSet") {
        typeOfData <- data@experimentData@other$typeOfData
        ll.process <- type <- NULL
        switch(typeOfData,
               peptide = {
                 print("peptide")
                 rv.openMSnSet$dataOut <- pepPipeline()
                 ll.process <- pipeline.def$peptide
                 type <- 'Peptide'
               },
               protein = {
                 print("protein")
                 rv.openMSnSet$dataOut <- PipelineProtein(analysis= input$file$name, 
                                                          pipelineType = "protein", 
                                                          processes=pipeline.def$protein, 
                                                          experiments=list(original=data), 
                                                          colData=Biobase::pData(data))
                 
                 ll.process <- pipeline.def$protein
                 type <- 'protein'
                 
                 
               }, 
               p2p = {
                 print("p2p")
                 rv.openMSnSet$dataOut <- p2pPipeline()
                 ll.process <- pipeline.def$p2p
                 type <- 'p2p'
                 
               }
        )
        
        
        
        
      } 
      
      else {
        shinyjs::info("Warning : this file is not a MSnset file ! 
                      Please choose another one.")
        return(NULL)
      }
      
    } else {
      
      ## The dataset is already in the new format 
      rv.openMSnSet$current.obj <- data
    }
    
    l.params <- list(filename = input$file$name)
    
    
    
    
  })
  
  
  return(reactive({rv.openMSnSet$dataOut }))
  
  
}


## RunApp()
shinyApp(ui, server)
