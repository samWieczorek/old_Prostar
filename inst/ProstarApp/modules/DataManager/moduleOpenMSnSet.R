source(file.path(".", "modules/moduleStaticDataTable.R"),  local = TRUE)$value



moduleOpenMSnSetUI  <- function(id){
  ns <- NS(id)
  tabPanel("Open MSnset file",
           value = "openMSnsetTab",
           tagList(
             uiOutput(ns("openMSnsetScreen")),
             uiOutput(ns("updateDesign"))
           )
  )
}




moduleOpenMSnSet  <- function(input, output, session){
  ns <- session$ns
  
  
  callModule(moduleStaticDataTable,"overview_openMSnset", 
             table2show=reactive({GetDatasetOverview2(rv.openmode$current.obj$datasets[[1]])}))
  
  
  rv.openmode <- reactiveValues(
    current.obj = NULL,
    dataOut = NULL,
    name ="openmode",
    current.obj.name =NULL,
    indexNA = NULL
  )
  
  
  
  
  
  
  
  
  
  
  
  
  
  
 
  
  

  
}
