rm(list=ls())
options(shiny.maxRequestSize=300*1024^2) 

require(compiler)
enableJIT(3)




onStart = function() {
  cat("Doing application setup\n")
  
  onStop(function() {
    cat("Doing application cleanup\n")
    graphics.off()
    unlink(sessionID, recursive = TRUE)
    unlink(paste(tempdir(), "*", sep="/"),recursive = TRUE)
  })
}



library(shinyBS)

#' Title
#'
#' @param input 
#' @param output 
#' @param session 
#'
#' @return
#' @export
#'
#' @examples
server <- function(input, output, session){
  
  source(file.path(".", "modules/Plots/modulePlots.R"),  local = TRUE)$value
  source(file.path(".", "modules/moduleBugReport.R"),  local = TRUE)$value
  
  source(file.path(".", "modules/DataManager/moduleDataManager.R"),  local = TRUE)$value
  source(file.path(".", "modules/moduleInsertMarkdown.R"),  local = TRUE)$value
  
  source(file.path(".", "modules/Export/moduleExport.R"),  local = TRUE)$value
  
  source(file.path(".", "pipelineCore.R"),  local = TRUE)$value
  source(file.path(".", "watchProcess.R"),  local = TRUE)$value
  
  
  
  loadLibraries()
  
  
  
 #####
 ## Launch modules
  obj <- callModule(module = moduleDataManager, 'datamanager')
  
 observeEvent(req(pipeline$current.dataset, pipeline$current.indice), {
   callModule(module = modulePlots, 'showPlots', 
              dataIn=reactive({pipeline$current.dataset[[pipeline$current.indice]]}), 
              llPlots=reactive({1:6}))
 })
 
  
 
 
 #Set up writing file for log
 logfilename <- tempfile(fileext=".log")
 print(logfilename)
 con <- file(logfilename,open="wt")
 if(!interactive()){
   sink(con, append=TRUE)
   sink(con, append=TRUE, type="message")
 }
 
 callModule(module = moduleBugReport, 'bugreport', logfile=reactive({logfilename}))
 callModule(moduleInsertMarkdown, "links_MD",URL_links)
 callModule(moduleInsertMarkdown, "FAQ_MD",URL_FAQ)
 
 
 
 
 observe({
   pipeline$current.dataset
   print("##### pipeline$current.dataset  ####")
   print(pipeline$current.dataset)
 })
 
 
 
 
 
 
  
  ## manual change of current dataset
 observeEvent(input$currentDataset,{
    n <- which(names(pipeline$current.dataset)==input$currentDataset)
    if (length(n)==0){
      pipeline$current.indice <- 1
    } else {
      pipeline$current.indice <- n
    }
  })


  
  
  output$chooseDataset <- renderUI({

    req(pipeline$current.dataset)
    req(pipeline$current.indice)
    absolutePanel(
      id  = "#AbsolutePanel",
      top = -10, right = 50, width = "500px",height = "50px",
      draggable = FALSE,fixed = TRUE,
      cursor = "default",
    tagList(
      div(
        div(
          style="display:inline-block; vertical-align: center; margin:0px",
          p('Current dataset')
        ),
        div(
        style="display:inline-block; vertical-align: center; margin:0px",
        selectInput('currentDataset', '',
                    choices = names(pipeline$current.dataset[!sapply(pipeline$current.dataset,is.null)]),
                    selected = names(pipeline$current.dataset)[pipeline$current.indice],
                    width='150px')
        )
      )
    )
    )
  })

  
  


  observeEvent(req(pipeline$current.indice),{

    print(paste0("Change of current dataset in pipeline :", pipeline$current.indice))
    pipeline$current.obj <- pipeline$current.dataset[[pipeline$current.indice]]

  })


  


  
  
  
  
  

   }
