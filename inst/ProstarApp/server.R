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
  env <- environment()
  
  source(file.path(".", "pipelineDefinition.R"), local = TRUE)$value
  
  
  #####
  ## Launch modules
  source(file.path(".", "modules/Plots/modulePlots.R"), local = TRUE)$value
  source(file.path(".", "modules/moduleBugReport.R"), local = TRUE)$value
  source(file.path(".", "modules/moduleHomepage.R"),  local = TRUE)$value
  source(file.path(".", "modules/moduleReleaseNotes.R"),  local = TRUE)$value
  
  source(file.path(".", "modules/moduleInsertMarkdown.R"), local = TRUE)$value
  
  source(file.path(".", "modules/Export/moduleExport.R"), local = TRUE)$value
  source(file.path(".", "modules/modulePopover.R"), local = TRUE)$value
  source(file.path(".", "commonFunc.R"), local = TRUE)$value
  source(file.path(".", "modules/moduleSettings.R"), local = TRUE)$value
  source(file.path(".", "pipelineCore.R"),  local = TRUE)$value
 
  
  source(file.path(".", "modules/moduleStaticDataTable.R"),  local = TRUE)$value
  source(file.path(".", "modules/DataManager/moduleInfoDataset.R"),  local = TRUE)$value
  
 
  
  
  loadLibraries()
  
    rv.prostar <- reactiveValues(
      obj = NULL,
      settings = NULL
      )
  
   
    
 
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
 rv.prostar$settings <- callModule(moduleSettings, "modSettings",dataIn=reactive({GetCurrentMSnSet()}))
 callModule(moduleHomepage, "homepage")
 callModule(moduleReleaseNotes, "modReleaseNotes")

  
  ## manual change of current dataset
 observeEvent(input$currentDataset,{
   print('!!!!! Manual change of current dataset')
    n <- which(names(pipeline$current.obj@datasets)==input$currentDataset)
    if (length(n)==0){
      pipeline$current.indice <- 1
    } else {
      pipeline$current.indice <- n
    }
  })


  observeEvent(input$sidebar_left, {
    print(input$sidebar_left)
    switch(input$sidebar_left,
           FAQ =  toggleModal(session, "modalFAQ"),
           links = toggleModal(session, "modallinks"),
           bugReport = toggleModal(session, "modalbugreport")

    )
  })

  
  
  shinyjs::hide(id = "loading_page", anim = FALSE)
  
  shinyjs::show("main_content", anim = TRUE, animType = "fade")
  

   }
