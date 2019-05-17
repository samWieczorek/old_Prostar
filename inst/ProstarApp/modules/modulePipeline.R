source(file.path(".", "modules/moduleNavigation.R"),  local = TRUE)$value



############# Definition of the module   #########################

modulePeptidePipelineUI <- function(id){
  ns <- NS(id)
  tagList(
    moduleNavigationUI(ns("moduleGeneral"))
  )
}

modulePeptidePipeline <- function(input, output, session, dataIn, screen.id, settings=NULL){
  ns <- session$ns
  
  
  ###### definition of RV for navigation process
  rvNav <- reactiveValues(
    Done = rep(FALSE,5),
    def = list(name = "Peptide pipeline",
               stepsNames = peptide.def,
               isMandatory = rep(TRUE,length(peptide.def)),
               ll.UI = list( screenStep1 = uiOutput(ns("screenPepPipeline1")),
                             screenStep2 = uiOutput(ns("screenPepPipeline2")),
                             screenStep3 = uiOutput(ns("screenPepPipeline3")),
                             screenStep4 = uiOutput(ns("screenPepPipeline4"))
               ),
               rstFunc = reactive({resetModulePeptidePipeline()}))
  )
  
  
  
  ### appel du module de navigation
  observe({
    callModule(moduleNavigation, "moduleGeneral", 
               isDone = reactive({rvNav$Done}), 
               pages = reactive({rvNav$def}),
               rstFunc = resetModulePeptidePipeline)
  })
  
  
  ### Definition of rv for the filtering module
  rv.pepPipeline <- reactiveValues(
    ## temporary current data in module
    obj =  NULL,
    
    ## return result of the module
    dataOut = NULL, 
    name = "processGeneral"
    
  )
  
  ################################################
  
  
  resetModulePeptidePipeline <- reactive({  
    
    rvNav$Done = rep(FALSE, length(peptide.def))
    
  })
  
  
  ### initialisation de la variable globale du process
  # observe({
  #   dataIn()
  #   rv.filtering$obj <- dataIn()
  # })
  
  
  ################# END of definitino part   #############################
  #######################################################################
  
  
  
  
  
  
  
  output$screenPepPipeline1 <- renderUI({
   
  })
  
  
  output$screenPepPipeline2 <- renderUI({
    
  })
  
  output$screenPepPipeline3 <- renderUI({
    
  })
  output$screenPepPipeline4 <- renderUI({
    
  })
  
 
  
  return(reactive({rv.pipeline$dataOut}))
  
}




