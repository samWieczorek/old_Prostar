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
  
  source(file.path(".", "modulesUI.R"),  local = TRUE)$value
  source(file.path(".", "moduleA.R"),  local = TRUE)$value
  source(file.path(".", "moduleB.R"),  local = TRUE)$value
  source(file.path(".", "moduleC.R"),  local = TRUE)$value
  source(file.path(".", "moduleD.R"),  local = TRUE)$value
  source(file.path(".", "modulePlots.R"),  local = TRUE)$value
  
  source(file.path(".", "modulePipelinePepUI.R"),  local = TRUE)$value
  source(file.path(".", "modulePipelinePep.R"),  local = TRUE)$value
  source(file.path(".", "modulePipelineProtUI.R"),  local = TRUE)$value
  source(file.path(".", "modulePipelineProt.R"),  local = TRUE)$value
  source(file.path(".", "modulePipelineP2pUI.R"),  local = TRUE)$value
  source(file.path(".", "modulePipelineP2p.R"),  local = TRUE)$value
  
 
  rv <- reactiveValues(
    obj = 10,
    current.obj = 10,
    returnValPipelinePeptide = NULL
    )
  
  
 callModule(module = modulePlots, 'showPlots', dataIn=reactive({rv$current.obj}))
 
 
  
  # observeEvent(req(input$currentDataset), {
  #   rv$current.obj <- pipeline.pep$dataset[[input$currentDataset]]
  # })

 
  observeEvent(rv$obj, {
    print('maj :  pipeline.pep$dataset$original <- rv$obj')
    
    rv$returnValPipelinePeptide <- callModule(module = modulePipelinePep, 'test', dataIn = reactive({rv$current.obj}), navPage=reactive({input$navPage}))
    prependTab(inputId = "navPage",
                modulePipelinePepUI('test')
              )

    callModule(module = modulePipelineProt, 'testProt')
    insertTab(inputId = "navPage",
              modulePipelineProtUI('testProt'),
        target="Pipeline peptide",
        position="after")
    
    
    callModule(module = modulePipelineP2p, 'testP2p')
    insertTab(inputId = "navPage",
              modulePipelineProtUI('testP2p'),
              target="Pipeline protein",
              position="after"
    )
    
  })
  
  
  
  output$summary <- renderUI({
   
    tagList(
      h3('General summary'),
      p(paste0('rv$obj =',rv$obj)),
      p(paste0('rv$current.obj()= ',rv$current.obj)),
      p(paste0('rv$returnVal() = ',rv$returnValPipelinePeptide()))
    )
  
  })
}
