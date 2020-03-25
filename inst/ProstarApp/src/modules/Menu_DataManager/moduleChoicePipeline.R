
moduleChoosePipelineUI  <- function(id) {
  ns <- NS(id)
  
  tagList(
      actionButton('toto', 'toto')
      #uiOutput(ns("selectWidgetPipeline"))
  )
}


moduleChoosePipeline  <- function(input, output, session, path){
  ns <- session$ns
  
  pipeline.def <- ReadPipelineConfig(path)
  
  rv.choosePipeline <- reactiveValues(
    choice = NULL )
  
  observeEvent( req(input$pipelineChoice), {
    rv.choosePipeline$choice <- pipeline.def[input$pipelineChoice]
     })
  
  output$selectWidgetPipeline<- renderUI({
    selectizeInput(ns("pipelineChoice"),
                   "What do you choose for pipeline?",
                   multiple = T,
                   options = list(maxItems = 1),
                   choices = names(pipeline.def))
  })
  
  return( rv.choosePipeline$choice )
}