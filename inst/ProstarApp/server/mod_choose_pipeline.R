# Module UI

#' @title xxx
#'
#' @description
#' Get the list of pipelines available in the package
#'
#' @export
#'
Pipelines <- function(){
  list(Protein = c('protein'),
       Peptide = c('peptide'),
       P2p = c('peptide'),
       Peptidomic = c('peptide')
  )
}


#' @title   mod_choose_pipeline_ui and mod_choose_pipeline_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#' @param dataType xxx
#' @param package xxx
#'
#' @rdname mod_choose_pipeline
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
#' @import sos
#' 
#' 
mod_choose_pipeline_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    h3('mod_choose_pipeline'),
    # div(
    #   style="display:inline-block; vertical-align: middle; padding-right: 20px;",
    #   selectInput(ns("dataType"), 'Data type', 
    #               choices = c('None'='None', 
    #                           'protein'='protein', 
    #                           'peptide'='peptide'), 
    #               width='150px')
    # ),
    div(
      style="display:inline-block; vertical-align: middle; padding-right: 20px;",
      uiOutput(ns("selectWidgetPipeline"))
    ),
    div(
      style="display:inline-block; vertical-align: middle; padding-right: 20px;",
      actionButton(ns('selectPipeline_btn'), 'load pipeline')
    )
    #uiOutput(ns('describePipeline'))
  )
}

# Module Server

#' @rdname mod_choose_pipeline
#' @export
#' @keywords internal
#' 
#' 

mod_choose_pipeline_server <- function(id, dataType = NULL, package = NULL){
  
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    rv.pipe <- reactiveValues(
      pipeline = NULL
    )
    
    output$selectWidgetPipeline <- renderUI({
      
      req(!is.null(dataType()), dataType() != 'None', dataType() != '')
      #if (dataType() == 'None') return(NULL)
      #print('inside selectWidgetPipeline')
      #print(paste0('dataType received = ', dataType()))
      #library(package, character.only = TRUE)
      selectInput(ns("pipelineChoice"),
                  "Choose the pipeline",
                  choices = c('None', names(Pipelines()[grep(dataType(), Pipelines())])), 
                  width='150px'
      )
    })
    
    # observeEvent(input$selectPipeline_btn, {
    #   rv.pipe$pipeline <- input$pipelineChoice
    #   #  shinyjs::toggleState('loadPipeline', condition = rv.pipe$pipeline != 'None')
    # })
    
    # output$describePipeline <- renderUI({
    #   req(input$pipelineChoice)
    #   includeMarkdown(system.file('md', paste0(input$pipelineChoice, '.md'), package=package))
    # })
    
    reactive({input$pipelineChoice})
  })
  
}