# Module UI

#' @title   mod_open_demo_dataset_ui and mod_open_demo_dataset_server
#' 
#' @description  A shiny Module.
#'
#' @param id shiny id
#' 
#' @param input internal
#' 
#' @param output internal
#' 
#' @param session internal
#' 
#' @param pipeline.def xxx
#' 
#' @return An object of class [`xxxx`]
#' 
#' @rdname mod_open_demo_dataset
#'
#' @keywords internal
#' 
#' @export 
#' 
#' @importFrom shiny NS tagList 
#' @import shinyjs
#' 
mod_open_demoDataset_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    div(
      div(
        style="display:inline-block; vertical-align: middle; padding-right: 20px;",
        div(id='div_demoDataset',
            tagList(
              uiOutput(ns("chooseDemoDataset")),
              uiOutput(ns("linktoDemoPdf"))
            )
        )
      ),
      div(
        style="display:inline-block; vertical-align: middle; padding-right: 20px;",
        shinyjs::disabled(
          actionButton(
            ns('load_dataset_btn'), 
            'Load dataset', 
            class=actionBtnClass)
        )
      )
    )
  )
}

# Module Server

#' @rdname mod_open_demo_dataset
#' 
#' @export
#' 
#' @keywords internal
#' 
#' @import DAPARdata2
#' @importFrom BiocGenerics get
#' @importFrom utils data
#' @importFrom shinyjs info
#' @import QFeatures
#' 
mod_open_demoDataset_server <- function(id){
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    
    rv.openDemo <- reactiveValues(
      dataRead = NULL,
      dataOut = NULL
    )
    
    
    ### function for demo mode
    output$chooseDemoDataset <- renderUI({
      print("DAPARdata is loaded correctly")
      selectInput(ns("demoDataset"),
                  "Demo dataset",
                  choices = c('None', utils::data(package="DAPARdata2")$results[,"Item"]),
                  selected = character(0),
                  width='200px')
    })
    
    
    
    observeEvent(req(input$demoDataset != 'None'), {
      nSteps <- 1
      withProgress(message = '',detail = '', value = 0, {
        incProgress(1/nSteps, detail = 'Loading dataset')
        utils::data(list=input$demoDataset, package='DAPARdata2')
        rv.openDemo$dataRead <- BiocGenerics::get(input$demoDataset)
        if (class(rv.openDemo$dataRead)!="QFeatures") {
          shinyjs::info("Warning : this file is not a QFeatures file ! 
                      Please choose another one.")
          return(NULL)
        }
        shinyjs::toggleState('load_dataset_btn', condition = !is.null(rv.openDemo$dataRead))
      }) # End withProgress
    }) # End observeEvent
    
    
    observeEvent(input$load_dataset_btn, {
      rv.openDemo$dataOut <- rv.openDemo$dataRead
    })
    
    output$linktoDemoPdf <- renderUI({
      req(input$demoDataset)
      
      # file<- paste(system.file(package = "DAPARdata2"),"/doc/",
      #              input$demoDataset,".pdf", sep="")
      # cmd <- paste("cp ",file," www", sep="")
      # system(cmd)
      # filename <-paste0(input$demoDataset,".pdf", sep="")
      # p("Dataset documentation ",a(href=filename, target='_blank', "(pdf)"))
      # 
    })
    
    reactive({rv.openDemo$dataOut })
  })
  
}

## To be copied in the UI
# mod_open_demo_dataset_ui("open_demo_dataset_ui_1")

## To be copied in the server
# callModule(mod_open_demo_dataset_server, "open_demo_dataset_ui_1")