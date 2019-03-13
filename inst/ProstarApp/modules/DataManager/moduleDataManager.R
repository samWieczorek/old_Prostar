moduleDataManagerUI <- function(id){
  ns <- NS(id)
  
  navbarMenu("Data manager" ,
             tabPanel("Open MSnset",
                      br(),br(),br(),br(),selectInput(ns("selectPipeline"), "Select pipeline",
                                                      choices=c("None"="","Peptide"="Peptide", "Protein"="Protein", "P2p" = "P2p"),
                                                      selected=character(0)),
                      uiOutput(ns("openMSnset")),
                      actionButton(ns('loadDataset'), "Load dataset")),
             tabPanel("Convert"),
             tabPanel("Demo data")
  )
  
}




moduleDataManager <- function(input, output, session){
  ns <- session$ns
  
  
  rav <- reactiveValues(
    current.obj = NULL,
    current.pipeline = NULL)
  
  
  
  
  observeEvent(input$loadDataset,{
    rav$current.obj <- input$n
    rav$current.pipeline <- input$selectPipeline
  })
  
  output$openMSnset <- renderUI({
    tagList(
      br(),br(),br(),br(),
    numericInput(ns("n"), "Choose a number", value=0, width='50px')
    )
  })
  
  
  return(reactive({list(pipeline = rav$current.pipeline,
                        initialData = rav$current.obj)
    }))
  
}
    