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
    