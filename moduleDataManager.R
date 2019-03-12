moduleDataManager <- function(input, output, session){
  ns <- session$ns
  
  
  rav <- reactiveValues(
    current.obj = NULL)
  
  
  
  
  observeEvent(input$loadDataset,{
    rav$current.obj <- input$n
  })
  
  output$openMSnset <- renderUI({
    tagList(
      br(),br(),br(),br(),
    numericInput(ns("n"), "Choose a number", value=0, width='50px')
    )
  })
  
  
  return(reactive({rav$current.obj}))
  
}
    