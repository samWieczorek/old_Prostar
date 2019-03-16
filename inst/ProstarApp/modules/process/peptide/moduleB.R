
######
moduleBUI <- function(id){
  ns <- NS(id)
  tagList(
    br(), br(),
    
    h3("Module B"),
    actionButton(ns("rst_btn"), "Reset mod B"),
    hr(),
    uiOutput(ns("screen1")),
    hr(),
    uiOutput(ns("screen2"))
  )
}





moduleB <- function(input, output, session, dataIn, screen.id){
  ns <- session$ns
  
  
  rv <- reactiveValues(
    obj = NULL,
    res = NULL, 
    name ="processB")
  
  
  observeEvent(dataIn(), {
    rv$obj <- dataIn()
    
  })
  
  
  
  output$screen1 <- renderUI({
    tagList(
      h4("Screen 1 du module B"),
      actionButton(ns('btn'), "Perform (Multiply by 2)")
    )
  })
  
  
  
  output$screen2 <- renderUI({
    tagList(
      h4('Screen2 du module B'),
      p(paste0('screen.id() = ', screen.id())),
      p(paste0('dataIn() = ', dataIn())),
      p(paste0('rv$obj = ', rv$obj)),
      p(paste0('rv$res = ', rv$res)),
      br(),
      
      actionButton(ns('btn_send'), "Save")
    )
  })
  
  
  
  observeEvent(input$btn,{
    rv$obj <- 2 * rv$obj
  })
  
  observeEvent(input$rst_btn,{
    rv$obj <- dataIn()
  })
  
  observeEvent(input$btn_send, {
    rv$res <- rv$obj
    })
  
  return(reactive({rv$res}))
}