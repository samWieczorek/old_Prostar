
moduleNormalizationUI <- function(id){
  ns <- NS(id)
  tagList(
    br(), br(),
    
    h3("Module Normalization"),
    actionButton(ns("rst_btn"), "Reset mod Normalization"),
    hr(),
    uiOutput(ns("screen1")),
    hr(),
    uiOutput(ns("screen2"))
  )
}





moduleNormalization <- function(input, output, session, dataIn, screen.id){
  ns <- session$ns
  
  #################################################################
  ## Mandatory code
  rv <- reactiveValues(
    obj = NULL,
    res = NULL, 
    name ="processNormalization")
  
  
  ## Initialization of the module
  observeEvent(dataIn(), {
    rv$obj <- dataIn()
  })
  
  
  observeEvent(input$btn_send, {
    rv$dataOut <- rv$obj
    print(paste0("save module Normalization : ",rv$obj, "__", rv$dataOut))
  })
  
  return(reactive({rv$dataOut}))
  
  
  #########################################################################
  
  
  output$screen1 <- renderUI({
    tagList(
      h4("Screen 1 du module Normalization"),
      actionButton(ns('btn'), "Perform (Add 1)")
    )
  })
  
  
  
  output$screen2 <- renderUI({
    tagList(
      h4('Screen2 du module Normalization'),
      p(paste0('screen.id() = ', screen.id())),
      p(paste0('dataIn() = ', dataIn())),
      br(),
      
      actionButton(ns('btn_send'), "Save")
    )
  })
  
  
  
  observeEvent(input$btn,{
    rv$obj <- rv$obj +1
  })
  
  observeEvent(input$rst_btn,{
    rv$obj <- dataIn()
  })
  
  
}