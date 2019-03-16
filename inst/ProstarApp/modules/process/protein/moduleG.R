
moduleGUI <- function(id){
  ns <- NS(id)
  tagList(
    h3("Module G"),
    actionButton(ns("rst_btn"), "Reset module G"),
    hr(),
    uiOutput(ns("screen1")),
    hr(),
    uiOutput(ns("screen2"))
  )
}


moduleG <- function(input, output, session, dataIn, screen.id){
  ns <- session$ns
  
  rv <- reactiveValues(
    obj = NULL,
    res = NULL,
    name = "moduleG"
  )
    
  observeEvent(dataIn(), {
    rv$obj <- dataIn()
  })
  
  
  
  
  output$screen1 <- renderUI({
    tagList(
      h4("Screen 1 du module G"),
      actionButton(ns('btn'), "Perform (Substract 1)")
    )
  })
  
  output$screen2 <- renderUI({
    tagList(
      h4('Screen2 du module G'),
      p(paste0('screen.id() = ', screen.id())),
      p(paste0('dataIn() = ', dataIn())),
      p(paste0('rv$obj = ', rv$obj)),
      p(paste0('rv$res = ', rv$res)),
      br(),
      
      actionButton(ns('btn_send'), "Save")
    )
  })
  
  
  observeEvent(input$btn,{
    rv$obj <- rv$obj - 1
    })
  
  observeEvent(input$rst_btn,{
    rv$obj <- dataIn()
    })
  
  observeEvent(input$btn_send, {
    rv$res <- rv$obj
    print(paste0("save module G : ",rv$obj, "__", rv$res))
  })
  
  return(reactive({rv$res}))
}