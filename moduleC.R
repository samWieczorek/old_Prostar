


moduleC <- function(input, output, session, dataIn, screen.id){
  ns <- session$ns
  
  observeEvent(dataIn(), {
    rv$obj <- dataIn()
  })
  
  
  rv <- reactiveValues(
    obj = NULL,
    res = NULL,
    a = 20,
    name = "moduleC"
    
  )
  
  
  output$screen1 <- renderUI({
    tagList(
      h4(paste0('screen.id() =', screen.id())),
      h4(paste0('screen1, rv$a=', rv$a)),
      actionButton(ns('btn'), "Perform (Substract 1)")
    )
  })
  
  output$screen2 <- renderUI({
    tagList(
      h4('screen2'),
      actionButton(ns('btn_send'), "Save")
    )
  })
  
  
  observeEvent(input$btn,{
    rv$obj <- rv$obj - 1
    rv$a <- rv$a +10
     
  })
  
  observeEvent(input$rst_btn,{
    rv$obj <- dataIn()
    rv$a <- 8
    print(rv$obj)
    
  })
  observeEvent(input$btn_send, {
    rv$res <- rv$obj
    print(paste0(rv$obj, "__", rv$res))
  })
  
  return(reactive({rv$res}))
}