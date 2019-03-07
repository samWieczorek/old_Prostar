


moduleB <- function(input, output, session, dataIn, screen.id){
  ns <- session$ns
  
  
  observeEvent(dataIn(), {
    rv$obj <- dataIn()
  })
  
  
  rv <- reactiveValues(
    obj = NULL,
    res = NULL,
    a = 10,
    name = "moduleB"
  )
  
  
  output$screen1 <- renderUI({
    tagList(
      h4(paste0('screen.id() =', screen.id())),
      h4(paste0('screen1, rv$a=', rv$a)),
      actionButton(ns('btn'), "Perform (Multiply by 2)")
    )
  })
  
  output$screen2 <- renderUI({
    tagList(
      h4('screen2'),
      actionButton(ns('btn_send'), "Save")
    )
  })
  
  
  observeEvent(input$btn,{
    rv$obj <- 2 * rv$obj
    rv$a <- rv$a +10
    print(rv$obj)
    
  })
  
  observeEvent(input$rst_btn,{
    rv$obj <- dataIn()
    rv$a <- 8
    print(rv$obj)
    
  })
  
  
  observeEvent(input$btn_send, {

    print("Action on input$btn_send")
    print(input$btn_send)
    rv$res <- rv$obj
    print(paste0(rv$obj, "__", rv$res))
  })
  
  # test <- eventReactive(input$btn_send,{
  #   rv$obj
  # })
  # 
  return(reactive({rv$res}))
  #return(reactive(list(name=rv$name,res=rv$res)))
}