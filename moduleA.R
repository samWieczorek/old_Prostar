

moduleA <- function(input, output, session, dataIn, screen.id){
  ns <- session$ns
  
  
  observeEvent(dataIn(), {
    print(paste0("In module A, observeEvent(dataIn() : ",dataIn()))
    rv$obj <- dataIn()
  })
  
  
  rv <- reactiveValues(
    obj = NULL,
    res = NULL, 
    name ="moduleA")
  
  
  output$screen1 <- renderUI({
    tagList(
      h4(paste0('screen.id() =', screen.id())),
      h4(paste0('dataIn() =', dataIn())),
      h4(paste0('rv$obj =', rv$obj)),
      h4(paste0('rv$res =', rv$res)),
      actionButton(ns('btn'), "Perform (Add 1)")
    )
  })
  
  

  output$screen2 <- renderUI({
    tagList(
      h4('screen2'),
      actionButton(ns('btn_send'), "Save")
    )
  })
  
  
  observeEvent(input$btn,{
    rv$obj <- rv$obj +1
    })
  
  observeEvent(input$rst_btn,{
    rv$obj <- dataIn()
  })
  
  observeEvent(input$btn_send, {
    rv$res <- rv$obj
    print(paste0("save module A : ",rv$obj, "__", rv$res))
  })
  
  #return(reactive(list(name=rv$name,res=rv$res)))
  return(reactive({rv$res}))
}