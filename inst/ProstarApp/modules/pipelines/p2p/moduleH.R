
######
moduleHUI <- function(id){
  ns <- NS(id)
  tagList(
    br(), br(),
    
    h3("Module H"),
    actionButton(ns("rst_btn"), "Reset mod H"),
    hr(),
    uiOutput(ns("screen1")),
    hr(),
    uiOutput(ns("screen2"))
  )
}





moduleH <- function(input, output, session, dataIn, screen.id){
  ns <- session$ns
  
  
  rv <- reactiveValues(
    obj = NULL,
    res = NULL, 
    name ="moduleH")
  
  
  observeEvent(dataIn(), {
    rv$obj <- dataIn()
    
  })
  
  
 
  output$screen1 <- renderUI({
    tagList(
      h4("Screen 1 du module H"),
      actionButton(ns('btn'), "Perform (Add 1)")
    )
  })
  
  

  output$screen2 <- renderUI({
    tagList(
      h4('Screen2 du module H'),
      p(paste0('screen.id() = ', screen.id())),
      p(paste0('dataIn() = ', dataIn())),
      p(paste0('rv$obj = ', rv$obj)),
      p(paste0('rv$res = ', rv$res)),
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
  
  observeEvent(input$btn_send, {
    rv$res <- rv$obj
    print(paste0("save module H : ",rv$obj, "__", rv$res))
  })
  
  #return(reactive(list(name=rv$name,res=rv$res)))
  return(reactive({rv$res}))
}