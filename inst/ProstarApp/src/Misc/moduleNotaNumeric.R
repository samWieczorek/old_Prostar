module_Not_a_numericUI <- function(id){
  ns <- NS(id)
  uiOutput(ns("msg_not_numeric"))
}


#################### MODULES DEFINITION #################################
module_Not_a_numeric <- function(input, output, session, n){
  
  output$msg_not_numeric <- renderUI({
    n()
    if (is.na(as.numeric(n()))){
      tags$p("Please choose a number")
    }
  })
}