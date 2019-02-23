######
moduleAUI <- function(id){
  ns <- NS(id)
  tagList(
    h3("Module A"),
    actionButton(ns("rst_btn"), "Reset"),
    uiOutput(ns("screen1")),
    uiOutput(ns("screen2"))
  )
}


moduleBUI <- function(id){
  ns <- NS(id)
  tagList(
    h3("Module B"),
    actionButton(ns("rst_btn"), "Reset"),
    uiOutput(ns("screen1")),
    uiOutput(ns("screen2"))
  )
}



moduleCUI <- function(id){
  ns <- NS(id)
  tagList(
    h3("Module C"),
    actionButton(ns("rst_btn"), "Reset"),
    uiOutput(ns("screen1")),
    uiOutput(ns("screen2"))
  )
}










######
modulePlotsUI <- function(id){
  ns <- NS(id)
  tagList(
    h3("Module Plots"),
    uiOutput(ns("screen1")),
    plotOutput(ns("screen2"))
  )
}


