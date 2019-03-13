

######
moduleAUI <- function(id){
  ns <- NS(id)
  tagList(
    br(), br(),
    
    h3("Module A"),
    actionButton(ns("rst_btn"), "Reset mod A"),
    hr(),
    uiOutput(ns("screen1")),
    hr(),
    uiOutput(ns("screen2"))
    )
}


moduleBUI <- function(id){
  ns <- NS(id)
  tagList(
    h3("Module B"),
    actionButton(ns("rst_btn"), "Reset module B"),
    hr(),
    uiOutput(ns("screen1")),
    hr(),
    uiOutput(ns("screen2"))
  )
}



moduleCUI <- function(id){
  ns <- NS(id)
  tagList(
    h3("Module C"),
    actionButton(ns("rst_btn"), "Reset module C"),
    hr(),
    uiOutput(ns("screen1")),
    hr(),
    uiOutput(ns("screen2"))
  )
}



moduleDUI <- function(id){
  ns <- NS(id)
  tagList(
    h3("Module D"),
    actionButton(ns("rst_btn"), "Reset module D"),
    hr(),
    uiOutput(ns("screen1")),
    hr(),
    uiOutput(ns("screen2"))
  )
}











