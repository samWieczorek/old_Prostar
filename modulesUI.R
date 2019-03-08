

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








######
modulePlotsUI <- function(id){
  ns <- NS(id)
  tagList(
    h3("Module Plots"),
    imageOutput(ns("plot1small")),
    shinyBS::bsModal("modalExample1", "Your plot", ns("plot1small"), size = "large",plotOutput(ns("plot1large"))),
    
    imageOutput(ns("plot2small")),
    shinyBS::bsModal("modalExample2", "Your plot", ns("plot2small"), size = "large",plotOutput(ns("plot2large"))),
    
    imageOutput(ns("plot3small")),
    shinyBS::bsModal("modalExample3", "Your plot", ns("plot3small"), size = "large",plotOutput(ns("plot3large")))

      
  )
}


