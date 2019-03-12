######
modulePlotsUI <- function(id){
  ns <- NS(id)
  
  
  tagList(
    tags$head(tags$style(".modal-dialog{ width:100%}")),
    tags$head(tags$style(".modal-body{ min-height:50%}")),
    h3("Module Plots"),
    imageOutput(ns("plot1small")),
    shinyBS::bsModal("modalExample1", "Your plot", ns("plot1small"), size = "large",plotOutput(ns("plot1large"))),
    
    imageOutput(ns("plot2small")),
    shinyBS::bsModal("modalExample2", "Your plot", ns("plot2small"), size = "large",plotOutput(ns("plot2large"))),
    
    imageOutput(ns("plot3small")),
    shinyBS::bsModal("modalExample3", "Your plot", ns("plot3small"), size = "large",plotOutput(ns("plot3large")))
    
    
  )
}