moduleDataManagerUI <- function(id){
  ns <- NS(id)
  
  navbarMenu("Data manager" ,
             tabPanel("Open MSnset",
                      uiOutput("openMSnset")),
             tabPanel("Convert"),
             tabPanel("Demo data")
  )
  
}
