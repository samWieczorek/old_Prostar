moduleDataManagerUI <- function(id){
  ns <- NS(id)
  
  navbarMenu("Data manager" ,
             tabPanel("Open MSnset",
                      br(),br(),br(),br(),selectInput("selectPipeline", "Select pipeline",
                                  choices=c("None"="","Peptide"="Peptide", "Protein"="Protein", "P2p" = "P2p"),
                                  selected=character(0)),
                      uiOutput(ns("openMSnset")),
                      actionButton(ns('loadDataset'), "Load dataset")),
             tabPanel("Convert"),
             tabPanel("Demo data")
  )
  
}
