modulePipelineProtUI <- function(id){
  ns <- NS(id)
  
  # div(
  #   id = "chooseDataset",
  #   absolutePanel(
  #     id  = "#AbsolutePanel",
  #     top = 0, right = 50, width = "500px",height = "50px",
  #     draggable = TRUE,fixed = TRUE,
  #     cursor = "default"
  #     ,uiOutput("chooseDataset" )
  #   )
  #   
  # )
  
  navbarMenu("Pipeline protein" ,
             
             tabPanel("ProcessA",moduleAUI('pep_processA')),
             tabPanel("ProcessB",moduleBUI('pep_processB')),
             tabPanel("ProcessC",moduleCUI('pep_processC'))
  )
  
}
