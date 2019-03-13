modulePipelineP2pUI <- function(id){
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
  
  navbarMenu("Pipeline p2p" ,
             tabPanel("ProcessA",moduleAUI('p2p_processA')),
             tabPanel("ProcessC",moduleCUI('p2p_processC')),
             tabPanel("ProcessD",moduleDUI('p2p_processD'))
  )
  
}
