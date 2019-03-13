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



modulePipelineP2p <- function(input, output, session, dataIn){
  ns <- session$ns
  
  # output$summary <- renderUI({
  #   
  #   tagList(
  #     p(paste0('pipeline.pep$dataset$original= ',pipeline.pep$dataset$original)),
  #     p(paste0('pipeline.pep$dataset$A_processed= ',pipeline.pep$dataset$A_processed)),
  #     p(paste0('pipeline.pep$dataset$B_processed= ',pipeline.pep$dataset$B_processed)),
  #     p(paste0('pipeline.pep$dataset$C_processed= ',pipeline.pep$dataset$C_processed))
  #   )
  #   
  # })
}