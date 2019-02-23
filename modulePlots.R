

modulePlots <- function(input, output, session, dataIn){
  ns <- session$ns
  
  
  
  output$screen1 <- renderUI({
    dataIn()
    h4(paste0('dataIn() = ', dataIn()))
  })
  
  
  
  output$screen2 <- renderPlot({
    dataIn()
    tmp <- rep(dataIn(), 10)
    tagList(
      plot(tmp)
    )
  })
  
  return(NULL)
}