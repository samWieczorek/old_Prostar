source(file.path("server", "srv_HypothesisTest_Peptidomic.R"),  local = TRUE)$value
source(file.path("server", "srv_Aggregation.R"),  local = TRUE)$value
source(file.path("server", "srv_PeptideLevelproteinTest_Peptide.R"),  local = TRUE)$value



output$aggregate_UI <- renderUI({
  req(input$chooseTest)
  if (input$chooseTest != 'Aggregate') {return(NULL)}
  moduleProcessUI("moduleProcess_Aggregation")
})


output$peptidomicTest_UI <- renderUI({
  req(input$chooseTest)
  if (input$chooseTest != 'peptidomictest') {return(NULL)}
  moduleProcessUI("moduleProcess_HypothesisTestPeptidomic")
})

output$peptidelevelproteintest_UI <- renderUI({
  req(input$chooseTest)
  if (input$chooseTest != 'peptidelevelproteintest') {return(NULL)}
  moduleProcessUI("moduleProcess_HypothesisTestPeptide")
  
})
