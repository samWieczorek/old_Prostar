
output$aggregate_UI <- renderUI({
  req(input$chooseTest)
  if (input$chooseTest != 'Aggregate') {return(NULL)}
  moduleProcessUI("moduleProcess_Aggregation")
})


output$peptidomicTest_UI <- renderUI({
  req(input$chooseTest)
  if (input$chooseTest != 'peptidomictest') {return(NULL)}
  moduleProcessUI("moduleProcess_HypothesisTest")
})

output$peptidelevelproteintest_UI <- renderUI({
  req(input$chooseTest)
  if (input$chooseTest != 'peptidelevelproteintest') {return(NULL)}
  
})
