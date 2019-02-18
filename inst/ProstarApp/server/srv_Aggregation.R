
callModule(moduleProcess, "moduleProcess_Aggregation", 
           isDone = reactive({rvModProcess$moduleAggregationDone}), 
           pages = reactive({rvModProcess$moduleAggregation}),
           rstFunc = resetModuleAggregation)



resetModuleAggregation <- reactive({  
  ## update widgets values (reactive values)
  resetModuleProcess("Aggregation")
  
  
  
  ## update widgets in UI
  updateSelectInput(session, "proteinId", selected = rv$widgets$aggregation$proteinId)
  updateRadioButtons(session, "radioBtn_includeShared", selected = rv$widgets$aggregation$includeSharedPeptides)
  updateRadioButtons(session, "AggregationConsider", selected = rv$widgets$aggregation$considerPeptides)
  updateNumericInput(session, "nTopn", value=rv$widgets$aggregation$topN)
  updateRadioButtons(session, "AggregationOperator", selected = rv$widgets$aggregation$operator)
  
  
  rvModProcess$moduleAggregationDone = rep(FALSE, 3)
  ##update dataset to put the previous one
  rv$current.obj <- rv$dataset[[last(names(rv$dataset))]] 
  
  ## reset temp object
  rv$temp.aggregate <- NULL
  
})



output$screenAggregation1 <- renderUI({
 

})


output$screenAggregation2 <- renderUI({
 
})


output$screenAggregation3 <- renderUI({
  tagList(
    actionButton("valid.aggregation","Save aggregation", class = actionBtnClass)
  )
})
