
callModule(moduleProcess, "moduleProcess_HypothesisTest", 
           isDone = reactive({rvModProcess$moduleHypothesisTestDone}), 
           pages = reactive({rvModProcess$moduleHypothesisTest}),
           rstFunc = resetModuleHypothesisTest)


resetModuleHypothesisTest <- reactive({  
  ## update widgets values (reactive values)
  resetModuleProcess("HypothesisTest")
    
  ## update widgets in UI
  updateSelectInput(session,"anaDiff_Design", selected = rv$widgets$hypothesisTest$design)
  updateSelectInput(session,"diffAnaMethod", selected = rv$widgets$hypothesisTest$method)
  updateRadioButtons(session,"ttest_options", selected = rv$widgets$hypothesisTest$ttest_options)
  updateTextInput(session, "seuilLogFC", value= rv$widgets$hypothesisTest$th_logFC)
    
  rvModProcess$moduleHypothesisTestDone = rep(FALSE, 2)
  ##update dataset to put the previous one
  rv$current.obj <- rv$dataset[[last(names(rv$dataset))]] 
  
})


output$screenHypoTest1 <- renderUI({
  
})


output$screenHypoTest2 <- renderUI({

})

