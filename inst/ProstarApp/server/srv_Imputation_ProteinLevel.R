
callModule(moduleProcess, "moduleProcess_ProtImputation", 
           isDone = reactive({rvModProcess$moduleProtImputationDone}), 
           pages = reactive({rvModProcess$moduleProtImputation}),
           rstFunc = resetModuleProtImputation)

resetModuleProtImputation <- reactive({  
  ## update widgets values (reactive values)
  resetModuleProcess("ProtImputation")
    
  ## update widgets in UI
  updateSelectInput(session,"POV_missing.value.algorithm",selected=rv$widgets$proteinImput$POV_algorithm)
  updateSelectInput(session,"MEC_missing.value.algorithm", selected=rv$widgets$proteinImput$MEC_algorithm)
  updateNumericInput(session,"POV_detQuant_quantile", value = rv$widgets$proteinImput$POV_detQuant_quantile)
  updateNumericInput(session,"POV_detQuant_factor", value = rv$widgets$proteinImput$POV_detQuant_factor)
  updateNumericInput(session,"KNN_nbNeighbors", value = rv$widgets$proteinImput$POV_KNN_n)
  updateNumericInput(session, "MEC_detQuant_quantile", value = rv$widgets$proteinImput$MEC_detQuant_quantile)
  updateNumericInput(session, "MEC_detQuant_factor", value = rv$widgets$proteinImput$MEC_detQuant_factor)
  updateNumericInput(session, "MEC_fixedValue", value = rv$widgets$proteinImput$MEC_fixedValue)
  
  
  
  
  rvModProcess$moduleProtImputationDone = rep(FALSE, 3)
  
  ##update dataset to put the previous one
  rv$current.obj <- rv$dataset[[last(names(rv$dataset))]] 
  
})


output$screenProtImput1 <- renderUI({
 
})



output$screenProtImput2 <- renderUI({
  
 
})



output$screenProtImput3 <- renderUI({
 
})




