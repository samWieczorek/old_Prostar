
callModule(moduleProcess, "moduleProcess_PepImputation", 
           isDone = reactive({rvModProcess$modulePepImputationDone}), 
           pages = reactive({rvModProcess$modulePepImputation}),
           rstFunc = resetModulePepImputation)


resetModulePepImputation <- reactive({  
  ## update widgets values (reactive values)
  resetModuleProcess("PepImputation")
    
  ## update widgets in UI
  updateSelectInput(session,"peptideLevel_missing.value.algorithm", selected = rv$widgets$peptideImput$pepLevel_algorithm)
  updateSelectInput(session,"peptideLevel_missing.value.basic.algorithm", selected = rv$widgets$peptideImput$pepLevel_basicAlgorithm)
  updateNumericInput(session,"peptideLevel_detQuant_quantile", value = rv$widgets$peptideImput$pepLevel_detQuantile)
  updateNumericInput(session,"peptideLevel_detQuant_factor", value = rv$widgets$peptideImput$pepLevel_detQuant_factor)
  updateNumericInput(session,"KNN_n",  value = rv$widgets$peptideImput$pepLevel_KNN_n)
  updateNumericInput(session,"peptideLevel_imp4p_nbiter", value = rv$widgets$peptideImput$pepLevel_imp4p_nbiter)
  updateCheckboxInput(session,"peptideLevel_imp4p_withLapala", value = rv$widgets$peptideImput$pepLevel_imp4p_withLapala)
  updateNumericInput(session,"peptideLevel_imp4p_qmin",  value = rv$widgets$peptideImput$pepLevel_imp4p_qmin)
  updateRadioButtons(session, "peptideLevel_imp4pLAPALA_distrib", selected = rv$widgets$peptideImput$pepLevel_imp4pLAPALA_distrib)

    
  rvModProcess$modulePepImputationDone = rep(FALSE, 2)
  ##update dataset to put the previous one
  rv$current.obj <- rv$dataset[[last(names(rv$dataset))]] 
  
})


##########
#####  UI for the PEPTIDE LEVEL Imputation process
##########
output$screenPepImputation1 <- renderUI({
 
  })



output$screenPepImputation2 <- renderUI({
})
