
callModule(moduleProcess, "moduleProcess_Normalization", 
           isDone = reactive({rvModProcess$moduleNormalizationDone}), 
           pages = reactive({rvModProcess$moduleNormalization}),
           rstFunc = resetModuleNormalization)






resetModuleNormalization <- reactive({  
  ## update widgets values (reactive values)
  resetModuleProcess("Normalization")
  
  
  ## update widgets in UI
  updateSelectInput(session, "normalization.method", selected = rv$widgets$normalization$method)
  updateSelectInput(session, "normalization.type", selected = rv$widgets$normalization$type)
  updateTextInput(session,"spanLOESS", value = rv$widgets$normalization$spanLOESS)
  updateTextInput(session, "normalization.quantile", value = rv$widgets$normalization$quantile)
  updateCheckboxInput(session, "normalization.variance.reduction", value = rv$widgets$normalization$varReduction)
  
  rvModProcess$moduleNormalizationDone =  rep(FALSE,2)
  
  ##update dataset to put the previous one
  rv$current.obj <- rv$dataset[[last(names(rv$dataset))]] 
  
})





############ SCREEN NORMALIZATION  #########
output$screenNormalization1 <- renderUI({

})




output$screenNormalization2 <- renderUI({

})