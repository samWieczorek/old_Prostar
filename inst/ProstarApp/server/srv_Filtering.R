
callModule(module_Nav_Process, "moduleProcess_Filtering", 
           isDone = reactive({rvModProcess$moduleFilteringDone}), 
           pages = reactive({rvModProcess$moduleFiltering}),
           rstFunc = resetModuleFiltering)

##---------------------------------------------------------------
##------------------------------------------------------------------


resetModuleFiltering <- reactive({  
  ## update widgets values (reactive values)
  resetModuleProcess("Filtering")
  
  rv$widgetsfiltering$seuilNA <- 0
  rv$deleted.stringBased <- NULL
  rv$deleted.mvLines <- NULL

  
  ## update widgets in UI
  updateSelectInput(session, "ChooseFilters", selected = rv$widgets$filtering$ChooseFilters)
  updateSelectInput(session, "seuilNA", selected = rv$widgets$filtering$seuilNA)
  
  rvModProcess$moduleFilteringDone = rep(FALSE, 4)
  ##update dataset to put the previous one
  rv$current.obj <- rv$dataset[[last(names(rv$dataset))]] 
  
  })


output$screenFiltering1 <- renderUI({
})




output$screenFiltering2 <- renderUI({
})



output$screenFiltering3 <- renderUI({
})




output$screenFiltering4 <- renderUI({     
  
  tagList(
    actionButton("ValidateFilters","Save filtered dataset",class = actionBtnClass)
    
  )
})


