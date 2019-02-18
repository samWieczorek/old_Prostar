
callModule(moduleProcess, "moduleProcess_AnaDiff", 
           isDone = reactive({rvModProcess$moduleAnaDiffDone}), 
           pages = reactive({rvModProcess$moduleAnaDiff}),
           rstFunc = resetModuleAnaDiff)




######
resetModuleAnaDiff <- reactive({  
  
  ## update widgets values (reactive values)
  resetModuleProcess("AnaDiff")
  
  #rv$widgetsfiltering$seuilNA <- 0
  #rv$deleted.stringBased <- NULL
  # rv$deleted.mvLines <- NULL
  
  
  ## update widgets in UI
  #if (!is.null(input$showpvalTable) )updateCheckboxInput(session, 'showpvalTable', value = FALSE)
  updateSelectInput(session, "selectComparison", selected=rv$widgets$anaDiff$Comparison)
  updateSelectInput(session, "AnaDiff_seuilNA", selected = rv$widgets$anaDiff$filter_th_NA)
  updateRadioButtons(session, "AnaDiff_ChooseFilters", selected=rv$widgets$anaDiff$filterType)
  updateSelectInput(session, "tooltipInfo", selected=character(0))
  updateSelectInput(session,"calibrationMethod", selected = rv$widgets$anaDiff$calibMethod)
  updateNumericInput(session,"numericValCalibration",value = rv$widgets$anaDiff$numValCalibMethod)
  updateNumericInput(session,"nBinsHistpval",value=80)
  updateTextInput(session, "seuilPVal",  value=rv$widgets$anaDiff$th_pval)
  updateRadioButtons(session, "downloadAnaDiff", selected="All")
  updateCheckboxInput(session, "swapVolcano", value = rv$widgets$anaDiff$swapVolcano)
            
  rvModProcess$moduleAnaDiffDone = rep(FALSE, 4)
  
  ##update dataset to put the previous one
  #rv$current.obj <- rv$dataset[[last(names(rv$dataset))]] 
  rv$resAnaDiff <- NULL
  
})
#####

##--------------------------------------------------------
##---------------------------------------------------------



output$screenAnaDiff1 <- renderUI({

})



output$anaDiffPanel <- renderUI({
   moduleProcessUI("moduleProcess_AnaDiff")

})



output$screenAnaDiff2 <- renderUI({
  
  })    

output$screenAnaDiff3 <- renderUI({
  
})     


    
output$screenAnaDiff4 <- renderUI({     

})
