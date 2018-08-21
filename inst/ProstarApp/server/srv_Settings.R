<<<<<<< HEAD
callModule(modulePopover,"modulePopover_numPrecision", data = reactive(list(title=HTML(paste0("<strong><font size=\"4\">Numerical precisions</font></strong>")),
                                                                        content= "Set the number of decimals to display for numerical values.")))



observeEvent(input$settings_nDigits,{
  rv$nDigits <- input$settings_nDigits
})


# observeEvent(input$settings_InteractivePlots,{
#  
=======
callModule(modulePopover,"modulePopover_numPrecision", data = reactive(list(title=HTML(paste0("<strong><font size=\"4\">Numerical precisions</font></strong>")),
                                                                        content= "Set the number of decimals to display for numerical values.")))



observeEvent(input$settings_nDigits,{
  rv$nDigits <- input$settings_nDigits
})


# observeEvent(input$settings_InteractivePlots,{
#  
>>>>>>> ceee6a0719f73dbf86eb71708e3099eee6d98083
# })