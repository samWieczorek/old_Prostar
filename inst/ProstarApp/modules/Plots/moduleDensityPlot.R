


moduleDensityplotUI <- function(id) {
  ns <- NS(id)
  
  
  highchartOutput(ns("Densityplot")) %>% withSpinner(type=spinnerType)
  
  
}

#------------------------------------------------------------
moduleDensityplot <- function(input, output, session) {
  
  #outputOptions(output, 'Densityplot', suspendWhenHidden=FALSE)
  
  output$Densityplot <- renderHighchart({
    req(GetCurrentMSnSet())
    rv$settings()$examplePalette
    rv$PlotParams$legendForSamples
    tmp <- NULL
    isolate({
      
      withProgress(message = 'Making plot', value = 100, {
        pattern <- paste0(GetCurrentObjName(),".densityplot")
        tmp <- DAPAR::densityPlotD_HC(GetCurrentMSnSet(), 
                                      rv$PlotParams$legendForSamples,
                                      rv$settings()$examplePalette)
        # future(createPNGFromWidget(rv$tempplot$boxplot,pattern))
      })
    })
    tmp
  })
}
