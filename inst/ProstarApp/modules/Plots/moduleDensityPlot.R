


moduleDensityplotUI <- function(id) {
  ns <- NS(id)
  highchartOutput(ns("Densityplot"))
}

#------------------------------------------------------------
moduleDensityplot <- function(input, output, session, dataIn) {
  ns <- session$ns
  #outputOptions(output, 'Densityplot', suspendWhenHidden=FALSE)
  
  output$Densityplot <- renderHighchart({
    req(dataIn())
    rv.prostar$settings()$examplePalette
    rv.settings$legendForSamples
    print("IN : moduleDensityplot ")
    tmp <- NULL
    isolate({
      
      withProgress(message = 'Making plot', value = 100, {
        pattern <- paste0(dataIn()$name,".densityplot")
        tmp <- DAPAR::densityPlotD_HC(dataIn()$obj, 
                                      rv.settings$legendForSamples,
                                      rv.prostar$settings()$examplePalette)
        # future(createPNGFromWidget(rv$tempplot$boxplot,pattern))
      })
    })
    tmp
  })
}
