


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
    settings <- rv.prostar$settings()
    rv.prostar$settings()$examplePalette
    rv.prostar$settings()$legendForSamples
    print("IN : moduleDensityplot ")
    tmp <- NULL
    isolate({
      
      withProgress(message = 'Making plot', value = 100, {
        pattern <- paste0(dataIn()$name,".densityplot")
        tmp <- densityPlotD_HC(dataIn()$obj, 
                                      rv.prostar$settings()$legendForSamples,
                                      rv.prostar$settings()$examplePalette)
        # future(createPNGFromWidget(rv$tempplot$boxplot,pattern))
      })
    })
    tmp
  })
}
