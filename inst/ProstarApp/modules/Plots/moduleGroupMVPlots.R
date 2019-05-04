missingValuesPlotsUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(width = 4, highchartOutput(ns("histo_MV")), height="600px"),
    column(width = 4, highchartOutput(ns("histo_MV_per_lines"))),
    column(width = 4, highchartOutput(ns("histo_MV_per_lines_per_conditions")))
  )
  
}




#------------------------------------------------------------
missingValuesPlots <- function(input, output, session, dataIn) {
  
  output$histo_MV <- renderHighchart({
    req(dataIn()$obj)
    rv.prostar$settings()$examplePalette
    tmp <- NULL
    #isolate({
    #pattern <- paste0(GetCurrentObjName(),".MVplot1")
    tmp <- wrapper.mvHisto_HC(dataIn()$obj,palette=rv.prostar$settings()$examplePalette)
    #future(createPNGFromWidget(tmp,pattern))
    #  })
    tmp
  })
  
  
  
  output$histo_MV_per_lines <- renderHighchart({
    req(dataIn()$obj)
    tmp <- NULL
    isolate({
     # pattern <- paste0(GetCurrentObjName(),".MVplot2")
      tmp <- 
        wrapper.mvPerLinesHisto_HC(dataIn()$obj, 
                                   c(2:length(colnames(Biobase::pData(dataIn()$obj)))))
      #future(createPNGFromWidget(tmp,pattern))
    })
    tmp
  })
  
  
  
  output$histo_MV_per_lines_per_conditions <- renderHighchart({
    req(dataIn()$obj)
    rv.prostar$settings()$examplePalette
    tmp <- NULL
    isolate({
     # pattern <- paste0(GetCurrentObjName(),".MVplot2")
      tmp <- wrapper.mvPerLinesHistoPerCondition_HC(dataIn()$obj, 
                                                    c(2:length(colnames(Biobase::pData(dataIn()$obj))))
                                                    ,rv.prostar$settings()$examplePalette)
      #future(createPNGFromWidget(tmp,pattern))
    })
    tmp
  })
}
