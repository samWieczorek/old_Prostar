missingValuesPlotsUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(width = 4, highchartOutput(ns("histo_MV")), height="600px"),
    column(width = 4, highchartOutput(ns("histo_MV_per_lines"))),
    column(width = 4, highchartOutput(ns("histo_MV_per_lines_per_conditions")))
  )
  
}




#------------------------------------------------------------
missingValuesPlots <- function(input, output, session, data, paletteConds) {
  
  output$histo_MV <- renderHighchart({
    req(data())
    paletteConds()
    tmp <- NULL
    #isolate({
    #pattern <- paste0(GetCurrentObjName(),".MVplot1")
    tmp <- wrapper.mvHisto_HC(data(),palette=paletteConds())
    #future(createPNGFromWidget(tmp,pattern))
    #  })
    tmp
  })
  
  
  
  output$histo_MV_per_lines <- renderHighchart({
    req(data())
    tmp <- NULL
    isolate({
     # pattern <- paste0(GetCurrentObjName(),".MVplot2")
      tmp <- 
        wrapper.mvPerLinesHisto_HC(data(), 
                                   c(2:length(colnames(Biobase::pData(data())))))
      #future(createPNGFromWidget(tmp,pattern))
    })
    tmp
  })
  
  
  
  output$histo_MV_per_lines_per_conditions <- renderHighchart({
    req(data())
    tmp <- NULL
    isolate({
     # pattern <- paste0(GetCurrentObjName(),".MVplot2")
      tmp <- wrapper.mvPerLinesHistoPerCondition_HC(data(), 
                                                    c(2:length(colnames(Biobase::pData(data()))))
                                                    ,paletteConds())
      #future(createPNGFromWidget(tmp,pattern))
    })
    tmp
  })
}
