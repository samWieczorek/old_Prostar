

mod_plots_mv_histo_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 4, highchartOutput(ns("histo_MV")), height="600px"),
      column(width = 4, highchartOutput(ns("histo_MV_per_lines"))),
      column(width = 4, highchartOutput(ns("histo_MV_per_lines_per_conditions")))
    )  
  )
}





mod_plots_mv_histo_server <- function(input, output, session, data, palette) {
  
  output$histo_MV <- renderHighchart({
    data()
    
    tmp <- NULL
    #isolate({
    #pattern <- paste0(GetCurrentObjName(),".MVplot1")
    tmp <- wrapper.mvHisto_HC(data(),
<<<<<<< HEAD
                              base_palette = unique(palette())) 
=======
                              palette=palette()
                              )
>>>>>>> origin
    #future(createPNGFromWidget(tmp,pattern))
    #  })
    tmp
  })
  
  
  
  output$histo_MV_per_lines <- renderHighchart({
    data()
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
    data()
    #palette()
    tmp <- NULL
    isolate({
      #pattern <- paste0(GetCurrentObjName(),".MVplot2")
      tmp <- wrapper.mvPerLinesHistoPerCondition_HC(data(), 
                                                    palette=unique(palette()))
      #future(createPNGFromWidget(tmp,pattern))
    })
    tmp
  })
}