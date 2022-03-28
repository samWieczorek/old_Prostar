mod_plotsMetacellHistos_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 4, highchartOutput(ns("histo_Metacell")), height="600px"),
      column(width = 4, highchartOutput(ns("histo_Metacell_per_lines"))),
      column(width = 4, highchartOutput(ns("histo_Metacell_per_lines_per_conditions")))
    )  
  )
}


mod_plotsMetacellHistos_server <- function(id, obj, pal, pattern) {

    moduleServer(
      id,
      function(input, output, session) {
 
  output$histo_Metacell <- renderHighchart({
    obj()
    
    tmp <- NULL
    #isolate({
    tmp <- metacellHisto_HC(obj = obj(), 
                            pattern = pattern(), 
                            pal = pal())
    #future(createPNGFromWidget(tmp,pattern))
    #  })
    tmp
  })
  
  
  
  output$histo_Metacell_per_lines <- renderHighchart({
    obj()
    tmp <- NULL
   # isolate({
     # pattern <- paste0(GetCurrentObjName(),".MVplot2")
      tmp <- 
        metacellPerLinesHisto_HC(obj = obj(), 
                                 pattern = pattern(),
                                 indLegend = c(2:length(colnames(pData(obj()))))
                                   )
      #future(createPNGFromWidget(tmp,pattern))
   # })
    tmp
  })
  
  
  
  output$histo_Metacell_per_lines_per_conditions <- renderHighchart({
    obj()
    tmp <- NULL
   # isolate({
      #pattern <- paste0(GetCurrentObjName(),".MVplot2")
      tmp <- metacellPerLinesHistoPerCondition_HC(obj = obj(), 
                                                  pattern = pattern(),
                                                  pal = pal())
      #future(createPNGFromWidget(tmp,pattern))
   # })
    tmp
  })
      }
    )
  }