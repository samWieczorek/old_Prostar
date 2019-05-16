


moduleMVPlotsUI <- function(id) {
  ns <- NS(id)
  tagList( 
    tags$div(
      tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                highchartOutput(ns("plot_viewNAbyMean"), width='600px')),
      tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                plotOutput(ns("plot_showImageNA"), width='600px'))
    )
  )
}


moduleMVPlots <- function(input, output, session, data) {
  
  output$plot_viewNAbyMean <- renderHighchart({
    req(data())
    
    wrapper.hc_mvTypePlot2(data())
  })
  
  output$plot_showImageNA <- renderPlot({
    req(data())
    isolate({
      wrapper.mvImage(data())
    })
  })
}

