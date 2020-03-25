 

moduleHeatmapUI <- function(id) {
  ns <- NS(id)
  

tagList(
  div(
    div(
      style="display:inline-block; vertical-align: middle; padding-right: 20px;",
      selectInput(ns("distance"),"Distance",
                  choices = G_heatmapDistance_Choices, 
                  selected = "euclidean",
                  width="150px")
    ),
    div(
      style="display:inline-block; vertical-align: middle;",
      selectInput(ns("linkage"),"Linkage",
                  choices=G_heatmapLinkage_Choices,
                  selected='complete',
                  width="150px")
    ),
    
    tags$hr(),
    uiOutput(ns("DS_PlotHeatmap"))
  )
)

}



moduleHeatmap <- function(input, output, session, dataIn) {
  ns <- session$ns
  


output$DS_PlotHeatmap <- renderUI({
  req(dataIn())
  if (nrow(dataIn()$obj) > limitHeatmap){
    tags$p("The dataset is too big to compute the heatmap in a reasonable time.")
  }else {
    tagList(
      plotOutput(ns("heatmap"), width = "900px", height = "600px") %>%  shinycssloaders::withSpinner(type=spinnerType)
      
    )
  }
})



output$heatmap <- renderPlot({
  heatmap()
})





# 
# observeEvent(input$distance,{rv.prostar$settings()$heatmap.distance <- input$distance})
# observeEvent(input$linkage,{rv.prostar$settings()$heatmap.linkage <- input$linkage})

heatmap <- reactive({
  
  req(dataIn()$obj)
  input$linkage
  input$distance
  
  isolate({  wrapper.heatmapD(dataIn()$obj,
                              input$distance, 
                              input$linkage,
                              TRUE)
  })
  
})


}
