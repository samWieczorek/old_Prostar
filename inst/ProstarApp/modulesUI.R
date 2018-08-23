
modulePopoverUI <- function(id){
    ns <- NS(id)
    uiOutput(ns("customPopover"))
}



moduleLegendColoredExprsUI <- function(id){
    ns <- NS(id)
    
    tagList(
        tags$p(tags$b("Legend of colors")),
        
        fluidRow(
            column(width=2, 
                   tags$div(class="input-color", checked=NA,
                            tags$input(type="text", value=""),
                            tags$div(class="color-box", style="background-color: lightblue;")
                   )),
            column(width=10, tags$p("Partially Observed Value"))
        ),
        
        fluidRow(
            column(width=2, 
                   tags$div(class="input-color", checked=NA,
                            tags$input(type="text", value=""),
                            tags$div(class="color-box", style="background-color: orange;")
                   )),
            column(width=10, tags$p("Missing in Entire Condition"))
        )
    )
}

moduleVolcanoplotUI <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("nbSelectedItems")),
  dataTableOutput(ns("Infos")),
  #uiOutput(ns("found")),
  highchartOutput(ns("volcanoPlot"))
  )
}


moduleDetQuantImpValuesUI <- function(id){
  ns <- NS(id)
  dataTableOutput(ns("detQuantValues_DT"))
}

missingValuesPlotsUI <- function(id) {
    ns <- NS(id)
    fluidRow(
        column(width = 4, highchartOutput(ns("histo_MV"))%>% withSpinner(type=spinnerType), height="600px"),
        column(width = 4, highchartOutput(ns("histo_MV_per_lines"))%>% withSpinner(type=spinnerType)),
        column(width = 4, highchartOutput(ns("histo_MV_per_lines_per_conditions"))%>% withSpinner(type=spinnerType))
    )
    
}


moduleDensityplotUI <- function(id) {
    ns <- NS(id)
    
   highchartOutput(ns("Densityplot")) %>% withSpinner(type=spinnerType)

}



moduleMVPlotsUI <- function(id) {
    ns <- NS(id)
      tags$div(
       tags$div( style="display:inline-block; vertical-align: middle;",
                 highchartOutput(ns("plot_viewNAbyMean")) %>% withSpinner(type=spinnerType)
       ),
       tags$div( style="display:inline-block; vertical-align: middle;",
                 plotOutput(ns("plot_showImageNA"))%>% withSpinner(type=spinnerType)
       )
     )
     
}



# moduleViewNAbyMeanUI <- function(id) {
#     ns <- NS(id)
#     plotOutput(ns("viewNAbyMean")) %>% withSpinner(type=spinnerType)
# }
# 
# moduleShowImageNAUI <- function(id) {
#     ns <- NS(id)
#     plotOutput(ns("showImageNA")) %>% withSpinner(type=spinnerType)
# }


moduleBoxplotUI <- function(id) {
    ns <- NS(id)
    plotOutput(ns("BoxPlot")) %>% withSpinner(type=spinnerType)
}


moduleDatasetOverviewUI <- function(id) {
    ns <- NS(id)
    dataTableOutput(ns("DatasetOverviewDT"))
}


moduleFilterStringbasedOptionsUI <- function(id) {
    ns <- NS(id)
    uiOutput(ns("FilterStringbasedOptions"))
}
