
modulePopoverUI <- function(id){
    ns <- NS(id)
    uiOutput(ns("customPopover"))
}



moduleLegendColoredExprsUI <- function(id){
    ns <- NS(id)
    
    tagList(
        hr(),
        h4("Legend of colors"),
        
        fluidRow(
            column(width=2, 
                   tags$div(class="input-color", checked=NA,
                            tags$input(type="text", value=""),
                            tags$div(class="color-box", style="background-color: lightblue;")
                   )),
            column(width=10, h5("Partially Observed Value"))
        ),
        
        fluidRow(
            column(width=2, 
                   tags$div(class="input-color", checked=NA,
                            tags$input(type="text", value=""),
                            tags$div(class="color-box", style="background-color: orange;")
                   )),
            column(width=10, h5("Missing in Entire Condition"))
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


missingValuesPlotsUI <- function(id) {
    ns <- NS(id)
    fluidRow(
        column(width = 4, highchartOutput(ns("histo_MV")), height="600px"),
        column(width = 4, highchartOutput(ns("histo_MV_per_lines"))),
        column(width = 4, highchartOutput(ns("histo_MV_per_lines_per_conditions")))
    )
    
}


moduleDensityplotUI <- function(id) {
    ns <- NS(id)
    
    highchartOutput(ns("Densityplot"))
}



moduleMVPlotsUI <- function(id) {
    ns <- NS(id)
     fluidRow(
        column(width = 7, highchartOutput(ns("plot_viewNAbyMean"), height="600px"), height="600px"),
        column(width = 5, plotOutput(ns("plot_showImageNA"), height=600, width=200), height="600px")
    )
}

moduleViewNAbyMeanUI <- function(id) {
    ns <- NS(id)
    plotOutput(ns("viewNAbyMean"))
}

moduleShowImageNAUI <- function(id) {
    ns <- NS(id)
    plotOutput(ns("showImageNA"))
}


moduleBoxplotUI <- function(id) {
    ns <- NS(id)
    plotOutput(ns("BoxPlot"))
}


moduleDatasetOverviewUI <- function(id) {
    ns <- NS(id)
    dataTableOutput(ns("DatasetOverviewDT"))
}


moduleFilterStringbasedOptionsUI <- function(id) {
    ns <- NS(id)
    uiOutput(ns("FilterStringbasedOptions"))
}
