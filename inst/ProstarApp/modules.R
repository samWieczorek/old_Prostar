
library(shiny)
library(highcharter)



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


moduleBoxplotUI <- function(id) {
    ns <- NS(id)
    
    plotOutput(ns("BoxPlot"))
}


moduleDatasetOverviewUI <- function(id) {
    ns <- NS(id)
    
    uiOutput(ns("DatasetOverview"))
}


moduleFilterStringbasedOptionsUI <- function(id) {
    ns <- NS(id)
    
    uiOutput(ns("FilterStringbasedOptions"))
}
