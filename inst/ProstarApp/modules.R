
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



moduleMVPlotsUI <- function(id) {
    ns <- NS(id)
     fluidRow(
        column(width = 5, plotOutput(ns("plot_viewNAbyMean"))),
        column(width = 7, plotOutput(ns("plot_showImageNA")))
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
    uiOutput(ns("DatasetOverview"))
}


moduleFilterStringbasedOptionsUI <- function(id) {
    ns <- NS(id)
    uiOutput(ns("FilterStringbasedOptions"))
}
