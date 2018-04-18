
moduleSymbolicFilterUI <- function(id) {
    ns <- NS(id)
    uiOutput(ns("SymbolicFilter"))
}




moduleSymbolicFilterUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("SymbolicFilter"))
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
        column(width = 5, plotOutput(ns("plot_showImageNA"), height="600px"), height="600px")
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
