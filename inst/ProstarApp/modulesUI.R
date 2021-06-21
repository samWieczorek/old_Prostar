

NUM_PAGES_FILTERING <- 3








module_Not_a_numericUI <- function(id){
  ns <- NS(id)
  uiOutput(ns("msg_not_numeric"))
}








moduleVolcanoplotUI <- function(id){
  ns <- NS(id)
  tagList(
          uiOutput(ns("nbSelectedItems")),
          highchartOutput(ns("volcanoPlot"), width='600px', height='600px'),

    uiOutput(ns("quantiDT"))
  )
}






########################################################
###### -------------------------------------------------
########################################################
moduleDetQuantImpValuesUI <- function(id){
  ns <- NS(id)
  tagList(
    h5("The missing values will be imputed by the following values :"),
    DT::dataTableOutput(ns("detQuantValues_DT"))
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
      tagList( 
        tags$div(
          tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                    highchartOutput(ns("plot_viewNAbyMean"), width='600px')),
          tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                    uiOutput(ns("WarnForImageNA")),
                    imageOutput(ns("plot_showImageNA"), width='600px'))
       )
      )
}



moduleDesignExampleUI <- function(id){
  ns <- NS(id)
  tagList(
     rHandsontableOutput(ns("nlevelsExample"))
  )
  
}


moduleBoxplotUI <- function(id) {
    ns <- NS(id)
    tagList(
      highchartOutput(ns("BoxPlot")),
      imageOutput(ns("viewViolinPlot")),
      selectInput(ns("choosePlot"), "Choose plot", choices=c( "violinplot"="violinplot","boxplot"="boxplot"), width='100px')
      )
}





moduleFilterStringbasedOptionsUI <- function(id) {
    ns <- NS(id)
    uiOutput(ns("FilterStringbasedOptions"))
}



moduleInsertMarkdownUI <- function(id){
  ns <- NS(id)
  uiOutput(ns("insertMD"))
}