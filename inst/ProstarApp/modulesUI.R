

NUM_PAGES_FILTERING <- 3



modulePopoverUI <- function(id){
    ns <- NS(id)
    uiOutput(ns("customPopover"))
}

module_Not_a_numericUI <- function(id){
  ns <- NS(id)
  uiOutput(ns("msg_not_numeric"))
}

moduleLegendColoredExprsUI <- function(id,colorsTypeMV){
    ns <- NS(id)
    
    tagList(
        tags$p(tags$b("Legend of colors")),
        
        fluidRow(
            column(width=2, 
                   tags$div(class="input-color", checked=NA,
                            tags$input(type="text", value=""),
                            tags$div(class="color-box", style=paste0("background-color: ",colorsTypeMV$POV, ";"))
                   )),
            column(width=10, tags$p("Partially Observed Value"))
        ),
        
        fluidRow(
            column(width=2, 
                   tags$div(class="input-color", checked=NA,
                            tags$input(type="text", value=""),
                            tags$div(class="color-box", style=paste0("background-color: ",colorsTypeMV$MEC, ";"))
                   )),
            column(width=10, tags$p("Missing in Entire Condition"))
        )
    )
}

moduleVolcanoplotUI <- function(id){
  ns <- NS(id)
  tagList(
    tags$div(
      tags$div( style="display:inline-block; vertical-align: top; padding-right: 40px;",
                uiOutput(ns("nbSelectedItems"))
      ),
      tags$div( style="display:inline-block; vertical-align: top;",
                highchartOutput(ns("volcanoPlot"), width='600px', height='600px')
      )
    ),

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
    
  
     highchartOutput(ns("Densityplot")) %>% withSpinner(type=spinnerType)
   

}



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



# moduleViewNAbyMeanUI <- function(id) {
#     ns <- NS(id)
#     plotOutput(ns("viewNAbyMean")) %>% withSpinner(type=spinnerType)
# }
# 
# moduleShowImageNAUI <- function(id) {
#     ns <- NS(id)
#     plotOutput(ns("showImageNA")) %>% withSpinner(type=spinnerType)
# }

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
      plotOutput(ns("viewViolinPlot")),
      selectInput(ns("choosePlot"), "Choose plot", choices=c( "violinplot"="violinplot","boxplot"="boxplot"), width='100px')
      )
}


moduleStaticDataTableUI <- function(id) {
    ns <- NS(id)
    tags$div(
      tags$div( style="display:inline-block; vertical-align: middle; align: center;",
                withSpinner(DT::dataTableOutput(ns("StaticDataTable")))
      )
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