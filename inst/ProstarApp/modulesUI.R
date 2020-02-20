

NUM_PAGES_FILTERING <- 3



modulePopoverUI <- function(id){
  ns <- NS(id)
  uiOutput(ns("customPopover"))
}




module_Not_a_numericUI <- function(id){
  ns <- NS(id)
  uiOutput(ns("msg_not_numeric"))
}


#####################################
moduleTrackProtUI <- function(id){
  ns <- NS(id)
  tagList(
    
    #uiOutput(ns("typeSelect")),
    uiOutput(ns("typeSelect_UI")),
    # selectInput(ns("typeSelect"), "Type of selection",
    #             choices=c("None" = "None", "Protein list"="ProteinList", "Random"="Random", "Column"="Column"),
    #             width=('130px')),
    
    uiOutput(ns("listSelect_UI")),
    uiOutput(ns("randomSelect_UI")),
    uiOutput(ns("columnSelect_UI"))
  )
}


moduleLegendColoredExprsUI <- function(id,colorsTypeMV){
  ns <- NS(id)
  
  tagList(
    tags$p(tags$b(moduleLegendColoredExprsUI_text)),
    
    fluidRow(
      column(width=2, 
             tags$div(class="input-color", checked=NA,
                      tags$input(type="text", value=""),
                      tags$div(class="color-box", style=paste0("background-color: ",colorsTypeMV$POV, ";"))
             )),
      column(width=10, tags$p(moduleLegendColoredExprsUI_POV_text))
    ),
    
    fluidRow(
      column(width=2, 
             tags$div(class="input-color", checked=NA,
                      tags$input(type="text", value=""),
                      tags$div(class="color-box", style=paste0("background-color: ",colorsTypeMV$MEC, ";"))
             )),
      column(width=10, tags$p(moduleLegendColoredExprsUI_MEC_text))
    )
  )
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
    h5(moduleDetQuantImpValuesUI_text),
    
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
    
    br(), br(),
    tags$div(
      tags$div(style="display:inline-block; vertical-align: middle;",
               highchartOutput(ns("BoxPlot")),
               imageOutput(ns("viewViolinPlot"))
      ),
      tags$div(style="display:inline-block; vertical-align: middle;",
               selectInput(ns("choosePlot"), "Choose plot", choices=c( "violinplot"="violinplot","boxplot"="boxplot"), width='100px')
      ),
      
      tags$div(style="display:inline-block; vertical-align: middle;",
               moduleTrackProtUI(ns('widgets'))
      )
      
    )         
    
    
  )
}


moduleStaticDataTableUI <- function(id) {
  ns <- NS(id)
  tags$div(
    tags$div( style="display:inline-block; vertical-align: middle; align: center;",
              uiOutput(ns("warningOnSize")),
              DT::dataTableOutput(ns("StaticDataTable"))
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