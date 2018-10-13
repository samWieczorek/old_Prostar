
modulePopoverUI <- function(id){
    ns <- NS(id)
    uiOutput(ns("customPopover"))
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
    
  dataTableOutput(ns("detQuantValues_DT"))
  )
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
                 highchartOutput(ns("plot_viewNAbyMean"))
       ),
       tags$div( style="display:inline-block; vertical-align: middle;",
                 plotOutput(ns("plot_showImageNA"))
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
      selectInput(ns("choosePlot"), "Choose plot", choices=c( "violinplot"="violinplot","boxplot"="boxplot"), width='150px'),
    highchartOutput(ns("BoxPlot")),
    plotOutput(ns("viewViolinPlot"))
    )
}


moduleStaticDataTableUI <- function(id) {
    ns <- NS(id)
    tags$div(
      tags$div( style="display:inline-block; vertical-align: middle; align: center;",
                withSpinner(dataTableOutput(ns("StaticDataTable")))
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