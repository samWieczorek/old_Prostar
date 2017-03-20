options(shiny.trace=FALSE)
options(shiny.reactlog=TRUE)


library(DAPAR)
library(DAPARdata)
library(shiny)
library(rhandsontable)
library(data.table)
library(shinyjs)
library(shinyAce)
library(highcharter)


source(file.path("ui", "uiConfigure.R"),  local = TRUE)$value



#---------------------------------------------------------------------------------------------------------

shinyUI <- tagList(
    useShinyjs(),
    #,tags$head(includeScript("google-analytics.js"))
    #,tags$head(includeScript("piwik.js"))
    
    inlineCSS(appCSS),
    
    # Loading message
    div(
        br(),br(),br(),br(),br(),br(),
        id = "loading-content",
        h2("Prostar is loading, please wait...")
    ),

    
titlePanel("", windowTitle = "Prostar"),
sidebarPanelWidth()
,includeCSS("www/progressBar/progressBar.css")
,includeScript("www/progressBar/ShinyProgress2.js")


,uiOutput("disableAggregationTool")
,navbarPage(
    id = "navPage",
    absolutePanel(id  = "#AbsolutePanel",
                top = 10,
                right = 300,
                # bottom = 600,
                # left = "600px",
                width = "200px",
                height = "50px",
                draggable = FALSE,
                fixed = FALSE,
                cursor = "default",
                selectInput("datasets", 
                            "Dataset versions", 
                            choices = list("None"=""), 
                            width = '250px')
    ),

    "",


            tabPanel(test,
                    uiOutput("aboutText")
                    ),
    
source(file.path("ui", "datasetManager.R"),  local = TRUE)$value,
source(file.path("ui", "descriptiveStatistics.R"),  local = TRUE)$value,

navbarMenu("Data processing",
    source(file.path("ui", "tabPanel_Filtering.R"),  local = TRUE)$value,
    source(file.path("ui", "tabPanel_Normalization.R"),  local = TRUE)$value,
    source(file.path("ui", "tabPanel_Imputation.R"),  local = TRUE)$value,
    source(file.path("ui", "tabPanel_Aggregation.R"),  local = TRUE)$value,
    source(file.path("ui", "tabPanel_AnaDiff.R"),  local = TRUE)$value),

    source(file.path("ui", "tabPanel_Help.R"),  local = TRUE)$value


)
)
