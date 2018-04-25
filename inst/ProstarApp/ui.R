options(shiny.trace=FALSE)
options(shiny.reactlog=TRUE)


library(highcharter)

source(file.path("ui", "uiConfigure.R"),  local = TRUE)$value



#---------------------------------------------------------------------------------------------------------

shinyUI <- tagList(
    
    
    shinyjs::useShinyjs(),
    #,tags$head(includeScript("google-analytics.js"))
    #,tags$head(includeScript("piwik.js"))
    tags$head(HTML("<script type='text/javascript' src='sbs/shinyBS.js'></script>")),
    
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
,includeCSS("www/css/legend.css")


#,uiOutput("disableAggregationTool")
#,uiOutput("disableBioanalysisTool")
,navbarPage(
    id = "navPage",
    absolutePanel(
        id  = "#AbsolutePanel",
                top = 10,
                right = 300,
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

   #"",

            tabPanel(test, uiOutput("aboutText")),
   

   navbarMenu("Dataset manager",
              source(file.path("ui", "ui_openMSnSetFile.R"),  local = TRUE)$value,
              source(file.path("ui", "ui_convertData.R"),  local = TRUE)$value,
              source(file.path("ui", "ui_DemoMode.R"),  local = TRUE)$value,
              source(file.path("ui", "ui_Export.R"),  local = TRUE)$value,
              #source(file.path("ui", "ui_Settings.R"),  local = TRUE)$value,
              source(file.path("ui", "ui_logSession.R"),  local = TRUE)$value
              
   ),
source(file.path("ui", "ui_descriptiveStatistics.R"),  local = TRUE)$value,

navbarMenu("Data processing",
           
    source(file.path("ui", "ui_Filtering.R"),  local = TRUE)$value,
    source(file.path("ui", "ui_Normalization.R"),  local = TRUE)$value,
    source(file.path("ui", "ui_Imputation.R"),  local = TRUE)$value,
    source(file.path("ui", "ui_Aggregation.R"),  local = TRUE)$value,
    source(file.path("ui", "ui_AnaDiff.R"),  local = TRUE)$value,
    source(file.path("ui", "ui_GO_Enrich.R"),  local = TRUE)$value
    ),

source(file.path("ui", "ui_Help.R"),  local = TRUE)$value
)
)
