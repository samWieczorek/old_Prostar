library(shiny)
library(shinyjs)

source(file.path("ui", "uiConfigure.R"),  local = TRUE)$value



#---------------------------------------------------------------------------------------------------------

shinyUI <- tagList(
shinyjs::useShinyjs(),
    includeCSS("www/progressBar/progressBar.css"),
    titlePanel("", windowTitle = "Prostar"),

    # Loading message
    div(
        id = "loading_page",
        tags$head(tags$style(
          HTML('#loadingPanel {background-color: rgba(0,36,106,190);}')
        )),
        absolutePanel(
          id  = "loadingPanel",
          class = "panel panel-default",
          top = '30%',
          left = '0%',
          width = "100%",
          height = "150px",
          draggable = FALSE,
          fixed = TRUE,
          tagList(
            tags$h1(style='text-align: center', "Prostar is loading, please wait..."),br(),
            tags$div(class="progress",
                     tags$div(class="indeterminate")
            )
          )
        )
    ),
    hidden(
        div(
            id = "main_content",
   
    
    
    #,tags$head(includeScript("google-analytics.js"))
    tags$head(HTML("<script type='text/javascript' src='sbs/shinyBS.js'></script>")),
    
    inlineCSS(appCSS),
    
sidebarPanelWidth()
,includeCSS("www/css/prostar.css")

#,navbarPageWithInputs(
,navbarPage(
    theme = shinytheme("cerulean"),
    
    id = "navPage",
    absolutePanel(
        id  = "#AbsolutePanel",
                top = 0,
                right = 50,
                width = "400px",
                height = "50px",
                draggable = FALSE,
                fixed = FALSE,
                cursor = "default",
                uiOutput("datasetAbsPanel" )
    ),
    navbarMenu("Prostar",
               #value = "ProstarMenu",
               source(file.path("ui", "ui_Home.R"),  local = TRUE)$value,
               source(file.path("ui", "ui_Settings.R"),  local = TRUE)$value
               ),
   

   navbarMenu("Dataset manager",
              #value = "DatasetManagerMenu",
              source(file.path("ui", "ui_OpenMSnSetFile.R"),  local = TRUE)$value,
              source(file.path("ui", "ui_ConvertData.R"),  local = TRUE)$value,
              source(file.path("ui", "ui_UpdateDesign.R"),  local = TRUE)$value,
              source(file.path("ui", "ui_DemoMode.R"),  local = TRUE)$value,
              source(file.path("ui", "ui_Export.R"),  local = TRUE)$value,
              source(file.path("ui", "ui_LogSession.R"),  local = TRUE)$value
              
   ),
source(file.path("ui", "ui_DescriptiveStatistics.R"),  local = TRUE)$value,

navbarMenu("Data processing",
           #value="DataProcessingMenu",
            source(file.path("ui", "ui_Filtering.R"),  local = TRUE)$value,
            source(file.path("ui", "ui_Normalization.R"),  local = TRUE)$value,
            source(file.path("ui", "ui_Imputation.R"),  local = TRUE)$value,
            source(file.path("ui", "ui_Aggregation.R"),  local = TRUE)$value,
            source(file.path("ui", "ui_AnaDiff.R"),  local = TRUE)$value,
            source(file.path("ui", "ui_GO_Enrich.R"),  local = TRUE)$value
    ),
navbarMenu("Help",
           #value="DataProcessingMenu",
           tabPanel("Useful links",
                    htmlOutput("References")
                    ),
           tabPanel("Release notes",
                    uiOutput("versionLog"),
                    uiOutput("warningDependanciesVersion")),
           tabPanel("FAQ",
                    htmlOutput("FAQ_output")
                    ),
           tabPanel("Check for updates",
                    br(),
                    br(),
                    dataTableOutput("tab_versions")
            )
)


#inputs = modulePopoverUI("modulePopover_dataset")
#inputs = list(uiOutput("datasetAbsPanel" )
)
        )

)

)
