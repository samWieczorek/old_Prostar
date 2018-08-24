

source(file.path("ui", "uiConfigure.R"),  local = TRUE)$value



#---------------------------------------------------------------------------------------------------------

shinyUI <- tagList(
    
    
    shinyjs::useShinyjs(),
    titlePanel("", windowTitle = "Prostar"),
    
    
    
    # Loading message
    div(
        id = "loading-content",
        h2("Prostar is loading, please wait...")
    ),
    hidden(
        div(
            id = "main-content",
   
    
    
    #,tags$head(includeScript("google-analytics.js"))
    tags$head(HTML("<script type='text/javascript' src='sbs/shinyBS.js'></script>")),
    
    inlineCSS(appCSS),
    
sidebarPanelWidth()
,includeCSS("www/progressBar/progressBar.css")
,includeScript("www/progressBar/ShinyProgress2.js")
,includeCSS("www/css/prostar.css")

#,navbarPageWithInputs(
,navbarPage(
    theme = shinytheme("cerulean"),
    
    id = "navPage",
    absolutePanel(
        id  = "#AbsolutePanel",
                top = 0,
                right = 50,
                width = "500px",
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
                    dataTableOutput("tab_versions", width = '600px')
            )
)


#inputs = modulePopoverUI("modulePopover_dataset")
#inputs = list(uiOutput("datasetAbsPanel" )
)
        )

)

)
