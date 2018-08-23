

source(file.path("ui", "ui_Configure.R"),  local = TRUE)$value



#---------------------------------------------------------------------------------------------------------

shinyUI <- tagList(
 
  rclipboardSetup(),
  
    shinyjs::useShinyjs(),
    #,tags$head(includeScript("google-analytics.js"))
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
,includeCSS("www/css/prostar.css")
, inlineCSS(".body { font-size:14px;}")
, inlineCSS(".rect {float: left;
                     width: 100px;
                      height: 20px;
                      margin: 2px;
                      border: 1px solid rgba(0, 0, 0, .2);}")
, inlineCSS(".green {background: green;}")
, inlineCSS(".red {background:red;}")
, inlineCSS(".grey {background:lightgrey;}")

#,source(file.path("ui", "ui_NavbarPage.R"),  local = TRUE)$value
,navbarPage(theme = shinytheme("cerulean")
            
            ,id = "navPage"
            ,absolutePanel(
              id  = "#AbsolutePanel",
              top = 0,
              right = 50,
              width = "500px",
              height = "50px",
              draggable = FALSE,
              fixed = FALSE,
              cursor = "default"
              ,uiOutput("datasetAbsPanel" )
            )
            ,navbarMenu("Prostar",
                        source(file.path("ui", "ui_Home.R"),  local = TRUE)$value,
                        source(file.path("ui", "ui_Settings.R"),  local = TRUE)$value
            )
            ,navbarMenu("Dataset manager",
                        source(file.path("ui", "ui_OpenMSnSetFile.R"),  local = TRUE)$value,
                        source(file.path("ui", "ui_ConvertData.R"),  local = TRUE)$value,
                        source(file.path("ui", "ui_DemoMode.R"),  local = TRUE)$value,
                        source(file.path("ui", "ui_Export.R"),  local = TRUE)$value,
                        source(file.path("ui", "ui_LogSession.R"),  local = TRUE)$value
            )
            
            ,navbarMenu("Help",
                        source(file.path("ui", "ui_UsefulLinks.R"),  local = TRUE)$value,
                        source(file.path("ui", "ui_ReleaseNotes.R"),  local = TRUE)$value,
                        source(file.path("ui", "ui_FAQ.R"),  local = TRUE)$value,
                        source(file.path("ui", "ui_CheckForUpdates.R"),  local = TRUE)$value,
                        source(file.path("ui", "ui_BugReport.R"),  local = TRUE)$value
            )
))


