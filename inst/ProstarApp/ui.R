
library(shiny)
library(shinyjs)
library(shinyjqui)
source(file.path("ui", "ui_Configure.R"),  local = TRUE)$value



#---------------------------------------------------------------------------------------------------------

shinyUI <- tagList(
  shinyjs::useShinyjs(),
  includeCSS("www/progressBar/progressBar.css"),
  
  titlePanel("", windowTitle = "Prostar"),
      div(
        id = "loading_page",
       # tagList(
      tags$head(tags$style(
      HTML('#AbsolutePanel {background-color: rgba(0,36,106,190);}')
    )),
    absolutePanel(
      id  = "AbsolutePanel",
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
      rclipboardSetup(),
      #tags$head(includeScript("www/google-analytics.js")),
      tags$head(HTML("<script type='text/javascript' src='sbs/shinyBS.js'></script>")),
      inlineCSS(appCSS),
      tags$head(tags$style(".modal-dialog{ width:200px}")),
      sidebarPanelWidth()
      ,includeCSS("www/css/prostar.css")
      ,includeCSS("www/css/fontawesome.css")
      , inlineCSS(".body { font-size:14px;}")
      , inlineCSS(".rect {float: left;
                  width: 100px;
                  height: 20px;
                  margin: 2px;
                  border: 1px solid rgba(0, 0, 0, .2);}")
      , inlineCSS(".green {background: green;}")
      , inlineCSS(".red {background:red;}")
      , inlineCSS(".grey {background:lightgrey;}"),
      
      # #header {
      # position: fixed;
      # z-index: 100000;
      # }

div(
        id = "header",
      #,source(file.path("ui", "ui_NavbarPage.R"),  local = TRUE)$value
      navbarPage(theme = shinytheme("cerulean")
                  ,id = "navPage"
                  ,absolutePanel(
                    id  = "#AbsolutePanel",
                    top = 0, right = 50, width = "500px",height = "50px",
                    draggable = FALSE,fixed = FALSE,
                    cursor = "default"
                    ,uiOutput("datasetAbsPanel" )
                  )
                  ,navbarMenu("Prostar",
                              source(file.path("ui", "ui_Home.R"),  local = TRUE)$value,
                              source(file.path("ui", "ui_Settings.R"),  local = TRUE)$value,
                              source(file.path("ui", "ui_ReleaseNotes.R"),  local = TRUE)$value,
                              source(file.path("ui", "ui_CheckForUpdates.R"),  local = TRUE)$value
                  )
                  ,navbarMenu("Data manager",
                              source(file.path("ui", "ui_OpenMSnSetFile.R"),  local = TRUE)$value,
                              source(file.path("ui", "ui_ConvertData.R"),  local = TRUE)$value,
                              source(file.path("ui", "ui_DemoMode.R"),  local = TRUE)$value,
                              source(file.path("ui", "ui_Export.R"),  local = TRUE)$value
                              #source(file.path("ui", "ui_LogSession.R"),  local = TRUE)$value
                  )
                  
                  ,navbarMenu("Help",
                              source(file.path("ui", "ui_UsefulLinks.R"),  local = TRUE)$value,
                              source(file.path("ui", "ui_FAQ.R"),  local = TRUE)$value,
                              source(file.path("ui", "ui_BugReport.R"),  local = TRUE)$value
                  )
    )
  )
    )
  
))


