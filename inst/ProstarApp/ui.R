
library(shiny)
library(shinyjs)
library(shinyjqui)
library(sass)
source(file.path("ui", "ui_Configure.R"),  local = TRUE)$value
source(file.path(".", "modules/Plots/modulePlots.R"),  local = TRUE)$value

theme = shinythemes::shinytheme("cerulean")
#---------------------------------------------------------------------------------------------------------
jsResetCode <- "shinyjs.reset = function() {history.go(0)}"

shinyUI <- fluidPage(
  #theme = "css/ceruleanProstar.css",
  theme = shinythemes::shinytheme("cerulean"),
  
  tagList(
  
  shinyjs::useShinyjs(),
  extendShinyjs(text = jsResetCode),
  
  includeCSS("www/progressBar/progressBar.css"),
  tags$head(tags$style(sass(sass_file("www/css/sass-size.scss"),
                            sass_options(output_style = "expanded")))),
  titlePanel("", windowTitle = "Prostar"),
  
  ###### DIV LOADING PAGE  #######
      div(
        id = "loading_page",
       absolutePanel(
      id  = "AbsolutePanel",
      class = "panel panel-default",
      style= "text-align: center; background-color: #25949A;",
      top = '30%',
      left = '25%',
      width = "50%",
      height = "150px",
      draggable = FALSE,
      fixed = TRUE,
      tagList(
        tags$h1(style='text-align: center; color: white', "Prostar is loading, please wait..."),
        br(),
        tags$div(class="progress",
                 tags$div(class="indeterminate")
        )
        )
 )
   ),
 
 ###### DIV MAIN CONTENT  #######
  hidden(
    div(
      id = "main_content",
      
      tags$head(includeCSS("www/css/arrow.css")),
      launchGA(),
      tags$head(HTML("<script type='text/javascript' src='sbs/shinyBS.js'></script>")),
      tags$head(tags$style(".modal-dialog{ width:200px}")),
      tags$head( tags$style(HTML("hr {border-top: 1px solid #000000;}"))),
      tags$style(HTML(".tab-content {padding-top: 40px; }")),
      
       sidebarPanelWidth()
      ,includeCSS("www/css/prostar.css")
      , inlineCSS(".body { font-size:14px;}")
      , inlineCSS(".rect {float: left;
                  width: 100px;
                  height: 20px;
                  margin: 2px;
                  border: 1px solid rgba(0, 0, 0, .2);}")
      , inlineCSS(".green {background: #06AB27}")
      , inlineCSS(".red {background: #C90404}")
      , inlineCSS(".grey {background:lightgrey;}"),
      
      div(
        id = "header",
      navbarPage(
                 #,inlineCSS('.btn-default{color:#555555;background-color:#000000;border-color:rgba(0,0,0,0.1)}')
        position = "fixed-top",
                  id = "navPage",

                  inverse = TRUE,

                  absolutePanel(
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
                              source(file.path("ui", "ui_CheckForUpdates.R"),  local = TRUE)$value,
                              source(file.path("ui", "ui_ReloadProstar.R"),  local = TRUE)$value
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

    ) ## end navbarPage
  )  ## end div for main content 2
    ) ## end div for main content 1
  
) ## end hidden

)
) ## end fluid


