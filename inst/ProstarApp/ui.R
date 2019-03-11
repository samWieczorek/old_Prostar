
library(shiny)
library(shinyjs)
library(shinyjqui)
#library(sass)
source(file.path("ui", "ui_Configure.R"),  local = TRUE)$value


theme = shinytheme("cerulean")
#---------------------------------------------------------------------------------------------------------

shinyUI <- fluidPage(
  #theme = "css/ceruleanProstar.css",
  theme = shinytheme("cerulean"),
  
  tagList(
  
  shinyjs::useShinyjs(),
  includeCSS("www/progressBar/progressBar.css"),
  if (require(sass))
    {
    tags$head(tags$style(sass(sass_file("www/css/sass-size.scss"),
                            sass_options(output_style = "expanded"))))},
  
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
      
      rclipboardSetup(),

      tags$head(includeCSS("www/css/arrow.css")),
      #tagList(tags$div(class="arrow")),
      tags$p("VERSION CHANTIER BIS !!!", style='font-size: 30px; color: red;'),
      #tags$head(includeScript("www/google-analytics.js")),
      tags$head(HTML("<script type='text/javascript' src='sbs/shinyBS.js'></script>")),
      #inlineCSS(appCSS),
      tags$head(tags$style(".modal-dialog{ width:200px}")),
      tags$head( tags$style(HTML("hr {border-top: 1px solid #000000;}"))),
      # tags$head(tags$style(" table.dataTable thead th {
      #                      padding: 8px 10px !important;
      #                      }
      #                      ")),
       sidebarPanelWidth()
      ,includeCSS("www/css/prostar.css")
      #,includeCSS("www/css/fontawesome.css")
      , inlineCSS(".body { font-size:14px;}")
      , inlineCSS(".rect {float: left;
                  width: 100px;
                  height: 20px;
                  margin: 2px;
                  border: 1px solid rgba(0, 0, 0, .2);}")
      , inlineCSS(".green {background: #06AB27}")
      , inlineCSS(".red {background: #C90404}")
      , inlineCSS(".grey {background:lightgrey;}"),
      #tags$style(".btn.disabled {background-color: red;}"),
     # inlineCSS('.btn{color: blue; background-color: #35e51d}'),
      #inlineCSS('.btn-default{color:#555555;background-color:#ffbbff;border-color:rgba(0,0,0,0.1)}'),
      
      # #header {
      # position: fixed;
      # z-index: 100000;
      # }

      div(
        id = "header",
      #,source(file.path("ui", "ui_NavbarPage.R"),  local = TRUE)$value
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

    ) ## end navbarPage
  )  ## end div for main content 2
    ) ## end div for main content 1
  
) ## end hidden

)
) ## end fluid


