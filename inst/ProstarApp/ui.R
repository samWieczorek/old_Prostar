library(shiny)
library(shinyjs)
library(shinyjqui)
library(sass)



source(file.path(".", "modules/DataManager/moduleDataManager.R"),  local = TRUE)$value
source(file.path(".", "modules/Plots/modulePlots.R"),  local = TRUE)$value
source(file.path(".", "modules/moduleBugReport.R"),  local = TRUE)$value

######

ui <- fluidPage(
  theme = shinytheme("cerulean"),
  inlineCSS(".modal-backdrop {z-index: 10}"),
  
  tagList(
    shinyjs::useShinyjs(),
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
        
        rclipboardSetup(),
        
        tags$head(includeCSS("www/css/arrow.css")),
        tags$head(HTML("<script type='text/javascript' src='sbs/shinyBS.js'></script>")),
        tags$head(tags$style(".modal-dialog{ width:200px}")),
        tags$head( tags$style(HTML("hr {border-top: 1px solid #000000;}"))),
        includeCSS("www/css/prostar.css")
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
        
        div(
          id = "header",
          
    navbarPage(
      position = "fixed-top",
      #itle = 'Home',
      id="navPage",
      inverse = TRUE,
      tagList(
        modulePlotsUI('showPlots'),
        uiOutput("chooseDataset")),
        tabPanel("Home"),
      
        moduleDataManagerUI('datamanager'),
        navbarMenu("Help",
                 tabPanel("Useful links",
                          moduleInsertMarkdownUI('links_MD')
                 ),
                 tabPanel("FAQ",
                          moduleInsertMarkdownUI("FAQ_MD")
                 ),
                 tabPanel("Bug report",
                          br(),br(),br(),br(),br(),br(),
                          moduleBugReportUI('bugreport'))
      )
    ))
        )
    ))
)