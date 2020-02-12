library(shiny)
library(shinyjs)
library(shinyjqui)
library(sass)
library(shinyBS)


## Load of modules that are to be imbedded in the sidebar
# files <-list.files('src',full.name = TRUE, pattern='*.R$', recursive=TRUE)
# 
# for (f in files){
#   print(paste0('sourcing ', f))
#   if (f != 'src/core.R')
#   source(f, local=TRUE)$value
# }
# 

source(file.path("./src", "modules/Misc/modulePopover.R"),  local = TRUE)$value
source(file.path("./src", "modules/Menu_Home/moduleReleaseNotes.R"),  local = TRUE)$value
source(file.path("./src", "modules/Misc/moduleStaticDataTable.R"),  local = TRUE)$value
source(file.path("./src", "modules/Menu_DataManager/moduleConvertData.R"),  local = TRUE)$value
source(file.path("./src", "modules/Menu_DataManager/moduleOpenMSnSet.R"),  local = TRUE)$value
source(file.path("./src", "modules/Menu_DataManager/moduleOpenDemoDataset.R"),  local = TRUE)$value
source(file.path("./src", "modules/Menu_DataManager/moduleInfoDataset.R"),  local = TRUE)$value

source(file.path("./src", "modules/Menu_Home/moduleHomepage.R"),  local = TRUE)$value
source(file.path("./src", "modules/Menu_Home/moduleCheckUpdates.R"),  local = TRUE)$value
source(file.path("./src", "modules/Menu_Home/moduleReleaseNotes.R"),  local = TRUE)$value
source(file.path("./src", "modules/Menu_Home/moduleSettings.R"),  local = TRUE)$value

source(file.path("./src", "modules/Menu_Help/moduleBugReport.R"),  local = TRUE)$value

theme = shinythemes::shinytheme("cerulean")
#---------------------------------------------------------------------------------------------------------
timeoutSeconds <- 30*60

inactivity <- sprintf("function idleTimer() {
var t = setTimeout(logout, %s);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions
function logout() {
Shiny.setInputValue('timeOut', '%ss')
}
function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, %s);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();", timeoutSeconds*1000, timeoutSeconds, timeoutSeconds*1000)




jsResetCode <- "shinyjs.reset = function() {history.go(0)}"



shinyUI <- fluidPage(
  tags$script(inactivity),    
  
  #theme = "css/ceruleanProstar.css",
  theme = shinythemes::shinytheme("cerulean"),
  tagList(
    
    shinyjs::useShinyjs(),
    extendShinyjs(text = jsResetCode, functions = c("reset")),
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
        
        shinyjs::useShinyjs(),
        includeCSS("www/progressBar/progressBar.css"),
        tags$head(tags$style(sass(sass_file("www/css/sass-size.scss"),
                                  sass_options(output_style = "expanded")))),
        tags$head(includeCSS("www/css/css-progress-wizard/css/progress-wizard.min.css")),
        tags$head(includeCSS('http://netdna.bootstrapcdn.com/font-awesome/4.1.0/css/font-awesome.min.css')),
        tags$head(includeCSS("www/css/arrow.css")),
        launchGA(),
        tags$head(HTML("<script type='text/javascript' src='sbs/shinyBS.js'></script>")),
        tags$head(tags$style(".modal-dialog{ width:200px}")),
        tags$head( tags$style(HTML("hr {border-top: 1px solid #000000;}"))),
        tags$style(HTML(".tab-content {padding-top: 40px; }"))
        
        #sidebarPanelWidth()
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
             position = "fixed-top",
            id = "navPage",
            
            inverse = TRUE,
            
            absolutePanel(
              id  = "#AbsolutePanel",
              top = 0, right = 50, width = "500px",height = "50px",
              draggable = FALSE,fixed = FALSE,
              cursor = "default"
              ,uiOutput("datasetAbsPanel" )
            ),
            #modulePlotsUI('showPlots')
            navbarMenu("Prostar",
                       tabPanel(title="Home",
                                value="HomeTab",moduleHomepageUI("homepage")),
                       tabPanel(title="Global settings",
                                value="GlobalSettingsTab", moduleSettingsUI("modSettings")),
                       tabPanel("Release notes",
                                value="ReleaseNotesTab",moduleReleaseNotesUI("modReleaseNotes")),
                       tabPanel("Check for updates",
                                value="CheckUpdatesTab",moduleCheckUpdatesUI("modCheckUpdates"))
                       
            ),
            navbarMenu("Data manager",
                       tabPanel("Open MSnset",value = 'openMSnsetTab',moduleOpenMSnSetUI("moduleOpenMSnSet")),
                       tabPanel("Convert",value = "convertTab",moduleConvertDataUI("moduleProcess_Convert")),
                       tabPanel("Demo data",  value='demoTab', moduleOpenDemoDatasetUI("mod_OpenDemoDataset")),
                       source(file.path("./src", "ui_ReloadProstar.R"),  local = TRUE)$value
            ),
            
          #,navbarMenu("Data analysis",
            #          uiOutput('UI_dataAnalysis'))
          
             navbarMenu("Help",
                         tabPanel("Links",value="usefulLinksTab",  moduleInsertMarkdownUI('links_MD')),
                         tabPanel("FAQ", value="faqTab",  moduleInsertMarkdownUI('FAQ_MD')),
                         tabPanel("Bug report",value="bugReportTab",  moduleBugReportUI('bugreport'))
            )
            
          ) ## end navbarPage
        )  ## end div for main content 2
      ) ## end div for main content 1
      
    ) ## end hidden
    
  )
) ## end fluid

