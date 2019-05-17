library(shiny)
library(shinydashboard)

library(shinyjs)
library(shinyjqui)
library(sass)



source(file.path(".", "modules/DataManager/moduleOpenDataset.R"),  local = TRUE)$value
source(file.path(".", "modules/Plots/modulePlots.R"),  local = TRUE)$value
source(file.path(".", "modules/moduleBugReport.R"),  local = TRUE)$value
source(file.path(".", "modules/moduleStaticDataTable.R"),  local = environment())$value
source(file.path(".", "modules/moduleSettings.R"),  local = TRUE)$value
source(file.path(".", "modules/moduleHomepage.R"),  local = TRUE)$value
source(file.path(".", "modules/moduleReleaseNotes.R"),  local = TRUE)$value

source(file.path(".", "modules/DataManager/moduleOpenDataset.R"), local = TRUE)$value



source(file.path(".", "modules/moduleStaticDataTable.R"),  local = TRUE)$value
source(file.path(".", "modules/DataManager/moduleInfoDataset.R"),  local = TRUE)$value

source(file.path(".", "modules/modulePipeline.R"),  local = TRUE)$value

######
source(file.path(".", "modules/modulePopover.R"), local = TRUE)$value




# 
# ui <- fluidPage(
#   theme = shinytheme("cerulean"),
#   tagList(
#     shinyjs::useShinyjs(),
#     includeCSS("www/progressBar/progressBar.css"),
#     tags$head(tags$style(sass(sass_file("www/css/sass-size.scss"),
#                               sass_options(output_style = "expanded")))),
#     titlePanel("", windowTitle = "Prostar"),
#     ###### DIV LOADING PAGE  #######
#     div(
#       id = "loading_page",
#       absolutePanel(
#         id  = "AbsolutePanel",
#         class = "panel panel-default",
#         style= "text-align: center; background-color: #25949A;",
#         top = '30%',
#         left = '25%',
#         width = "50%",
#         height = "150px",
#         draggable = FALSE,
#         fixed = TRUE,
#         tagList(
#           tags$h1(style='text-align: center; color: white', "Prostar is loading, please wait..."),
#           br(),
#           tags$div(class="progress",
#                    tags$div(class="indeterminate")
#           )
#         )
#       )
#     ),
#     
#     ###### DIV MAIN CONTENT  #######
#     hidden(
#       div(
#         id = "main_content",
#         
#         rclipboardSetup(),
#         div(
#           id = "header",
#           #inlineCSS(".modal-backdrop {z-index: 1000}"),
#           SetCustomCSS(),
#           
#           navbarPageWithInputs(
#           position = "fixed-top",
#           #itle = 'Home',
#           id="navPage",
#           inverse = TRUE,
#           "",
#           navbarMenu("Prostar",
#                      tabPanel("Home",
#                               moduleHomepageUI("homepage")),
#                      tabPanel("Settings",
#                               moduleSettingsUI("modSettings")
#                      ),
#                      tabPanel("Release notes",
#                               moduleReleaseNotesUI("modReleaseNotes")
#                               #source(file.path("ui", "ui_ReleaseNotes.R"),  local = TRUE)$value)
#                      ),
#                      tabPanel("Check for updates"
#                               #source(file.path("ui", "ui_CheckForUpdates.R"),  local = TRUE)$value)
#                      )
#           ),
#           tagList( modulePlotsUI('showPlots')),
#           moduleOpenDatasetUI("openDataset"),
#           navbarMenu("Help",
#                  tabPanel("Useful links",
#                           moduleInsertMarkdownUI('links_MD')
#                  ),
#                  tabPanel("FAQ",
#                           moduleInsertMarkdownUI("FAQ_MD")
#                  ),
#                  tabPanel("Bug report",
#                           moduleBugReportUI('bugreport'))
#               ),
#           inputs=uiOutput("chooseDataset")
#     ))
#         )
#     ))
# )



################################ header ####################################
header <- dashboardHeader(
  title = "Prostar",
  # Set height of dashboardHeader
  tags$li(class = "dropdown",
          tags$style(".main-header {max-height: 10px}"),
          tags$style(".main-header .logo {height: 40px;}"),
          tags$style(".sidebar-toggle {height: 30px; padding-top: 1px !important;}"),
          tags$style(".navbar {min-height:10px !important}")
  )
  
)


################################# sidebar ###################################
sidebar <- dashboardSidebar(
   sidebarMenu(
     menuItem("Home" , tabName = "prostar"),
     menuItem("toto",
              menuSubItem("Settings",tabName = "settings"),
              menuSubItem("Release notes",tabName = "releaseNotes"),
              menuSubItem("Check for updates",tabName = "checkUpdates")
              ),
     menuItem("Data manager", tabName = "dataManager"),
    menuItem("Help" , tabname = "my_table",
             menuSubItem("Useful links",tabName = "links"),
             menuSubItem("FAQ",tabName = "FAQ"),
             menuSubItem("Bug report",tabName = "bugReport")
             )
    )
  
)



################################ body ######################################
body <- dashboardBody(
  tags$script(HTML("$('body').addClass('fixed');")),
  tags$style(HTML(".content {padding-top: 0px; padding-left: 20px;}")),
  tags$style(HTML(".main-sidebar {padding-top: 40px;}")),
  tags$head(tags$style(HTML('
                                /* logo */
                            /*.skin-blue .main-header .logo {
                            background-color: #aabbaa;
                            }*/
                            
                            /* logo when hovered */
                            /*.skin-blue .main-header .logo:hover {
                            background-color: #ffffff;
                            }*/
                            
                            /* navbar (rest of the header) */
                            .skin-blue .main-header .navbar {
                            background-color: #ffffff;
                            }
                            
                            /* main sidebar */
                            /*.skin-blue .main-sidebar {
                            background-color: #f0f0f0;
                            }*/
                            
                            /* active selected tab in the sidebarmenu */
                            /*.skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                            background-color: red;
                            }*/
                            
                            /* other links in the sidebarmenu */
                            /*.skin-blue .main-sidebar .sidebar .sidebar-menu a{
                            background-color: blue;
                            color: #000000;
                            }*/
                            
                            /* other links in the sidebarmenu when hovered */
                            /*.skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                            background-color: green;
                            }*/

                            /* toggle button when hovered  */
                            .skin-blue .main-header .navbar .sidebar-toggle:hover{
                            background-color: grey;
                            }

                            /* toggle button  */
                            .skin-blue .main-header .navbar .sidebar-toggle{
                            background-color: grey;
                            }
                            
                            /* body */
                            .content-wrapper, .right-side {
                            background-color: #ffffff;
                            }
                            
                            
                            '))),
  
  shinyjs::useShinyjs(),
  includeCSS("www/progressBar/progressBar.css"),
  tags$head(tags$style(sass(sass_file("www/css/sass-size.scss"),
                            sass_options(output_style = "expanded")))),
  titlePanel("", windowTitle = "Prostar"),
  
  tabItems(
    tabItem(tabName = "prostar", moduleHomepageUI("homepage") ),
    
    tabItem(tabName = "dataManager",
            uiOutput('tutu'),
            modulePeptidePipelineUI("test")
            ),

    
    tabItem(tabName = "settings", moduleSettingsUI("modSettings")),
    
    tabItem(tabName = "releaseNotes", moduleReleaseNotesUI("modReleaseNotes")),
    tabItem(tabName = "checkUpdates"),
    
    
    tabItem(tabName = "links", moduleInsertMarkdownUI('links_MD')),
    tabItem(tabName = "FAQ", moduleInsertMarkdownUI("FAQ_MD")),
    tabItem(tabName = "bugReport", moduleBugReportUI('bugreport'))
  )
  
)



# Put them together into a dashboardPage
ui <- dashboardPage(
  
  
  
  # skin = "black",
  header,
  sidebar,
  body
)
