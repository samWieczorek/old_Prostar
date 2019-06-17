library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
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


######
source(file.path(".", "modules/modulePopover.R"), local = TRUE)$value



  
  
          
 ################################ header ####################################
 header <- dashboardHeaderPlus(
  fixed = TRUE,
  title = "Prostar",
  titleWidth = 150,
  enable_rightsidebar = TRUE,
  
  rightSidebarIcon = "gears",
  tags$li(class = "dropdown",
        tags$style(".main-header {max-height: 10px}"),
        tags$style(".main-header .logo {height: 40px;}"),
        tags$style(".sidebar-toggle {height: 30px; padding-top: 1px !important;}"),
        tags$style(".navbar {min-height:10px !important}")
        ),
      left_menu = tagList(
              uiOutput('header')
            )
  )
          
          
 rightsidebar <- rightSidebar(
  #background = "grey",
  rightSidebarTabContent(
    id = 1,
    title = "Descr. stats",
    icon = "desktop",
    active = TRUE
    #modulePlotsUI('showPlots')
    )
  )
          
            ################################# sidebar ###################################
  sidebar <- dashboardSidebar(
    width = 150,
    sidebarMenu(id = 'sidebar_left',
                menuItem("Home" , tabName = "prostar", selected = T),
                menuItem("Misc.",
                         menuSubItem("Settings",tabName = "settings"),
                         menuSubItem("Release notes",tabName = "releaseNotes"),
                         menuSubItem("Check for updates",tabName = "checkUpdates")
                        ),
                menuItem("Data manager", tabName = "dataManager"),
                menuItemOutput("menuItem_dataAnalysis"),
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
                                      .skin-blue .main-header .logo {
                                      
                                      }
                                      
                                      /* logo when hovered */
                                      /*.skin-blue .main-header .logo:hover {
                                      background-color: #ffffff;
                                      }*/
                                      
                                      /* navbar (rest of the header) */
                                      .skin-blue .main-header .navbar {
                                      background-color: lightgrey;
                                      
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
                                      
                                      
                                      ')
                         )
              ),
            ###### DIV LOADING PAGE  #######
    #         div(
    #           id = "loading_page",
    #           modalDialog(
    #             tagList(
    #               tags$h1(style='text-align: center; color: black', "Prostar is loading, please wait..."),
    #               br(),
    #               tags$div(class="progress",
    #                        tags$div(class="indeterminate")
    #               )
    #             ),
    #             easyClose = FALSE,
    #             size='s',
    #             footer = NULL
    #           )
    #         ),
    #         
    #         ###### DIV MAIN CONTENT  #######
    # hidden(
    #    div(
    #        id = "main_content",
    shinyjs::useShinyjs(),
    includeCSS("www/progressBar/progressBar.css"),
    tags$head(tags$style(sass(sass_file("www/css/sass-size.scss"),
                              sass_options(output_style = "expanded")))),
    tags$head(includeCSS("www/css/css-progress-wizard/css/progress-wizard.min.css")),
    tags$head(includeCSS('http://netdna.bootstrapcdn.com/font-awesome/4.1.0/css/font-awesome.min.css')),
    titlePanel("", windowTitle = "Prostar"),
    

    
           rclipboardSetup(),
           modulePlotsUI('showPlots'),
           tabItems(
              tabItem(tabName = "prostar", moduleHomepageUI("homepage") ),
    
              tabItem(tabName = "dataManager",
                      moduleOpenDatasetUI("moduleOpenDataset"),
                    uiOutput('btn_launch')
                    ),
              tabItem(tabName = "dataAnalysis",
                      h3('data analysis'),
                      uiOutput('UI_dataAnalysis')
              ),
              tabItem(tabName = "settings", moduleSettingsUI("modSettings")),
    
              tabItem(tabName = "releaseNotes", moduleReleaseNotesUI("modReleaseNotes")),
              tabItem(tabName = "checkUpdates",
                      uiOutput("baseVersions"),
                      DT::dataTableOutput("tab_versions", width = '600px'),
                      br(), br(),
                      uiOutput("infoForNewVersions")
                      ),
    
    
             tabItem(tabName = "links", bsModal("modallinks", "Links", NULL, size = "large", moduleInsertMarkdownUI('links_MD'))),
              tabItem(tabName = "FAQ",bsModal("modalFAQ", "FAQ", NULL, size = "large", moduleInsertMarkdownUI('FAQ_MD'))),
              tabItem(tabName = "bugReport", bsModal("modalbugreport", "Bug report", NULL, size = "large", moduleBugReportUI('bugreport')))
              )
            )


ui <- dashboardPagePlus(
    enable_preloader = TRUE,
    
  
  
  # skin = "black",
  header,
  sidebar,
  body,
  rightsidebar
)


