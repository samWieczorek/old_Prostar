library(shiny)
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

######




ui <- fluidPage(
  theme = shinytheme("cerulean"),
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
        div(
          id = "header",
          #inlineCSS(".modal-backdrop {z-index: 1000}"),
          SetCustomCSS(),
          
          navbarPageWithInputs(
          position = "fixed-top",
          #itle = 'Home',
          id="navPage",
          inverse = TRUE,
          "",
          navbarMenu("Prostar",
                     tabPanel("Home",
                              moduleHomepageUI("homepage")),
                     tabPanel("Settings",
                              moduleSettingsUI("modSettings")
                     ),
                     tabPanel("Release notes",
                              moduleReleaseNotesUI("modReleaseNotes")
                              #source(file.path("ui", "ui_ReleaseNotes.R"),  local = TRUE)$value)
                     ),
                     tabPanel("Check for updates"
                              #source(file.path("ui", "ui_CheckForUpdates.R"),  local = TRUE)$value)
                     )
          ),
          tagList( modulePlotsUI('showPlots')),
          moduleOpenDatasetUI("openDataset"),
          navbarMenu("Help",
                 tabPanel("Useful links",
                          moduleInsertMarkdownUI('links_MD')
                 ),
                 tabPanel("FAQ",
                          moduleInsertMarkdownUI("FAQ_MD")
                 ),
                 tabPanel("Bug report",
                          moduleBugReportUI('bugreport'))
              ),
          inputs=uiOutput("chooseDataset")
    ))
        )
    ))
)