library(shiny)
library(shinyjs)
library(shinyjqui)
library(sass)



source(file.path(".", "modules/DataManager/moduleDataManager.R"),  local = TRUE)$value
source(file.path(".", "modules/Plots/modulePlots.R"),  local = TRUE)$value
source(file.path(".", "modules/moduleBugReport.R"),  local = TRUE)$value

######

ui <- fluidPage(
  
  tagList(
    modulePlotsUI('showPlots'),
    
    
    navbarPage(
      position = "fixed-top",
      #itle = 'Home',
      id="navPage",
      absolutePanel(
        id  = "#AbsolutePanel",
        top = 0, right = 50, width = "500px",height = "50px",
        draggable = FALSE,fixed = FALSE,
        cursor = "default"
        ,uiOutput("chooseDataset")
      ),
      tabPanel("Home"),
      moduleDataManagerUI('datamanager'),
      navbarMenu("Help",
                 tabPanel("Useful links",
                          #value="usefulLinksTab",
                          moduleInsertMarkdownUI('links_MD')
                 ),
                 tabPanel("FAQ",
                          #value="faqTab",
                          moduleInsertMarkdownUI("FAQ_MD")
                 ),
                 tabPanel("Bug report",
                          br(),br(),br(),br(),br(),br(),
                          moduleBugReportUI('bugreport'))
      )
      )
    )
)