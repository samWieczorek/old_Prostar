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
    
    navbarPage(
      position = "fixed-top",
      #itle = 'Home',
      id="navPage",
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
    )
    
    )
)