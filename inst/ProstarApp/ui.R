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
    uiOutput("plots"),
    
    navbarPage(
      position = "fixed-top",
      #itle = 'Home',
      id="navPage",
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
      ),
      tabPanel(value='testChooseDataset',
               div(
                 id='.test',
                 uiOutput("chooseDataset" )
               )
      )
    )
  )

)