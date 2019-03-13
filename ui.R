
source(file.path(".", "modules/modulesUI.R"),  local = TRUE)$value
source(file.path(".", "modules/moduleDataManagerUI.R"),  local = TRUE)$value
source(file.path(".", "modules/modulePlotsUI.R"),  local = TRUE)$value

######

ui <- fluidPage(
  
  tagList(
    navbarPage(
      position = "fixed-top",
      #itle = 'Home',
      id="navPage",
      tabPanel("Home"),
      moduleDataManagerUI('datamanager'),
      tabPanel("Plots",
                modulePlotsUI('showPlots')),
      tabPanel(value='testChooseDataset',
               div(
                 id='.test',
                 uiOutput("chooseDataset" )
               )
      )
    )
  )

)