
source(file.path(".", "modules/modulesUI.R"),  local = TRUE)$value
source(file.path(".", "modules/DataManager/moduleDataManagerUI.R"),  local = TRUE)$value
source(file.path(".", "modules/Plots/modulePlotsUI.R"),  local = TRUE)$value

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
      
      tabPanel(value='testChooseDataset',
               div(
                 id='.test',
                 uiOutput("chooseDataset" )
               )
      )
    )
  )

)