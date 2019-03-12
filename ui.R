
source(file.path(".", "modulesUI.R"),  local = TRUE)$value
source(file.path(".", "moduleDataManagerUI.R"),  local = TRUE)$value

######

ui <- fluidPage(
  tagList(
    navbarPage(
      position = "fixed-top",
      #itle = 'Home',
      id="navPage",
      absolutePanel(
        id  = "#AbsolutePanel",
        top = 0, right = 50, width = "500px",height = "50px",
        draggable = FALSE,fixed = FALSE,
        cursor = "default"
        ,uiOutput("chooseDataset" )
      ),
      
      tabPanel("Home"),
      moduleDataManagerUI('datamanager'),
      tabPanel("Plots",
                modulePlotsUI('showPlots'))
    )
  )

)