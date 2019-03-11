
source(file.path(".", "modulesUI.R"),  local = TRUE)$value
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
      
      tabPanel("Home",
               br(),br(),br(),br(),
               selectInput("selectPipeline", "Select pipeline",
                           choices=c("None"="","Peptide"="Peptide", "Protein"="Protein", "P2p" = "P2p"),
                           selected=character(0))),
      tabPanel("Plots",
                modulePlotsUI('showPlots'))
    ),
    
    div(
      id = "summary",
      absolutePanel(
        id  = "AbsolutePanel2",
        class = "panel panel-default",
        style= "text-align: center; background-color: lightgrey;",
        top = '30%',
        left = '25%',
        width = "200px",
        height = "150px",
        draggable = TRUE,
        fixed = TRUE,
        tagList(
          uiOutput("summary")
        )
      )
      
    )
  )

)