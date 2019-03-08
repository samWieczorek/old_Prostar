
source(file.path(".", "modulesUI.R"),  local = TRUE)$value
######

ui <- fluidPage(
  tagList(
    navbarPage(
      position = "fixed-top",
      #itle = 'Home',
      id="navPage",
      "",
      
      
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