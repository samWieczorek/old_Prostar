
source(file.path(".", "modulesUI.R"),  local = TRUE)$value
######

ui <- fluidPage(
  tagList(
    actionButton("rst_process", "reset process"),
    navbarPage(
      title = 'test',
      id="navPage",
      navbarMenu("Process",
                 tabPanel("ProcessA",moduleAUI('processA')),
                 tabPanel("ProcessB",moduleBUI('processB')),
                 tabPanel("ProcessC",moduleCUI('processC'))
      )
    ),
    div(
      id = "loading_page",
      absolutePanel(
        id  = "AbsolutePanel",
        class = "panel panel-default",
        style= "text-align: center; background-color: lightgrey;",
        top = '30%',
        left = '25%',
        width = "50%",
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