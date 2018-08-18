tabPanel(title="Home",
         value="HomeTab",
          tagList(
           uiOutput("citationText"),
           tags$hr(),
           tags$div(
             style="padding: 0 50px; float: left;",
             tags$img(src='images/logoProstarComplet.png', width='150px', height='150px')
                     ),
         tags$div(
           style="margin-top: 50px;",
             tags$p("")
         ),
         uiOutput("versionsText"),
         tags$br(), tags$br(),
         uiOutput("descriptionText")
         )
)
