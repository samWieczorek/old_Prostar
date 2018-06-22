tabPanel("Update design",
         value = "updateDesignTab",
         br(), br(),
         tagList(
             fluidRow(
                 column(width=6,tags$b("1 - Fill the \"Label\" column to identify the conditions to compare.")),
                 column(width=6,uiOutput("updateDesign_UI_checkConditions")  )
             ),
             fluidRow(
                 column(width=6,uiOutput("updateDesign_UI_hierarchicalExp")),
                 column(width=6,uiOutput("updateDesign_checkDesign") )
             ),
             uiOutput("updateDesign_SaveDesign"),
         uiOutput("designUpdated"),
         
         hr(),
         tags$div(

             tags$div(style="display:inline-block; vertical-align: top;",
                      uiOutput("viewNewDesign",width="100%")
             ),
             tags$div(style="display:inline-block; vertical-align: top;",
                      shinyjs::hidden(
                          div(id = "updateDesign_exLevels",uiOutput("updateDesign_designExamples")))
             )

)

)
)
