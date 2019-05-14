tabPanel("Demo data",
         value = "demoTab",
         tags$div(
           tags$div( style="display:inline-block; vertical-align: middle; padding-right: 20px;",
                     uiOutput("chooseDataset")
           ),
           
           tags$div( style="display:inline-block; vertical-align: middle; padding-right: 20px;",
                     p(""),
                     actionButton("loadDemoDataset", "Load demo dataset",class = actionBtnClass)
           ),
           tags$div( style="display:inline-block; vertical-align: middle;",
                     p(""),
                     uiOutput("linktoDemoPdf")
           )
         ),
         
         hr(),
         uiOutput("infoAboutDemoDataset")
         

)