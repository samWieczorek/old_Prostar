tabPanel("Demo data",
         value = "demoTab",
         tags$div(
           tags$div( style="display:inline-block; vertical-align: middle; padding-right: 20px;",
                     uiOutput("chooseDataset")
           ),
           tags$div( style="display:inline-block; vertical-align: middle;",
                     p(""),
                     uiOutput("linktoDemoPdf")
           )),
           
           actionButton("loadDemoDataset", "Load demo dataset",class = actionBtnClass),

         
         hr(),
         uiOutput("infoAboutDemoDataset")
         

)