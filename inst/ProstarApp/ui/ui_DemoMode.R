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
         #checkboxInput("showDemoDatasetPDF", "Show PDF documentation", value=FALSE),
         
         hr(),
         uiOutput("infoAboutDemoDataset"),
         tags$div(
           tags$div( style="display:inline-block; vertical-align: top;",
                     moduleStaticDataTableUI("overview_DemoMode")
           )
         )
 

)