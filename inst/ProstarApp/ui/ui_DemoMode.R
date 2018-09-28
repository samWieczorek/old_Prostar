tabPanel("Demo data",
         value = "demoTab",
         tags$div(
           tags$div( style="display:inline-block; vertical-align: middle; padding-right: 20px;",
                     uiOutput("chooseDataset")
           ),
           tags$div( style="display:inline-block; vertical-align: middle;",
                     p(""),
                     actionButton("loadDemoDataset", "Load demo dataset",
                                  color = "primary"
                     )
           )
         ),
         checkboxInput("showDemoDatasetPDF", "Show PDF documentation", value=FALSE),
         hr(),
         moduleStaticDataTableUI("overview_DemoMode")
         ,uiOutput("showDatasetDoc")
         #,uiOutput("progressDemoMode")
         ,uiOutput("infoAboutDemoDataset")
)