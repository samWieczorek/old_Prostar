tabPanel("Demo mode",
         value = "demoTab",
         tags$div(
           tags$div( style="display:inline-block; vertical-align: middle; padding-right: 20px;",
                     uiOutput("chooseDataset")
           ),
           tags$div( style="display:inline-block; vertical-align: middle;",
                     p(""),
                     actionButton("loadDemoDataset", "Load demo dataset")
           )
         ),
         checkboxInput("showDemoDatasetPDF", "Show PDF documentation", value=FALSE),
         hr(),
         moduleDatasetOverviewUI("overview_DemoMode")
         ,uiOutput("showDatasetDoc")
          #,uiOutput("progressDemoMode")
         ,uiOutput("infoAboutDemoDataset")
         )