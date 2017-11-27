tabPanel("Demo mode",
         id = "demo",
         sidebarCustom(),
         splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
                     wellPanel(id = "chooseDatasetFromDAPARdata_wellPanel"
                               ,uiOutput("chooseDataset")
                               ,checkboxInput("showDemoDatasetPDF", "Show PDF documentation", value=FALSE)
                               ,actionButton("loadDemoDataset", "Load demo dataset")
                     ),
                     conditionalPanel(id = "wellPanelOpenFile",
                                      condition = "true",
                                      h3("Quick overview of the dataset"),
                                      moduleDatasetOverviewUI("overview_DemoMode"),
                                      uiOutput("showDatasetDoc")
                                      
                                      # uiOutput("infoAboutDemoDataset")
                     )
         ))