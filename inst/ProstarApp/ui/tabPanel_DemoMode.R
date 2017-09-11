tabPanel("Demo mode",
         id = "demo",
         sidebarCustom(),
         splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
                     wellPanel(id = "chooseDatasetFromDAPARdata_wellPanel"
                               ,uiOutput("chooseDataset")
                               ,actionButton("loadDemoDataset", "Load demo dataset")
                     ),
                     conditionalPanel(id = "wellPanelOpenFile",
                                      condition = "true",
                                      h3("Quick overview of the dataset"),
                                      uiOutput("overviewDemoDataset"),
                                      uiOutput("showDatasetDoc")
                                      
                                      # uiOutput("infoAboutDemoDataset")
                     )
         ))