tabPanel("Open MSnset file",
         value = "open",
         sidebarCustom(),
         splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
                     wellPanel(id = "wellPanelFileOpen"
                               ,fileInput("file", 
                                          "Open a MSnset file",
                                          multiple = FALSE)
                     ),
                     tagList(
                         moduleDatasetOverviewUI("overview_openMSnset"),
                        uiOutput("infoAboutAggregationTool")
                     )
         )
)