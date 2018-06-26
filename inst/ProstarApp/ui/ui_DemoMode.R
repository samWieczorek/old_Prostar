tabPanel("Demo mode",
         value = "demoTab",
         sidebarCustom(),
         splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
                     wellPanel(id = "chooseDatasetFromDAPARdata_wellPanel",
                               uiOutput("chooseDataset"),
                               uiOutput("optionsDemomode")
                               
                     ),
                     tagList(
                       moduleDatasetOverviewUI("overview_DemoMode")
                       ,uiOutput("showDatasetDoc")
                       #,uiOutput("progressDemoMode")
                       ,uiOutput("infoAboutDemoDataset")
                     )
         ))