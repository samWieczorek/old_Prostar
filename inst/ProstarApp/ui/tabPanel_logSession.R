tabPanel("Log session",
         #value = "ChangeDataset",
         
         tabsetPanel(
             id = "test",
             tabPanel("Log session",
                      value = "ChangeDataset",
                      sidebarCustom(),
                      conditionalPanel(
                          id = "wellPanel_changeDataset",
                          condition =  "true",
                          width=widthWellPanel,
                          DT::dataTableOutput("logSession")
                      )
             ),
            tabPanel("R source code (Beta)", 
                     busyIndicator("Calculation in progress",wait = 0),
                     
                      uiOutput("code")
             )
         )
)
