tabPanel("Session logs",
         #value = "ChangeDataset",
         
         tabsetPanel(
             id = "test",
             tabPanel("Session logs",
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
                     busyIndicator(WaitMsgCalc,wait = 0),
                     
                      uiOutput("code")
             )
         )
)
