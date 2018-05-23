tabPanel("Session logs",
         #value = "ChangeDataset",
         
         tabsetPanel(
             id = "test",
             tabPanel("Session logs",
                      value = "ChangeDataset",
                      sidebarCustom(),
                      tagList(
                          DT::dataTableOutput("logSession", width = "800px")
                      )
             ),
            tabPanel("R source code (Beta)", 
                     busyIndicator(WaitMsgCalc,wait = 0),
                     tagList(
                       uiOutput("InfoTextSourceCode"),
                      uiOutput("code")
                     )
             )
         )
)
