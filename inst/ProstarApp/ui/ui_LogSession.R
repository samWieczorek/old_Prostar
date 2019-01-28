tabPanel("Session logs",
         value = "SessionLogsTab",
         
         tabsetPanel(
             id = "test",
             tabPanel("Session logs",
                      value = "ChangeDataset",
                      sidebarCustom(),
                      tagList(
                          DT::dataTableOutput("logSession", width = "800px")
                      )
             ),
            tabPanel("R source code (Deprecated)", 
                     tagList(
                       uiOutput("InfoTextSourceCode"),
                      uiOutput("code")
                     )
             )
         )
)
