<<<<<<< HEAD
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
=======
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
>>>>>>> ceee6a0719f73dbf86eb71708e3099eee6d98083
