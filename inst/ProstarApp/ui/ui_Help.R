tabPanel("Help",
         value = "HelpTab",
         tabsetPanel(
             tabPanel( "Useful links",
                       id = "tabHelp",
                        htmlOutput("References")
             ),
             tabPanel( "FAQ",
                       id = "FAQ",
                       htmlOutput("FAQ_output")
             ),
             tabPanel("Check for updates",
                      
                      br(),
                      br(),
                      dataTableOutput("tab_versions"))
         )
)