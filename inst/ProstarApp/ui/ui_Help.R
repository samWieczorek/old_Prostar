tabPanel("Help",
         value = "HelpTab",
         tabsetPanel(
             tabPanel( "Useful links",
                       id = "tabHelp",
                        htmlOutput("References")
             ),
             tabPanel( "FAQ",
                       id = "FAQ"
             ),
             tabPanel("Check for updates",
                      
                      br(),
                      br(),
                      dataTableOutput("tab_versions", width = '600px'))
         )
)