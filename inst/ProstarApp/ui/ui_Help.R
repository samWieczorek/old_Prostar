tabPanel("Help",
         id = "toto",
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
                      dataTableOutput("tab_versions"))
         )
)