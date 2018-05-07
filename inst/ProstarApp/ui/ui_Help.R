tabPanel("Help",
         tabsetPanel(
             tabPanel( "Help Prostar",
                        htmlOutput("References")
             ),
             tabPanel( "FAQ"
             ),
             tabPanel("Check for updates",
                      
                      br(),
                      br(),
                      dataTableOutput("tab_versions"))
         )
)