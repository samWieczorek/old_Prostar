

tabPanel("Filter data",
         value = "FilterDataTab",
         tabsetPanel(
           id = "DP_Filtering_tabSetPanel",
           tabPanel( "1 - Missing values filtering",
                     uiOutput("mv_Filtering")),
           tabPanel( "2 - String based filtering",
                     uiOutput("stringBased_Filtering")),
           tabPanel( "3 - Visualize filtered data and validate",
                     uiOutput("valid_Filtering")
           )
         )
)

