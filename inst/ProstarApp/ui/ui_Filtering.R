tabPanel("Filter data",
         value = "FilterDataTab",
         uiOutput("checkFilteringPanel" ),
         actionButton("prevBtnFiltering", "< Previous"),
         actionButton("nextBtnFiltering", "Next >"),
         uiOutput("filteringDone"),
         hr(),
         uiOutput("mv_Filtering"),
         uiOutput("stringBased_Filtering"),
         uiOutput("valid_Filtering")
         

)


