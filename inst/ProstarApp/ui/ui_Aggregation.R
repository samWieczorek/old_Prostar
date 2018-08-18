tabPanel("Aggregation",
         value="AggregationTab",
         uiOutput("checkAggregPanel" ),
         actionButton("prevBtnAggreg", "< Previous"),
         actionButton("nextBtnAggreg", "Next >"),
         hr(),
         
         uiOutput("Aggreg_Aggreg"),
         uiOutput("Aggreg_Valid")
         
)