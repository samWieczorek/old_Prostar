tabPanel("Aggregation",
         value="AggregationTab",
         tabsetPanel(
             tabPanel("1 - Aggregate peptides",
                      id = "aggregation",
                      uiOutput("Aggreg_Aggreg")
             ),
             tabPanel("2 - Configure protein dataset",
                      id = "configureProteinDataset",
                      uiOutput("Aggreg_Valid")
             )
         )
         
)