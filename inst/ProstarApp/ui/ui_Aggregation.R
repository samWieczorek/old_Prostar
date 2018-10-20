tabPanel("Aggregation",
         value="AggregationTab",
         uiOutput("checkAggregPanel" ),
         actionButton("prevBtnAggreg", "< Previous", class = PrevNextBtnClass),
         actionButton("nextBtnAggreg", "Next >", class = PrevNextBtnClass),
         hr(),
         
         uiOutput("Aggreg_Aggreg"),
         uiOutput("Aggreg_Valid")
         
)

# 
# 
# tabPanel("Aggregation",
#          value="AggregationTab",
#          isolate({
#            tabsetPanel(
#              tabPanel("1 - Aggregate peptides",
#                       id = "aggregation",
#                       
#              ),
#              tabPanel("2 - Configure protein dataset",
#                       id = "configureProteinDataset",
#                       uiOutput("Aggreg_Valid")
#              )
#          )
#          })
# )