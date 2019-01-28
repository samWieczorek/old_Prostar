# tabPanel("Aggregation",
#          value="AggregationTab",
# 
#          
#          tags$div(tags$div( style="align: center;display:inline-block; vertical-align: top; padding: 7px",
#                             actionButton("prevBtnAggreg", "<<", class = PrevNextBtnClass,style='padding:4px; font-size:80%')),
#                   tags$div( style="align: center;display:inline-block; vertical-align: top;",
#                             uiOutput("checkAggregPanel" )),
#                   tags$div( style="align: center;display:inline-block; vertical-align: top; padding: 7px",
#                             actionButton("nextBtnAggreg", ">>", class = PrevNextBtnClass, style='padding:4px; font-size:80%')),
#                   tags$div( style="align: center;display:inline-block; vertical-align: top; padding: 7px",
#                   uiOutput("ObserverAggregationDone"))
#                   
#                   
#          ),
#          
#          
#          hr(),
#          
#          uiOutput("Aggreg_Aggreg"),
#          uiOutput("Aggreg_Valid")
#          
# )


tabPanel("Aggregation",
         value="AggregationTab",
         isolate({
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
         })
)