tabPanel("Aggregation",
         value="AggregationTab",
         tabsetPanel(
             tabPanel("1 - Aggregate peptides",
                      id = "aggregation",
                      splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
                                  wellPanel(id = "sidebar_Aggregation",
                                            height = "100%",
                                              uiOutput("warningAgregationMethod"),
                                              uiOutput("chooseProteinId"),
                                            radioButtons("radioBtn_includeShared", "Include shared peptides", choices=
                                                           c("No (only protein-specific peptides)" = "No",
                                                             "Yes (shared peptides processed as protein specific)"= "Yes1" ,
                                                             "Yes (proportional redistribution of shared peptides). Better but slower." = "Yes2" ),
                                                         selected="Yes2"),
                                            radioButtons("AggregationConsider", "Consider", 
                                                         choices=c('all peptides'="allPeptides", 
                                                                   "only the N most abundant ones"="onlyN"), selected='onlyN'),
                                            
                                            radioButtons("AggregationOperator", "Operator", choices=c("mean"="mean")),
                                            numericInput("nTopn", "N",value = 3, min = 0, step=1),
                                              actionButton("perform.aggregation","Perform aggregation")
                                           
                                  ),
                                  tagList(uiOutput("AggregationWellPanel_Step1")
                                  )
                      )
             ),
             tabPanel("2 - Configure protein dataset",
                      id = "configureProteinDataset",
                      uiOutput("Aggreg_Valid")
             )
         )
         
)