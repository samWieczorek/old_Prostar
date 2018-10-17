tabPanel("Aggregation",
         value="AggregationTab",
         isolate({
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
                                                         selected=rv$widgets$aggregation$includeSharedPeptides),
                                            radioButtons("AggregationConsider", "Consider", 
                                                         choices=c('all peptides'="allPeptides", 
                                                                   "only the N most abundant ones"="onlyN"), 
                                                         selected=rv$widgets$aggregation$considerPeptides),
                                            
                                            radioButtons("AggregationOperator", "Operator", 
                                                         choices=c("Mean"="Mean"), 
                                                         selected=rv$widgets$aggregation$operator),
                                            numericInput("nTopn", "N",value = rv$widgets$aggregation$topN, min = 0, step=1, width='100px'),
                                              actionButton("perform.aggregation","Perform aggregation", class = actionBtnClass)
                                           
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
         })
)