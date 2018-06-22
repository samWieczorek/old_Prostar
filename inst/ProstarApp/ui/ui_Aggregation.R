tabPanel("Aggregation",
         value="AggregationTab",
         tabsetPanel(
             tabPanel("1 - Aggregate peptides",
                      id = "aggregation",
                      sidebarCustom(),
                      splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
                                  uiOutput("AggregationSideBar_Step1"),
                                  tagList(uiOutput("AggregationWellPanel_Step1")
                                  )
                      )
             ),
             tabPanel("2 - Configure protein dataset",
                      id = "configureProteinDataset",
                      uiOutput(outputId = "progressSaveAggregation"),
                      busyIndicator(WaitMsgCalc,wait = 0),
                      uiOutput("Aggregation_Step2")
             )
         )
         
)