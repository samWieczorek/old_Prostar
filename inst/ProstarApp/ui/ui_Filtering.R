tabPanel("Filter data",
         value = "FilterDataTab",
        tabsetPanel(
             id = "DP_Filtering_tabSetPanel",
             tabPanel( "1 - Missing values filtering",
                      value = "DP_FilterMissingValues",
                       splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
                                   wellPanel(id = "sidebar_Filter1"
                                             #,height = "100%"
                                             ,uiOutput("DP_sidebar_FilterTab1")
                                             
                                   ),
                                   tagList(
                                     HTML("The user-defined threshold allows it to tune the minimum amount of non-NA
                                                         values for each line to <br> be kept in the dataset 
                                                         (the line is filtered out otherwise). 
                                                         The threshold either applies on the whole  <br> dataset, on 
                                                         each condition or on at least one condition."),
                                            tags$div(
                                                style="margin-bottom:200px;",
                                                missingValuesPlotsUI("MVPlots_filtering")
                                            )

                                                    )

                       )
         )
         ,tabPanel( "2 - String based filtering",
                    id =  "DP_FilterContaminants",
                    value = "DP_FilterContaminants",
                    splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
                                wellPanel(id = "sidebar_Filter2",
                                          uiOutput("SymbolicFilterOptions")
                                ),
                                tagList(
                                    busyIndicator(WaitMsgCalc,wait = 0),
                                    DT::dataTableOutput("FilterSummaryData")
                                  )
                    )
         )
         ,tabPanel( "3 - Visualize filtered data and Validate",
                    value = "DP_FilterValidate",
                    id = "sidebar_Filter3",
                    splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
                                wellPanel(id = "sidebar_Filter3",
                                          uiOutput("DP_sidebar_FilterTab3"),
                                          actionButton("ValidateFilters","Save filtered dataset",styleclass = "primary"),
                                          uiOutput("legendForExprsData2")
                                ),
                                tagList(
                                    DT::dataTableOutput("VizualizeFilteredData"),
                                    uiOutput("helpTextMV")
                                )
                    )
         )
)
)


