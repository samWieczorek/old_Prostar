tabPanel("Filter data",
         #icon = icon("download"),
         
         tabsetPanel(
             id = "DP_Filtering_tabSetPanel",
             
             tabPanel( "1 - Missing values",
                       #id =  "DP_FilterMissingValues",
                       value = "DP_FilterMissingValues",
                       sidebarCustom(),
                       splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
                                   wellPanel(id = "sidebar_Filter1"
                                             ,uiOutput("DP_sidebar_FilterTab1")
                                             
                                   ),
                                   conditionalPanel(id = "wellPanelMVFilterTab1",
                                                    condition = "true",
                                                    HTML("The user-defined threshold allows it to tune the minimum amount of non-NA
                                                         values for each line to <br> be kept in the dataset 
                                                         (the line is filtered out otherwise). 
                                                         The threshold either applies on the whole  <br> dataset, on 
                                                         each condition or on at least one condition."),
                                                    fluidRow(
                                                        column(width = 4, highchartOutput("histoMV_Image")),
                                                        column(width = 4,highchartOutput("histo_missvalues_per_lines_Image")),
                                                        column(width = 4,highchartOutput("histo_missvalues_per_lines_per_conditions_Image"))
                                                    )
                                                    )
                       )
         )
         ,tabPanel( "2 - String based filtering",
                    id =  "DP_FilterContaminants",
                    value = "DP_FilterContaminants",
                    splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
                                wellPanel(id = "sidebar_Filter2",
                                          uiOutput("DP_sidebar_FilterTab2")
                                          
                                ),
                                tagList(
                                  busyIndicator("Calculation in progress",wait = 0),
                                  highchartOutput("GlobalPieChart"),
                                  uiOutput("ObserverStringBasedFilteringDone")
                                )
                    )
         )
         ,tabPanel( "3 - Visualize and Validate",
                    value = "DP_FilterValidate",
                    id = "sidebar_Filter3",
                    splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
                                wellPanel(id = "sidebar_Filter3",
                                          uiOutput("DP_sidebar_FilterTab3")
                                          ,actionButton("ValidateFilters","Save filtered dataset",
                                                        styleclass = "primary")
                                ),
                                conditionalPanel(id = "wellPanelMVFilterTab3"
                                                 ,condition = "true"
                                                 ,DT::dataTableOutput("VizualizeFilteredData")
                                                 ,uiOutput("helpTextMV")
                                )
                    )
         )
)
)
