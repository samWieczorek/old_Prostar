tabPanel("Filter data",
         icon = icon("download"),
         
         tabsetPanel(
             #id = "DP_Filtering_tabSetPanel"
             
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
                                                    HTML("The filter below allows keeping the lines that 
                                                         contain a certain amount of quantitative data rather than NA values. <br>
                                                         The threshold to define corresponds to the number of quantitative values in a 
                                                         line and means that the lines which contain <br> at least this threshold value 
                                                         are kept. This filtering threshold may be applied on the whole  dataset, on 
                                                         each condition <br> or on at least one condition."),
                                                    fluidRow(
                                                        column(width = 4, 
                                                               plotOutput("histoMV_Image")
                                                        ),
                                                        column(width = 4,plotOutput("histo.missvalues.per.lines_Image")),
                                                        column(width = 4,plotOutput("histo.missvalues.per.lines.per.conditions_Image"))
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
                                conditionalPanel(id = "wellPanelMVFilterTab2",
                                                 condition = "true",
                                                 plotOutput("GlobalPieChart")
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