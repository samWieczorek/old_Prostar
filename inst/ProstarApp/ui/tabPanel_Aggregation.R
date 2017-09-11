tabPanel("Aggregation",
         id = "Aggregation",
         value="Aggregation",
         tabsetPanel(
             #"agreagationTabsetPanel",
             id = "agreagationTabsetPanel",
             tabPanel("1 - Aggregate peptides",
                      value = "aggregation",
                      sidebarCustom(),
                      splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
                                  uiOutput("AggregationSideBar_Step1"),
                                  conditionalPanel(id = "wellPanel_Agregation",
                                                   condition = 'true',
                                                   uiOutput("AggregationWellPanel_Step1")
                                  )
                      )
             ),
             tabPanel("2 - Configure protein dataset",
                      value = "configureProteinDataset",
                      #sidebarCustom(),
                      #splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
                      # wellPanel(id = "sidebar_imputation",
                      #          height = "100%",
                      uiOutput(outputId = "progressSaveAggregation"),
                      #),
                      #conditionalPanel(id = "wellPanel_Imputation",
                      #                 condition = "true",
                      busyIndicator("Calculation in progress",wait = 0),
                      uiOutput("Aggregation_Step2")
             )
         )
         
)