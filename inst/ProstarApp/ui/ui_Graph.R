tabPanel("Graph",
         value = "graphTab",
         tabsetPanel(
           id = "graphsPanel",
           tabPanel("CC one prot",
                    tagList(
                       bsCollapse(id = "collapseCCInfos", 
                                  open = "",
                                  multiple = TRUE,
                                  bsCollapsePanel("One - One CC", DT::dataTableOutput("OneOneDT"),style = "info"),
                                  bsCollapsePanel("One - Multi CC", DT::dataTableOutput("OneMultiDT"), style = "primary")
                    )
                    )
                    ),
           tabPanel("CC multi prot",
                    moduleGraphMulti2AnyUI("CC_Multi_Any")
           )
         )
)
