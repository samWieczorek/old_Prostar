tabPanel("Graph",
         value = "graphTab",
         tabsetPanel(
           id = "graphsPanel",
           tabPanel("CC one prot",
                    tagList(
                      h3('test'),
                      dataTableOutput("OneOneDT"),
                      dataTableOutput("OneMultiDT")
                      )
                    ),
           tabPanel("CC multi prot",
                    moduleGraphMulti2AnyUI("CC_Multi_Any")
           )
         )
)
