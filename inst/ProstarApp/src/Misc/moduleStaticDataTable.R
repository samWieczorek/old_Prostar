### UI  ###
moduleStaticDataTableUI <- function(id) {
  ns <- NS(id)
  div(
    div( style="display:inline-block; vertical-align: middle; align: center;",
              DT::dataTableOutput(ns("StaticDataTable"))
    )
  )
}




### server ###
moduleStaticDataTable <- function(input, output, session,
                                  table2show,
                                  withBtns=NULL,
                                  showRownames=FALSE,
                                  dom='Bt') {
  ns <- session$ns
  proxy = DT::dataTableProxy(session$ns('StaticDataTable'), session)
  
  observe({DT::replaceData(proxy, table2show(), resetPaging = FALSE)  })
  
  output$StaticDataTable <- DT::renderDT({
    req(table2show())
    print("IN StaticDataTable")
    print(table2show())
    if (length(table2show())==0){return(NULL)}
    print("continue")
    isolate({
      DT::datatable(table2show(), 
                    extensions = c('Scroller', 'Buttons'),
                    escape = FALSE,
                    rownames= showRownames,
                    option=list(initComplete = initComplete(),
                                dom = dom,
                                server = FALSE,
                                autoWidth=TRUE,
                                columnDefs = list(list(width='150px',targets= "_all")),
                                ordering = FALSE
                    )
      )
    })
    
  })
}
