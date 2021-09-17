


mod_staticDT_ui <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    shinyjs::hidden(
                div(id = ns('dl_div'),
                    mod_download_btns_ui(ns("DL_btns"))
                )
                ),
              DT::dataTableOutput(ns("StaticDataTable"))
    )
}




mod_staticDT_server <- function(id,
                                data, 
                                withDLBtns = TRUE,
                                showRownames = FALSE, 
                                dom = 't', 
                                filename = 'Prostar_export') {
  
    moduleServer(
      id,
      function(input, output, session) {
        
        
  
  proxy = dataTableProxy(session$ns('StaticDataTable'), session)
  
  
  observe({replaceData(proxy, data(), resetPaging = FALSE)  })
  
  
  observe({
    shinyjs::toggle('dl_div', condition = isTRUE(withDLBtns))
  })
  
  
  mod_download_btns_server(id = 'DL_btns',
                           df.data = reactive({data()}), 
                           name = reactive({filename}), 
                           colors = reactive({NULL}),
                           df.tags = reactive({NULL})
  )
  
  output$StaticDataTable <- DT::renderDataTable(server=TRUE,{
    req(length(data) > 0)
    
     DT::datatable(data(), 
                    escape = FALSE,
                    rownames= FALSE,
                    options=list(
                      #initComplete = initComplete(),
                      dom = dom
                      #    server = FALSE,
                      #    autoWidth=TRUE,
                      #columnDefs = list(list(width='150px',targets= "_all")),
                      #ordering = FALSE
                    )
      )
    })
  })
}
