mod_MSnSetExplorer_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
#     tags$head(tags$style("
#   table.dataTable thead th {
#     padding: 8px 10px !important;
#   }
# ")),
    uiOutput(ns("DS_sidebarPanel_tab")),
    fluidRow(
      column(
        align = "center",
        width = 12,
        uiOutput(ns("tabToShow"))
      )
    )
      )
}



#------------------------------------------------------------
#' import DT
#' 
mod_MSnSetExplorer_server <- function(id, 
                                      data, 
                                      digits = 2, 
                                      palette.conds = NULL) {
  
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
  
      observeEvent(req(data()), {
        if (class(data())[1] != "MSnSet")
          stop("The dataset is not of 'MSnSet' class. Abort.")
      })
      
      
  output$DS_sidebarPanel_tab <- renderUI({
    req(GetTypeofData(data()))
    
    .choices <- NULL
    .choices <- switch(GetTypeofData(data()),
           protein = list( "Quantitative data" = "tabExprs",
                               "Proteins metadata" = "tabfData",
                               "Experimental design" = "tabpData"),
           peptide =  list("Quantitative data" = "tabExprs",
                              "Peptides metadata" = "tabfData",
                              "Experimental design" = "tabpData"),
           default = list("Quantitative data" = "tabExprs",
                              "Analyte metadata" = "tabfData",
                              "Experimental design" = "tabpData")
           )
    
    tagList(
      tags$div(
        tags$div( style="display:inline-block; vertical-align: middle; padding-right: 40px;",
                  radioButtons(ns("DS_TabsChoice"), "Table to display",
                               choices = .choices,
                               inline = TRUE,
                               selected=character(0))
        ),
        tags$div( style="display:inline-block; vertical-align: middle;",
                  uiOutput(ns("legendForExprsData"))
        )
      )
    )
    
    
  })
  
  
  mod_LegendColoredExprs_server('ExprsColorLegend_DS',
                                obj = reactive({data()}))
  
  output$legendForExprsData <- renderUI({
    req(input$DS_TabsChoice == "tabExprs")
    mod_LegendColoredExprs_ui(id = ns('ExprsColorLegend_DS'))
    })
  
  
  
  
  #----------------------------------------------
  output$tabToShow <- renderUI({
    req(input$DS_TabsChoice)
    
     switch(input$DS_TabsChoice,
           None = return(NULL),
           tabExprs = tagList(
             
             mod_download_btns_ui(ns('table_DL_btns')),
             fluidRow(
               column(
                 align = "center",
                 width = 12,
                 DT::dataTableOutput(ns("table")))
               )
             ),
           tabfData = tagList(
             mod_download_btns_ui(ns('fData_DL_btns')),
             fluidRow(
               column(
                 align = "center",
                 width = 12,
                 DT::dataTableOutput(ns("viewfData")))
             )
             ),
           tabpData = tagList(
             mod_download_btns_ui(ns('pData_DL_btns')),
             fluidRow(
               column(
                 align = "center",
                 width = 12,
                 DT::dataTableOutput(ns("viewpData")))
             )
             )
    )
    
  })
  
  
  
  mod_download_btns_server('pData_DL_btns',
                           df.data = reactive({pData(data())}),
                           name = reactive({'sampleData'}),
                           colors = reactive({
                             pal <- as.list(palette.conds())
                             names(pal) <- unique(pData(data())$Condition)
                             pal[['blank']] <- 'white'
                             pal
                           }),
                           df.tags = reactive({
                             tags <- pData(data())
                             tags[,] <- 'blank'
                             tags$Sample.name <- pData(data())$Condition
                             tags$Condition <- pData(data())$Condition
                             tags
                           })
  )
  
  ##' show pData of the MSnset object
  ##' @author Samuel Wieczorek
  output$viewpData <- DT::renderDataTable(server=TRUE,{
    req(data())
    data <- as.data.frame(pData(data()))
    dt <- DT::datatable(data,
                        extensions = c('Scroller'),
                        rownames = FALSE,
                        options = list(initComplete = initComplete(),
                                       dom = 'frtip',
                                       pageLength = DT_pagelength,
                                       orderClasses = TRUE,
                                       autoWidth = TRUE,
                                       deferRender = TRUE,
                                       bLengthChange = FALSE,
                                       scrollX = 200,
                                       scrollY = 500,
                                       scroller = TRUE,
                                       columnDefs = list(list(width='60px',targets= "_all"))
                          )) %>%
      formatStyle(
        columns = colnames(data)[1:2],
        valueColumns = colnames(data)[2],
        backgroundColor = styleEqual(unique(data$Condition), palette.conds())
      )
    
  })
  
  
  
  
  mod_download_btns_server('fData_DL_btns',
                           df.data = reactive({fData(data())}),
                           name = reactive({'featureData'}),
                           colors = reactive({NULL}),
                           df.tags = reactive({NULL})
  )
  
  
  ##' show fData of the MSnset object in a table
  ##' @author Samuel Wieczorek
  output$viewfData <- DT::renderDataTable(server=TRUE,{
    req(data())
    
    
    
    if ('Significant' %in% colnames(fData(data()))){
      print("got significant")
      dat <- DT::datatable(as.data.frame(fData(data())),
                           rownames = TRUE,
                           escape = FALSE,
                           plugins = "ellipsis",
                           extensions = c( 'FixedColumns'),
                           options=list(initComplete = initComplete(),
                                        dom = 'rtip',
                                        pageLength = DT_pagelength,
                                        orderClasses = TRUE,
                                        autoWidth=FALSE,
                                        deferRender = TRUE,
                                        bLengthChange = FALSE,
                                        scrollX = '100%',
                                        scrollY = 200,
                                        scroller = TRUE,
                                        columns.searchable = FALSE,
                                        fixedColumns = list(leftColumns = 1),
                                        columnDefs = list(list(className = 'dt-center',
                                                               targets = "_all",
                                                               render = JS("$.fn.dataTable.render.ellipsis( 30 )")
                                                               
                                                               # render = JS(
                                                               #   "function(data, type, row, meta) {",
                                                               #   "return type === 'display' && data != null && data.length > 30 ?",
                                                               #   "'<span title=\"' + data + '\">' + data.substr(0, 30) + '...</span>' : data;",
                                                               #   "}")
                                                               )
                                                          )
                                        )
                           ) %>%
        formatStyle(columns = 'Significant',
                    target = 'row',
                    background = styleEqual(1, 'lightblue'))
    } else {
      dat <- DT::datatable(as.data.frame(fData(data())),
                           rownames = TRUE,
                           #escape = FALSE,
                           plugins = "ellipsis",
                           extensions = c('Scroller', 'FixedColumns'),
                           options=list(initComplete = initComplete(),
                                        dom = 'rtip',
                                        pageLength = DT_pagelength,
                                        deferRender = TRUE,
                                        bLengthChange = FALSE,
                                        scrollX = TRUE,
                                        scrollY = 600,
                                        scroller = TRUE,
                                        orderClasses = TRUE,
                                        autoWidth = TRUE,
                                        columns.searchable = FALSE,
                                        fixedColumns = list(leftColumns = 1),
                                        columnDefs = list(list(className = 'dt-center',
                                                               targets='_all', 
                                                               render = JS("$.fn.dataTable.render.ellipsis( 30 )")
                                                               # render = JS(
                                                               #   "function(data, type, row, meta) {",
                                                               #   "return type === 'display' && data != null && data.length > 30 ?",
                                                               #   "'<span title=\"' + data + '\">' + data.substr(0, 30) + '...</span>' : data;",
                                                               #   "}")
                                                               )
                                                          )))
    }
    
    return(dat)
  }

  )
  
  
  
  
  mod_download_btns_server('table_DL_btns',
                           df.data = reactive({
                             cbind(keyId = fData(data())[, GetKeyId(data())],
                                   round(exprs(data()), digits = digits())
                                   )
                             }),
                           name = reactive({'quantiData'}),
                           colors = reactive({
                             mc <- metacell.def(GetTypeofData(data()))
                             as.list(setNames(mc$color, mc$node))
                           }),
                           df.tags = reactive({
                             cbind(keyId = rep('identified', nrow(data())),
                                   GetMetacell(data())
                                   )
                             })
  )
  
 
  
  #################
  output$table <- DT::renderDataTable(server=TRUE,{
    req(data())
    df <- cbind(keyId = fData(data())[, GetKeyId(data())],
                round(exprs(data()), digits = digits()), 
                DAPAR::GetMetacell(data())
                )
    mc <- metacell.def(GetTypeofData(data()))
    colors <- as.list(setNames(mc$color, mc$node))
    
    DT::datatable( df,
                    extensions = c('Scroller'),
                   #
                   options = list(
                       #dom = 'frtip',
                     initComplete = initComplete(),
                       displayLength = 20,
                       deferRender = TRUE,
                       bLengthChange = FALSE,
                       scrollX = 200,
                       scrollY = 600,
                       scroller = TRUE,
                       ordering = FALSE,
                       server = TRUE,
                       columnDefs = list(list(targets = c((( 2 + (ncol(df)-1)/2)):ncol(df)), visible = FALSE)))) %>%
      formatStyle(
        colnames(df)[2:(1 + (ncol(df)-1)/2)],
        colnames(df)[((2 + (ncol(df)-1)/2)):ncol(df)],
        backgroundColor = styleEqual(names(colors), unlist(colors)),
        backgroundSize = '98% 48%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )

  })
  
    })
  
  
}