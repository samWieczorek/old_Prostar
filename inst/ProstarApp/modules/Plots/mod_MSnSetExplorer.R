mod_MSnSetExplorer_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("DS_sidebarPanel_tab")),
    uiOutput(ns("tabToShow"))
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
  
  
  mod_LegendColoredExprs_server('ExprsColorLegend_DS')
  
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
             DT::dataTableOutput(ns("table"))),
           tabfData = tagList(
             mod_download_btns_ui(ns('fData_DL_btns')),
             DT::dataTableOutput(ns("viewfData"))),
           tabpData = tagList(
             mod_download_btns_ui(ns('pData_DL_btns')),
             DT::dataTableOutput(ns("viewpData")))
    )
    
  })
  
  
  
  mod_download_btns_server('pData_DL_btns',
                           df.data = reactive({Biobase::pData(data())}),
                           name = reactive({'sampleData'}),
                           colors = reactive({
                             pal <- as.list(palette.conds())
                             names(pal) <- unique(Biobase::pData(data())$Condition)
                             pal[['blank']] <- 'white'
                             pal
                           }),
                           df.tags = reactive({
                             tags <- Biobase::pData(data())
                             tags[,] <- 'blank'
                             tags$Sample.name <- Biobase::pData(data())$Condition
                             tags$Condition <- Biobase::pData(data())$Condition
                             tags
                           })
  )
  
  ##' show pData of the MSnset object
  ##' @author Samuel Wieczorek
  output$viewpData <- DT::renderDataTable(server=TRUE,{
    req(data())
    data <- as.data.frame(Biobase::pData(data()))
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
                                       #columnDefs = list(
                                       #list(columns.width=c("60px"), columnDefs.targets= c(list(0),list(1),list(2))))
                                       columnDefs = list(list(width='60px',targets= "_all"))
                          )) %>%
      formatStyle(
        columns = colnames(data)[1:2],
        valueColumns = colnames(data)[2],
        backgroundColor = styleEqual(unique(data$Condition), palette.conds())
      )
    
  })
  
  
  
  
  mod_download_btns_server('fData_DL_btns',
                           df.data = reactive({Biobase::fData(data())}),
                           name = reactive({'featureData'}),
                           colors = reactive({NULL}),
                           df.tags = reactive({NULL})
  )
  
  
  ##' show fData of the MSnset object in a table
  ##' @author Samuel Wieczorek
  output$viewfData <- DT::renderDataTable(server=TRUE,{
    req(data())
    
    
    if ('Significant' %in% colnames(Biobase::fData(data()))){
      dat <- DT::datatable(as.data.frame(Biobase::fData(data())),
                           rownames = TRUE,
                           extensions = c('Scroller', 'FixedColumns'),
                           options=list(initComplete = initComplete(),
                                        dom = 'frtip',
                                        pageLength = DT_pagelength,
                                        orderClasses = TRUE,
                                        autoWidth=FALSE,
                                        deferRender = TRUE,
                                        bLengthChange = FALSE,
                                        scrollX = 200,
                                        scrollY = 200,
                                        scroller = TRUE,
                                        columns.searchable = FALSE,
                                        fixedColumns = list(leftColumns = 1),
                                        columnDefs = list(list(columns.width=c("60px"),
                                                               columnDefs.targets=c(list(0),list(1),list(2)))))) %>%
        formatStyle(columns = 'Significant',
                    target = 'row',
                    background = styleEqual(1, 'lightblue'))
    } else {
      dat <- DT::datatable(as.data.frame(Biobase::fData(data())),
                           rownames = TRUE,
                           extensions = c('Scroller', 'FixedColumns'),
                           options=list(initComplete = initComplete(),
                                        dom = 'frtip',
                                        pageLength = DT_pagelength,
                                        deferRender = TRUE,
                                        bLengthChange = FALSE,
                                        scrollX = 200,
                                        scrollY = 600,
                                        scroller = TRUE,
                                        orderClasses = TRUE,
                                        autoWidth = FALSE,
                                        columns.searchable = FALSE,
                                        fixedColumns = list(leftColumns = 1),
                                        columnDefs = list(list(columns.width=c("60px"),
                                                               columnDefs.targets=c(list(0),list(1),list(2))))))
    }
    
    return(dat)
  }

  )
  
  
  colors.def <- list('missing POV' = "lightblue",
                     'missing MEC' = "orange",
                     'recovered' = "lightgrey",
                     'identified' = "white",
                     'combined' = "red")
  
  mod_download_btns_server('table_DL_btns',
                           df.data = reactive({
                             cbind(keyId = Biobase::fData(data())[, GetKeyId(data())],
                                   round(Biobase::exprs(data()), digits = digits())
                                   )
                             }),
                           name = reactive({'quantiData'}),
                           colors = reactive({colors.def}),
                           df.tags = reactive({
                             cbind(keyId = rep('identified', nrow(data())),
                                   GetMetacell(data())
                                   )
                             })
  )
  
 
  
  #################
  output$table <- DT::renderDataTable(server=TRUE,{
    req(data())
    df <- cbind(keyId = Biobase::fData(data())[, GetKeyId(data())],
                round(Biobase::exprs(data()), digits = digits()), 
                DAPAR::GetMetacell(data())
                )
   
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
        backgroundColor = styleEqual(names(colors.def), unlist(colors.def)),
        backgroundSize = '98% 48%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )

  })
  
    })
  
  
}