MSnSetExplorerUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("DS_sidebarPanel_tab")),
    uiOutput(ns("tabToShow"))
  )
}



#------------------------------------------------------------
MSnSetExplorer <- function(input, output, session, data) {
  ns <- session$ns
  
  callModule(moduleLegendColoredExprs, 
             "ExprsColorLegend_DS", 
             legend = rv$legendTypeMV,
             colors = rv$colorsTypeMV)
  
  
  output$DS_sidebarPanel_tab <- renderUI({
    req(rv$typeOfDataset)
    
    .choices<- NULL
    switch(rv$typeOfDataset,
           protein = {
             .choices <- list( "Quantitative data" = "tabExprs",
                               "Proteins metadata" = "tabfData",
                               "Experimental design" = "tabpData")
           },
           peptide = {
             .choices <- list("Quantitative data" = "tabExprs",
                              "Peptides metadata" = "tabfData",
                              "Experimental design" = "tabpData")
           },
           {
             .choices <- list("Quantitative data" = "tabExprs",
                              "Analyte metadata" = "tabfData",
                              "Experimental design" = "tabpData")
           }
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
  
  
  
  callModule(moduleLegendColoredExprs, 
             "FilterColorLegend_DS", 
             legend = rv$legendTypeMV,
             colors = rv$colorsTypeMV)
  
  output$legendForExprsData <- renderUI({
    req(input$DS_TabsChoice=="tabExprs")
    
     moduleLegendColoredExprsUI("ExprsColorLegend_DS")
    
  })
  
  
  
  
  #----------------------------------------------
  output$tabToShow <- renderUI({
    req(input$DS_TabsChoice)
    req(rv$current.obj)
    
     switch(input$DS_TabsChoice,
           None = {return(NULL)},
           tabExprs = tagList(
             if (nrow(pData(rv$current.obj))>153) p(MSG_WARNING_SIZE_DT),
             DT::dataTableOutput(ns("table"))),
           tabfData = tagList(
             if (nrow(pData(rv$current.obj))>153) p(MSG_WARNING_SIZE_DT),
             DT::dataTableOutput(ns("viewfData"))),
           tabpData = tagList(
             if (nrow(pData(rv$current.obj))>153) p(MSG_WARNING_SIZE_DT),
             DT::dataTableOutput(ns("viewpData")))
    )
    
  })
  
  
  
  ##' show pData of the MSnset object
  ##' @author Samuel Wieczorek
  output$viewpData <- DT::renderDataTable(server=TRUE,{
    req(rv$current.obj)
    
    data <- as.data.frame(pData(rv$current.obj))
    pal <- rv$PlotParams$paletteForConditions
    dt <- DT::datatable(  data,
                           extensions = c('Scroller', 'Buttons'),
                          rownames=  FALSE,
                          
                          options=list(initComplete = initComplete(),
                                       buttons = list('copy',
                                                      list(
                                                        extend = 'csv',
                                                        filename = 'phenoData'
                                                      ),'print'),
                                       dom='Bfrtip',
                                       pageLength=DT_pagelength,
                                       orderClasses = TRUE,
                                       autoWidth=TRUE,
                                       deferRender = TRUE,
                                       bLengthChange = FALSE,
                                       scrollX = 200,
                                       scrollY = 500,
                                       scroller = TRUE,
                                       #columnDefs = list(
                                       #list(width=c("60px"), targets= c(list(0),list(1),list(2))))
                                       columnDefs = list(list(width='60px',targets= "_all"))
                          )) %>%
      formatStyle(
        columns = colnames(data)[1:2],
        valueColumns = colnames(data)[2],
        backgroundColor = styleEqual(unique(data$Condition), pal)
      )
    
  })
  
  ##' show fData of the MSnset object in a table
  ##' @author Samuel Wieczorek
  output$viewfData <- DT::renderDataTable(server=TRUE,{
    req(rv$current.obj)
    
    
    if ('Significant' %in% colnames(fData(rv$current.obj))){
      dat <- DT::datatable(as.data.frame(fData(rv$current.obj)),
                           rownames = TRUE,
                           extensions = c('Scroller', 'Buttons', 'FixedColumns'),
                           options=list(initComplete = initComplete(),
                                        buttons = list('copy',
                                                       list(
                                                         extend = 'csv',
                                                         filename = 'feature metadata'
                                                       ),'print'),
                                        dom='Bfrtip',
                                        pageLength=DT_pagelength,
                                        orderClasses = TRUE,
                                        autoWidth=FALSE,
                                        deferRender = TRUE,
                                        bLengthChange = FALSE,
                                        scrollX = 200,
                                        scrollY = 200,
                                        scroller = TRUE,
                                        columns.searchable=F,
                                        fixedColumns = list(leftColumns = 1),
                                        columnDefs = list(list(width=c("60px"),
                                                               targets=c(list(0),list(1),list(2)))))) %>%
        formatStyle(columns = 'Significant',
                    target = 'row',
                    background = styleEqual(1, 'lightblue'))
    } else {
      dat <- DT::datatable(as.data.frame(fData(rv$current.obj)),
                           rownames = TRUE,
                           extensions = c('Scroller', 'Buttons', 'FixedColumns'),
                           options=list(initComplete = initComplete(),
                                        buttons = list('copy',
                                                       list(
                                                         extend = 'csv',
                                                         filename = 'feature metadata'
                                                       ),'print'),
                                        dom='Bfrtip',pageLength=DT_pagelength,
                                        deferRender = TRUE,
                                        bLengthChange = FALSE,
                                        scrollX = 200,
                                        scrollY = 600,
                                        scroller = TRUE,
                                        orderClasses = TRUE,
                                        autoWidth=FALSE,
                                        columns.searchable=F,
                                        fixedColumns = list(leftColumns = 1),
                                        columnDefs = list(list(width=c("60px"),
                                                               targets=c(list(0),list(1),list(2))))))
    }
    
    return(dat)
  }
  
  #              
  #             ))
  )
  
  
  
  
  
  #################
  output$table <- DT::renderDataTable(server=TRUE,{
    req(rv$current.obj)
    df <- getDataForExprs(rv$current.obj, rv$settings_nDigits)
    c.tags <- BuildColorStyles(rv$current.obj, rv$colorsTypeMV)$tags
    c.colors <-  BuildColorStyles(rv$current.obj, rv$colorsTypeMV)$colors
    
    dt <- DT::datatable( df,
                     rownames=TRUE,
                     extensions = c('Scroller', 'Buttons', 'FixedColumns'),
                     options = list(
                       buttons = list('copy',
                                      list(
                                        extend = 'csv',
                                        filename = 'quantitation data'
                                      ),'print'),
                       dom='Bfrtip',
                       initComplete = initComplete(),
                       displayLength = 20,
                       deferRender = TRUE,
                       bLengthChange = FALSE,
                       scrollX = 200,
                       scrollY = 600,
                       scroller = TRUE,
                       ordering=FALSE,
                       server = TRUE,
                       fixedColumns = list(leftColumns = 1),
                       columnDefs = list(list(targets = c(((ncol(df)/2)+1):ncol(df)), visible = FALSE)))) %>%
      formatStyle(
        colnames(df)[1:(ncol(df)/2)],
        colnames(df)[((ncol(df)/2)+1):ncol(df)],
        backgroundColor = styleEqual(c.tags, c.colors),
        backgroundSize = '98% 48%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
    
    
    dt
  })
  
  
  
  
}