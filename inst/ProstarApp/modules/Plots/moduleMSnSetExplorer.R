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
  
  #callModule(moduleLegendColoredExprs, "ExprsColorLegend_DS", colorsTypeMV = reactive({rv.prostar$settings()$colorsTypeMV}))
  
  
  output$DS_sidebarPanel_tab <- renderUI({
    typeOfDataset <- data()@experimentData@other$typeOfData
    .choices<- NULL
    switch(typeOfDataset,
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
  
  
  
  #callModule(moduleLegendColoredExprs, "FilterColorLegend_DS")
  
  output$legendForExprsData <- renderUI({
    req(input$DS_TabsChoice)
    
    if (input$DS_TabsChoice != "tabExprs"){return(NULL)}
    
    tagList(
      tags$p(tags$b("Legend of colors")),
      
      fluidRow(
        column(width=2, HTML(paste0("<div style=\"width:50px;height:20px;border:0px solid #000; background-color: ",
                                    rv.prostar$settings()$colorsTypeMV$POV,";\"></div>")) ),
        column(width=10, tags$p("Partially Observed Value"))
      ),
      
      fluidRow(
        column(width=2,HTML(paste0("<div style=\"width:50px;height:20px;border:0px solid #000; background-color: ",
                                   rv.prostar$settings()$colorsTypeMV$MEC,";\"></div>"))),
        column(width=10, tags$p("Missing in Entire Condition"))
      )
    )
  })
  
  
  
  
  #----------------------------------------------
  output$tabToShow <- renderUI({
    req(input$DS_TabsChoice)
    req(data())
    print(paste0('input$DS_TabsChoice', input$DS_TabsChoice))
    switch(input$DS_TabsChoice,
           None = {return(NULL)},
           tabExprs = DT::dataTableOutput(ns("table")),
           tabfData = DT::dataTableOutput(ns("viewfData")),
           tabpData = DT::dataTableOutput(ns("viewpData"))
    )
    
  })
  
  
  
  ##' show pData of the MSnset object
  ##' @author Samuel Wieczorek
  output$viewpData <- DT::renderDataTable({
    req(data())
    
    data <- as.data.frame(Biobase::pData(data()))
    pal <- unique(rv.prostar$settings()$examplePalette)
    dt <- DT::datatable(  data,
                          extensions = c('Scroller', 'Buttons'),
                          rownames=  FALSE,
                          
                          options=list(initComplete = initComplete(),
                                       dom = 'Brtip',
                                       pageLength=DT_pagelength,
                                       orderClasses = TRUE,
                                       autoWidth=TRUE,
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
        backgroundColor = styleEqual(unique(data$Condition), pal)
      )
    
  })
  
  ##' show fData of the MSnset object in a table
  ##' @author Samuel Wieczorek
  output$viewfData <- DT::renderDataTable({
    req(data())
    
    
    if ('Significant' %in% colnames(Biobase::fData(data()))){
      dat <- DT::datatable(as.data.frame(Biobase::fData(data())),
                           rownames = TRUE,
                           extensions = c('Scroller', 'Buttons', 'FixedColumns'),
                           options=list(initComplete = initComplete(),
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
                                        columnDefs = list(list(columns.width=c("60px"),
                                                               columnDefs.targets=c(list(0),list(1),list(2)))))) %>%
        formatStyle(columns = 'Significant',
                    target = 'row',
                    background = styleEqual(1, 'lightblue'))
    } else {
      dat <- DT::datatable(as.data.frame(Biobase::fData(data())),
                           rownames = TRUE,
                           extensions = c('Scroller', 'Buttons', 'FixedColumns'),
                           options=list(initComplete = initComplete(),
                                        dom='Bfrtip',
                                        pageLength=DT_pagelength,
                                        deferRender = TRUE,
                                        bLengthChange = FALSE,
                                        scrollX = 200,
                                        scrollY = 600,
                                        scroller = TRUE,
                                        orderClasses = TRUE,
                                        autoWidth=FALSE,
                                        columns.searchable=F,
                                        fixedColumns = list(leftColumns = 1),
                                        columnDefs = list(list(columns.width=c("60px"),
                                                               columnDefs.targets=c(list(0),list(1),list(2))))))
    }
    
    return(dat)
  }
  
  #              
  #             ))
  )
  
  
  
  
  
  #################
  output$table <- DT::renderDataTable({
    req(data())
    df <- getDataForExprs(data())
    print(head(df))
    dt <- datatable( df,
                     rownames=TRUE,
                     extensions = c('Scroller', 'Buttons', 'FixedColumns'),
                     options = list(
                       dom = 'Bfrtip',
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
        backgroundColor = styleEqual(c("POV", "MEC"), c(rv.prostar$settings()$colorsTypeMV$POV, rv.prostar$settings()$colorsTypeMV$MEC)),
        backgroundSize = '98% 48%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
    
    
    dt
  })
  
  
  
  
}