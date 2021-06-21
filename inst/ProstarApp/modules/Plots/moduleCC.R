library(visNetwork)

moduleCCUI <- function(id) {
  ns <- NS(id)
  tabPanel("Peptide-Protein Graph",
           value = "graphTab",
           tabsetPanel(
             id = "graphsPanel",
            
             tabPanel("One-One Connected Components",
                      tagList(
                        fluidRow(
                                 column(width=4, tagList(
                                   mod_download_btns_ui(ns('OneOneDT_DL_btns')),
                                   DT::dataTableOutput(ns("OneOneDT")))
                                            ),
                                 column(width=8, tagList(
                                  # uiOutput(ns('OneOneDTDetailed_ui')),
                                  DT::dataTableOutput(ns("OneOneDTDetailed")))
                                 )
                        #visNetworkOutput(ns("visNet_CC_OneOne"), height='600px')
                        )
                      )
             ),
             tabPanel("One-Multi Connected Components",
                      tagList(
                        fluidRow(
                                  column(width=4, tagList(
                                    mod_download_btns_ui(ns('OneMultiDT_DL_btns')),
                                    DT::dataTableOutput(ns("OneMultiDT")))),
                                  column(width=8, tagList(
                                   # uiOutput(ns('OneMultiDTDetailed_ui')),
                                    DT::dataTableOutput(ns("OneMultiDTDetailed"))))
                        )
                      )
             ),
             tabPanel("Multi-Multi Connected Components",
                      tagList(
                        selectInput(ns('pepInfo'), "Peptide Info", choices=colnames(fData(rv$current.obj)),
                                    multiple=TRUE),
                        selectInput(ns("searchCC"), 'Search for CC', 
                                    choices = c('Tabular view' = 'tabular',
                                                'Graphical view' = 'graphical'),
                                    width='150px'),
                        fluidRow(
                          column(width=6,tagList(
                            highchartOutput(ns("jiji")),
                            uiOutput(ns('CCMultiMulti_DL_btns_ui')),
                            shinyjs::hidden( dataTableOutput(ns('CCMultiMulti')))
                            )),
                          column(width=6, tagList(
                            visNetworkOutput(ns("visNet_CC"), height='600px')))
                        ),
                        uiOutput(ns('CCDetailed'))
                      )
             )
           )
  )
}


moduleCC <- function(input, output, session,cc){
  
  ns <- session$ns
  
  rvCC <- reactiveValues(
    ## selected CC in global CC list (tab or plot)
    selectedCC = NULL,
    
    selectedNode = NULL,
    selectedNeighbors = NULL,
    selectedCCgraph = NULL,
    
    # when the user selects a node in the graph
    detailedselectedNode = list(
      sharedPepLabels = NULL, 
      specPepLabels  = NULL,
      protLabels  = NULL)
  
  )
  
  observeEvent(req(input$searchCC), {
    shinyjs::toggle('jiji', condition = input$searchCC=='graphical')
    shinyjs::toggle('CCMultiMulti', condition = input$searchCC=='tabular')
  })
  
  
  
  
  
  # select a point in the grpah
  observeEvent(input$click,{
    rvCC$selectedNode <- input$click
    })
  

  # Get the id of selected neighbors in the graph
  observeEvent(input$visNet_CC_highlight_color_id,{
    rvCC$selectedNeighbors <- input$visNet_CC_highlight_color_id
    })
  
  
  # select a CC in the summary table
  observeEvent(input$CCMultiMulti_rows_selected, {
    rvCC$selectedCC <- input$CCMultiMulti_rows_selected
  })
  
  # select a CC in the jitter plot
  observeEvent(req(input$eventPointClicked), {
    this.index <- as.integer(strsplit(input$eventPointClicked, "_")[[1]][1])
    this.index+1
    rvCC$selectedCC <- this.index+1
  })
  
  
  # output$visNet_CC_OneOne <- renderVisNetwork({
  #   req(rvCC$selectedCC)
  #   local <-   cc()[Get_CC_One2One()]
  #   
  #   rvCC$selectedCCgraph <- buildGraph(local[[rvCC$selectedCC]], rv$matAdj$matWithSharedPeptides)
  #   
  #   display.CC.visNet(rvCC$selectedCCgraph) %>%
  #     visEvents(click = paste0("function(nodes){
  #               Shiny.onInputChange('",ns("click"),"', nodes.nodes[0]);
  #               Shiny.onInputChange('",ns("node_selected"), "', nodes.nodes.length);
  #               ;}")
  #     ) %>%
  #     visOptions(highlightNearest = TRUE )
  # })
  # 
  
  

output$visNet_CC <- renderVisNetwork({
    req(rvCC$selectedCC)
    local <-   cc()[Get_CC_Multi2Any()]
    
    rvCC$selectedCCgraph <- buildGraph(local[[rvCC$selectedCC]], rv$matAdj$matWithSharedPeptides)
    
    display.CC.visNet(rvCC$selectedCCgraph) %>%
    visEvents(click = paste0("function(nodes){
                Shiny.onInputChange('",ns("click"),"', nodes.nodes[0]);
                Shiny.onInputChange('",ns("node_selected"), "', nodes.nodes.length);
                ;}")
      ) %>%
      visOptions(highlightNearest = TRUE )
  })
  
  
  output$jiji <- renderHighchart({
    tooltip <- NULL
    
   isolate({
     local <-   cc()[Get_CC_Multi2Any()]
     n.prot <- unlist(lapply(local, function(x){length(x$proteins)}))
     n.pept <- unlist(lapply(local, function(x){length(x$peptides)}))
     df <- tibble::tibble(x=jitter(n.pept),
                     y = jitter(n.prot),
                     index = 1:length(local))
        
        if (!is.null( tooltip)){
          df <- cbind(df,fData(rv$current.obj)[ tooltip])
        }

        colnames(df) <- gsub(".", "_", colnames(df), fixed=TRUE)
        if (ncol(df) > 3){
          colnames(df)[4:ncol(df)] <-
            paste("tooltip_", colnames(df)[4:ncol(df)], sep="")
        }

        clickFun <-
          JS(paste0("function(event) {Shiny.onInputChange('",ns("eventPointClicked"),"', [this.index]+'_'+ [this.series.name]);}"))

          rv$tempplot$plotCC <-  plotJitter_rCharts(df,clickFunction=clickFun)

      })
     rv$tempplot$plotCC

  })
  

  GetDataFor_CCMultiMulti <- reactive({
    Get_CC_Multi2Any()

    df <- cbind(id = 1:length(Get_CC_Multi2Any()),
                nProt = cbind(lapply(rv$CC$allPep[Get_CC_Multi2Any()],
                                     function(x){length(x$proteins)})),
                nPep = cbind(lapply(rv$CC$allPep[Get_CC_Multi2Any()], 
                                    function(x){length(x$peptides)})),
                proteins =  cbind(lapply(rv$CC$allPep[Get_CC_Multi2Any()],
                                         function(x){paste(x$proteins, collapse=",")})),
                peptides = cbind(lapply(rv$CC$allPep[Get_CC_Multi2Any()],
                                        function(x){paste(x$proteins, collapse=",")}))
                )

    colnames(df) <-c('id', 'nProt', 'nPep', 'Proteins Ids', 'Peptides Ids')

    df
  })
  
  
  
  output$CCMultiMulti_DL_btns_ui <- renderUI({
    req(input$searchCC == 'tabular')
    mod_download_btns_ui(ns('CCMultiMulti_DL_btns'))
  })
  
  mod_download_btns_server('CCMultiMulti_DL_btns',
                           df.data = reactive({GetDataFor_CCMultiMulti()}),
                           name = reactive({'CC_MultiMulti'}),
                           colors = reactive({NULL}),
                           df.tags = reactive({NULL})
  )
  
  
 
  
  output$CCMultiMulti <- renderDataTable(server=TRUE,{
    dat <- DT::datatable(GetDataFor_CCMultiMulti(),
                         selection = 'single',
                         rownames=FALSE,
                         extensions = c('Scroller'),
                         options=list(initComplete = initComplete(),
                                      dom = 'frt',
                                      #deferRender = TRUE,
                                      #bLengthChange = FALSE,
                                      scrollX = 400,
                                      scrollY = 400,
                                      displayLength = 10,
                                      scroller = TRUE
                                      #orderClasses = TRUE,
                                      #autoWidth=TRUE
                                     )
                         )
    
    return(dat)
  })
  
  
 
 observeEvent(c(rvCC$selectedNeighbors,input$node_selected,rvCC$selectedCCgraph), {
    
    local <-   cc()[Get_CC_Multi2Any()]
    rvCC$selectedNeighbors
    
    nodes <- rvCC$selectedCCgraph$nodes
    
    if(!is.null(input$node_selected) && input$node_selected == 1){ 
      sharedPepIndices <- intersect(rvCC$selectedNeighbors, which(nodes[,'group'] == "shared.peptide"))
      specPepIndices <- intersect(rvCC$selectedNeighbors, which(nodes[,'group'] == "spec.peptide"))
      protIndices <- intersect(rvCC$selectedNeighbors,which(nodes[,'group'] == "protein"))
    
    } else {
      sharedPepIndices <- which(nodes[,'group'] == "shared.peptide")
      specPepIndices <- which(nodes[,'group'] == "spec.peptide")
      protIndices <- which(nodes[,'group'] == "protein")
    }
    rvCC$detailedselectedNode$sharedPepLabels <- as.numeric(nodes[sharedPepIndices, 'label'])
    rvCC$detailedselectedNode$specPepLabels <-  as.numeric(nodes[specPepIndices, 'label'])
    rvCC$detailedselectedNode$protLabels <-  as.numeric(nodes[protIndices, 'label'])
    
})


output$CCDetailed <- renderUI({
   req(rvCC$detailedselectedNode)
   req(rvCC$selectedCC)
   
   tagList(
      h4("Proteins"),
      dataTableOutput(ns('CCDetailedProt')),
      h4("Specific peptides"),
      dataTableOutput(ns('CCDetailedSpecPep')),
      h4("Shared peptides"),
      dataTableOutput(ns('CCDetailedSharedPep'))
    )
})
 
  output$CCDetailedProt<- renderDataTable(server=TRUE,{
    req(rvCC$selectedCC)
    rvCC$detailedselectedNode
    if(is.null(rvCC$detailedselectedNode$protLabels)){return(NULL)}
    
    df <- data.frame(proteinId = unlist(rvCC$detailedselectedNode$protLabels))
    colnames(df) <-c('Proteins Ids')
    dt <- DT::datatable( df,
                     extensions = c('Scroller'),
                     options = list(initComplete = initComplete(),
                                    dom = 'rt',
                                    blengthChange = FALSE,
                                    ordering = FALSE,
                                    scrollX = 400,
                                    scrollY = 100,
                                    displayLength = 10,
                                    scroller = TRUE,
                                    header = FALSE,
                                    server = FALSE)) 
    dt
  })
  
  
  
  #######
  
  output$CCDetailedSharedPep <- renderDataTable(server=TRUE,{
    rvCC$detailedselectedNode
    input$pepInfo
    
    req(rvCC$detailedselectedNode$sharedPepLabels)
    
    
    ind <- 1:ncol(rv$current.obj)
    data <- getDataForExprs(rv$current.obj, rv$settings_nDigits)
    pepLine <- rvCC$detailedselectedNode$sharedPepLabels
    indices <- unlist(lapply(pepLine, function(x){which(rownames(data)==x)}))
    data <- data[indices, c(ind, (ind + ncol(data)/2))]

    if(!is.null(input$pepInfo))
      {
      data <- cbind(data, fData(rv$current.obj)[pepLine,input$pepInfo])
      colnames(data)[(1+ncol(data)-length(input$pepInfo)):ncol(data)] <- input$pepInfo
    }
    
    offset <- length(input$pepInfo)
    c.tags <- BuildColorStyles(rv$current.obj, rv$colorsTypeMV)$tags
    c.colors <-  BuildColorStyles(rv$current.obj, rv$colorsTypeMV)$colors
    
    dt <- DT::datatable( data,
                     extensions = c('Scroller'),
                     options = list(initComplete = initComplete(),
                                    dom='rt',
                                    blengthChange = FALSE,
                                    ordering= FALSE,
                                    scrollX = 400,
                                    scrollY = 150,
                                    displayLength = 10,
                                    scroller = TRUE,
                                    header = FALSE,
                                    server = FALSE,
                                    columnDefs = list(list(targets = c((((ncol(data)-offset)/2)+1):(ncol(data)-offset)), visible = FALSE))
                     )) %>%
      formatStyle(
        colnames(data)[1:((ncol(data)-offset)/2)],
        colnames(data)[(((ncol(data)-offset)/2)+1):(ncol(data)-offset)],
        backgroundColor = styleEqual(c.tags, c.colors))
    
    dt
  })
  
  
  
  
  
  #####-----------
  output$CCDetailedSpecPep <- renderDataTable(server=TRUE,{
    rvCC$detailedselectedNode
    input$pepInfo
    req(rvCC$detailedselectedNode$specPepLabels)
    
    ind <- 1:ncol(rv$current.obj)
    data <- getDataForExprs(rv$current.obj, rv$settings_nDigits)
    pepLine <-  rvCC$detailedselectedNode$specPepLabels
    indices <- unlist(lapply(pepLine, function(x){which(rownames(data)==x)}))
    data <- data[indices,c(ind, (ind + ncol(data)/2))]
    
    if(!is.null(input$pepInfo))
    {
      data <- cbind(data, fData(rv$current.obj)[pepLine,input$pepInfo])
      colnames(data)[(1+ncol(data)-length(input$pepInfo)):ncol(data)] <- input$pepInfo
    }
    
    offset <- length(input$pepInfo)
    
    c.tags <- BuildColorStyles(rv$current.obj, rv$colorsTypeMV)$tags
    c.colors <-  BuildColorStyles(rv$current.obj, rv$colorsTypeMV)$colors
    
    dt <- DT::datatable( data,
                     extensions = c('Scroller'),
                     options = list(initComplete = initComplete(),
                                    dom = 'rt',
                                    blengthChange = FALSE,
                                    ordering = FALSE,
                                    scrollX = 400,
                                    scrollY = 100,
                                    displayLength = 10,
                                    scroller = TRUE,
                                    header = FALSE,
                                    server = FALSE,
                                    columnDefs = list(list(targets = c((((ncol(data)-offset)/2)+1):(ncol(data)-offset)), visible = FALSE))
                     )) %>%
      formatStyle(
        colnames(data)[1:((ncol(data)-offset)/2)],
        colnames(data)[(((ncol(data)-offset)/2)+1):(ncol(data)-offset)],
        backgroundColor = styleEqual(c.tags, c.colors))
    
    dt
  })
  
  
  
  
  
  
  Get_CC_One2One <- reactive({
    rv$CC$allPep
    ll.prot <- lapply(rv$CC$allPep, function(x){length(x$proteins)})
    ll.pept <- lapply(rv$CC$allPep, function(x){length(x$peptides)})
    ll.prot.one2one <- intersect(which(ll.prot == 1),which(ll.pept == 1))
    ll.prot.one2one
  })
  
  Get_CC_One2multi <- reactive({
    rv$CC$allPep
    ll.prot <- lapply(rv$CC$allPep, function(x){length(x$proteins)})
    ll.pept <- lapply(rv$CC$allPep, function(x){length(x$peptides)})
    ll.prot.one2multi <- intersect(which(ll.prot == 1), which(ll.pept > 1))
    ll.prot.one2multi
  })
  
  Get_CC_Multi2Any <- reactive({
    rv$CC$allPep
    ll.prot <- lapply(rv$CC$allPep, function(x){length(x$proteins)})
    ll.pept <- lapply(rv$CC$allPep, function(x){length(x$peptides)})
    ll.prot.multi2any <- which(ll.prot > 1)
    ll.prot.multi2any
  })
  
  
  
  BuildOne2OneTab <- reactive({
    rv$CC$allPep
   # table <- do.call(rbind,lapply(rv$CC$allPep[Get_CC_One2One()],function(x){data.frame(rbind(x))}))
    #browser()
    df <- cbind(
      cbind(lapply(rv$CC$allPep[Get_CC_One2One()], function(x){x$proteins})),
      cbind(lapply(rv$CC$allPep[Get_CC_One2One()], function(x){x$peptides}))
    )
    
    colnames(df) <- c('proteins', 'peptides')
    
    df
  })
  
  BuildOne2MultiTab <- reactive({
    rv$CC$allPep

    df <- cbind(proteins = cbind(lapply(rv$CC$allPep[Get_CC_One2multi()],
                                     function(x){x$proteins})),
                nPep = cbind(lapply(rv$CC$allPep[Get_CC_One2multi()], 
                                    function(x){length(x$peptides)})),
                peptides =  cbind(lapply(rv$CC$allPep[Get_CC_One2multi()],
                                         function(x){paste(x$peptides, collapse=',')}))
                )
    colnames(df) <- c('proteins', 'nPep', 'peptides')
    
    df
  })
  
  
  BuildMulti2AnyTab <- reactive({
    rv$CC$allPep
    df <- cbind(id = 1:length(Get_CC_Multi2Any()),
                proteins = cbind(lapply(rv$CC$allPep[Get_CC_Multi2Any()],
                                        function(x){x$proteins})),
                nProt = cbind(lapply(rv$CC$allPep[Get_CC_Multi2Any()], 
                                    function(x){length(x$proteins)})),
                nPep = cbind(lapply(rv$CC$allPep[Get_CC_Multi2Any()], 
                                    function(x){length(x$peptides)})),
                peptides =  cbind(lapply(rv$CC$allPep[Get_CC_Multi2Any()],
                                         function(x){paste(x$peptides, collapse=',')}))
    )
    colnames(df) <- c('proteins', 'nPep', 'peptides')
    
    df
  })
  

  
  mod_download_btns_server('OneMultiDT_DL_btns',
                           df.data = reactive({
                             df <- BuildOne2MultiTab()
                             colnames(df) <- c('Proteins Ids', 'nPep', 'Peptides Ids')
                             df
                             }),
                           name = reactive({'CC_OneMulti'}),
                           colors = reactive({NULL}),
                           df.tags = reactive({NULL})
  )
  
  
  output$OneMultiDT <- renderDataTable(server=TRUE,{
    req(rv$CC$allPep)
    df <- BuildOne2MultiTab()
      colnames(df) <-c(c('Proteins Ids', 'nPep', 'Peptides Ids'))
    
  dat <- DT::datatable(df,
                         selection = 'single',
                         rownames=FALSE,
                         extensions = c('Scroller'),
                         options=list(initComplete = initComplete(),
                                      dom = 'rt',
                                      deferRender = TRUE,
                                      bLengthChange = TRUE,
                                      displayLength = 10,
                                      scrollX = 400,
                                      scrollY = 400,
                                      scroller = TRUE,
                                       autoWidth = FALSE,
                                      columns.searchable = FALSE,
                                      columnDefs = list(list(columns.width=c("60px")
                                                             )
                                                        )
                                      )
                       )
    
    return(dat)
  })
  

  
  GetDataFor_OneMultiDTDetailed <- reactive({
    input$pepInfo
    req(input$OneMultiDT_rows_selected)

    line <- input$OneMultiDT_rows_selected
     ind <- 1:ncol(rv$current.obj)
    data <- getDataForExprs(rv$current.obj, rv$settings_nDigits)
    pepLine <- as.numeric(unlist(strsplit(unlist(BuildOne2MultiTab()[line,"peptides"]), split=",")))
    
    indices <- unlist(lapply(pepLine, function(x){which(rownames(data)==x)}))
    
    data <- data[indices, c(ind, (ind + ncol(data)/2))]
    
    if(!is.null(input$pepInfo))
    {
      data <- cbind(data, fData(rv$current.obj)[pepLine, input$pepInfo])
      colnames(data)[(1+ncol(data)-length(input$pepInfo)):ncol(data)] <- input$pepInfo
    }
    
    data
  })
  
  
  output$OneMultiDTDetailed <- renderDataTable(server=TRUE,{
    input$pepInfo
    req(input$OneMultiDT_rows_selected)
    
    data <- GetDataFor_OneMultiDTDetailed()
    offset <- length(input$pepInfo)
    
    c.tags <- BuildColorStyles(rv$current.obj, rv$colorsTypeMV)$tags
    c.colors <-  BuildColorStyles(rv$current.obj, rv$colorsTypeMV)$colors
    
    dt <- DT::datatable(data ,
                     extensions = c('Scroller'),
                     options = list(initComplete = initComplete(),
                                    dom = 'frtip',pageLength = 10,
                                    blengthChange = FALSE,
                                    displayLength = 10,
                                    ordering = FALSE,
                                    header = FALSE,
                                    server = FALSE,
                                    columnDefs = list(list(targets = c((((ncol(data)-offset)/2)+1):(ncol(data)-offset)), visible = FALSE))
                     )) %>%
      formatStyle(
        colnames(data)[1:((ncol(data)-offset)/2)],
        colnames(data)[(((ncol(data)-offset)/2)+1):(ncol(data)-offset)],
        backgroundColor = styleEqual(c.tags, c.colors))
    
    dt
  })
  

  
  
  mod_download_btns_server('OneOneDT_DL_btns',
                           df.data = reactive({df <- BuildOne2OneTab()
                           colnames(df) <- c('Proteins Ids', 'Peptides Ids')
                           df}),
                           name = reactive({'CC_OneOne'}),
                           colors = reactive({NULL}),
                           df.tags = reactive({NULL})
  )
  
  
  output$OneOneDT <- renderDataTable(server=TRUE,{
    req(rv$CC$allPep)
    df <- BuildOne2OneTab()
    colnames(df) <- c('Proteins Ids', 'Peptides Ids')
    dat <- DT::datatable(df,
                         selection = 'single',
                         rownames=FALSE,
                         extensions = c('Scroller'),
                         options=list(initComplete = initComplete(),
                                      dom = 'frtip', 
                                      deferRender = TRUE,
                                      bLengthChange = FALSE,
                                      scrollX = 400,
                                      scrollY = 200,
                                      scroller = TRUE,
                                      orderClasses = TRUE,
                                      autoWidth=FALSE,
                                      columns.searchable=F,
                                      columnDefs = list(list(columns.width=c("60px"),
                                                             columnDefs.targets=c(list(0),list(1),list(2))))))
    
    return(dat)
  })
  

  
  GetDataFor_OneOneDTDetailed <- reactive({
    req(rv$CC$allPep)
    req(input$OneOneDT_rows_selected)
    input$pepInfo
    line <- input$OneOneDT_rows_selected
    
    ind <- 1:ncol(rv$current.obj)
    data <- getDataForExprs(rv$current.obj, rv$settings_nDigits)
    pepLine <- as.numeric(BuildOne2OneTab()[line,2])
    indices <- unlist(lapply(pepLine, function(x){which(rownames(data)==x)}))
    data <- data[indices,c(ind, (ind + ncol(data)/2))]
    if(!is.null(input$pepInfo))
    {
      data <- cbind(data, fData(rv$current.obj)[pepLine, input$pepInfo])
      colnames(data)[(1+ncol(data)-length(input$pepInfo)):ncol(data)] <- input$pepInfo
    }
    
    data
  })
  
  
  # output$OneOneDTDetailed_ui <- renderUI({
  #   req(input$OneOneDT_rows_selected)
  #   mod_download_btns_ui('OneOneDTDetailed_DL_btns')
  #   
  # })
  #   
  # 
  # mod_download_btns_server('OneOneDTDetailed_DL_btns',
  #                          df.data = reactive({ GetDataFor_OneOneDTDetailed()}),
  #                          name = reactive({'CC_OneOne_Detailed'}),
  #                          colors = reactive({NULL}),
  #                          df.tags = reactive({NULL})
  # )
    
    output$OneOneDTDetailed <- renderDataTable(server=TRUE,{
      req(rv$CC$allPep)
      req(input$OneOneDT_rows_selected)
      data <- GetDataFor_OneOneDTDetailed()
      offset <- length(input$pepInfo)
      
      c.tags <- BuildColorStyles(rv$current.obj, rv$colorsTypeMV)$tags
      c.colors <-  BuildColorStyles(rv$current.obj, rv$colorsTypeMV)$colors
      
      dt <- DT::datatable( data,
                           extensions = c('Scroller'),
                           options = list(initComplete = initComplete(),
                                          dom = 'frtip',
                                          blengthChange = FALSE,
                                          pageLength = 10,
                                          displayLength = 10,
                                          ordering = FALSE,
                                          header = FALSE,
                                          server = FALSE,
                                          columnDefs = list(list(targets = c((((ncol(data)-offset)/2)+1):(ncol(data)-offset)), visible = FALSE))
                           )) %>%
        formatStyle(
          colnames(data)[1:((ncol(data)-offset)/2)],
          colnames(data)[(((ncol(data)-offset)/2)+1):(ncol(data)-offset)],
          backgroundColor = styleEqual(c.tags, c.colors))
      
      dt
    })

  
  
  
  
}
