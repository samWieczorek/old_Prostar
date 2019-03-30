library(visNetwork)

moduleCCUI <- function(id) {
  ns <- NS(id)
  tabPanel("Graph",
           value = "graphTab",
           tabsetPanel(
             id = "graphsPanel",
             tabPanel("CC one prot",
                      tagList(
                        bsCollapse(id = "collapseCCInfos", 
                                   open = "",
                                   multiple = TRUE,
                                   bsCollapsePanel("One - One CC", 
                                                   fluidRow(
                                                     column(width=4, DT::dataTableOutput(ns("OneOneDT"))),
                                                     column(width=8, DT::dataTableOutput(ns("OneOneDTDetailed")))
                                                     ),style = "info"),
                                   bsCollapsePanel("One - Multi CC", 
                                                   fluidRow(
                                                     column(width=4, DT::dataTableOutput(ns("OneMultiDT"))),
                                                     column(width=8, DT::dataTableOutput(ns("OneMultiDTDetailed")))
                                                   ), style = "primary")
                        )
                      )
             ),
             tabPanel("CC multi prot",
                      tagList(
                        #uiOutput(ns("CCTooltip_UI")),
                        # highchartOutput(ns("jiji")))
                        selectInput(ns("searchCC"), 'Search for CC', 
                                    choices = c('Tabular view' = 'tabular',
                                                'Graphical view' = 'graphical'),
                                    width='150px'),
                        fluidRow(
                          column(width=6,tagList(
                            highchartOutput(ns("jiji")),
                            shinyjs::hidden( dataTableOutput(ns('CCMultiMulti')))
                            )),
                          column(width=6, visNetworkOutput(ns("visNet_CC"), height='600px'))
                        ),
                        uiOutput(ns('CCDetailed'))
                      )
             )
           )
  )
}


moduleCC <- function(input, output, session,cc){
  
  ns <- session$ns
  
  # 
  # rv <- reactiveValues(
  #   indiceInCC = NULL
  # )
  # 
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
  
  
  observeEvent(input$click,{
    rvCC$selectedNode <- input$click
    
  })
  
  
  observeEvent(input$visNet_CC_highlight_color_id,{
    rvCC$selectedNeighbors <- input$visNet_CC_highlight_color_id
  })
  
  
  
  observeEvent(input$CCMultiMulti_rows_selected, {
    rvCC$selectedCC <- input$CCMultiMulti_rows_selected
  })
  
  observeEvent(req(input$eventPointClicked), {
    this.index <- as.integer(strsplit(input$eventPointClicked, "_")[[1]][1])
    this.index+1
    rvCC$selectedCC <- this.index+1
  })
  

observe({
  rvCC$selectedCC
  print(paste0("new value for rvCC$selectedCC : ",rvCC$selectedCC))
  })
  
output$visNet_CC <- renderVisNetwork({
    req(rvCC$selectedCC)
    local <-   cc()[Get_CC_Multi2Any()]
    
    rvCC$selectedCCgraph <- buildGraph(local[[rvCC$selectedCC]], rv$matAdj$matWithSharedPeptides)
    
    display.CC.visNet(rvCC$selectedCCgraph) %>%
    visEvents(click = paste0("function(nodes){
                Shiny.onInputChange('",ns("click"),"', nodes.nodes[0]);
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
     df <- tibble(x=jitter(n.pept),
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
  
  
  
  
  
  output$CCTooltip_UI <- renderUI({
    req(rv$current.obj)
    if(rv$typeOfDataset != 'peptide'){return(NULL)}
    tagList(
      #modulePopoverUI("modulePopover_volcanoTooltip"),
      selectInput(ns("CCtooltipInfo"),
                  label = NULL,
                  choices = colnames(fData(rv$current.obj)),
                  multiple = TRUE, selectize=FALSE,width='200px', size=5)
    )
  })
  

  
  output$CCMultiMulti <- renderDataTable({
    Get_CC_Multi2Any()
    df <- do.call(rbind,lapply(rv$CC$allPep[Get_CC_Multi2Any()],
                               function(x){
                                 data.frame(rbind(x),
                                            nPep = length(x$peptides),
                                            nProt = length(x$proteins))}))
    df <- cbind(df,id = 1:nrow(df))
    df <- df[c('id', 'nProt', 'nPep', 'proteins', 'peptides')]
    
    dat <- DT::datatable(df,
                         selection = 'single',
                         rownames=FALSE,
                         extensions = c('Scroller', 'Buttons'),
                         options=list(initComplete = initComplete(),
                                      dom='Bfrtip',
                                      deferRender = TRUE,
                                      bLengthChange = FALSE,
                                      scrollX = 400,
                                      scrollY = 400,
                                      displayLength = 10,
                                      scroller = TRUE,
                                      orderClasses = TRUE,
                                      autoWidth=TRUE,
                                      columns.searchable=F,
                                      columnDefs = list(list(columns.width=c("60px"),
                                                             columnDefs.targets=c(list(0),list(1),list(2))))))
    
    return(dat)
  })
  
  
  
  
  
  
  
  output$CCDetailed <- renderUI({
    req(rvCC$selectedNeighbors)
    req(rvCC$selectedCCgraph)
    req(rvCC$selectedCC)
    
    local <-   cc()[Get_CC_Multi2Any()]
    rvCC$selectedNeighbors
    
    nodes <- rvCC$selectedCCgraph$nodes
    sharedPepIndices <- intersect(rvCC$selectedNeighbors, 
                                  which(nodes[,'group'] == "shared.peptide"))
    specPepIndices <- intersect(rvCC$selectedNeighbors, 
                                which(nodes[,'group'] == "spec.peptide"))
    protIndices <- intersect(rvCC$selectedNeighbors,
                                 which(nodes[,'group'] == "protein"))
    
    
    rvCC$detailedselectedNode$sharedPepLabels <- as.numeric(nodes[sharedPepIndices, 'label'])
    rvCC$detailedselectedNode$specPepLabels <-  as.numeric(nodes[specPepIndices, 'label'])
    rvCC$detailedselectedNode$protLabels <-  as.numeric(nodes[protIndices, 'label'])
    
    #dataTableOutput(ns('CCMultiMultiDetailed'))
    tagList(
      dataTableOutput(ns('CCDetailedProt')),
      dataTableOutput(ns('CCDetailedSpecPep')),
      dataTableOutput(ns('CCDetailedSharedPep'))
    )
  })
  
  output$CCDetailedProt<- renderDataTable({
    rvCC$detailedselectedNode
    if(is.null(rvCC$detailedselectedNode$protLabels)){return(NULL)}
    
    print("output$CCDetailedProt<- renderDataTable(")
    print(rvCC$detailedselectedNode$protLabels)
    df <- data.frame(proteinId = unlist(rvCC$detailedselectedNode$protLabels),
                     other = rep(NA,length(rvCC$detailedselectedNode$protLabels)))
                       
    dt <- datatable( df,
                     extensions = c('Scroller', 'Buttons'),
                     options = list(initComplete = initComplete(),
                                    dom='Bfrtip',
                                    blengthChange = FALSE,
                                    ordering=FALSE,
                                    scrollX = 400,
                                    scrollY = 100,
                                    displayLength = 10,
                                    scroller = TRUE,
                                    header=FALSE,
                                    server = FALSE,
                                    columnDefs = list(list(targets = c(((ncol(data)/2)+1):(ncol(data))), visible = FALSE))
                     )) 
    dt
  })
  
  
  
  #######
  
  output$CCDetailedSharedPep <- renderDataTable({
    rvCC$detailedselectedNode
    if(is.null((rvCC$detailedselectedNode$sharedPepLabels))){return(NULL)}
    
    
    ind <- 1:ncol(rv$current.obj)
    data <- getDataForExprs(rv$current.obj)
    pepLine <- 1 + rvCC$detailedselectedNode$sharedPepLabels
    data <- data[pepLine,c(ind, (ind + ncol(data)/2))]
    
    dt <- datatable( data,
                     extensions = c('Scroller', 'Buttons'),
                     options = list(initComplete = initComplete(),
                                    dom='Bfrtip',
                                    blengthChange = FALSE,
                                    ordering=FALSE,
                                    scrollX = 400,
                                    scrollY = 100,
                                    displayLength = 10,
                                    scroller = TRUE,
                                    header=FALSE,
                                    server = FALSE,
                                    columnDefs = list(list(targets = c(((ncol(data)/2)+1):(ncol(data))), visible = FALSE))
                     )) %>%
      formatStyle(
        colnames(data)[1:(ncol(data)/2)],
        colnames(data)[((ncol(data)/2)+1):(ncol(data))],
        backgroundColor = styleEqual(c("POV", "MEC"), c(rv$colorsTypeMV$POV, rv$colorsTypeMV$MEC)))
    
    dt
  })
  
  
  
  
  
  #####-----------
  output$CCDetailedSpecPep <- renderDataTable({
    rvCC$detailedselectedNode
    if(is.null((rvCC$detailedselectedNode$specPepLabels))){return(NULL)}
    
    
    ind <- 1:ncol(rv$current.obj)
    data <- getDataForExprs(rv$current.obj)
    pepLine <-  1 + rvCC$detailedselectedNode$specPepLabels
    data <- data[pepLine,c(ind, (ind + ncol(data)/2))]
    
    dt <- datatable( data,
                     extensions = c('Scroller', 'Buttons'),
                     options = list(initComplete = initComplete(),
                                    dom='Bfrtip',
                                    blengthChange = FALSE,
                                    ordering=FALSE,
                                    scrollX = 400,
                                    scrollY = 100,
                                    displayLength = 10,
                                    scroller = TRUE,
                                    header=FALSE,
                                    server = FALSE,
                                    columnDefs = list(list(targets = c(((ncol(data)/2)+1):(ncol(data))), visible = FALSE))
                     )) %>%
      formatStyle(
        colnames(data)[1:(ncol(data)/2)],
        colnames(data)[((ncol(data)/2)+1):(ncol(data))],
        backgroundColor = styleEqual(c("POV", "MEC"), c(rv$colorsTypeMV$POV, rv$colorsTypeMV$MEC)))
    
    dt
  })
  
  
  
  
  
  
  Get_CC_One2One <- reactive({
    rv$CC$allPep
    ll.prot <- lapply(rv$CC$allPep, function(x){length(x$proteins)})
    ll.pept <- lapply(rv$CC$allPep, function(x){length(x$peptides)})
    ll.prot.one2one <- intersect(which(ll.prot == 1),which(ll.pept == 1))
    print(paste0("In Get_CC_One2One:  ", length(ll.prot.one2one)))
    ll.prot.one2one
  })
  
  Get_CC_One2multi <- reactive({
    rv$CC$allPep
    ll.prot <- lapply(rv$CC$allPep, function(x){length(x$proteins)})
    ll.pept <- lapply(rv$CC$allPep, function(x){length(x$peptides)})
    ll.prot.one2multi <- intersect(which(ll.prot == 1),which(ll.pept > 1))
    print(paste0("In Get_CC_One2multi:  ", length(ll.prot.one2multi)))
    ll.prot.one2multi
  })
  
  Get_CC_Multi2Any <- reactive({
    rv$CC$allPep
    ll.prot <- lapply(rv$CC$allPep, function(x){length(x$proteins)})
    ll.pept <- lapply(rv$CC$allPep, function(x){length(x$peptides)})
    ll.prot.multi2any <- which(ll.prot > 1)
    print(paste0("In Get_CC_Multi2Any:  ", length(ll.prot.multi2any)))
    ll.prot.multi2any
  })
  
  
  
  BuildOne2OneTab <- reactive({
    rv$CC$allPep
    table <- do.call(rbind,lapply(rv$CC$allPep[Get_CC_One2One()],function(x){data.frame(rbind(x))}))
    print(paste0("In BuildOne2OneTab:  ", nrow(table)))
    table
  })
  
  BuildOne2MultiTab <- reactive({
    rv$CC$allPep
    table <- do.call(rbind,lapply(rv$CC$allPep[Get_CC_One2multi()],function(x){data.frame(rbind(x), nPep = length(x$peptides))}))
    table <- table[c('proteins', 'nPep', 'peptides')]
    print(paste0("In BuildOne2MultiTab:  ", nrow(table)))
    table
  })
  
  
  BuildMulti2AnyTab <- reactive({
    rv$CC$allPep
    table <- do.call(rbind,lapply(rv$CC$allPep[Get_CC_Multi2Any()],function(x){data.frame(rbind(x), nPep = length(x$peptides))}))
    table <- table[c('proteins', 'nPep', 'peptides')]
    
    print(paste0("In BuildMulti2AnyTab:  ", nrow(table)))
    table
  })
  
  
  
  
  
  output$OneMultiDT <- renderDataTable({
    req(rv$CC$allPep)
    
    dat <- DT::datatable(BuildOne2MultiTab(),
                         selection = 'single',
                         rownames=FALSE,
                         extensions = c('Scroller', 'Buttons'),
                         options=list(initComplete = initComplete(),
                                      dom='Bfrtip',
                                      deferRender = TRUE,
                                      bLengthChange = TRUE,
                                      displayLength = 10,
                                      scrollX = 400,
                                      scrollY = 400,
                                      scroller = TRUE,
                                      orderClasses = TRUE,
                                      autoWidth=FALSE,
                                      columns.searchable=F,
                                      columnDefs = list(list(columns.width=c("60px"),
                                                             columnDefs.targets=c(list(0),list(1),list(2))))))
    
    return(dat)
  })
  
  
  
  output$OneMultiDTDetailed <- renderDataTable({
    req(input$OneMultiDT_rows_selected)
    
    line <- input$OneMultiDT_rows_selected
    
    ind <- 1:ncol(rv$current.obj)
    data <- getDataForExprs(rv$current.obj)
    pepLine <- 1 + as.numeric(unlist(BuildOne2MultiTab()[line,"peptides"]))
    data <- data[pepLine,c(ind, (ind + ncol(data)/2))]
    dt <- datatable( data,
                     extensions = c('Scroller', 'Buttons'),
                     options = list(initComplete = initComplete(),
                                    dom='Bfrtip',
                                    pageLength = 10,
                                    blengthChange = FALSE,
                                    displayLength = 10,
                                    ordering=FALSE,
                                    header=FALSE,
                                    server = FALSE,
                                    columnDefs = list(list(targets = c(((ncol(data)/2)+1):(ncol(data))), visible = FALSE))
                     )) %>%
      formatStyle(
        colnames(data)[1:(ncol(data)/2)],
        colnames(data)[((ncol(data)/2)+1):(ncol(data))],
        backgroundColor = styleEqual(c("POV", "MEC"), c(rv$colorsTypeMV$POV, rv$colorsTypeMV$MEC)))
    
    dt
  })
  
  
  
  
  output$OneOneDT <- renderDataTable({
    req(rv$CC$allPep)
    
    dat <- DT::datatable(BuildOne2OneTab(),
                         selection = 'single',
                         rownames=FALSE,
                         extensions = c('Scroller', 'Buttons'),
                         options=list(initComplete = initComplete(),
                                      dom='Bfrtip',
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
  
  
  
  output$OneOneDTDetailed <- renderDataTable({
    req(rv$CC$allPep)
    req(input$OneOneDT_rows_selected)
    
    line <- input$OneOneDT_rows_selected
    
    print(paste0('Line selected = ', line))
    
    ind <- 1:ncol(rv$current.obj)
    data <- getDataForExprs(rv$current.obj)
    pepLine <- 1 + as.numeric(BuildOne2OneTab()[line,2])
    data <- data[pepLine,c(ind, (ind + ncol(data)/2))]
    dt <- datatable( data,
                     extensions = c('Scroller', 'Buttons'),
                     options = list(initComplete = initComplete(),
                                    dom='Bfrtip',
                                    blengthChange = FALSE,
                                    pageLength = 10,
                                    displayLength = 10,
                                    ordering=FALSE,
                                    header=FALSE,
                                    server = FALSE,
                                    columnDefs = list(list(targets = c(((ncol(data)/2)+1):(ncol(data))), visible = FALSE))
                     )) %>%
      formatStyle(
        colnames(data)[1:(ncol(data)/2)],
        colnames(data)[((ncol(data)/2)+1):(ncol(data))],
        backgroundColor = styleEqual(c("POV", "MEC"), c(rv$colorsTypeMV$POV, rv$colorsTypeMV$MEC)))
    
    dt
  })
  
  
  
}
