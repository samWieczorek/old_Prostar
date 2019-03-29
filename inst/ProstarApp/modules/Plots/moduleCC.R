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
                        fluidRow(
                          column(width=6,dataTableOutput(ns('CCMultiMulti'), height='200px')),
                          column(width=6, visNetworkOutput(ns("visNet_CC"), height='600px'))
                        ),
                        dataTableOutput(ns('CCMultiMultiDetailed'))
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
  
  observeEvent(input$click,{
    #GetClickedPoint()
    cc()
    print(paste0("Node ID clicked : ", input$click))
    
    #tt <- cc()[[GetClickedPoint()]]
    #lst <- c(tt$peptides, tt$proteins)
    #print(paste0("Entity ID clicked : ", lst[input$click]))
    #print(exprs(rv$current.obj)[lst[input$click],])
    print(fData(rv$current.obj)[lst[input$click],])
  })
  
  
  GetClickedPoint <- reactive({
    req(rv$current.obj)
    req(input$eventPointClicked)
    
    this.index <- as.integer(strsplit(input$eventPointClicked, "_")[[1]][1])
    print(paste0("this.index = ", this.index))
    this.index+1
  })
  
  
  output$visNet_CC <- renderVisNetwork({
   
    #GetClickedPoint()
    req(input$CCMultiMulti_rows_selected)
    
    local <-   cc()[Get_CC_Multi2Any()]
    print("######### input$CCMultiMulti_row_selected ###############")
    indice <- input$CCMultiMulti_rows_selected
    print(indice)
    
    display.CC.visNet(local[[indice]], rv$matAdj$matWithSharedPeptides) %>%
    visEvents(click = paste0("function(nodes){
                Shiny.onInputChange('",ns("click"),"', nodes.nodes[0]);
                ;}")
      ) %>% 
      visOptions(highlightNearest = list(enabled = TRUE, hover = TRUE))
    
    
   
  })
  
  
  output$jiji <- renderHighchart({
    tooltip <- NULL
    
   isolate({
     n.prot <- unlist(lapply(cc(), function(x){length(x$proteins)}))
     n.pept <- unlist(lapply(cc(), function(x){length(x$peptides)}))
     df <- tibble(x=jitter(n.pept),
                     y = jitter(n.prot),
                     index = 1:length(cc()))
        
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
  
  
  
  output$CCMultiMultiDetailed <- renderDataTable({
    Get_CC_Multi2Any()
    req(input$CCMultiMulti_rows_selected)
    
    line <- input$CCMultiMulti_rows_selected
    
    ind <- 1:ncol(rv$current.obj)
    data <- getDataForExprs(rv$current.obj)
    pepLine <- 1 + as.numeric(unlist(BuildMulti2AnyTab()[line,"peptides"]))
    data <- data[pepLine,c(ind, (ind + ncol(data)/2))]
    
    dt <- datatable( data,
                     extensions = c('Scroller', 'Buttons'),
                     options = list(initComplete = initComplete(),
                                    dom='Bfrtip',
                                    blengthChange = FALSE,
                                    ordering=FALSE,
                                    scrollX = 400,
                                    scrollY = 400,
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
  
  
  
  # output$quantiDT <- renderUI({
  #   req(input$eventPointClicked)
  #   
  #   if (is.null(rv$matAdj)){
  #     bsCollapse(id = ns("collapseVolcanoInfos"), open = "Protein",multiple = TRUE,
  #                bsCollapsePanel("Protein", DT::dataTableOutput(ns("Infos")),style = "info"))
  #   } else {
  #     bsCollapse(id = ns("collapseVolcanoInfos"), open = "Protein",multiple = TRUE,
  #                bsCollapsePanel("Protein", DT::dataTableOutput(ns("Infos")),style = "info"),
  #                bsCollapsePanel("Specific peptides", DT::dataTableOutput(ns("specificPeptidesInfos")), style = "primary"),
  #                bsCollapsePanel("Shared peptides", DT::dataTableOutput(ns("sharedPeptidesInfos")), style = "primary"))
  #   }
  # })
  
  
  # output$nbSelectedItems <- renderUI({ 
  #   
  #   rv$widgets$anaDiff$th_pval
  #   rv$widgets$hypothesisTest$th_logFC
  #   rv$current.obj
  #   rv$resAnaDiff
  #   
  #   
  #   if(is.null(rv$resAnaDiff$logFC) || is.null(rv$resAnaDiff$P_Value)){return(NULL)}
  #   if (length(which(is.na(Biobase::exprs(rv$current.obj)))) > 0) {return(NULL)}
  #   p <- NULL
  #   p <- rv$resAnaDiff
  #   upItemsPVal <- NULL
  #   upItemsLogFC <- NULL
  #   
  #   
  #   upItemsLogFC <- which(abs(p$logFC) >= as.numeric(rv$widgets$hypothesisTest$th_logFC))
  #   upItemsPVal <- which(-log10(p$P_Value) >= as.numeric(rv$widgets$anaDiff$th_pval)
  #   )
  #   
  #   rv$nbTotalAnaDiff <- nrow(Biobase::exprs(rv$current.obj))
  #   rv$nbSelectedAnaDiff <- NULL
  #   t <- NULL
  #   
  #   if (!is.null(rv$widgets$anaDiff$th_pval
  #   ) && !is.null(rv$widgets$hypothesisTest$th_logFC) ) {
  #     t <- intersect(upItemsPVal, upItemsLogFC)}
  #   else if (!is.null(rv$widgets$anaDiff$th_pval
  #   ) && is.null(rv$widgets$hypothesisTest$th_logFC) ) {
  #     t <- upItemsPVal}
  #   else if (is.null(rv$widgets$anaDiff$th_pval
  #   ) && !is.null(rv$widgets$hypothesisTest$th_logFC) ) {
  #     t <- upItemsLogFC}
  #   rv$nbSelectedAnaDiff <- length(t)
  #   
  #   txt <- paste("Total number of ",rv$typeOfDataset, "(s) = ",
  #                rv$nbTotalAnaDiff,"<br>",
  #                "Number of selected ",rv$typeOfDataset, "(s) = ",
  #                rv$nbSelectedAnaDiff,"<br>",
  #                "Number of non selected ",rv$typeOfDataset, "(s) = ",
  #                (rv$nbTotalAnaDiff -rv$nbSelectedAnaDiff), sep="")
  #   HTML(txt)
  # })
  
  
  # GetSortingIndices <- reactive({
  #   req(comp())
  #   
  #   condition1 = strsplit(comp(), "_vs_")[[1]][1]
  #   condition2 = strsplit(comp(), "_vs_")[[1]][2]
  #   if (length(grep("all",condition2))==0) {
  #     ind <- c( which(pData(rv$current.obj)$Condition==condition1), 
  #               which(pData(rv$current.obj)$Condition==condition2))
  #   } else {
  #     ind <- c( which(pData(rv$current.obj)$Condition==condition1), 
  #               c(1:nrow(pData(rv$current.obj)))[-(which(pData(rv$current.obj)$Condition==condition1))])
  #   }
  #   
  #   ind
  #   
  # })
  
  # GetBorderIndices <- reactive({
  #   conds <- (pData(rv$current.obj)$Condition)[GetSortingIndices()]
  #   ## build index for border-formatting
  #   borders_index <- unlist(lapply(unique(conds), function(x){first(grep(x, conds))}))
  #   borders_index
  # })
  
  
  # output$sharedPeptidesInfos <- renderDataTable({
  #   #req(rv$current.obj)
  #   req(comp())
  #   #req(rv$matAdj)
  #   
  #   ind <- GetSortingIndices()
  #   borders_index <- GetBorderIndices()
  #   
  #   prev.dataset <- rv$dataset[[names(rv$dataset)[last(grep(pattern='peptide', names(rv$dataset)))]]]
  #   
  #   prot <- GetExprsClickedProtein()
  #   prot.indice <- rownames(prot)
  #   #print(prot.indice)
  #   
  #   data <- getDataForExprs(prev.dataset)
  #   data <- data[,c(ind, (ind + ncol(data)/2))]
  #   
  #   Xspec <- rv$matAdj$matWithUniquePeptides
  #   Xshared <- rv$matAdj$matWithSharedPeptides
  #   
  #   i <- which(colnames(Xspec)==prot.indice)
  #   specificPeptidesIndices <- which(Xspec[,i]==1)
  #   allPeptidesIndices <- which(Xshared[,i]==1)
  #   peptidesIndices <- setdiff(allPeptidesIndices, specificPeptidesIndices)
  #   data <- data[peptidesIndices,]
  #   
  #   dt <- datatable( data,
  #                    #colnames=NULL,
  #                    extensions = c('Scroller', 'Buttons'),
  #                    options = list(initComplete = initComplete(),
  #                                   dom='Bfrtip',
  #                                   blengthChange = FALSE,
  #                                   displayLength = 20,
  #                                   ordering=FALSE,
  #                                   server = FALSE,
  #                                   columnDefs = list(list(targets = c(((ncol(data)/2)+1):(ncol(data))), visible = FALSE))
  #                    )) %>%
  #     formatStyle(
  #       colnames(data)[1:(ncol(data)/2)],
  #       colnames(data)[((ncol(data)/2)+1):(ncol(data))],
  #       backgroundColor = styleEqual(c("POV", "MEC"), c(rv$colorsTypeMV$POV, rv$colorsTypeMV$MEC))
  #     ) %>% 
  #     formatStyle(borders_index, borderLeft = '3px solid #000000')
  #   
  #   dt
  # })
  
  # output$specificPeptidesInfos <- renderDataTable({
  #   #req(rv$current.obj)
  #   req(comp())
  #   #req(rv$matAdj)
  #   
  #   ind <- GetSortingIndices()
  #   borders_index <- GetBorderIndices()
  #   
  #   prev.dataset <- rv$dataset[[names(rv$dataset)[last(grep(pattern='peptide', names(rv$dataset)))]]]
  #   
  #   prot <- GetExprsClickedProtein()
  #   prot.indice <- rownames(prot)
  #   
  #   data <- getDataForExprs(prev.dataset)
  #   data <- data[,c(ind, (ind + ncol(data)/2))]
  #   
  #   
  #   Xspec <- rv$matAdj$matWithUniquePeptides
  #   
  #   i <- which(colnames(Xspec)==prot.indice)
  #   peptidesIndices <- which(Xspec[,i]==1)
  #   data <- data[peptidesIndices,]
  #   
  #   #data <- data[,ind]
  #   
  #   dt <- datatable( data, 
  #                    #colnames=NULL,
  #                    extensions = c('Scroller', 'Buttons'),
  #                    options = list(initComplete = initComplete(),
  #                                   dom='Bfrtip',
  #                                   blengthChange = FALSE,
  #                                   displayLength = 20,
  #                                   ordering=FALSE,
  #                                   server = FALSE,
  #                                   columnDefs = list(list(targets = c(((ncol(data)/2)+1):(ncol(data))), visible = FALSE))
  #                    )) %>%
  #     formatStyle(
  #       colnames(data)[1:(ncol(data)/2)],
  #       colnames(data)[((ncol(data)/2)+1):(ncol(data))],
  #       backgroundColor = styleEqual(c("POV", "MEC"), c(rv$colorsTypeMV$POV, rv$colorsTypeMV$MEC))
  #     ) %>% 
  #     formatStyle(borders_index, borderLeft = '3px solid #000000')
  #   
  #   dt
  # })
  
  
  ##------------------------------------------------------------------------------
 
  
  
  
 
  ##------------------------------------------------------------------------------
  # output$Infos <- renderDataTable({ 
  #   req(comp())
  #   
  #   borders_index <- GetBorderIndices()
  #   
  #   data <- GetExprsClickedProtein()
  #   
  #   print('################### Dans Infos  #################')
  #   print(colnames(data))
  #   
  #   dt <- datatable( data,
  #                    extensions = c('Scroller', 'Buttons'),
  #                    options = list(initComplete = initComplete(),
  #                                   dom='Bfrtip',
  #                                   blengthChange = FALSE,
  #                                   displayLength = 20,
  #                                   ordering=FALSE,
  #                                   header=FALSE,
  #                                   server = FALSE,
  #                                   columnDefs = list(list(targets = c(((ncol(data)/2)+1):(ncol(data))), visible = FALSE))
  #                    )) %>%
  #     formatStyle(
  #       colnames(data)[1:(ncol(data)/2)],
  #       colnames(data)[((ncol(data)/2)+1):(ncol(data))],
  #       backgroundColor = styleEqual(c("POV", "MEC"), c(rv$colorsTypeMV$POV, rv$colorsTypeMV$MEC))) %>% 
  #     formatStyle(borders_index, borderLeft = '3px solid #000000')
  #   
  #   
  #   
  #   dt
  # })
  
  ##---------------------------------------------------------------------
  # output$volcanoPlot <-  renderHighchart({ 
  #   rv$widgets$anaDiff$th_pval
  #   rv$widgets$hypothesisTest$th_logFC
  #   rv$colorsVolcanoplot
  #   rv$resAnaDiff
  #   tooltip()
  #   swap()
  #   
  #   
  #   #if (is.null(rv$widgets$hypothesisTest$th_logFC) || is.na(rv$widgets$hypothesisTest$th_logFC) ){return()}
  #   if ((length(rv$resAnaDiff$logFC) == 0)  ){return()}
  #   
  #   if (length(which(is.na(Biobase::exprs(rv$current.obj)))) > 0) { return()}
  #   
  #   isolate({
  #     
  #     df <- data_frame(x=rv$resAnaDiff$logFC, 
  #                      y = -log10(rv$resAnaDiff$P_Value),
  #                      index = 1:nrow(fData(rv$current.obj)))
  #     if (!is.null( tooltip())){
  #       df <- cbind(df,fData(rv$current.obj)[ tooltip()])
  #     }
  #     
  #     colnames(df) <- gsub(".", "_", colnames(df), fixed=TRUE)
  #     if (ncol(df) > 3){
  #       colnames(df)[4:ncol(df)] <- 
  #         paste("tooltip_", colnames(df)[4:ncol(df)], sep="")
  #     }
  #     
  #     clickFun <-   
  #       JS(paste0("function(event) {Shiny.onInputChange('",ns("eventPointClicked"),"', [this.index]+'_'+ [this.series.name]);}"))
  #     
  #     cond <- c(rv$resAnaDiff$condition1, rv$resAnaDiff$condition2)
  #     rv$tempplot$volcano <-  diffAnaVolcanoplot_rCharts(df,
  #                                                        threshold_logFC = as.numeric(rv$widgets$hypothesisTest$th_logFC),
  #                                                        threshold_pVal = as.numeric(rv$widgets$anaDiff$th_pval),
  #                                                        conditions = cond,
  #                                                        clickFunction=clickFun,
  #                                                        rv$colorsVolcanoplot, swap())
  #     
  #   })
  #   
  #   rv$tempplot$volcano
  # })
  
  
}
