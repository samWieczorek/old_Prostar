moduleGraphMulti2AnyUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("CCTooltip_UI")),
    
    highchartOutput(ns("jiji")),
    highchartOutput(ns("graph"))
    #moduleStaticDataTableUI(ns("MultiAnyDT"))
    
    #uiOutput('CC_Multi_Multi')
  )
  
}


moduleGraphMulti2Any <- function(input, output, session,cc, tooltip=NULL){
  
  ns <- session$ns
  
  
  
  output$graph <- renderHighchart({
    GetClickedPoint()
    print(paste0("GetClickedPoint() = ", GetClickedPoint()))
    display.CC(cc()[[GetClickedPoint()]], rv$matAdj$matWithSharedPeptides)
    
  })
  
  
  
  
  
  output$jiji <- renderHighchart({
    tooltip()
    
   isolate({
     n.prot <- unlist(lapply(cc(), function(x){length(x$proteins)}))
     n.pept <- unlist(lapply(cc(), function(x){length(x$peptides)}))
     df <- tibble(x=jitter(n.pept),
                     y = jitter(n.prot),
                     index = 1:length(cc()))
        
        if (!is.null( tooltip())){
          df <- cbind(df,fData(rv$current.obj)[ tooltip()])
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
  
  
  
  GetClickedPoint <- reactive({
    req(rv$current.obj)
    req(input$eventPointClicked)
    
    this.index <- as.integer(strsplit(input$eventPointClicked, "_")[[1]][1])
    print(paste0("this.index = ", this.index))
    this.index
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
