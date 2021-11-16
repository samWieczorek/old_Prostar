

#------------------------------------------------------------
moduleVolcanoplotUI <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("nbSelectedItems")),
    highchartOutput(ns("volcanoPlot"), width='600px', height='600px'),
    
    uiOutput(ns("quantiDT"))
  )
}


#------------------------------------------------------------

moduleVolcanoplot <- function(input, output, session,
                              data, 
                              comp, 
                              tooltip){
  
  ns <- session$ns
  
  
  output$quantiDT <- renderUI({
    req(input$eventPointClicked)
    
    if (DAPAR::GetTypeofData(rv$current.obj) == 'protein'){
      if (is.null(DAPAR::GetMatAdj(rv$current.obj))){
        shinyBS::bsCollapse(id = ns("collapseVolcanoInfos"), 
                            open = "Protein",
                            multiple = TRUE,
                            shinyBS::bsCollapsePanel("Protein", 
                                                     tagList(
                                                       uiOutput(ns("Warning_Infos")),
                                                       DT::dataTableOutput(ns("Infos"))),
                                                     style = "info"))
      } else {
        shinyBS::bsCollapse(id = ns("collapseVolcanoInfos"), 
                            open = "Protein",
                            multiple = TRUE,
                            shinyBS::bsCollapsePanel("Protein", 
                                                     tagList(
                                                       uiOutput(ns("Warning_Infos")),
                                                       DT::dataTableOutput(ns("Infos"))),
                                                     style = "info"),
                            shinyBS::bsCollapsePanel("Specific peptides",
                                                     tagList(
                                                       uiOutput(ns("Warning_specificPeptidesInfos")),
                                                       DT::dataTableOutput(ns("specificPeptidesInfos"))), 
                                                     style = "primary"),
                            shinyBS::bsCollapsePanel("Shared peptides",
                                                     tagList(
                                                       uiOutput(ns("Warning_sharedPeptidesInfos")),
                                                       DT::dataTableOutput(ns("sharedPeptidesInfos"))), 
                                                     style = "primary")
        )
      }
    } else if (GetTypeofData(rv$current.obj) == 'peptide'){
      shinyBS::bsCollapse(id = ns("collapseVolcanoInfos"), 
                          open = "peptide",
                          multiple = TRUE,
                          shinyBS::bsCollapsePanel("Peptide", 
                                                   tagList(
                                                     uiOutput(ns("Warning_Infos")),
                                                     DT::dataTableOutput(ns("Infos"))),
                                                   style = "info"))
    }
  })
  
  
  output$nbSelectedItems <- renderUI({ 

    rv$widgets$anaDiff$th_pval
    rv$widgets$hypothesisTest$th_logFC
    rv$current.obj
    req(data()$logFC)
    req(data()$P_Value)
    data()
    
    
    m <- match.metacell(DAPAR::GetMetacell(rv$current.obj), 
                        pattern="missing", 
                        level = 'peptide')
   # req(length(which(m)) > 0)
    
    p <- data()
    upItemsPVal <- NULL
    upItemsLogFC <- NULL
    
    
    upItemsLogFC <- which(abs(p$logFC) >= as.numeric(rv$widgets$hypothesisTest$th_logFC))
    upItemsPVal <- which(-log10(p$P_Value) >= as.numeric(rv$widgets$anaDiff$th_pval)
    )
    
    rv$nbTotalAnaDiff <- nrow(Biobase::exprs(rv$current.obj))
    rv$nbSelectedAnaDiff <- NULL
    t <- NULL
    
    if (!is.null(rv$widgets$anaDiff$th_pval
    ) && !is.null(rv$widgets$hypothesisTest$th_logFC) ) {
      t <- intersect(upItemsPVal, upItemsLogFC)}
    else if (!is.null(rv$widgets$anaDiff$th_pval
    ) && is.null(rv$widgets$hypothesisTest$th_logFC) ) {
      t <- upItemsPVal}
    else if (is.null(rv$widgets$anaDiff$th_pval
    ) && !is.null(rv$widgets$hypothesisTest$th_logFC) ) {
      t <- upItemsLogFC}
    rv$nbSelectedAnaDiff <- length(t)
    
    txt <- paste("Total number of ",rv$typeOfDataset, "(s) = ",
                 rv$nbTotalAnaDiff,"<br>",
                 "Number of selected ",rv$typeOfDataset, "(s) = ",
                 rv$nbSelectedAnaDiff,"<br>",
                 "Number of non selected ",rv$typeOfDataset, "(s) = ",
                 (rv$nbTotalAnaDiff -rv$nbSelectedAnaDiff), sep="")
    HTML(txt)
  })
  
  
  GetSortingIndices <- reactive({
    req(comp())
    
    condition1 = gsub('[()]', '', strsplit(comp(), "_vs_")[[1]][1])
    condition2 = gsub('[()]', '', strsplit(comp(), "_vs_")[[1]][2])
    if (length(grep("all",condition2))==0)
      ind <- c( which(pData(rv$current.obj)$Condition==condition1), 
                which(pData(rv$current.obj)$Condition==condition2))
    else
      ind <- c( which(pData(rv$current.obj)$Condition==condition1), 
                c(1:nrow(pData(rv$current.obj)))[-(which(pData(rv$current.obj)$Condition==condition1))])
    ind
  })
  
  GetBorderIndices <- reactive({
    conds <- (pData(rv$current.obj)$Condition)[GetSortingIndices()]
    ## build index for border-formatting
    borders_index <- unlist(lapply(unique(conds), function(x){which.max(x== conds)}))
    borders_index
  })
  
  
  
  output$Warning_sharedPeptidesInfos <- renderUI({
    GetDataFor_sharedPeptidesInfos()
    if (nrow(GetDataFor_sharedPeptidesInfos())>153)
      p(MSG_WARNING_SIZE_DT)
    
  })
  
  
  
  
  GetDataFor_sharedPeptidesInfos <- reactive({
    req(comp())
    
    ind <- GetSortingIndices()
    borders_index <- GetBorderIndices()
    
    prev.dataset <- rv$dataset[[names(rv$dataset)[last(grep(pattern='peptide', names(rv$dataset)))]]]
    
    prot <- GetExprsClickedProtein()
    prot.indice <- rownames(prot)
    
    data <- getDataForExprs(prev.dataset, rv$settings_nDigits)
    data <- data[,c(ind, (ind + ncol(data)/2))]
    
    Xspec <- DAPAR::GetMatAdj(rv$current.obj)$matWithUniquePeptides
    Xshared <- DAPAR::GetMatAdj(rv$current.obj)$matWithSharedPeptides
    
    i <- which(colnames(Xspec)==prot.indice)
    specificPeptidesIndices <- which(Xspec[,i]==1)
    allPeptidesIndices <- which(Xshared[,i]==1)
    peptidesIndices <- setdiff(allPeptidesIndices, specificPeptidesIndices)
    data <- data[peptidesIndices,]
    data
  })
  
  output$sharedPeptidesInfos <- renderDataTable(server=TRUE,{
    data <-  GetDataFor_sharedPeptidesInfos()
    c.tags <- BuildColorStyles(rv$current.obj)$tags
    c.colors <-  BuildColorStyles(rv$current.obj)$colors
    
    dt <- DT::datatable(data,
                        #colnames=NULL,
                        extensions = c('Scroller'),
                        options = list(initComplete = initComplete(),
                                       dom = 'frtip',
                                       blengthChange = FALSE,
                                       displayLength = 20,
                                       ordering = FALSE,
                                       server = FALSE,
                                       columnDefs = list(list(targets = c(((ncol(data)/2)+1):(ncol(data))), visible = FALSE))
                        )) %>%
      formatStyle(
        colnames(data)[1:(ncol(data)/2)],
        colnames(data)[((ncol(data)/2)+1):(ncol(data))],
        backgroundColor = styleEqual(c.tags, c.colors)
      ) %>% 
      formatStyle(borders_index, borderLeft = '3px solid #000000')
    
    dt
  })
  
  output$Warning_specificPeptidesInfos <- renderUI({
    GetDataFor_specificPeptidesInfos()
    if (nrow(GetDataFor_specificPeptidesInfos())>153)
      p(MSG_WARNING_SIZE_DT)
    
  })
  
  GetDataFor_specificPeptidesInfos <- reactive({
    req(comp())
    
    ind <- GetSortingIndices()
    borders_index <- GetBorderIndices()
    
    prev.dataset <- rv$dataset[[names(rv$dataset)[last(grep(pattern='peptide', names(rv$dataset)))]]]
    
    prot <- GetExprsClickedProtein()
    prot.indice <- rownames(prot)
    
    data <- getDataForExprs(prev.dataset, rv$settings_nDigits)
    data <- data[,c(ind, (ind + ncol(data)/2))]
    
    
    Xspec <- DAPAR::GetMatAdj(rv$current.obj)$matWithUniquePeptides
    
    i <- which(colnames(Xspec)==prot.indice)
    peptidesIndices <- which(Xspec[,i]==1)
    data <- data[peptidesIndices,]
    data
  })
  
  
  output$specificPeptidesInfos <- renderDataTable(server=TRUE,{
    
    data <- GetDataFor_specificPeptidesInfos()
    c.tags <- BuildColorStyles(rv$current.obj)$tags
    c.colors <-  BuildColorStyles(rv$current.obj)$colors
    
    dt <- DT::datatable( data, 
                         #colnames=NULL,
                         extensions = c('Scroller'),
                         options = list(initComplete = initComplete(),
                                        dom = 'frtip',
                                        blengthChange = FALSE,
                                        displayLength = 20,
                                        ordering = FALSE,
                                        columnDefs = list(list(targets = c(((ncol(data)/2)+1):(ncol(data))), visible = FALSE))
                         )) %>%
      formatStyle(
        colnames(data)[1:(ncol(data)/2)],
        colnames(data)[((ncol(data)/2)+1):(ncol(data))],
        backgroundColor = styleEqual(c.tags, c.colors)
      ) %>% 
      formatStyle(borders_index, borderLeft = '3px solid #000000')
    
    dt
  })
  
  
  ##------------------------------------------------------------------------------
  GetExprsClickedProtein <- reactive({
    req(rv$current.obj)
    req(comp() != 'None')
    req(input$eventPointClicked)
    rv$widgets$hypothesisTest$th_logFC
    rv$widgets$anaDiff$th_pval
    data()
    
    ind <- GetSortingIndices()
    #browser()
    this.index <- as.integer(strsplit(input$eventPointClicked, "_")[[1]][1])
    this.series.name <- strsplit(input$eventPointClicked, "_")[[1]][2]
    
    data <- getDataForExprs(rv$current.obj, rv$settings_nDigits)
    data <- data[,c(ind, (ind + ncol(data)/2))]
    
    index.g1 <- which((-log10(data()$P_Value) >= rv$widgets$anaDiff$th_pval
    ) & (abs(data()$logFC) >= as.numeric(rv$widgets$hypothesisTest$th_logFC)))
    
    data.g1 <- data[index.g1, ]
    data.g2 <- data[-index.g1, ]
    
    switch (this.series.name,
            g1 = data <- data.g1[this.index+1, ],
            g2 = data <- data.g2[this.index+1, ]
    )
    data
  })
  
  
  
  output$Warning_Infos <- renderUI({
    GetDataFor_Infos()
    if (nrow(GetDataFor_Infos()) > 153)
      p(MSG_WARNING_SIZE_DT)
  })
  
  
  
  
  GetDataFor_Infos <- reactive({
    req(comp())
    data <- GetExprsClickedProtein()
    data
  })
  
  ##------------------------------------------------------------------------------
  output$Infos <- renderDataTable(server = TRUE, { 
    req(comp())
    
    borders_index <- GetBorderIndices()
    data <- GetExprsClickedProtein()
    c.tags <- BuildColorStyles(rv$current.obj)$tags
    c.colors <-  BuildColorStyles(rv$current.obj)$colors
    
    dt <- DT::datatable(data,
                        extensions = c('Scroller'),
                        options = list(initComplete = initComplete(),
                                       dom = 'frtip',
                                       blengthChange = FALSE,
                                       displayLength = 20,
                                       ordering = FALSE,
                                       header = FALSE,
                                       columnDefs = list(list(targets = c(((ncol(data)/2)+1):(ncol(data))), visible = FALSE))
                        )) %>%
      formatStyle(
        colnames(data)[1:(ncol(data)/2)],
        colnames(data)[((ncol(data)/2)+1):(ncol(data))],
        backgroundColor = styleEqual(c.tags, c.colors)) %>% 
      formatStyle(borders_index, borderLeft = '3px solid #000000')
    
    
    
    dt
  })
  
  ##---------------------------------------------------------------------
  output$volcanoPlot <-  renderHighchart({ 
    rv$widgets$anaDiff$th_pval
    rv$widgets$hypothesisTest$th_logFC
    rv$colorsVolcanoplot
    data()$P_Value
    req(length(data()$logFC) > 0)
    tooltip()
    
    #browser()
    isolate({
      
      withProgress(message = 'Building plot...',detail = '', value = 0, {
        m <- match.metacell(DAPAR::GetMetacell(rv$current.obj), 
                            pattern="missing",
                            level = DAPAR::GetTypeofData(rv$current.obj)
        )
        if (length(which(m)) > 0)
          return(NULL)
        df <-  data.frame(x = data()$logFC, 
                          y = -log10(data()$P_Value),
                          index = 1:nrow(fData(rv$current.obj)))
        if (length( tooltip()) > 0 && !is.na(tooltip()))
          df <- cbind(df,
                      fData(rv$current.obj)[ tooltip()]
          )
        
        colnames(df) <- gsub(".", "_", colnames(df), fixed=TRUE)
        if (ncol(df) > 3)
          colnames(df)[4:ncol(df)] <- 
          paste("tooltip_", colnames(df)[4:ncol(df)], sep="")
        
        clickFun <-   
          JS(paste0("function(event) {Shiny.onInputChange('",ns("eventPointClicked"),"', [this.index]+'_'+ [this.series.name]);}"))
        
        cond <- c(data()$condition1, data()$condition2)
        rv$tempplot$volcano <-  diffAnaVolcanoplot_rCharts(df,
                                                           threshold_logFC = as.numeric(rv$widgets$hypothesisTest$th_logFC),
                                                           threshold_pVal = as.numeric(rv$widgets$anaDiff$th_pval),
                                                           conditions = cond,
                                                           clickFunction = clickFun,
                                                           pal = rv$colorsVolcanoplot
        )
        
      })
      
      rv$tempplot$volcano
    })
  })
  
  
}
