




#################### MODULES DEFINITION #################################
module_Not_a_numeric <- function(input, output, session, n){
  
  output$msg_not_numeric <- renderUI({
    req(n())
    if (is.na(as.numeric(n()))){
      tags$p("Please choose a number")
    }
  })
}


#-----------------------------------------------
moduleTrackProt <- function(input, output, session, params, reset=FALSE){
  
  ns <- session$ns
  
  
  observe({
    reset()
    print("In track module =RESET observe")
    print(reset())
    if (reset() > 0) {
      updateSelectInput(session, "typeSelect", selected="None")
      updateSelectInput(session, "listSelect", NULL)
      updateSelectInput(session, "randSelect", selected="1")
      updateSelectInput(session, "colSelect", selected=NULL)
    }
  })
  
  observe({
    params()
    updateSelectInput(session, "typeSelect", selected=params()$type)
    updateSelectInput(session, "listSelect", selected=params()$list)
    updateSelectInput(session, "randSelect", selected=params()$rand)
    updateSelectInput(session, "colSelect", selected=params()$col)
    })
  
  observeEvent(input$typeSelect, {
    shinyjs::toggle("listSelect", condition=(input$typeSelect=="ProteinList")&&(input$typeSelect!="None"))
    shinyjs::toggle("randSelect", condition=(input$typeSelect=="Random")&&(input$typeSelect!="None"))
    shinyjs::toggle("colSelect", condition=(input$typeSelect=="Column")&&(input$typeSelect!="None"))
  })
  
  output$listSelect_UI <- renderUI({
    isolate({
      ll <-  Biobase::fData(rv$current.obj)[,rv$current.obj@experimentData@other$proteinId]
    hidden(selectInput(ns("listSelect"), "Protein for normalization", choices=ll, multiple = TRUE, width='400px'))
  })
  })
  
  
  output$randomSelect_UI <- renderUI({
    isolate({
      ll <-  Biobase::fData(rv$current.obj)[,rv$current.obj@experimentData@other$proteinId]
    hidden(textInput(ns("randSelect"), "Random", value="1", width=('120px')))
    })
  })
  
  output$columnSelect_UI <- renderUI({
    isolate({
      ll <-  colnames(Biobase::fData(rv$current.obj))
    hidden(selectInput(ns("colSelect"), "Column", choices=ll))
    })
  })
  

  BuildResult <- reactive({
    
    #isolate({
      ll <-  Biobase::fData(rv$current.obj)[,rv$current.obj@experimentData@other$proteinId]
    res <- list(type= input$typeSelect,
                list = input$listSelect,
                rand = as.numeric(input$randSelect),
                col = input$colSelect,
                list.indices = if (length(input$listSelect)==0){NULL} else match(input$listSelect, ll),
                rand.indices = if (length(input$randSelect)==0){NULL} else sample(1:length(ll), as.numeric(input$randSelect), replace=FALSE),
                col.indices =  if (length(input$colSelect)==0){NULL} else which(input$colSelect == 1)
    )
   # })
     
    res
  })
  
  return(reactive({BuildResult()}))
}







moduleDesignExample <- function(input, output, session, n){
  
  
  output$nlevelsExample <- renderRHandsontable({
   
    
    if (n == 2){
                df <- data.frame(Sample.name= paste0("Sample ",as.character(1:14)),
                     Condition = c(rep("A", 4), rep("B", 4), rep("C", 6)),
                     Bio.Rep = as.integer(c(1,1,2,2,3,3,4,4,5,5,6,6,7,7)),
                     Tech.Rep = c(1:14),
                     stringsAsFactors = FALSE)
    
    
                pal <- RColorBrewer::brewer.pal(3,listBrewerPalettes[1])
                color_rend <- paste0("function (instance, td, row, col, prop, value, cellProperties) {
                         Handsontable.renderers.TextRenderer.apply(this, arguments);
                         
                         if(col==1 && (row>=0 && row<=3)) {td.style.background = '",pal[1], "';}
                         if(col==1 && (row>=4 && row<=7)) {td.style.background = '",pal[2], "';}
                         if(col==1 && (row>=8 && row<=14)) {td.style.background = '",pal[3], "';}
                         
                         
                         if(col==2 && (row==0||row==1||row==4||row==5||row==8||row==9||row==12||row==13)) 
                         {td.style.background = 'lightgrey';}
                         
                         if(col==3 && (row==0||row==2||row==4||row==6||row==8||row==10||row==12)) 
                         {td.style.background = 'lightgrey';}
                    }")

    } else if (n == 3){
              df <- data.frame(Sample.name= paste0("Sample ",as.character(1:16)),
                       Condition = c(rep( "A", 8), rep("B", 8)),
                       Bio.Rep = as.integer(c(rep(1,4),rep(2,4),rep(3,4),rep(4,4))),
                       Tech.Rep = as.integer(c(1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8)),
                       Analyt.Rep = c(1:16),
                       stringsAsFactors = FALSE)
      
      
            pal <- RColorBrewer::brewer.pal(3,listBrewerPalettes[1])[1:2]
      
            color_rend <- paste0("function (instance, td, row, col, prop, value, cellProperties) {
                           Handsontable.renderers.TextRenderer.apply(this, arguments);
                           
                           if(col==1 && (row>=0 && row<=7)) {td.style.background = '",pal[1], "';}
                           
                           if(col==1 && (row>=8 && row<=15))  {td.style.background = '",pal[2], "';}
                           
                           if(col==2 && (row==0||row==1||row==2||row==3||row==8||row==9||row==10||row==11)) 
                           {td.style.background = 'lightgrey';}
                           
                           if(col==3 && (row==0||row==1||row==4||row==5|| row==8||row==9||row==12||row==13)) 
                           {td.style.background = 'lightgrey';}
                           
                           
                           if(col==4 && (row==0||row==2||row==4||row==6|| row==8||row==10||row==12||row==14)) 
                           {td.style.background = 'lightgrey';}
                            }")
  
          }
    
    rhandsontable::rhandsontable(df,rowHeaders=NULL,fillHandle = list(direction='vertical', autoInsertRow=FALSE,
                                                                      maxRows=nrow(rv$hot))) %>%
      rhandsontable::hot_rows(rowHeights = 30) %>%
      rhandsontable::hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE,
                                      allowInsertRow = FALSE,allowInsertColumn = FALSE,
                                      allowRemoveRow = FALSE,allowRemoveColumn = FALSE,
                                      autoInsertRow=FALSE     ) %>%
      rhandsontable::hot_cols(readOnly = TRUE,renderer = color_rend)
    
    
   
})
  
  
}






moduleDetQuantImpValues <- function(input, output, session, quant,factor)
{
  
  output$detQuantValues_DT <- renderDataTable({
    req(rv$current.obj, quant(), factor())
    
    values <- getQuantile4Imp(Biobase::exprs(rv$current.obj), quant()/100, factor())
      DT::datatable(as.data.frame(t(values$shiftedImpVal)),
                    rownames = FALSE,
                    options = list(initComplete = initComplete(),
                                   dom = 't',
                                   bLengthChange = FALSE))
 })
}


modulePopover <- function(input, output, session, data){
    
    ns <- session$ns
    
    output$customPopover <- renderUI({
          req(data())
        
          div(
            div(
                # edit1
                style="display:inline-block; vertical-align: middle; padding-bottom: 5px;",
                    data()$title
            ),
            div(
            # edit2
            style="display:inline-block; vertical-align: middle;padding-bottom: 5px;",
            if (!is.null(data()$color) && ('white' == data()$color)) {
              tags$button(id=ns("q1"), tags$sup("[?]"), class="Prostar_tooltip_white")
              } else {
                tags$button(id=ns("q1"), tags$sup("[?]"), class="Prostar_tooltip")
                },
            shinyBS::bsPopover(id = ns("q1"), title = "",
                      content = data()$content,
                      placement = "right", 
                      trigger = "hover", 
                      options = list(container = "body")
            )
        )
        )
 
        
    })
}



#------------------------------------------------------------
moduleLegendColoredExprs <- function(input, output, session){}


#------------------------------------------------------------

moduleVolcanoplot <- function(input, output, session,comp, tooltip, isSwaped){
  
  ns <- session$ns
  
 
  output$quantiDT <- renderUI({
    req(input$eventPointClicked)
    
    if (is.null(rv$matAdj)){
      shinyBS::bsCollapse(id = ns("collapseVolcanoInfos"), open = "Protein",multiple = TRUE,
                          shinyBS::bsCollapsePanel("Protein", DT::dataTableOutput(ns("Infos")),style = "info"))
    } else {
      shinyBS::bsCollapse(id = ns("collapseVolcanoInfos"), open = "Protein",multiple = TRUE,
                 shinyBS::bsCollapsePanel("Protein", DT::dataTableOutput(ns("Infos")),style = "info"),
                 shinyBS::bsCollapsePanel("Specific peptides", DT::dataTableOutput(ns("specificPeptidesInfos")), style = "primary"),
                 shinyBS::bsCollapsePanel("Shared peptides", DT::dataTableOutput(ns("sharedPeptidesInfos")), style = "primary"))
    }
  })
  
  
  output$nbSelectedItems <- renderUI({ 
    
    rv$widgets$anaDiff$th_pval
    rv$widgets$hypothesisTest$th_logFC
    rv$current.obj
    rv$resAnaDiff
    
    
    if(is.null(rv$resAnaDiff$logFC) || is.null(rv$resAnaDiff$P_Value)){return(NULL)}
   if (length(which(is.na(Biobase::exprs(rv$current.obj)))) > 0) {return(NULL)}
    p <- NULL
    p <- rv$resAnaDiff
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
    
    condition1 = strsplit(comp(), "_vs_")[[1]][1]
    condition2 = strsplit(comp(), "_vs_")[[1]][2]
    if (length(grep("all",condition2))==0) {
      ind <- c( which(pData(rv$current.obj)$Condition==condition1), 
                which(pData(rv$current.obj)$Condition==condition2))
    } else {
      ind <- c( which(pData(rv$current.obj)$Condition==condition1), 
                c(1:nrow(pData(rv$current.obj)))[-(which(pData(rv$current.obj)$Condition==condition1))])
    }
    
    ind
    
  })
  
  GetBorderIndices <- reactive({
    conds <- (pData(rv$current.obj)$Condition)[GetSortingIndices()]
    ## build index for border-formatting
    borders_index <- unlist(lapply(unique(conds), function(x){first(grep(x, conds))}))
    borders_index
  })
  
  
  output$sharedPeptidesInfos <- renderDataTable({
    #req(rv$current.obj)
    req(comp())
    #req(rv$matAdj)
    
    ind <- GetSortingIndices()
    borders_index <- GetBorderIndices()
    
    prev.dataset <- rv$dataset[[names(rv$dataset)[last(grep(pattern='peptide', names(rv$dataset)))]]]
    
    prot <- GetExprsClickedProtein()
    prot.indice <- rownames(prot)
    #print(prot.indice)
    
    data <- getDataForExprs(prev.dataset)
    data <- data[,c(ind, (ind + ncol(data)/2))]
    
    Xspec <- rv$matAdj$matWithUniquePeptides
    Xshared <- rv$matAdj$matWithSharedPeptides
    
    i <- which(colnames(Xspec)==prot.indice)
    specificPeptidesIndices <- which(Xspec[,i]==1)
    allPeptidesIndices <- which(Xshared[,i]==1)
    peptidesIndices <- setdiff(allPeptidesIndices, specificPeptidesIndices)
    data <- data[peptidesIndices,]
    
    dt <- datatable( data,
                     #colnames=NULL,
                     extensions = c('Scroller', 'Buttons'),
                     options = list(initComplete = initComplete(),
                                    dom='Bfrtip',
                                    buttons = list('copy',
                                                   list(
                                                     extend = 'csv',
                                                     filename = 'sharedPeptidesInfos'
                                                   ),'print'),
                                    blengthChange = FALSE,
                                    displayLength = 20,
                                    ordering=FALSE,
                                    server = FALSE,
                                    columnDefs = list(list(targets = c(((ncol(data)/2)+1):(ncol(data))), visible = FALSE))
                     )) %>%
      formatStyle(
        colnames(data)[1:(ncol(data)/2)],
        colnames(data)[((ncol(data)/2)+1):(ncol(data))],
        backgroundColor = styleEqual(c("POV", "MEC"), c(rv$colorsTypeMV$POV, rv$colorsTypeMV$MEC))
      ) %>% 
      formatStyle(borders_index, borderLeft = '3px solid #000000')
    
    dt
  })
  
  output$specificPeptidesInfos <- renderDataTable({
    #req(rv$current.obj)
    req(comp())
    #req(rv$matAdj)
    
    ind <- GetSortingIndices()
    borders_index <- GetBorderIndices()
    
    prev.dataset <- rv$dataset[[names(rv$dataset)[last(grep(pattern='peptide', names(rv$dataset)))]]]
    
    prot <- GetExprsClickedProtein()
    prot.indice <- rownames(prot)
    
    data <- getDataForExprs(prev.dataset)
    data <- data[,c(ind, (ind + ncol(data)/2))]
    
    
    Xspec <- rv$matAdj$matWithUniquePeptides
    
    i <- which(colnames(Xspec)==prot.indice)
    peptidesIndices <- which(Xspec[,i]==1)
    data <- data[peptidesIndices,]
    
    #data <- data[,ind]
    
    dt <- datatable( data, 
                     #colnames=NULL,
                     extensions = c('Scroller', 'Buttons'),
                     options = list(initComplete = initComplete(),
                                    dom='Bfrtip',
                                    buttons = list('copy',
                                                   list(
                                                     extend = 'csv',
                                                     filename = 'specific peptides infos'
                                                   ),'print'),
                                    blengthChange = FALSE,
                                    displayLength = 20,
                                    ordering=FALSE,
                                    server = FALSE,
                                    columnDefs = list(list(targets = c(((ncol(data)/2)+1):(ncol(data))), visible = FALSE))
                     )) %>%
      formatStyle(
        colnames(data)[1:(ncol(data)/2)],
        colnames(data)[((ncol(data)/2)+1):(ncol(data))],
        backgroundColor = styleEqual(c("POV", "MEC"), c(rv$colorsTypeMV$POV, rv$colorsTypeMV$MEC))
      ) %>% 
      formatStyle(borders_index, borderLeft = '3px solid #000000')
    
    dt
  })
  
  
  ##------------------------------------------------------------------------------
  GetExprsClickedProtein <- reactive({
    req(rv$current.obj)
    req(comp())
    req(input$eventPointClicked)
    rv$widgets$hypothesisTest$th_logFC
    rv$widgets$anaDiff$th_pval
    
    rv$resAnaDiff
    
    ind <- GetSortingIndices()
    
    this.index <- as.integer(strsplit(input$eventPointClicked, "_")[[1]][1])
    this.series.name <- strsplit(input$eventPointClicked, "_")[[1]][2]
    
    data <- getDataForExprs(rv$current.obj)
    data <- data[,c(ind, (ind + ncol(data)/2))]
    
    index.g1 <- which((-log10(rv$resAnaDiff$P_Value) >= rv$widgets$anaDiff$th_pval
    ) & (abs(rv$resAnaDiff$logFC) >= as.numeric(rv$widgets$hypothesisTest$th_logFC)))
    
    data.g1 <- data[index.g1,]
    data.g2 <- data[-index.g1,]
    
    switch (this.series.name,
            g1=data <- data.g1[this.index+1,],
            g2 = data <- data.g2[this.index+1,] 
    )
    
    data
  })
  
  
  
  
  ##------------------------------------------------------------------------------
  output$Infos <- renderDataTable({ 
    req(comp())
    
    borders_index <- GetBorderIndices()
    
    data <- GetExprsClickedProtein()
    
    print('################### Dans Infos  #################')
    print(colnames(data))
    
    dt <- datatable( data,
                     extensions = c('Scroller', 'Buttons'),
                     options = list(initComplete = initComplete(),
                                    dom='Bfrtip',
                                    buttons = list('copy',
                                                   list(
                                                     extend = 'csv',
                                                     filename = 'Infos'
                                                   ),'print'),
                                    blengthChange = FALSE,
                                    displayLength = 20,
                                    ordering=FALSE,
                                    header=FALSE,
                                    server = FALSE,
                                    columnDefs = list(list(targets = c(((ncol(data)/2)+1):(ncol(data))), visible = FALSE))
                     )) %>%
      formatStyle(
        colnames(data)[1:(ncol(data)/2)],
        colnames(data)[((ncol(data)/2)+1):(ncol(data))],
        backgroundColor = styleEqual(c("POV", "MEC"), c(rv$colorsTypeMV$POV, rv$colorsTypeMV$MEC))) %>% 
      formatStyle(borders_index, borderLeft = '3px solid #000000')
    
    
    
    dt
  })
  
  ##---------------------------------------------------------------------
  output$volcanoPlot <-  renderHighchart({ 
    rv$widgets$anaDiff$th_pval
    rv$widgets$hypothesisTest$th_logFC
    rv$colorsVolcanoplot
    rv$resAnaDiff
    tooltip()
    
    if ((length(rv$resAnaDiff$logFC) == 0)  ){return()}
    
    if (length(which(is.na(Biobase::exprs(rv$current.obj)))) > 0) { return()}
    
    isolate({
      
      df <- data_frame(x=rv$resAnaDiff$logFC, 
                       y = -log10(rv$resAnaDiff$P_Value),
                       index = 1:nrow(fData(rv$current.obj)))
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
      
      cond <- c(rv$resAnaDiff$condition1, rv$resAnaDiff$condition2)
      rv$tempplot$volcano <-  diffAnaVolcanoplot_rCharts(df,
                                                         threshold_logFC = as.numeric(rv$widgets$hypothesisTest$th_logFC),
                                                         threshold_pVal = as.numeric(rv$widgets$anaDiff$th_pval),
                                                         conditions = cond,
                                                         clickFunction=clickFun,
                                                         palette = rv$colorsVolcanoplot,
                                                         swap = isSwaped()
      )
      
    })
    
    rv$tempplot$volcano
  })
  
  
}




#------------------------------------------------------------
missingValuesPlots <- function(input, output, session, data, title=NULL, palette) {
    
  output$histo_MV <- renderHighchart({
    data()
    rv$PlotParams$paletteConditions
    tmp <- NULL
    #isolate({
    pattern <- paste0(GetCurrentObjName(),".MVplot1")
    tmp <- wrapper.mvHisto_HC(data(),palette=rv$PlotParams$paletteConditions)
    #future(createPNGFromWidget(tmp,pattern))
    #  })
    tmp
  })
    
    
    
    output$histo_MV_per_lines <- renderHighchart({
        data()
      tmp <- NULL
      isolate({
        pattern <- paste0(GetCurrentObjName(),".MVplot2")
       tmp <- 
         wrapper.mvPerLinesHisto_HC(data(), 
                                   c(2:length(colnames(Biobase::pData(rv$current.obj)))))
       #future(createPNGFromWidget(tmp,pattern))
      })
      tmp
    })
    
    
    
    output$histo_MV_per_lines_per_conditions <- renderHighchart({
        data()
      palette()
      tmp <- NULL
      isolate({
        pattern <- paste0(GetCurrentObjName(),".MVplot2")
        tmp <- wrapper.mvPerLinesHistoPerCondition_HC(data(), 
                                                      palette=palette())
        #future(createPNGFromWidget(tmp,pattern))
      })
      tmp
    })
}


#------------------------------------------------------------
moduleDensityplot <- function(input, output, session) {
    
  #outputOptions(output, 'Densityplot', suspendWhenHidden=FALSE)
  
    output$Densityplot <- renderHighchart({
      req(rv$current.obj)
      rv$PlotParams$paletteConditions
      rv$PlotParams$legendForSamples
      tmp <- NULL
      isolate({
      
      withProgress(message = 'Making plot', value = 100, {
        pattern <- paste0(GetCurrentObjName(),".densityplot")
          tmp <- DAPAR::densityPlotD_HC(rv$current.obj, 
                                        rv$PlotParams$legendForSamples,
                                        rv$PlotParams$paletteConditions)
          future(createPNGFromWidget(rv$tempplot$boxplot,pattern))
        })
      })
      tmp
    })
}


#------------------------------------------------------------
moduleBoxplot <- function(input, output, session, params, reset) {
    
  ns <- session$ns
  rv.modboxplot <- reactiveValues(
    var = NULL,
    ind = NULL,
    indices = NULL
  )
  
  rv.modboxplot$var <- callModule(moduleTrackProt, "widgets", params=reactive({params()}), reset=reactive({reset()}))
  
  observeEvent(req(rv.modboxplot$var()),{
    print("In observe rv.modboxplot$var")
    print(rv.modboxplot$var())
    
    
    if (is.null(rv.modboxplot$var()$type)){return(NULL)}
    
    ll <- Biobase::fData(rv$current.obj)[,rv$current.obj@experimentData@other$proteinId]
    switch(rv.modboxplot$var()$type,
           #ProteinList = rv.modboxplot$ind <- rv.modboxplot$var()$list,
           #Random = rv.modboxplot$ind <- rv.modboxplot$var()$rand,
          # Column = rv.modboxplot$ind <- rv.modboxplot$var()$col,
          ProteinList = rv.modboxplot$indices <- rv.modboxplot$var()$list.indices,
          Random = rv.modboxplot$indices <- rv.modboxplot$var()$rand.indices,
          Column = rv.modboxplot$indices <- rv.modboxplot$var()$col.indices
    )
    #if (length(rv.modboxplot$ind)==0){rv.modboxplot$ind <- NULL}
    if (length(rv.modboxplot$indices)==0){rv.modboxplot$indices <- NULL}
  })
  
  
  observeEvent(input$choosePlot, {
    switch(input$choosePlot,
    boxplot={
      shinyjs::hide('viewViolinPlot')
      shinyjs::show('BoxPlot')
      },
    violinplot={
      shinyjs::hide('BoxPlot')
      shinyjs::show('viewViolinPlot')
      }
    )
  })
  
 
  
    output$BoxPlot <- renderHighchart({
      req(rv$current.obj)
      rv$current.obj.name
      rv$PlotParams$paletteConditions
      rv$PlotParams$legendForSamples
      rv.modboxplot$indices
      tmp <- NULL
      isolate({
        ll <- Biobase::fData(rv$current.obj)[,rv$current.obj@experimentData@other$proteinId]
        
        pattern <- paste0(GetCurrentObjName(),".boxplot")
        tmp <- DAPAR::boxPlotD_HC(rv$current.obj, rv$PlotParams$legendForSamples, palette=rv$PlotParams$paletteConditions,
                                  subset.view = rv.modboxplot$indices)
        #future(createPNGFromWidget(tmp,pattern))
          
        
      })
      tmp
    })
    
    
    output$viewViolinPlot<- renderImage({
      req(rv$current.obj)
      rv$PlotParams$legendForSamples
      rv$PlotParams$paletteConditions
      rv.modboxplot$indices
      tmp <- NULL
      isolate({
       
       # A temp file to save the output. It will be deleted after renderImage
        # sends it, because deleteFile=TRUE.
        outfile <- tempfile(fileext='.png')
        print("IN violinPlot")
        print(rv.modboxplot$indices)
        print("END IN violinplot")
        # Generate a png
        # png(outfile, width = 640, height = 480, units = "px")
        png(outfile)
        pattern <- paste0(GetCurrentObjName(),".violinplot")
        tmp <- DAPAR::violinPlotD(rv$current.obj, legend = rv$PlotParams$legendForSamples, 
                                  palette = rv$PlotParams$paletteConditions,
                                  subset.view =  rv.modboxplot$indices)
        #future(createPNGFromWidget(tmp,pattern))
        dev.off()
})
      tmp

    # Return a list
    list(src = outfile,
         alt = "This is alternate text")
}, deleteFile = TRUE)
    
    
    return(reactive({rv.modboxplot$var()}))
}




moduleMVPlots <- function(input, output, session, data, title, palette) {
  
  output$plot_viewNAbyMean <- renderHighchart({
    req(data())
    wrapper.hc_mvTypePlot2(obj=data(), title=title(), palette = palette())
  })
  
  output$plot_showImageNA <- renderImage({
    req(data())
    isolate({
      # A temp file to save the output. It will be deleted after renderImage
      # sends it, because deleteFile=TRUE.
      outfile <- tempfile(fileext='.png')
      
      # Generate a png
      # png(outfile, width = 640, height = 480, units = "px")
      png(outfile)
      wrapper.mvImage(data())
      dev.off()
    })
      # Return a list
      list(src = outfile,
           alt = "This is alternate text")
    }, deleteFile = TRUE)

}


moduleFilterStringbasedOptions <- function(input, output, session) {
    
    output$FilterStringbasedOptions <- renderUI({
        rv$current.obj
        if (is.null(rv$current.obj)){return()}
        
        tagList(
            h4("String based filtering options")
            ,hr()
            ,h4("Filter contaminants"),
            uiOutput("id_Contaminants"),
            uiOutput("choosePrefixContaminants"),
            br(),
            h4("Filter reverse"),
            uiOutput("id_Reverse"),
            uiOutput("choosePrefixReverse"),
            br(),
            #actionButton("resetFilterParamsButton","Reset parameters"),
            actionButton("performFilteringContaminants",
                         "Perform string-based filtering", class = actionBtnClass)
        )
        
        
        
        
        
        
    })
}




moduleStaticDataTable <- function(input, output, session,table2show, withBtns, showRownames=FALSE, dom='Bt', filename='Prostar_export') {
  
  
  proxy = dataTableProxy(session$ns('StaticDataTable'), session)
  
  observe({replaceData(proxy, table2show(), resetPaging = FALSE)  })

  
    output$StaticDataTable <- DT::renderDataTable({
      req(rv$current.obj)
      #table2show
      if (length(table2show())==0){return(NULL)}
      
      isolate({
           DT::datatable(table2show(), 
                         extensions = 'Buttons',
                         #escape = TRUE,
                         # rownames= showRownames,
                         options=list(
                           buttons = list(
                             list(
                               extend = 'csv',
                               filename = filename
                             ),
                             list(
                               extend = 'pdf',
                               filename = filename
                             ),'print'),
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



moduleInsertMarkdown <- function(input, output, session,url){
  
  ns <- session$ns
  output$insertMD <- renderUI({
    print(url)
    tryCatch(
      {
        includeMarkdown(url)
      }
      , warning = function(w) {
        tags$p("URL not found<br>",conditionMessage(w))
        #shinyjs::info(paste("URL not found",":",conditionMessage(w), sep=" "))
      }, error = function(e) {
        shinyjs::info(paste("Error :","in moduleInsertMarkdown",":", conditionMessage(e), sep=" "))
      }, finally = {
        #cleanup-code 
      })
    
  })
  
}
