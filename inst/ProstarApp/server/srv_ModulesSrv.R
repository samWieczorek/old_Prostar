
#################### MODULES DEFINITION #################################
module_Not_a_numeric <- function(input, output, session, n){
  
  output$msg_not_numeric <- renderUI({
    n()
    if (is.na(as.numeric(n()))){
      tags$p("Please choose a number")
    }
  })
}



moduleDesignExample <- function(input, output, session, n){
  
  
  output$nlevelsExample <- renderRHandsontable({
   
    
    if (n == 2){
                df <- data.frame(Sample.name= paste0("Sample ",as.character(1:14)),
                     Condition = c(rep( "A", 4), rep("B", 4), rep("C", 6)),
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
                style="display:inline-block; vertical-align: middle;",
                if (regexpr("Subsets", data()$title)[1] ==1){
                    data()$title}
                else
                {
                  data()$title
                  }
            ),
            div(
            # edit2
            style="display:inline-block; vertical-align: middle;",
            if (regexpr("Subsets", data()$title)[1] ==1){
                tags$button(id=ns("q1"), tags$sup("[?]"), class="Prostar_tooltip_white")
                } else {
                tags$button(id=ns("q1"), tags$sup("[?]"), class="Prostar_tooltip")
                    },
            bsPopover(id = ns("q1"), title = "",
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

moduleVolcanoplot <- function(input, output, session,comp, tooltip){
  
  ns <- session$ns
  
 
  output$quantiDT <- renderUI({
    req(input$eventPointClicked)
    
    if (is.null(rv$matAdj)){
      bsCollapse(id = ns("collapseVolcanoInfos"), open = "Protein",multiple = TRUE,
                 bsCollapsePanel("Protein", DT::dataTableOutput(ns("Infos")),style = "info"))
    } else {
      bsCollapse(id = ns("collapseVolcanoInfos"), open = "Protein",multiple = TRUE,
               bsCollapsePanel("Protein", DT::dataTableOutput(ns("Infos")),style = "info"),
               bsCollapsePanel("Specific peptides", DT::dataTableOutput(ns("specificPeptidesInfos")), style = "primary"),
               bsCollapsePanel("Shared peptides", DT::dataTableOutput(ns("sharedPeptidesInfos")), style = "primary"))
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
  
  
  
  
  output$sharedPeptidesInfos <- renderDataTable({
    req(rv$current.obj)
    req(comp())
    req(rv$matAdj)
    
    
    prev.dataset <- rv$dataset[[names(rv$dataset)[last(grep(pattern='peptide', names(rv$dataset)))]]]
    
    prot <- GetExprsClickedProtein()
    prot.indice <- rownames(prot)
    print(prot.indice)
    
    data <- getDataForExprs(prev.dataset)
    Xspec <- rv$matAdj$matWithUniquePeptides
    Xshared <- rv$matAdj$matWithSharedPeptides
    
    i <- which(colnames(Xspec)==prot.indice)
    specificPeptidesIndices <- which(Xspec[,i]==1)
    allPeptidesIndices <- which(Xshared[,i]==1)
    peptidesIndices <- setdiff(allPeptidesIndices, specificPeptidesIndices)
    data <- data[peptidesIndices,]
    
    dt <- datatable( data,colnames=NULL,
                     options = list(initComplete = initComplete(),
                                    dom='t',
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
      )
    
    dt
  })
  
  
  output$specificPeptidesInfos <- renderDataTable({
    req(rv$current.obj)
    req(comp())
    req(rv$matAdj)
    
    
    prev.dataset <- rv$dataset[[names(rv$dataset)[last(grep(pattern='peptide', names(rv$dataset)))]]]
    
    prot <- GetExprsClickedProtein()
    prot.indice <- rownames(prot)
    print(prot.indice)
    
    data <- getDataForExprs(prev.dataset)
    Xspec <- rv$matAdj$matWithUniquePeptides
    
    i <- which(colnames(Xspec)==prot.indice)
    print(i)
    peptidesIndices <- which(Xspec[,i]==1)
    data <- data[peptidesIndices,]
    
    dt <- datatable( data, colnames=NULL,
                     options = list(initComplete = initComplete(),
                                    dom='t',
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
      )
    
    dt
  })
  
  
  GetExprsClickedProtein <- reactive({
    req(rv$current.obj)
    req(comp())
    req(input$eventPointClicked)
    rv$widgets$hypothesisTest$th_logFC
    rv$widgets$anaDiff$th_pval
    
    rv$resAnaDiff
    
    condition1 = strsplit(comp(), "_vs_")[[1]][1]
    condition2 = strsplit(comp(), "_vs_")[[1]][2]
     

    ind <- c( which(pData(rv$current.obj)$Condition==condition1), 
              which(pData(rv$current.obj)$Condition==condition2))
    
     
    this.index <- as.integer(strsplit(input$eventPointClicked, "_")[[1]][1])
    this.series.name <- strsplit(input$eventPointClicked, "_")[[1]][2]
    
    data <-getDataForExprs(rv$current.obj)
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
  
  
  output$Infos <- renderDataTable({ 
    

    data <- GetExprsClickedProtein()
    
    dt <- datatable( data,
                     options = list(initComplete = initComplete(),
                                    dom='t',
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
        backgroundColor = styleEqual(c("POV", "MEC"), c(rv$colorsTypeMV$POV, rv$colorsTypeMV$MEC))
      )
    
    
    dt
  })
  
  
  output$volcanoPlot <-  renderHighchart({ 
    rv$widgets$anaDiff$th_pval
    rv$widgets$hypothesisTest$th_logFC
    rv$colorsVolcanoplot
    tooltip()
    
    print(rv$widgets$anaDiff$th_pval)
    print(rv$widgets$hypothesisTest$th_logFC)
    print(str(rv$resAnaDiff))
    
    #if (is.null(rv$widgets$hypothesisTest$th_logFC) || is.na(rv$widgets$hypothesisTest$th_logFC) ){return()}
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
                                   rv$colorsVolcanoplot)
        
        })
    
    rv$tempplot$volcano
    })
  
  
}




#------------------------------------------------------------
missingValuesPlots <- function(input, output, session) {
    
    output$histo_MV <- renderHighchart({
        #histo_MV()
      req(rv$current.obj)
      rv$PlotParams$paletteConditions
      tmp <- NULL
      isolate({
        pattern <- paste0(GetCurrentObjName(),".MVplot1")
        tmp <- wrapper.mvHisto_HC(rv$current.obj,palette=rv$PlotParams$paletteConditions)
        #future(createPNGFromWidget(tmp,pattern))
        })
      tmp
    })
    
    
    
    output$histo_MV_per_lines <- renderHighchart({
        #histo_MV_per_lines()
      req(rv$current.obj)
      tmp <- NULL
      isolate({
        pattern <- paste0(GetCurrentObjName(),".MVplot2")
       tmp <- 
         wrapper.mvPerLinesHisto_HC(rv$current.obj, 
                                   c(2:length(colnames(Biobase::pData(rv$current.obj)))))
       #future(createPNGFromWidget(tmp,pattern))
      })
      tmp
    })
    
    
    
    output$histo_MV_per_lines_per_conditions <- renderHighchart({
        #histo_MV_per_lines_per_conditions()
      req(rv$current.obj)
      tmp <- NULL
      isolate({
        pattern <- paste0(GetCurrentObjName(),".MVplot2")
        tmp <- wrapper.mvPerLinesHistoPerCondition_HC(rv$current.obj, 
                                                      c(2:length(colnames(Biobase::pData(rv$current.obj))))
                                                      ,palette=rv$PlotParams$paletteConditions)
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
         # future(createPNGFromWidget(rv$tempplot$boxplot,pattern))
        })
      })
      tmp
    })
}


#------------------------------------------------------------
moduleBoxplot <- function(input, output, session) {
    
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
      tmp <- NULL
      isolate({
        pattern <- paste0(GetCurrentObjName(),".boxplot")
        tmp <- DAPAR::boxPlotD_HC(rv$current.obj, rv$PlotParams$legendForSamples, palette=rv$PlotParams$paletteConditions)
        #future(createPNGFromWidget(tmp,pattern))
          
        
      })
      tmp
    })
    
    
    output$viewViolinPlot <- renderPlot({
      
      req(rv$current.obj)
      rv$PlotParams$legendForSamples
      rv$PlotParams$paletteConditions
      tmp <- NULL
      isolate({
        pattern <- paste0(GetCurrentObjName(),".violinplot")
           tmp <- DAPAR::violinPlotD(rv$current.obj, rv$PlotParams$legendForSamples, palette=rv$PlotParams$paletteConditions)
           #future(createPNGFromWidget(tmp,pattern))
      })
      tmp
      }) 
  
}



moduleMVPlots <- function(input, output, session, data, title) {
  
  output$plot_viewNAbyMean <- renderHighchart({
    req(data())
    req(title())
    wrapper.hc_mvTypePlot2(data(), title = title())
  })
  
  output$plot_showImageNA <- renderPlot({
    req(data())
    isolate({
      wrapper.mvImage(data())
    })
  })
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




moduleStaticDataTable <- function(input, output, session,table2show, withBtns, showRownames=FALSE, dom='t') {
    
  
  proxy = dataTableProxy(session$ns('StaticDataTable'), session)
  
  observe({replaceData(proxy, table2show(), resetPaging = FALSE)  })

  
    output$StaticDataTable <- renderDT({
      req(rv$current.obj)
      #table2show
      if (length(table2show())==0){return(NULL)}
      
      isolate({
           DT::datatable(table2show(), 
                          escape = FALSE,
                          rownames= showRownames,
                          option=list(initComplete = initComplete(),
                                dom = dom,
                                server = FALSE,
                                autoWidth=TRUE,
                          columnDefs = list(list(width='150px',targets= "_all")),
                          ordering = FALSE
              )
            )
      })

    })
}



moduleInsertMarkdown <- function(input, output, session,url){
  
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
        shinyjs::info(paste("Error :","CreateMSnSet",":", conditionMessage(e), sep=" "))
      }, finally = {
        #cleanup-code 
      })
    
  })
  
}
