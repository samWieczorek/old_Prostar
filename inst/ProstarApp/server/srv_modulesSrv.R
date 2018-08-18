
#################### MODULES DEFINITION #################################

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
                      trigger = "click", 
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
  
  output$nbSelectedItems <- renderUI({ 
    
    rv$seuilPVal
    rv$seuilLogFC
    rv$current.obj
    rv$resAnaDiff
    
    
    if(is.null(rv$resAnaDiff$logFC) || is.null(rv$resAnaDiff$P_Value)){return(NULL)}
   if (length(which(is.na(Biobase::exprs(rv$current.obj)))) > 0) {return(NULL)}
    p <- NULL
    p <- rv$resAnaDiff
    upItemsPVal <- NULL
    upItemsLogFC <- NULL
    
    
    upItemsLogFC <- which(abs(p$logFC) >= rv$seuilLogFC)
    upItemsPVal <- which(-log10(p$P_Value) >= rv$seuilPVal)
    
    rv$nbTotalAnaDiff <- nrow(Biobase::exprs(rv$current.obj))
    rv$nbSelectedAnaDiff <- NULL
    t <- NULL
    
    if (!is.null(rv$seuilPVal) && !is.null(rv$seuilLogFC) ) {
      t <- intersect(upItemsPVal, upItemsLogFC)}
    else if (!is.null(rv$seuilPVal) && is.null(rv$seuilLogFC) ) {
      t <- upItemsPVal}
    else if (is.null(rv$seuilPVal) && !is.null(rv$seuilLogFC) ) {
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
  
  
  output$Infos <- renderDataTable({ 
    req(rv$current.obj)
    req(comp())
    req(input$eventPointClicked)
    rv$seuilLogFC
    rv$seuilPVal
    rv$resAnaDiff
    
    condition1 = strsplit(comp(), "_vs_")[[1]][1]
    condition2 = strsplit(comp(), "_vs_")[[1]][2]
    ind <- c( which(pData(rv$current.obj)$Condition==condition1), 
              which(pData(rv$current.obj)$Condition==condition2))
    
    #data <-getDataForExprs()
    
    this.index <- as.integer(strsplit(input$eventPointClicked, "_")[[1]][1])
    this.series.name <- strsplit(input$eventPointClicked, "_")[[1]][2]
    
    data <-getDataForExprs()
    data <- data[,c(ind, (ind + ncol(data)/2))]
    
    index.g1 <- which((-log10(rv$resAnaDiff$P_Value) >= rv$seuilPVal) & (abs(rv$resAnaDiff$logFC) >= rv$seuilLogFC))
    
    data.g1 <- data[index.g1,]
    data.g2 <- data[-index.g1,]
    
    if(this.series.name=='g1') {
      data <- data.g1[this.index+1,]
    } else if(this.series.name=='g2') {
      data <- data.g2[this.index+1,]
    }
    # data <- data[(input$eventPointClicked+1),]
    dt <- datatable( data,
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
        backgroundColor = styleEqual(c("POV", "MEC"), c('lightblue', 'orange'))
      )
    
    
    dt
  })
  
  
  output$volcanoPlot <-  renderHighchart({ 
    rv$seuilPVal
    rv$seuilLogFC
    #req(rv$resAnaDiff)
    #req(rv$current.obj)
    tooltip()
    
    if (is.null(rv$seuilLogFC) || is.na(rv$seuilLogFC) ){return()}
    if ((length(rv$resAnaDiff$logFC) == 0)  ){return()}
    
    if (length(which(is.na(Biobase::exprs(rv$current.obj)))) > 0) { return()}

    isolate({
      print("############### VOLCANOPLOT ####")
    
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
        
          diffAnaVolcanoplot_rCharts(df,
                                   threshold_logFC = rv$seuilLogFC,
                                   threshold_pVal = rv$seuilPVal,
                                   conditions = cond,
                                   clickFunction=clickFun)
        })
    })
  
  
}




#------------------------------------------------------------
missingValuesPlots <- function(input, output, session) {
    
    output$histo_MV <- renderHighchart({
        #histo_MV()
      req(rv$current.obj)
      
      isolate({
        rv$tempplot$mvHisto_HC <- wrapper.mvHisto_HC(rv$current.obj)
        })
      rv$tempplot$mvHisto_HC
    })
    
    output$histo_MV_per_lines <- renderHighchart({
        #histo_MV_per_lines()
      req(rv$current.obj)
      isolate({
        rv$tempplot$mvPerLinesHisto_HC <- 
         wrapper.mvPerLinesHisto_HC(rv$current.obj, 
                                   c(2:length(colnames(Biobase::pData(rv$current.obj)))))
      })
      rv$tempplot$mvPerLinesHisto_HC
    })
    
    output$histo_MV_per_lines_per_conditions <- renderHighchart({
        #histo_MV_per_lines_per_conditions()
      req(rv$current.obj)
      isolate({
        rv$tempplot$histo_missvalues_per_lines_per_conditions <- wrapper.mvPerLinesHistoPerCondition_HC(rv$current.obj, 
                                                                                                        c(2:length(colnames(Biobase::pData(rv$current.obj)))))
      })
      rv$tempplot$histo_missvalues_per_lines_per_conditions
    })
}


#------------------------------------------------------------
moduleDensityplot <- function(input, output, session, lab2Show, whichGroup2Color) {
    
    output$Densityplot <- renderHighchart({
       req(rv$current.obj)
      lab2Show()
      whichGroup2Color()
      
      isolate({
        print("############### Densityplot ####")
      labels <- NULL
      labelsToShow <- NULL
      gToColor <- NULL
      if (is.null(lab2Show())) { 
        labelsToShow <- c(1:nrow(Biobase::pData(rv$current.obj)))
      }
      else { labelsToShow <- lab2Show()}
      
      if (is.null(whichGroup2Color())){
        gToColor <- "Condition"
      }else{gToColor <- whichGroup2Color()}
      if (is.null(whichGroup2Color()) 
          || (whichGroup2Color() == "Condition")){
        labels <- Biobase::pData(rv$current.obj)[,"Condition"]
      }else {
        labels <- apply(pData(rv$current.obj), 1, function(x){paste0(x, collapse='_')})
        names(labels)<- NULL
      }
      withProgress(message = 'Making plot', value = 100, {
   
          rv$tempplot$Density <- wrapper.densityPlotD_HC(rv$current.obj, 
                                                     labels, 
                                                     as.numeric(labelsToShow), 
                                                     gToColor)
        })
      })
      
      #rv$tempplot$Density
     
      
      
    })
}


#------------------------------------------------------------
moduleBoxplot <- function(input, output, session,legendXAxis) {
    
    output$BoxPlot <- renderPlot({
      req(rv$current.obj)
      legendXAxis()
      
      isolate({
        print(legendXAxis())
        print("################## BoxPlot ###############")
       rv$legDS <- legendXAxis()
      
      wrapper.boxPlotD(rv$current.obj,  rv$legDS)
      })
    }, width=600, height=400)
      
  
}



moduleMVPlots <- function(input, output, session, data) {
  
  output$plot_viewNAbyMean <- renderHighchart({
    # viewNAbyMean(data())
    req(data())
    # isolate({
    print("######## output$plot_viewNAbyMean <- renderHighchart ####")
    wrapper.hc_mvTypePlot2(data())
    # })
  })
  
  output$plot_showImageNA <- renderPlot({
    #showImageNA(data())
    req(data())
    isolate({
      print("######## output$plot_showImageNA <- renderPlot ####")
      wrapper.mvImage(data())
    })
  }, width=400, height=600)
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
                         "Perform string-based filtering")
        )
        
        
        
        
        
        
    })
}




moduleDatasetOverview <- function(input, output, session) {
    
    output$DatasetOverviewDT <- renderDataTable({
          req(rv$current.obj)
          
          isolate({
            h3("Quick overview of the dataset")
            df <- data.frame(Definition=c("Number of samples","Number of lines", "% of missing values", "Number of empty lines"),
                             Value=rep(0,4))
            
            NA.count<-apply(data.frame(Biobase::exprs(rv$current.obj)), 
                            2, 
                            function(x) length(which(is.na(data.frame(x))==TRUE)) )
            pourcentage <- 100 * round(sum(NA.count)/
                                         (dim(Biobase::exprs(rv$current.obj))[1]*
                                            dim(Biobase::exprs(rv$current.obj))[2]), digits=4)
            nb.empty.lines <- sum(apply(
              is.na(as.matrix(Biobase::exprs(rv$current.obj))), 1, all))
            
            
            val <- c(ncol((Biobase::exprs(rv$current.obj))),
                     nrow((Biobase::exprs(rv$current.obj))),
                     pourcentage,
                     nb.empty.lines)
            df$Value <- val
            
            DT::datatable(df, 
                          escape = FALSE,
                          rownames= FALSE,
                          option=list(initComplete = initComplete(),
                                dom = 't',
                                autoWidth=TRUE,
                                ordering=F,
                          columnDefs = list(list(width='200px',targets= "_all"))
              )
            )
            
            
          })
    })
}
