







#######################################



output$DS_sidebarPanel_tab <- renderUI({
    input$DS_tabSetPanel
    rv$typeOfDataset
    
    .choices<- NULL
    switch(rv$typeOfDataset,
           protein = {
                      .choices <- list("Quantitative data" = "tabExprs",
                                        "Proteins metadata" = "tabfData",
                                        "Replicate metadata" = "tabpData",
                                        "Dataset history" = "processingData")
                      },
        peptide = {
                      .choices <- list("Quantitative data" = "tabExprs",
                                       "Peptides metadata" = "tabfData",
                                        "Replicate metadata" = "tabpData",
                                        "Dataset history" = "processingData")
                      },
                {
                .choices <- list("Quantitative data" = "tabExprs",
                                "Analyte metadata" = "tabfData",
                                "Replicate metadata" = "tabpData",
                                "Dataset history" = "processingData")
                }
    )
    
    tagList(
                     radioButtons("DS_TabsChoice", "Choose the tab to display",
                                  choices = .choices),
                     br(),
                     checkboxInput("nDigits", 
                                   "Show full length intensities", 
                                   value = FALSE)
    )
    
})


output$DS_sidebarPanel_heatmap <- renderUI({
    
    tagList(
                     h3("Clustering Options"),
                     selectInput("distance","Distance",
                                  choices = G_heatmapDistance_Choices),
                     br(),
                     selectInput("linkage","Linkage for clustering",
                                  choices=G_heatmapLinkage_Choices))
})

#----------------------------------------------
output$tabToShow <- renderUI({
    input$DS_TabsChoice
    rv$current.obj
    rv$indexNA
    if (is.null(input$DS_TabsChoice)) {return(NULL)}
    if (is.null(rv$current.obj)) {return(NULL)}
    
    switch(input$DS_TabsChoice,
          tabExprs = DT::dataTableOutput("table"),
          tabfData = DT::dataTableOutput("viewfData"),
          tabpData = DT::dataTableOutput("viewpData"),
          processingData = {
                      helpText("Previous operations made on the original dataset :")
                      DT::dataTableOutput("viewProcessingData")
                      }
    )
    
})


##' show intensity values of the MSnset object in a table
##' @author Samuel Wieczorek
output$viewExprs <- renderDataTable(
    # rv$current.obj
    # input$nDigits
    # if (is.null(rv$current.obj)) {return(NULL)}
    # if (input$nDigits == T){nDigits = 1e100}else {nDigits = 3}
    # 
    # df <- cbind(ID = rownames(Biobase::fData(rv$current.obj)),
    #               round(Biobase::exprs(rv$current.obj), 
    #               digits=nDigits))
    # 
    # 
    # test.table <- data.frame(lapply(1:8, function(x) {1:1000}))
    # test.table[c(2,3,7), c(2,7,6)] <- NA
    # id <- which(is.na(test.table))
    # colonnes <- trunc(id / nrow(test.table))+1
    # lignes <- id %% nrow(test.table)
    # formattable(test.table, list(area(col = colonnes, row = lignes) ~ color_tile("red", "lightblue")))
    # 
    # id <- which(is.na(exprs(Exp1_R25_prot)))
    #colonnes <- trunc(id / nrow(exprs(Exp1_R25_prot)))+1
    #lignes <- id %% nrow(exprs(Exp1_R25_prot))
    #formattable(as.data.frame(exprs(Exp1_R25_prot)), list(area(col = colonnes, row = lignes) ~ color_tile("red", "lightblue")))
    
    
    
    #id <- which(is.na(exprs(Exp1_R25_prot)))
    
    test.table,
    options = list(
        displayLength = 3,
        drawCallback=JS(
            paste("function(row, data) {",
                  paste(sapply(1:ncol(test.table),function(i)
                     paste( "$(this.api().cell(",
                        id %% nrow(test.table)-1,",",
                        trunc(id / nrow(test.table))+1,
                        ").node()).css({'background-color': 'lightblue'});")
                  ),collapse = "\n"),"}" )
        ), 
        server = TRUE)
    
    
    # id <- which(is.na(df))
    # datatable(df,
    #               options=list(drawCallback=JS(
    #               paste("function(row, data,index) {",
    #               paste(sapply(1:ncol(df),function(i) 
    #              {paste( "$(this.api().cell(",id %% nrow(df)-1,",",trunc(id / nrow(df))+1,").node()).css({'background-color': 'lightblue'});")}
    #              #{paste( "$(this.api().cell(index,",trunc(i / nrow(data))+1,").node()).css({'background-color': 'lightblue'});")}
    #               
    #              ),collapse = "\n"),"}" ) )
    #     )
    #     ) 
    
)




##' show pData of the MSnset object
##' @author Samuel Wieczorek
output$viewpData <- DT::renderDataTable({
    rv$current.obj
    if (is.null(rv$current.obj)) {return(NULL)}
    
    result = tryCatch(
        {
            as.data.frame(Biobase::pData(rv$current.obj))
        }
        , warning = function(w) {
            shinyjs::info(conditionMessage(w))
        }, error = function(e) {
            shinyjs::info(paste(match.call()[[1]],":",
                                conditionMessage(e), 
                                sep=" "))
        }, finally = {
            #cleanup-code 
        })
    
    
    
},
option=list(pageLength=DT_pagelength,
            orderClasses = TRUE,
            autoWidth=FALSE,
            columnDefs = list(
                list(columns.width=c("60px"),
                     columnDefs.targets= c(list(0),list(1),list(2)))))
)

##' show fData of the MSnset object in a table
##' @author Samuel Wieczorek
output$viewfData <- DT::renderDataTable({
    rv$current.obj
    if (is.null(rv$current.obj)) {return(NULL)}
    
    
    if ('Significant' %in% colnames(Biobase::fData(rv$current.obj))){
        dat <- DT::datatable(as.data.frame(Biobase::fData(rv$current.obj)),
                        options=list(pageLength=DT_pagelength,
                                    orderClasses = TRUE,
                                    autoWidth=FALSE,
                                    columns.searchable=F,
                            columnDefs = list(list(columns.width=c("60px"),
                        columnDefs.targets=c(list(0),list(1),list(2)))))) %>%
            formatStyle(columns = 'Significant',
                        target = 'row',
                        background = styleEqual(1, 'lightblue'))
    } else {
        dat <- DT::datatable(as.data.frame(Biobase::fData(rv$current.obj)),
                             options=list(pageLength=DT_pagelength,
                            orderClasses = TRUE,
                            autoWidth=FALSE,
                            columns.searchable=F,
                            columnDefs = list(list(columns.width=c("60px"),
                            columnDefs.targets=c(list(0),list(1),list(2))))))
    }
    
    return(dat)
}

#              
#             ))
)



##' Visualisation of missing values table
##' @author Samuel Wieczorek
output$viewExprsMissValues <- DT::renderDataTable({
    rv$current.obj
    if (is.null(rv$current.obj)) {return(NULL)}
    result = tryCatch(
        {
            as.data.frame(cbind(ID = rownames(Biobase::fData(rv$current.obj)),
                                Biobase::exprs(rv$current.obj)))
        }
        , warning = function(w) {
            shinyjs::info(conditionMessage(w))
        }, error = function(e) {
            shinyjs::info(paste(match.call()[[1]],":",
                                conditionMessage(e), 
                                sep=" "))
        }, finally = {
            #cleanup-code 
        })
    
    
},

option=list(orderClasses = TRUE,
            autoWidth=FALSE,
            columns.searchable=F,
            pageLength = DT_pagelength,
            columnDefs = list(list(columns.width=c("60px"),
                            columnDefs.targets=c(list(0),list(1),list(2)))))
)



##' Quick overview of the MSnbase object
##' @author Florence Combes
output$overview <- renderUI({
    rv$current.obj
    rv$typeOfDataset
    if (is.null(rv$current.obj)) {return(NULL)    }
    
    isolate({
        
        
        result = tryCatch(
            {
                
                rv$current.obj
                rv$typeOfDataset
                NA.count <- 
                    apply(data.frame(Biobase::exprs(rv$current.obj)), 
                          2, 
                        function(x) length(which(is.na(data.frame(x))==TRUE)) )
                rv$pourcentageNA <- 100 * round(sum(NA.count)/
                                    (dim(Biobase::exprs(rv$current.obj))[1]*
                                    dim(Biobase::exprs(rv$current.obj))[2]), 
                                    digits=4)
                d <- "lines"
                switch(rv$typeOfDataset,
                       peptide = {d <- "peptides"},
                       protein = {d <- "proteins"},
                       {d <- "analytes"}
                       )
                
                rv$nb.empty.lines <- sum(apply(
                    is.na(as.matrix(exprs(rv$current.obj))), 1, all))
                tags$ul(
                    tags$li(paste("There are", 
                                  dim(Biobase::exprs(rv$current.obj))[2], 
                                  " samples in your data.", sep=" ")),
                    
                    tags$li(paste("There are", 
                                  dim(Biobase::exprs(rv$current.obj))[1], d,
                                  " in your data.", sep=" ")), 
                    tags$li(paste("Percentage of missing values:",
                                  rv$pourcentageNA , "%", sep=" ")),
                    tags$li(paste("Number of lines with only NA values =",
                                  rv$nb.empty.lines , sep=" "))
                )
                
               # initRmd()
            }
            , warning = function(w) {
                shinyjs::info(conditionMessage(w))
            }, error = function(e) {
                shinyjs::info(paste(match.call()[[1]],":",
                                    conditionMessage(e), 
                                    sep=" "))
            }, finally = {
                #cleanup-code 
            })
        
        
    })
})



histo_missvalues_per_lines_Image <- reactive({
    rv$current.obj
    if (is.null(rv$current.obj)) {return(NULL)}
    result = tryCatch(
        {
            wrapper.mvPerLinesHisto_HC(rv$current.obj)
            
        }
        , warning = function(w) {
            shinyjs::info(conditionMessage(w))
        }, error = function(e) {
            shinyjs::info(paste(match.call()[[1]],":",
                                conditionMessage(e), 
                                sep=" "))
        }, finally = {
            #cleanup-code 
        })
})




histo_missvalues_per_lines_per_conditions_Image <- reactive({
    rv$current.obj
    if (is.null(rv$current.obj)) {return(NULL)}
    result = tryCatch(
        {
            wrapper.mvPerLinesHistoPerCondition_HC(rv$current.obj)
            
            
            
        }
        , warning = function(w) {
            shinyjs::info(conditionMessage(w))
        }, error = function(e) {
            shinyjs::info(paste(match.call()[[1]],":",
                                conditionMessage(e), 
                                sep=" "))
        }, finally = {
            #cleanup-code 
        })
    
    
})

   


histo_missvalues_per_lines_DS <- reactive({
    rv$current.obj
    if (is.null(rv$current.obj)) {return(NULL)}
    
    result = tryCatch(
        {
            #wrapper.mvPerLinesHisto(rv$current.obj, 
            #            c(2:length(colnames(Biobase::pData(rv$current.obj)))))
            
            
            rv$tempplot$mvPerLinesHisto_HC <- 
              wrapper.mvPerLinesHisto_HC(rv$current.obj, 
                                       c(2:length(colnames(Biobase::pData(rv$current.obj)))))
            rv$tempplot$mvPerLinesHisto_HC
        }
        , warning = function(w) {
            shinyjs::info(conditionMessage(w))
        }, error = function(e) {
            shinyjs::info(paste(match.call()[[1]],":",
                                conditionMessage(e),
                                sep=" "))
        }, finally = {
            #cleanup-code
        })
    
    
    
})



histo_missvalues_per_lines_per_conditions_DS <- reactive({
    rv$current.obj
    if (is.null(rv$current.obj)) {return(NULL)}
    
    result = tryCatch(
        {
            rv$tempplot$histo_missvalues_per_lines_per_conditions   <- wrapper.mvPerLinesHistoPerCondition_HC(rv$current.obj, 
                                                   c(2:length(colnames(Biobase::pData(rv$current.obj)))))
            rv$tempplot$histo_missvalues_per_lines_per_conditions 
        }
        , warning = function(w) {
            shinyjs::info(conditionMessage(w))
        }, error = function(e) {
            shinyjs::info(paste(match.call()[[1]],":",
                                conditionMessage(e), 
                                sep=" "))
        }, finally = {
            #cleanup-code 
        })
})















boxPlot <- reactive({
    rv$current.obj
    input$legendXAxis_DS
    if (is.null(rv$current.obj)) {return(NULL)}
    
    if (!is.null(input$legendXAxis_DS)){
        rv$legDS <- input$legendXAxis_DS}
    
    result = tryCatch(
        {
          wrapper.boxPlotD(rv$current.obj,  rv$legDS)
        }
        , warning = function(w) {
            shinyjs::info(conditionMessage(w))
        }, error = function(e) {
            shinyjs::info(paste(match.call()[[1]],":",
                                conditionMessage(e), 
                                sep=" "))
        }, finally = {
            #cleanup-code 
        })
    
    
})






violinPlot2 <- reactive({
    rv$current.obj
    input$legendXAxisViolin_DS
    if (is.null(rv$current.obj)) {return(NULL)}
    if (is.null(input$legendXAxisViolin_DS)) {return(NULL)}
    
    
    if (!is.null(input$legendXAxisViolin_DS)){
      rv$PlotParams$legDS_Violinplot <- input$legendXAxisViolin_DS}
    
    result = tryCatch(
        {
            if (is.null(rv$PlotParams$legDS_Violinplot)) {
                wrapper.violinPlotD(rv$current.obj)
                }  else {
                    wrapper.violinPlotD(rv$current.obj,  rv$PlotParams$legDS_Violinplot)
                }
        }
        , warning = function(w) {
            shinyjs::info(conditionMessage(w))
        }, error = function(e) {
            shinyjs::info(paste(match.call()[[1]],":",
                                conditionMessage(e), 
                                sep=" "))
        }, finally = {
            #cleanup-code 
        })
    
})




Densityplot_DS <- reactive({
    rv$current.obj
    input$lab2Show_DS
    input$whichGroup2Color_DS
    if (is.null(rv$current.obj)) {return(NULL)}
    if (is.null(input$lab2Show_DS)) {return(NULL)}
    if (is.null(input$whichGroup2Color_DS)) {return(NULL)}
    
    labels_DS <- NULL
    labelsToShow_DS <- NULL
    gToColor_DS <- NULL
    if (is.null(input$lab2Show_DS)) { 
        labelsToShow_DS <- c(1:nrow(Biobase::pData(rv$current.obj)))
    }
    else { labelsToShow_DS <- input$lab2Show_DS}
    
    if (is.null(input$whichGroup2Color_DS)){
        gToColor_DS <- "Condition"
    }else{gToColor_DS <- input$whichGroup2Color_DS}
    
    if (is.null(input$whichGroup2Color_DS) 
        || (input$whichGroup2Color_DS == "Condition")){
        labels_DS <- Biobase::pData(rv$current.obj)[,"Label"]
    }else {
        labels_DS <- paste(Biobase::pData(rv$current.obj)[,"Label"],
                           Biobase::pData(rv$current.obj)[,"Bio.Rep"],
                           Biobase::pData(rv$current.obj)[,"Tech.Rep"],
                           Biobase::pData(rv$current.obj)[,"Analyt.Rep"],
                           sep= "_")
    }
    
    #result = tryCatch(
    #    {
    rv$tempplot$Density <- wrapper.densityPlotD_HC(rv$current.obj, 
                                    labels_DS, 
                                    as.numeric(labelsToShow_DS), 
                                    gToColor_DS)
            rv$tempplot$Density
        # }
        # , warning = function(w) {
        #     shinyjs::info(conditionMessage(w))
        # }, error = function(e) {
        #     shinyjs::info(paste(match.call()[[1]],":",
        #                         conditionMessage(e), 
        #                         sep=" "))
        # }, finally = {
        #     #cleanup-code 
        # })
    
    
    
})



viewDistCV <- reactive({
    
    rv$current.obj
    
    if (is.null(rv$current.obj)) {return(NULL)}
    result = tryCatch(
        {
            rv$tempplot$varDist <- wrapper.CVDistD_HC(rv$current.obj)
            rv$tempplot$varDist
        }
        , warning = function(w) {
            shinyjs::info(conditionMessage(w))
        }, error = function(e) {
            shinyjs::info(paste(match.call()[[1]],":",
                                conditionMessage(e), 
                                sep=" "))
        }, finally = {
            #cleanup-code 
        })
    
    
})



corrMatrix <- reactive({
    
    rv$current.obj
    input$expGradientRate
    if (is.null(rv$current.obj)) {return(NULL)}
    
    gradient <- NULL
    if (is.null(input$expGradientRate)){gradient <- defaultGradientRate}
    else{
        gradient <- input$expGradientRate}
    
    result = tryCatch(
        {
            rv$tempplot$corrMatrix <- wrapper.corrMatrixD_HC(rv$current.obj,gradient)
            rv$tempplot$corrMatrix
            
        }
        , warning = function(w) {
            shinyjs::info(conditionMessage(w))
        }, error = function(e) {
            shinyjs::info(paste(match.call()[[1]],":",
                                conditionMessage(e), 
                                sep=" "))
        }, finally = {
            #cleanup-code 
        })
    
})



heatmap <- reactive({
    
    rv$current.obj
    input$linkage
    input$distance
    if (is.null(rv$current.obj)) {return(NULL)}
    if (!is.null(input$linkage) && !is.null(input$distance)
        #&& (getNumberOfEmptyLines(Biobase::exprs(rv$current.obj)) == 0)
    ) {
        
        result = tryCatch(
            {
                rv$PlotParams$HeatmapLinkage <- input$linkage
      rv$PlotParams$HeatmapDistance <- input$distance
      
                wrapper.heatmapD(rv$current.obj,
                                 rv$PlotParams$HeatmapDistance, 
                                 rv$PlotParams$HeatmapLinkage,
                                 TRUE)

            }
            , warning = function(w) {
                shinyjs::info(conditionMessage(w))
            }, error = function(e) {
                shinyjs::info(paste(match.call()[[1]],":",
                                    conditionMessage(e), 
                                    sep=" "))
            }, finally = {
                #cleanup-code 
            })
        
        
    }
    
})








output$DS_PlotHeatmap <- renderUI({
    rv$current.obj
    
    if (is.null(rv$current.obj)) {return(plot.new())}
    # if (getNumberOfEmptyLines(Biobase::exprs(rv$current.obj)) != 0) {return (NULL)}
    
    tagList(
        busyIndicator(WaitMsgPlot,wait = 0),
        plotOutput("heatmap", width = "900px", height = "600px")
    )
})






output$DS_sidebarPanel_Densityplot <- renderUI({
    conditionalPanel(condition= "true",
                     uiOutput("nGroup_DS"),
                     br(),
                     uiOutput("nShow_DS"))
    
})





output$DS_sidebarPanel_Boxplot <- renderUI({
    uiOutput("ChooseLegendForAxis_DS")
})

output$DS_sidebarPanel_Violinplot <- renderUI({
    uiOutput("ChooseLegendForAxisViolin_DS")
    
})






getData <- reactive({
    input$nDigits
    rv$current$obj
    
    if (input$nDigits == TRUE){nDigits = 1e100} else {nDigits = 3}
    
    # test.table <- cbind(ID = rownames(Biobase::fData(rv$current.obj)),
    #                     round(Biobase::exprs(rv$current.obj), 
    #                     digits=nDigits))
    test.table <- round(Biobase::exprs(rv$current.obj),digits=nDigits)
    test.table
})



data <- eventReactive(rv$current$obj, {
    input$nDigits
    rv$current$obj
    
    if (input$nDigits == TRUE){nDigits = 1e100} else {nDigits = 3}
    
    # test.table <- cbind(ID = rownames(Biobase::fData(rv$current.obj)),
    #                     round(Biobase::exprs(rv$current.obj), 
    #                     digits=nDigits))
    test.table <- round(Biobase::exprs(rv$current.obj),digits=nDigits)
    test.table
}, ignoreNULL = FALSE)



#################
output$table <- renderDataTable(
    
    getData(),
    options = list(
        displayLength = 20
        
        # ,drawCallback=JS(
        # paste("function(row, data) {",
        #       paste(sapply(1:ncol(getData()),function(i)
        #           paste( "$(this.api().cell(",indewq() %% nrow(data())-1,",",trunc(indewq() / nrow(getData()))+1,").node()).css({'background-color': 'lightblue'});")
        #       ),collapse = "\n"),"}" ))
        
    ),server = TRUE)







output$overviewNewData <- renderUI({
    rv$current.obj
    if (is.null(rv$current.obj)) {return(NULL)}
    
    isolate({
        
        verb <- NULL
        plurial <- NULL
        
        
        if( dim(Biobase::exprs(rv$current.obj))[2] > 1){
            verb <- "are"
            plurial <- "s"} else {
                verb <- "is"
                plurial <- ""}
        
        
        
        txt1 <- paste("There ", verb, " " ,
                      dim(Biobase::exprs(rv$current.obj))[2],
                      " sample", plurial, " in your data.", sep="")
        
        if( dim(Biobase::exprs(rv$current.obj))[2] > 1){
            verb <- "are"
            plurial <- "s"} else {
                verb <- "is"
                plurial <- ""}
        txt2 <- paste("There ", verb, " ",
                      dim(Biobase::exprs(rv$current.obj))[1], 
                      " line", plurial, " in your data.", sep="")
        
        NA.count<-apply(data.frame(Biobase::exprs(rv$current.obj)), 
                        2, 
                        function(x) length(which(is.na(data.frame(x))==TRUE)) )
        pourcentage <- 100 * round(sum(NA.count)/
                                (dim(Biobase::exprs(rv$current.obj))[1]*
                            dim(Biobase::exprs(rv$current.obj))[2]), digits=4)
        txt3 <- paste("Percentage of missing values:",pourcentage , "%.")
        
        nb.empty.lines <- sum(apply(
            is.na(as.matrix(Biobase::exprs(rv$current.obj))), 1, all))
        txt4 <- NULL
        if (nb.empty.lines > 0){
            if( nb.empty.lines > 1){
                verb <- "are"
                plurial <- "s"} else {
                    verb <- "is"
                    plurial <- ""}
            
            
            txt4 <- paste("There ", verb, " ",
                          nb.empty.lines ," line",
                          plurial," with only NA values."
                          ,sep="")
        }
        
        tags$ul(
            tags$li(txt1), 
            tags$li(txt2), 
            tags$li(txt3),
            if (!is.null(txt4)){tags$li(txt4)}
        )
        
    })
})


# options for vioplot
output$ChooseLegendForAxisViolin_DS <- renderUI({
  rv$current.obj
  if (is.null(rv$current.obj)){return(NULL)}
  isolate(rv$current.obj)
  .names <- colnames(Biobase::pData(rv$current.obj))[-1]
  tags$head(tags$link(rel="stylesheet", type="text/css", 
                      href="css/overrides.css"))
  
  checkboxGroupInput("legendXAxisViolin_DS",
                     label = "Choose data to show in legend",
                     choices = .names,
                     selected = .names[1])
})


# options for boxplot
#------------------------------------------------------
output$ChooseLegendForAxis_DS <- renderUI({
    rv$current.obj
    if (is.null(rv$current.obj)){return(NULL)}
    isolate(rv$current.obj)
    .names <- colnames(Biobase::pData(rv$current.obj))[-1]
    tags$head(tags$link(rel="stylesheet", type="text/css", 
                        href="css/overrides.css"))
    
    checkboxGroupInput("legendXAxis_DS",
                       label = "Choose data to show in legend",
                       choices = .names,
                       selected = .names[1])
})



##' Select the labels to be highlighted in densityplots
##' @author Samuel Wieczorek
output$nGroup_DS <- renderUI({
    rv$current.obj
    if (is.null(rv$current.obj) ) {return(NULL) }
    
    radioButtons("whichGroup2Color_DS",
                 "Color lines",
                 choices=list("By condition" = "Condition",
                              "By replicate" = "Replicate"))
    
})




##' Select the labels to show in densityplots
##' @author Samuel Wieczorek
output$nShow_DS <- renderUI({
    rv$current.obj
    if (is.null(rv$current.obj) ) {return(NULL) }
    
    isolate({
        rv$current.obj
        labs <- paste(Biobase::pData(rv$current.obj)[,"Label"],
                      Biobase::pData(rv$current.obj)[,"Bio.Rep"],
                      Biobase::pData(rv$current.obj)[,"Tech.Rep"],
                      Biobase::pData(rv$current.obj)[,"Analyt.Rep"],
                      sep= "_")
        
        label.names <- setNames(as.list(c(1:length(labs))),labs)
        
        
        checkboxGroupInput("lab2Show_DS"
                           , label = "Hide/show replicates"
                           , choices = label.names
                           , selected = unlist(label.names))
    })
})





histoMV_Image_DS <- reactive({
    rv$current.obj
    if (is.null(rv$current.obj)) {return(NULL)}
    
    result = tryCatch(
        {
            rv$tempplot$mvHisto_HC <- wrapper.mvHisto_HC(rv$current.obj)
            rv$tempplot$mvHisto_HC
        }
        , warning = function(w) {
            shinyjs::info(conditionMessage(w))
        }, error = function(e) {
            shinyjs::info(paste(match.call()[[1]],":",conditionMessage(e), sep=" "))
        }, finally = {
            #cleanup-code 
        })
    
    
})



output$histoMV_Image_DS <- renderHighchart({
    histoMV_Image_DS()
})



##' boxplot of intensities in current.obj
##' @author Samuel Wieczorek
output$viewBoxPlot_DS <- renderPlot({
    boxPlot()
    
})


output$viewViolinPlot_DS <- renderPlot({
    violinPlot2()
}) 



##' Distribution of intensities in current.obj
##' @author Samuel Wieczorek
output$viewDensityplot_DS <- renderHighchart({
    Densityplot_DS()
    
})



##' distribution of missing values in current.obj
##' @author Samuel Wieczorek
output$histo_missvalues_per_lines_Image <- renderHighchart({
    histo_missvalues_per_lines_Image()
})


##' distribution of missing values in current.obj
##' @author Samuel Wieczorek
output$histo_missvalues_per_lines_per_conditions_Image <- renderHighchart({
    histo_missvalues_per_lines_per_conditions_Image()
}) 



##' distribution of missing values in current.obj
##' @author Samuel Wieczorek
output$histo_missvalues_per_lines_per_conditions_DS <- renderHighchart({
    histo_missvalues_per_lines_per_conditions_DS()
})



##' distribution of missing values in current.obj
##' @author Samuel Wieczorek
output$histo_missvalues_per_lines_DS <- renderHighchart({
    histo_missvalues_per_lines_DS()
    
})

##' Draw a heatmap of current data
##' 
##' @author Samuel Wieczorek
output$heatmap <- renderPlot({
    heatmap()
})




##' distribution of the variance in current.obj
##' 
##' @author Samuel Wieczorek
output$viewDistCV <- renderHighchart({
    viewDistCV()
    
})



##' Draw a correlation matrix of intensities in current.obj
##' 
##' @author Samuel Wieczorek
output$corrMatrix <- renderHighchart({
    corrMatrix()
}) 










