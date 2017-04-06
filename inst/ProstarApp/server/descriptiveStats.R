output$DS_sidebarPanel_tab <- renderUI({
    input$DS_tabSetPanel
    rv$typeOfDataset
    
    .choices<- NULL
    if (rv$typeOfDataset == "protein") {
        .choices <- list("Quantitative data" = "tabExprs",
                         "Proteins metadata" = "tabfData",
                         "Replicate metadata" = "tabpData",
                         "Dataset history" = "processingData")
    } else if (rv$typeOfDataset == "peptide"){
        .choices <- list("Quantitative data" = "tabExprs",
                         "Peptides metadata" = "tabfData",
                         "Replicate metadata" = "tabpData",
                         "Dataset history" = "processingData")
    } else if (rv$typeOfDataset == ""){
        .choices <- list("Quantitative data" = "tabExprs",
                         "Analyte metadata" = "tabfData",
                         "Replicate metadata" = "tabpData",
                         "Dataset history" = "processingData")
    }
    
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
                     radioButtons("distance","Distance",
                                  choices = list(euclidean ="euclidean",
                                                 manhattan="manhattan")),
                     br(),
                     radioButtons("linkage","Linkage for clustering",
                                  choices=list(average="average",
                                               ward.D="ward.D")))
})

#----------------------------------------------
output$tabToShow <- renderUI({
    input$DS_TabsChoice
    rv$current.obj
    rv$indexNA
    if (is.null(input$DS_TabsChoice)) {return(NULL)}
    if (is.null(rv$current.obj)) {return(NULL)}
    
    if (input$DS_TabsChoice == "tabExprs"){DT::dataTableOutput("table")}
    else if (input$DS_TabsChoice == "tabfData"){DT::dataTableOutput("viewfData")}
    else if (input$DS_TabsChoice == "tabpData"){DT::dataTableOutput("viewpData")}
    else if (input$DS_TabsChoice == "processingData"){
        helpText("Previous operations made on the original dataset :")
        DT::dataTableOutput("viewProcessingData")
    }
    
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
        serverSide = FALSE)
    
    
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
                pourcentage <- 100 * round(sum(NA.count)/
                                    (dim(Biobase::exprs(rv$current.obj))[1]*
                                    dim(Biobase::exprs(rv$current.obj))[2]), 
                                    digits=4)
                d <- "lines"
                if (rv$typeOfDataset == "peptide") {d <- "peptides"}
                else if (rv$typeOfDataset == "protein") {d <- "proteins"}
                else {d <- "analytes"}
                
                nb.empty.lines <- sum(apply(
                    is.na(as.matrix(exprs(rv$current.obj))), 1, all))
                tags$ul(
                    tags$li(paste("There are", 
                                  dim(Biobase::exprs(rv$current.obj))[2], 
                                  " samples in your data.", sep=" ")),
                    
                    tags$li(paste("There are", 
                                  dim(Biobase::exprs(rv$current.obj))[1], d,
                                  " in your data.", sep=" ")), 
                    tags$li(paste("Percentage of missing values:",
                                  pourcentage , "%", sep=" ")),
                    tags$li(paste("Number of lines with only NA values =",
                                  nb.empty.lines , sep=" "))
                )
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






##' distribution of missing values in current.obj
##' @author Samuel Wieczorek
output$histo.missvalues.per.lines_Image <- renderPlot({
    rv$current.obj
    if (is.null(rv$current.obj)) {return(NULL)}
    result = tryCatch(
        {
        wrapper.mvPerLinesHisto(rv$current.obj, 
                        c(2:length(colnames(Biobase::pData(rv$current.obj)))))
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


##' distribution of missing values in current.obj
##' @author Samuel Wieczorek
output$histo.missvalues.per.lines.per.conditions_Image <- renderPlot({
    
    rv$current.obj
    if (is.null(rv$current.obj)) {return(NULL)}
    result = tryCatch(
        {
            wrapper.mvPerLinesHistoPerCondition(rv$current.obj, 
                        c(2:length(colnames(Biobase::pData(rv$current.obj)))))
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




##' distribution of missing values in current.obj
##' @author Samuel Wieczorek
output$histo.missvalues.per.lines_DS <- renderPlot({
    rv$current.obj
    rv$current.obj
    if (is.null(rv$current.obj)) {return(NULL)}
    
    result = tryCatch(
        {
            wrapper.mvPerLinesHisto(rv$current.obj, 
                        c(2:length(colnames(Biobase::pData(rv$current.obj)))))
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



##' distribution of missing values in current.obj
##' @author Samuel Wieczorek
output$histo.missvalues.per.lines.per.conditions_DS <- renderPlot({
    
    rv$current.obj
    if (is.null(rv$current.obj)) {return(NULL)}
    
    result = tryCatch(
        {
            wrapper.mvPerLinesHistoPerCondition(rv$current.obj, 
                        c(2:length(colnames(Biobase::pData(rv$current.obj)))))
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


##' boxplot of intensities in current.obj
##' @author Samuel Wieczorek
output$viewBoxPlot_DS <- renderPlot({
    rv$current.obj
    input$legendXAxis_DS
    if (is.null(rv$current.obj)) {return(NULL)}
    
    legDS <- NULL
    if (is.null(input$legendXAxis_DS)){
        .names <- colnames(Biobase::pData(rv$current.obj))[-1]
        legDS <- .names[1]}
    else{legDS <- input$legendXAxis_DS}
    
    result = tryCatch(
        {
            wrapper.boxPlotD(rv$current.obj,  legDS)
            
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



output$viewViolinPlot_DS <- renderPlot({
    rv$current.obj
    input$legendXAxis_DS
    if (is.null(rv$current.obj)) {return(NULL)}
    
    
    legDS <- NULL
    if (is.null(input$legendXAxis_DS)){
        .names <- colnames(Biobase::pData(rv$current.obj))[-1]
        legDS <- .names[1]}
    else{legDS <- input$legendXAxis_DS}
    
    result = tryCatch(
        {
            wrapper.violinPlotD(rv$current.obj,  legDS)
            
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




##' Distribution of intensities in current.obj
##' @author Samuel Wieczorek
output$viewDensityplot_DS <- renderPlot({
    rv$current.obj
    input$lab2Show_DS
    input$whichGroup2Color_DS
    if (is.null(rv$current.obj)) {return(NULL)}
    
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
    
    result = tryCatch(
        {
            wrapper.densityPlotD(rv$current.obj, 
                                 labels_DS, 
                                 as.numeric(labelsToShow_DS), 
                                 gToColor_DS)
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





##' distribution of the variance in current.obj
##' 
##' @author Samuel Wieczorek
output$viewDistCV <- renderPlot({
    
    rv$current.obj
    
    if (is.null(rv$current.obj)) {return(NULL)}
    result = tryCatch(
        {
            wrapper.CVDistD(rv$current.obj)
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


##' Draw a correlation matrix of intensities in current.obj
##' 
##' @author Samuel Wieczorek
output$corrMatrix <- renderPlot({
    
    rv$current.obj
    input$expGradientRate
    if (is.null(rv$current.obj)) {return(NULL)}
    
    gradient <- NULL
    if (is.null(input$expGradientRate)){gradient <- defaultGradientRate}
    else{gradient <- input$expGradientRate}
    
    result = tryCatch(
        {
            wrapper.corrMatrixD(rv$current.obj, rate = gradient)
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



##' Draw a heatmap of current data
##' 
##' @author Samuel Wieczorek
output$heatmap <- renderPlot({
    
    rv$current.obj
    input$linkage
    input$distance
    if (is.null(rv$current.obj)) {return(NULL)}
    
    
    if (!is.null(input$linkage) && !is.null(input$distance)
        #&& (getNumberOfEmptyLines(Biobase::exprs(rv$current.obj)) == 0)
    ) {
        
        result = tryCatch(
            {
                
                wrapper.heatmapD(rv$current.obj,
                                 input$distance, 
                                 input$linkage,
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
        busyIndicator("Calculation in progress",wait = 0),
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
        
    ),server = FALSE)







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
        txt3 <- paste("Percentage of missing values:",pourcentage , "%")
        
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
                          plurial," with only NA values !!"
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
                 "Plot to show",
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
                           , label = "Select data to show"
                           , choices = label.names
                           , selected = unlist(label.names))
    })
})

