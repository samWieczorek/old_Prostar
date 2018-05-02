


callModule(moduleLegendColoredExprs, "ExprsColorLegend_DS")
callModule(moduleLegendColoredExprs, "FilterColorLegend_DS")


callModule(moduleDensityplot, "densityPlot_DS")
callModule(moduleDensityplot,"densityPlot_Norm")


callModule(missingValuesPlots, "MVPlots_DS")
callModule(missingValuesPlots,"MVPlots_filtering")


callModule(moduleBoxplot, "boxPlot_DS")
callModule(moduleBoxplot,"boxPlot_Norm")

callModule(moduleDatasetOverview,"overview_DS")
callModule(moduleDatasetOverview,"overview_DemoMode")
callModule(moduleDatasetOverview,"overview_openMSnset")
callModule(moduleDatasetOverview,"overview_convertData")
callModule(moduleFilterStringbasedOptions,"filteringStringBasedOptions")



activatePopover <- function(){
    txt_histo_M <- paste0("<p>Test",
                          "test</p><p>Explanation .</p>")
    
    txt_histo_MV_per_lines <- paste0("<p>Test",
                          "test</p><p>Explanation .</p>")
    
    
    txt_histo_MV_per_lines_per_conditions <- paste0("<p>Test",
                          "test</p><p>Explanation .</p>")
    
    
    addPopover(session, "MVPlots_DS-histo_MV", "Info", 
               content = txt_histo_M, trigger = 'click')
    
    addPopover(session, "MVPlots_DS-histo_MV_per_lines", "Info", 
               content = txt_histo_MV_per_lines, trigger = 'click')
    
    addPopover(session, "MVPlots_DS-histo_MV_per_lines_per_conditions", "Info", 
               content = txt_histo_MV_per_lines_per_conditions, trigger = 'click')
    
    
    addPopover(session, "MVPlots_filtering-histo_MV", "Info", 
               content = txt_histo_M, trigger = 'click')
    
    addPopover(session, "MVPlots_filtering-histo_MV_per_lines", "Info", 
               content = txt_histo_MV_per_lines, trigger = 'click')
    
    addPopover(session, "MVPlots_filtering-histo_MV_per_lines_per_conditions", "Info", 
               content = txt_histo_MV_per_lines_per_conditions, trigger = 'click')
    
    
}


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
                                   value = FALSE),
                     uiOutput("legendForExprsData")
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
          tabExprs = DT::dataTableOutput("viewExprs"),
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
output$viewExprs <- renderDataTable({
    # req(rv$current.obj)
    df <- getDataForExprs()
    dt <- datatable( df,
                     options = list(displayLength = 20,
                                    ordering=FALSE,
                                    server = TRUE,
                                    columnDefs = list(list(targets = c(((ncol(df)/2)+1):ncol(df)), visible = FALSE))
                     )) %>%
        formatStyle(
            colnames(df)[1:(ncol(df)/2)],
            colnames(df)[((ncol(df)/2)+1):ncol(df)],
            backgroundColor = styleEqual(c("POV", "MEC"), c('lightblue', 'orange'))
        )
    
    
    dt
})




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



histo_MV_per_lines <- reactive({
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



histo_MV_per_lines_per_conditions <- reactive({
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




BoxPlot <- reactive({
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




DensityPlot <- reactive({
    rv$current.obj
    input$lab2Show_DS
    input$whichGroup2Color_DS
    if (is.null(rv$current.obj)) {return(NULL)}
    #if (is.null(input$lab2Show_DS)) {return(NULL)}
    #if (is.null(input$whichGroup2Color_DS)) {return(NULL)}
    
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



getDataForExprs <- reactive({
    input$nDigits
    rv$current$obj
    
    if (input$nDigits == TRUE){nDigits = 1e100} else {nDigits = 3}
    
    # test.table <- cbind(ID = rownames(Biobase::fData(rv$current.obj)),
    #                     round(Biobase::exprs(rv$current.obj), 
    #                     digits=nDigits))
    test.table <- as.data.frame(round(Biobase::exprs(rv$current.obj),digits=nDigits))
    test.table <- cbind(test.table, Biobase::fData(rv$current.obj)[,rv$current.obj@experimentData@other$OriginOfValues])
    #colnames(test.table) <- c(colnames(Biobase::exprs(rv$current.obj), rv$current.obj@experimentData@other$OriginOfValues))
    test.table
    #print(colnames(test.table))
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





histo_MV <- reactive({
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





##' boxplot of intensities in current.obj
##' @author Samuel Wieczorek
# output$viewBoxPlot <- renderPlot({
#     boxPlot()
#     
# })


output$viewViolinPlot_DS <- renderPlot({
    violinPlot2()
}) 




##' distribution of missing values in current.obj
##' @author Samuel Wieczorek
output$histo_MV_per_lines <- renderHighchart({
    histo_MV_per_lines()
})





##' distribution of missing values in current.obj
##' @author Samuel Wieczorek
output$histo_MV_per_lines_per_conditions <- renderHighchart({
    histo_missvalues_per_lines_per_conditions()
})
addPopover(session, "histo_missvalues_per_lines_per_conditions", "Info", 
           content = paste0("<p>Test",
                            "test</p><p>Explanation .</p>"), trigger = 'click')




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




# 
 output$legendForExprsData <- renderUI({
   req(input$DS_TabsChoice)
     
     if (input$DS_TabsChoice != "tabExprs"){return(NULL)}
     moduleLegendColoredExprsUI("ExprsColorLegend_DS")

 })







