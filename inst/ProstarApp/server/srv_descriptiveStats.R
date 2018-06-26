callModule(moduleLegendColoredExprs, "ExprsColorLegend_DS")
callModule(moduleLegendColoredExprs, "FilterColorLegend_DS")
callModule(moduleDensityplot, "densityPlot_DS",reactive({input$lab2Show_DS}),reactive({ input$whichGroup2Color_DS}))
callModule(missingValuesPlots, "MVPlots_DS")
callModule(moduleBoxplot, "boxPlot_DS", reactive({input$legendXAxis_DS}))
callModule(moduleDatasetOverview,"overview_DS")





output$viewProcessingData <- DT::renderDataTable({
  rv$current.obj
  if (is.null(rv$current.obj)) {return(NULL)}
  
  result = tryCatch(
    {
      data.frame(History=(rv$current.obj)@processingData@processing
                 [-grep("Subset", (rv$current.obj)@processingData@processing)])
    }
    , warning = function(w) {
      shinyjs::info(conditionMessage(w))
    }, error = function(e) {
      shinyjs::info(paste("view processing data",":",
                          conditionMessage(e), 
                          sep=" "))
    }, finally = {
      #cleanup-code 
    })
  
},
option=list(initComplete = initComplete(),
            pageLength=DT_pagelength,
            orderClasses = TRUE,
            autoWidth=FALSE,
            dom = 'R<"clear">lfrtip',
            columnDefs = list(list(columns.width=c("60px"),
                                   columnDefs.targets= c(list(0),list(1),list(2)))))
)




#######################################


output$DS_sidebarPanel_tab <- renderUI({
    req(rv$typeOfDataset)
    
    .choices<- NULL
    switch(rv$typeOfDataset,
           protein = {
                      .choices <- list( "Quantitative data" = "tabExprs",
                                        "Proteins metadata" = "tabfData",
                                        "Experimental design" = "tabpData",
                                        "Dataset history" = "processingData")
                      },
        peptide = {
                      .choices <- list("Quantitative data" = "tabExprs",
                                       "Peptides metadata" = "tabfData",
                                        "Experimental design" = "tabpData",
                                        "Dataset history" = "processingData")
                      },
                {
                .choices <- list("Quantitative data" = "tabExprs",
                                "Analyte metadata" = "tabfData",
                                "Experimental design" = "tabpData",
                                "Dataset history" = "processingData")
                }
    )
    
    tagList(
                     radioButtons("DS_TabsChoice", "Table to display",
                                  choices = .choices,
                                  selected=character(0)),
                     br(),
                    
                     uiOutput("legendForExprsData")
    )
    
})


output$DS_sidebarPanel_heatmap <- renderUI({
    req(rv$current.obj)
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
    req(input$DS_TabsChoice)
    req(rv$current.obj)
    
    switch(input$DS_TabsChoice,
          None = {return(NULL)},
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
# output$viewExprs <- renderDataTable(
#     # rv$current.obj
#     # input$nDigits
#     # if (is.null(rv$current.obj)) {return(NULL)}
#     # if (input$nDigits == T){nDigits = 1e100}else {nDigits = 3}
#     # 
#     # df <- cbind(ID = rownames(Biobase::fData(rv$current.obj)),
#     #               round(Biobase::exprs(rv$current.obj), 
#     #               digits=nDigits))
#     # 
#     # 
#     # test.table <- data.frame(lapply(1:8, function(x) {1:1000}))
#     # test.table[c(2,3,7), c(2,7,6)] <- NA
#     # id <- which(is.na(test.table))
#     # colonnes <- trunc(id / nrow(test.table))+1
#     # lignes <- id %% nrow(test.table)
#     # formattable(test.table, list(area(col = colonnes, row = lignes) ~ color_tile("red", "lightblue")))
#     # 
#     # id <- which(is.na(exprs(Exp1_R25_prot)))
#     #colonnes <- trunc(id / nrow(exprs(Exp1_R25_prot)))+1
#     #lignes <- id %% nrow(exprs(Exp1_R25_prot))
#     #formattable(as.data.frame(exprs(Exp1_R25_prot)), list(area(col = colonnes, row = lignes) ~ color_tile("red", "lightblue")))
#     
#     
#     
#     #id <- which(is.na(exprs(Exp1_R25_prot)))
#     
#     test.table,
#     extensions = 'Scroller',
#     options = list(initComplete = initComplete(),
#         
#         displayLength = 3,
#         deferRender = TRUE,
#         bLengthChange = FALSE,
#         scrollX = 200,
#         scrollY = 600,
#         scroller = TRUE,
#         drawCallback=JS(
#             paste("function(row, data) {",
#                   paste(sapply(1:ncol(test.table),function(i)
#                      paste( "$(this.api().cell(",
#                         id %% nrow(test.table)-1,",",
#                         trunc(id / nrow(test.table))+1,
#                         ").node()).css({'background-color': 'lightblue'});")
#                   ),collapse = "\n"),"}" )
#         ), 
#         server = TRUE)
#     
#     
#     # id <- which(is.na(df))
#     # datatable(df,
#     #               options=list(drawCallback=JS(
#     #               paste("function(row, data,index) {",
#     #               paste(sapply(1:ncol(df),function(i) 
#     #              {paste( "$(this.api().cell(",id %% nrow(df)-1,",",trunc(id / nrow(df))+1,").node()).css({'background-color': 'lightblue'});")}
#     #              #{paste( "$(this.api().cell(index,",trunc(i / nrow(data))+1,").node()).css({'background-color': 'lightblue'});")}
#     #               
#     #              ),collapse = "\n"),"}" ) )
#     #     )
#     #     ) 
#     
# )




##' show pData of the MSnset object
##' @author Samuel Wieczorek
output$viewpData <- DT::renderDataTable({
    req(rv$current.obj)
    
  data <- as.data.frame(Biobase::pData(rv$current.obj))
  
  pal <- brewer.pal(length(unique(data$Condition)),"Dark2")
  pal <- pal[1:length(unique(data$Condition))]
  dt <- DT::datatable(  data,
                        extensions = 'Scroller',
                    options=list(initComplete = initComplete(),
                                 pageLength=DT_pagelength,
                                 orderClasses = TRUE,
                                 autoWidth=FALSE,
                                 deferRender = TRUE,
                                 bLengthChange = FALSE,
                                 scrollX = 200,
                                 scrollY = 500,
                                 scroller = TRUE,
                                columnDefs = list(
                                list(columns.width=c("60px"),
                                      columnDefs.targets= c(list(0),list(1),list(2)))))) %>%
    formatStyle(
      columns = colnames(data)[1:2],
      valueColumns = colnames(data)[2],
      backgroundColor = styleEqual(unique(data$Condition), pal)
    )
  
})

##' show fData of the MSnset object in a table
##' @author Samuel Wieczorek
output$viewfData <- DT::renderDataTable({
    req(rv$current.obj)
    
    
    if ('Significant' %in% colnames(Biobase::fData(rv$current.obj))){
        dat <- DT::datatable(as.data.frame(Biobase::fData(rv$current.obj)),
                             extensions = 'Scroller',
                        options=list(initComplete = initComplete(),
                                     pageLength=DT_pagelength,
                                    orderClasses = TRUE,
                                    autoWidth=FALSE,
                                    deferRender = TRUE,
                                    bLengthChange = FALSE,
                                    scrollX = 200,
                                    scrollY = 200,
                                    scroller = TRUE,
                                    columns.searchable=F,
                            columnDefs = list(list(columns.width=c("60px"),
                        columnDefs.targets=c(list(0),list(1),list(2)))))) %>%
            formatStyle(columns = 'Significant',
                        target = 'row',
                        background = styleEqual(1, 'lightblue'))
    } else {
        dat <- DT::datatable(as.data.frame(Biobase::fData(rv$current.obj)),
                             extensions = 'Scroller',
                             options=list(initComplete = initComplete(),
                                 pageLength=DT_pagelength,
                                 deferRender = TRUE,
                                 bLengthChange = FALSE,
                                 scrollX = 200,
                                 scrollY = 600,
                                 scroller = TRUE,
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
    req(rv$current.obj)
  dt <- DT::datatable(as.data.frame(cbind(ID = rownames(Biobase::fData(rv$current.obj)),
                                Biobase::exprs(rv$current.obj))),
                      extensions = 'Scroller',
                      rownames = FALSE,
                      
options=list(orderClasses = TRUE,
            autoWidth=FALSE,
            bLengthChange = FALSE,
            scrollX = 200,
            scrollY =600,
            scroller = TRUE,
            columns.searchable=F,
            pageLength = DT_pagelength,
            columnDefs = list(list(columns.width=c("60px"),
                            columnDefs.targets=c(list(0),list(1),list(2)))))
)
})





violinPlot2 <- reactive({
    req(rv$current.obj)
    req(input$legendXAxisViolin_DS)
    
    
    if (!is.null(input$legendXAxisViolin_DS)){
      rv$PlotParams$legDS_Violinplot <- input$legendXAxisViolin_DS}
    
    # result = tryCatch(
    #     {
            isolate({
              if (is.null(rv$PlotParams$legDS_Violinplot)) {
                wrapper.violinPlotD(rv$current.obj)
                }  else {
                    wrapper.violinPlotD(rv$current.obj,  rv$PlotParams$legDS_Violinplot)
                }
              
            })
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
        # 
})





viewDistCV <- reactive({
    
    req(rv$current.obj)
    
    # result = tryCatch(
    #     {
            isolate({rv$tempplot$varDist <- wrapper.CVDistD_HC(rv$current.obj)})
            rv$tempplot$varDist
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
        # 
    
})



corrMatrix <- reactive({
    
    req(rv$current.obj)
    input$expGradientRate
    
    gradient <- NULL
    if (is.null(input$expGradientRate)){gradient <- defaultGradientRate}
    else{
        gradient <- input$expGradientRate}
    
    # result = tryCatch(
    #     {
            isolate({rv$tempplot$corrMatrix <- wrapper.corrMatrixD_HC(rv$current.obj,gradient)})
            rv$tempplot$corrMatrix
            
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
        # 
})



heatmap <- reactive({
    
    req(rv$current.obj)
    input$linkage
    input$distance
    if (!is.null(input$linkage) && !is.null(input$distance)
        #&& (getNumberOfEmptyLines(Biobase::exprs(rv$current.obj)) == 0)
    ) {
        
        # result = tryCatch(
        #     {
                rv$PlotParams$HeatmapLinkage <- input$linkage
      rv$PlotParams$HeatmapDistance <- input$distance
      
              isolate({  wrapper.heatmapD(rv$current.obj,
                                 rv$PlotParams$HeatmapDistance, 
                                 rv$PlotParams$HeatmapLinkage,
                                 TRUE)
              })
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




# 
# 
# output$DS_sidebarPanel_Densityplot <- renderUI({
#     conditionalPanel(condition= "true",
#                      uiOutput("nGroup_DS"),
#                      br(),
#                      uiOutput("nShow_DS"))
#     
# })
# 




output$DS_sidebarPanel_Boxplot <- renderUI({
    uiOutput("ChooseLegendForAxis_DS")
})

output$DS_sidebarPanel_Violinplot <- renderUI({
    uiOutput("ChooseLegendForAxisViolin_DS")
    
})







#################
output$table <- renderDataTable({
   # req(rv$current.obj)
    df <- getDataForExprs()
    dt <- datatable( df,
                     extensions = c('Scroller', 'Buttons'),
                    options = list(
                      dom = 'Bfrtip',
                      initComplete = initComplete(),
                        displayLength = 20,
                        deferRender = TRUE,
                        bLengthChange = FALSE,
                        scrollX = 200,
                        scrollY = 600,
                        scroller = TRUE,
                        ordering=FALSE,
                        buttons=c('copy', 'csv', 'excel'),
                        server = TRUE,
                            columnDefs = list(list(targets = c(((ncol(df)/2)+1):ncol(df)), visible = FALSE)))) %>%
       formatStyle(
           colnames(df)[1:(ncol(df)/2)],
           colnames(df)[((ncol(df)/2)+1):ncol(df)],
           backgroundColor = styleEqual(c("POV", "MEC"), c('lightblue', 'orange')),
           backgroundSize = '98% 48%',
           backgroundRepeat = 'no-repeat',
           backgroundPosition = 'center'
       )
    
    
    dt
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
                     label = "Data to show in legend",
                     choices = .names,
                     selected = .names[1])
})


# options for boxplot
#------------------------------------------------------
output$ChooseLegendForAxis_DS <- renderUI({
    rv$current.obj
    if (is.null(rv$current.obj)){return(NULL)}
    
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
# output$nGroup_DS <- renderUI({
#    # req(rv$current.obj)
#     #if (is.null(rv$current.obj) ) {return(NULL) }
#     
#     radioButtons("whichGroup2Color_DS",
#                  "Color lines",
#                  choices=list("By condition" = "Condition",
#                               "By replicate" = "Replicate"))
#     
# })




##' Select the labels to show in densityplots
##' @author Samuel Wieczorek
output$nShow_DS <- renderUI({
   # rv$current.obj
   # if (is.null(rv$current.obj) ) {return(NULL) }
    
         # labs <- paste(Biobase::pData(rv$current.obj)[,"Condition"],
         #              Biobase::pData(rv$current.obj)[,"Bio.Rep"],
         #              Biobase::pData(rv$current.obj)[,"Tech.Rep"],
         #              Biobase::pData(rv$current.obj)[,"Analyt.Rep"],
         #              sep= "_")
         labs <- apply(pData(rv$current.obj), 1, function(x){paste0(x, collapse='_')})
         names(labs)<- NULL
        label.names <- setNames(as.list(c(1:length(labs))),labs)
        
        
        checkboxGroupInput("lab2Show_DS"
                           , label = "Hide/show replicates"
                           , choices = label.names
                           , selected = unlist(label.names))
   
})





##' boxplot of intensities in current.obj
##' @author Samuel Wieczorek
# output$viewBoxPlot <- renderPlot({
#     boxPlot()
#     
# })


output$viewViolinPlot_DS <- renderPlot({
    violinPlot2()
}, width=600, height=400) 








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







