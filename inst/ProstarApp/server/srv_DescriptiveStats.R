callModule(moduleLegendColoredExprs, "ExprsColorLegend_DS")
callModule(moduleLegendColoredExprs, "FilterColorLegend_DS")
callModule(moduleDensityplot, "densityPlot_DS")
callModule(missingValuesPlots, "MVPlots_DS")
callModule(moduleBoxplot, "boxPlot_DS")
callModule(moduleStaticDataTable,"overview_DS", table2show=GetDatasetOverview(), withBtns = FALSE)


callModule(moduleStaticDataTable,"PCAvarCoord", table2show=round(ComputePCA()$var$coord, digits=7), withBtns = FALSE, showRownames=TRUE)


# outs <- outputOptions(output)
# print(names(outs))
# outputOptions(output, 'densityPlot_DS-Densityplot', suspendWhenHidden = FALSE)
# outputOptions(output, 'boxPlot_DS-BoxPlot', suspendWhenHidden = FALSE)


# lapply(names(outs), function(name) {
#   outputOptions(output, name, suspendWhenHidden = FALSE)
# })


eventReactive(ComputePCA(), {
  shinyjs::toggle("PCAvarCoord")
})

Compute_PCA_nbDimensions <- reactive({
  nmax <- 12 # ncp should not be greater than... 
  # pour info, ncp = nombre de composantes ou de dimensions dans les résultats de l'ACP
  
  y <- exprs(rv$current.obj)
  nprot <- dim(y)[1]
  n <- dim(y)[2] # If too big, take the number of conditions.
  
  if (n > nmax){
    n <- length(unique(Biobase::pData(rv$current.obj)$Condition))
  }
  
  
  ncp <- min(n, nmax)
  ncp
})

ComputePCA <- reactive({
  req(input$varScale_PCA)
  Compute_PCA_nbDimensions()
  
  pca <- wrapper.pca(rv$current.obj, input$varScale_PCA, ncp=Compute_PCA_nbDimensions())

  pca
  })


output$pcaPlot1 <- renderPlot({
  req(c(input$pca.axe1,input$pca.axe2))
  
  res.pca <- ComputePCA()
  plot.pca.var(res.pca, c(input$pca.axe1, input$pca.axe2))
  
})

output$pcaPlot2 <- renderPlot({
  req(c(input$pca.axe1,input$pca.axe2))
  
  res.pca <- ComputePCA()
  plot.pca.ind(res.pca, c(input$pca.axe1, input$pca.axe2))
  
})


output$pcaPlotEigen <- renderHighchart({
  res.pca <- ComputePCA()

  #plot.pca.eigen(res.pca)
  plot.pca.eigen.hc(res.pca)
})

output$pcaOptions <- renderUI({
  tagList(
    tags$div(
      
      tags$div( style="display:inline-block; vertical-align: middle;",
        checkboxInput('varScale_PCA', "Var scaling", value=TRUE)),
      
    tags$div( style="display:inline-block; vertical-align: middle;",
              numericInput('pca.axe1', "Dim 1", min=1, max=Compute_PCA_nbDimensions(),value=1,width='100px')
    ),
    tags$div( style="display:inline-block; vertical-align: middle;",
              numericInput('pca.axe2', "Dim 2", min=1, max=Compute_PCA_nbDimensions(),value=2,width='100px')
    )

  )
)
})
    

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
                                  choices = G_heatmapDistance_Choices, 
                                 selected = rv$PlotParams$heatmap.distance),
                     br(),
                     selectInput("linkage","Linkage for clustering",
                                  choices=G_heatmapLinkage_Choices,
                                 selected=rv$PlotParams$heatmap.linkage))
})

#----------------------------------------------
output$tabToShow <- renderUI({
    req(input$DS_TabsChoice)
    req(rv$current.obj)
    
    switch(input$DS_TabsChoice,
          None = {return(NULL)},
          tabExprs = DT::dataTableOutput("table"),
          tabfData = DT::dataTableOutput("viewfData"),
          tabpData = DT::dataTableOutput("viewpData")
          # processingData = {
          #             helpText("Previous operations made on the original dataset :")
          #             DT::dataTableOutput("viewProcessingData")
          #             }
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
  print(data)
  pal <- unique(rv$PlotParams$paletteConditions)
  print(pal)
  dt <- DT::datatable(  data,
                        extensions = 'Scroller',
                        rownames=  FALSE,
                        
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




viewDistCV <- reactive({
    
    req(rv$current.obj)
  rv$PlotParams$paletteConditions

            isolate({rv$tempplot$varDist <- wrapper.CVDistD_HC(rv$current.obj,rv$PlotParams$paletteConditions)})
            rv$tempplot$varDist

    
})



corrMatrix <- reactive({
    
    req(rv$current.obj)
    input$expGradientRate
    
    gradient <- NULL
    if (is.null(input$expGradientRate)){gradient <- defaultGradientRate}
    else{
        gradient <- input$expGradientRate}
        isolate({
          rv$tempplot$corrMatrix <- wrapper.corrMatrixD_HC(rv$current.obj,gradient)
            rv$tempplot$corrMatrix
            })

})



heatmap <- reactive({
    
    req(rv$current.obj)
  rv$PlotParams$heatmap.linkage
  rv$PlotParams$heatmap.distance
  
  isolate({  wrapper.heatmapD(rv$current.obj,
                                 rv$PlotParams$heatmap.distance, 
                                 rv$PlotParams$heatmap.linkage,
                                 TRUE)
              })

})








output$DS_PlotHeatmap <- renderUI({
    req(rv$current.obj)
  if (nrow(rv$current.obj) > limitHeatmap){
    tags$p("The dataset is too big to compute the heatmap in a reasonable time.")
  }else {
    tagList(
        plotOutput("heatmap", width = "900px", height = "600px") %>% withSpinner(type=spinnerType)
 
    )
  }
})



#################
output$table <- renderDataTable({
   # req(rv$current.obj)
    df <- getDataForExprs(rv$current.obj)
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
           backgroundColor = styleEqual(c("POV", "MEC"), c(rv$colorsTypeMV$POV, rv$colorsTypeMV$MEC)),
           backgroundSize = '98% 48%',
           backgroundRepeat = 'no-repeat',
           backgroundPosition = 'center'
       )
    
    
    dt
    })




# options for boxplot
# #------------------------------------------------------
output$ChooseLegendForSamples <- renderUI({
    req(rv$current.obj)

  .names <- colnames(Biobase::pData(rv$current.obj))
 

    checkboxGroupInput("legendForSamples",
                       label = "Choose data to show in legend",
                       choices = .names,
                       selected=.names[2])
})

observeEvent(input$legendForSamples, {
  rv$PlotParams$legendForSamples <- as.vector(apply(as.data.frame(Biobase::pData(rv$current.obj)[,input$legendForSamples]), 1, function(x) paste(x, collapse="_")))
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
     moduleLegendColoredExprsUI("ExprsColorLegend_DS",rv$colorsTypeMV)

 })







