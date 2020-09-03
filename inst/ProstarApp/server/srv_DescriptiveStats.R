callModule(moduleLegendColoredExprs, "ExprsColorLegend_DS")
callModule(moduleLegendColoredExprs, "FilterColorLegend_DS")

callModule(missingValuesPlots, "MVPlots_DS", 
           data=reactive({rv$current.obj}),
           palette = reactive({rv$PlotParams$paletteConditions})
)
callModule(moduleDensityplot, "densityPlot_DS",data=reactive({rv$current.obj}))
callModule(moduleBoxplot, "boxPlot_DS",data=reactive({rv$current.obj}))
callModule(moduleStaticDataTable,"overview_DS", table2show=reactive({GetDatasetOverview()}),
           filename='DescriptiveStats_Overview')

callModule(moduleStaticDataTable,"PCAvarCoord", 
           table2show=reactive({if (!is.null(rv$res.pca)) round(rv$res.pca$var$coord, digits=7)}), 
           showRownames=TRUE,
           filename = 'PCA_Var_Coords')



# outs <- outputOptions(output)
# print(names(outs))
# outputOptions(output, 'densityPlot_DS-Densityplot', suspendWhenHidden = FALSE)
# outputOptions(output, 'boxPlot_DS-BoxPlot', suspendWhenHidden = FALSE)


# lapply(names(outs), function(name) {
#   outputOptions(output, name, suspendWhenHidden = FALSE)
# })


observeEvent(c(input$pca.axe1,input$pca.axe2),{rv$PCA_axes <- c(input$pca.axe1,input$pca.axe2)})

observeEvent(input$varScale_PCA,{
  rv$PCA_varScale <- input$varScale_PCA
  rv$res.pca <- wrapper.pca(rv$current.obj, rv$PCA_varScale, ncp=Compute_PCA_nbDimensions())
})

observeEvent(rv$current.obj, {
  rv$res.pca <- wrapper.pca(rv$current.obj, rv$PCA_varScale, ncp=Compute_PCA_nbDimensions())
})



output$plotsCorM <- renderUI({
  tagList(
    tags$br(),tags$br(),
    tags$div(
      tags$div(style="display:inline-block; vertical-align: middle;",
               tags$p("Plot options")
      ),
      
      tags$div(style="display:inline-block; vertical-align: middle;",
               
               tags$div(
                 tags$div(style="display:inline-block; vertical-align: top;",
                          shinyWidgets::dropdownButton(
                            tags$div(
                              tags$div(style="display:inline-block; vertical-align: bottom;",
                                       sliderInput("expGradientRate",
                                                   "Tune to modify the color gradient",
                                                   min = 0,max = 1,value = defaultGradientRate,step=0.01),
                                       tooltip="Plots parameters",
                                       icon = icon("gear"), status = optionsBtnClass
                                       
                              )
                            ),
                            tooltip="Plots parameters",
                            icon = icon("gear"), status = optionsBtnClass
                          ))
               )
               
      )
    ),
    withProgress(message = '',detail = '', value = 1, {
      highchartOutput("corrMatrix",width = plotWidth,height = plotHeight)
    })
  )
})



output$IntensityStatsPlots <- renderUI({
  tagList(
    tags$br(),tags$br(),
    tags$div(
      tags$div(style="display:inline-block; vertical-align: middle;",
               tags$p("Plot options")
      ),
      
      tags$div(style="display:inline-block; vertical-align: middle;",
               
               tags$div(
                 tags$div(style="display:inline-block; vertical-align: top;",
                          shinyWidgets::dropdownButton(
                            tags$div(
                              tags$div(style="display:inline-block; vertical-align: bottom;",
                                       selectInput("whichGroup2Color",
                                                   "Color lines",
                                                   choices=list("By condition" = "Condition",
                                                                "By replicate" = "Replicate"),
                                                   selected=GetWhichGroup2Color(), width='150px')
                              ),
                              tags$div(style="display:inline-block; vertical-align: bottom;",
                                       uiOutput("ChooseLegendForSamples")
                              )
                            ),
                            tooltip="Plots parameters",
                            style = "material-circle", icon = icon("gear"), status = optionsBtnClass
                          )))
               
      )),
    
    
    fluidRow(
      column(width=6,moduleDensityplotUI("densityPlot_DS")),
      column(width=6, moduleBoxplotUI("boxPlot_DS"))
    )
  )
  
})

output$plotsMVHistograms <- renderUI({
  tagList(
    helpText("These barplots display the distribution of missing values in the dataset."),
    missingValuesPlotsUI("MVPlots_DS")
  )
})



output$plotsDistCV <- renderUI({
  
  tagList(
    helpText("Display the condition-wise distributions of the log-intensity CV (Coefficient of Variation) 
             of the protein/peptides."),
    helpText("For better visualization, it is possible to zoom in by click-and-drag."),
    withProgress(message = '',detail = '', value = 1, {
      highchartOutput("viewDistCV",width = plotWidth, height = plotHeight)
    })
  )
})


output$plotsHeatmap <- renderUI({
  tagList(
    div(
      div(
        style="display:inline-block; vertical-align: middle; padding-right: 20px;",
        selectInput("distance","Distance",
                    choices = G_heatmapDistance_Choices, 
                    selected = rv$PlotParams$heatmap.distance,
                    width="150px")
      ),
      div(
        style="display:inline-block; vertical-align: middle;",
        selectInput("linkage","Linkage",
                    choices=G_heatmapLinkage_Choices,
                    selected=rv$PlotParams$heatmap.linkage,
                    width="150px")
      ),
      
      tags$hr(),
      uiOutput("DS_PlotHeatmap")
    )
  )
})


output$plotsPCA <- renderUI({
  tagList(
    uiOutput("WarningNA_PCA"),
    uiOutput("pcaOptions"),
    fluidRow(
      column(width=6,  imageOutput("pcaPlotVar", width='auto', height='auto')),
      column(width=6,  imageOutput("pcaPlotInd", width='auto', height='auto'))
    ),
    fluidRow(
      column(width=6,  highchartOutput("pcaPlotEigen")),
      column(width=6,  moduleStaticDataTableUI("PCAvarCoord"))
    )
  )
})




output$pcaPlotInd <- renderImage({
  #req(rv$PCA_axes)
  # req(rv$res.pca)
  
  outfile <- tempfile(fileext='.png')
  print(paste0("Outfile = ", outfile))
  # Generate a png
  png(outfile)
  image <- DAPAR::plotPCA_Ind(rv$res.pca, rv$PCA_axes)
  print(image)
  dev.off()
  
  # Return a list
  list(src = outfile,
       alt = "This is alternate text")
}, deleteFile = FALSE)


output$pcaPlotVar <- renderImage({
  print("IN ####output$pcaPlotVar <- renderImage" )
  
  req(rv$PCA_axes)
  req(rv$res.pca)
  
  outfile <- tempfile(fileext='.png')
  # Generate a png
  png(outfile)
  image <- DAPAR::plotPCA_Var(rv$res.pca, rv$PCA_axes)
  print(image)
  dev.off()
  
  # Return a list
  list(src = outfile,
       alt = "This is alternate text")
}, deleteFile = FALSE)



output$pcaPlotEigen <- renderHighchart({
  req(rv$res.pca)
  plotPCA_Eigen_hc(rv$res.pca)
})

output$pcaOptions <- renderUI({
  req(rv$current.obj)
  
  tagList(
    
    if (length(which(is.na(Biobase::exprs(rv$current.obj)))) > 0)
    {
      tags$p("Warning: As your dataset contains missing values, the PCA cannot be computed.
             Please impute them first")
    }
    else{
      
      tags$div(
        
        
        tags$div( style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                  numericInput('pca.axe1', "Dimension 1", min=1, max=Compute_PCA_nbDimensions(),value=1,width='100px')
        ),
        tags$div( style="display:inline-block; vertical-align: middle;",
                  numericInput('pca.axe2', "Dimension 2", min=1, max=Compute_PCA_nbDimensions(),value=2,width='100px')
        ),
        
        tags$div( style="display:inline-block; vertical-align: middle; padding-right: 20px;",
                  checkboxInput('varScale_PCA', "Variance scaling", value=rv$PCA_varScale))
      )
      
    }
    
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
                             "Experimental design" = "tabpData")
         },
         peptide = {
           .choices <- list("Quantitative data" = "tabExprs",
                            "Peptides metadata" = "tabfData",
                            "Experimental design" = "tabpData")
         },
         {
           .choices <- list("Quantitative data" = "tabExprs",
                            "Analyte metadata" = "tabfData",
                            "Experimental design" = "tabpData")
         }
  )
  
  tagList(
    tags$div(
      tags$div( style="display:inline-block; vertical-align: middle; padding-right: 40px;",
                radioButtons("DS_TabsChoice", "Table to display",
                             choices = .choices,
                             inline = TRUE,
                             selected=character(0))
      ),
      tags$div( style="display:inline-block; vertical-align: middle;",
                uiOutput("legendForExprsData")
      )
    )
  )
  
  
})





output$DS_sidebarPanel_heatmap <- renderUI({
  req(rv$current.obj)
  tagList(
    h3("Clustering Options"),
    selectInput("distance","Distance",
                choices = G_heatmapDistance_Choices, 
                selected = rv$PlotParams$heatmap.distance,
                width="150px"),
    br(),
    selectInput("linkage","Linkage",
                choices=G_heatmapLinkage_Choices,
                selected=rv$PlotParams$heatmap.linkage,
                width="150px"))
})

#----------------------------------------------
output$tabToShow <- renderUI({
  req(input$DS_TabsChoice)
  req(rv$current.obj)
  
  switch(input$DS_TabsChoice,
         None = {return(NULL)},
         tabExprs = {
           tagList(
             if (nrow(rv$current.obj)>153) p(MSG_WARNING_SIZE_DT),
             DT::dataTableOutput("table"))
         },
         tabfData = {
           tagList(
             if (nrow(rv$current.obj)>153) p(MSG_WARNING_SIZE_DT),
             DT::dataTableOutput("viewfData"))
         },
         tabpData = {
           tagList(
             if (nrow(pData(rv$current.obj))>153) p(MSG_WARNING_SIZE_DT),
             DT::dataTableOutput("viewpData"))
         }
         # processingData = {
         #             helpText("Previous operations made on the original dataset :")
         #             DT::dataTableOutput("viewProcessingData")
         #             }
  )
  
})




##' show pData of the MSnset object
##' @author Samuel Wieczorek
output$viewpData <- DT::renderDataTable(server=TRUE,{
  req(rv$current.obj)
  
  data <- as.data.frame(Biobase::pData(rv$current.obj))
  pal <- unique(rv$PlotParams$paletteConditions)
  dt <- DT::datatable(  data,
                        extensions = c('Scroller', 'Buttons'),
                        rownames=  FALSE,
                        
                        options=list(initComplete = initComplete(),
                                     buttons = list('copy',
                                                    list(
                                                      extend = 'csv',
                                                      filename = 'Samples data'
                                                    ),'print'),
                                     dom='Bfrtip',
                                     pageLength=DT_pagelength,
                                     orderClasses = TRUE,
                                     autoWidth=TRUE,
                                     deferRender = TRUE,
                                     bLengthChange = FALSE,
                                     scrollX = 200,
                                     scrollY = 500,
                                     scroller = TRUE,
                                     #columnDefs = list(
                                     #list(columns.width=c("60px"), columnDefs.targets= c(list(0),list(1),list(2))))
                                     columnDefs = list(list(width='60px',targets= "_all"))
                        )) %>%
    formatStyle(
      columns = colnames(data)[1:2],
      valueColumns = colnames(data)[2],
      backgroundColor = styleEqual(unique(data$Condition), pal)
    )
  
})

##' show fData of the MSnset object in a table
##' @author Samuel Wieczorek
output$viewfData <- DT::renderDataTable(server=TRUE,{
  req(rv$current.obj)
  
  
  if ('Significant' %in% colnames(Biobase::fData(rv$current.obj))){
    dat <- DT::datatable(as.data.frame(Biobase::fData(rv$current.obj)),
                         rownames = TRUE,
                         extensions = c('Scroller', 'Buttons','FixedColumns'),
                         options=list(
                           initComplete = initComplete(),
                           buttons = list('copy',
                                          list(
                                            extend = 'csv',
                                            filename = 'feature metadata'
                                          ),'print'),
                           dom='Bfrtip',
                           pageLength=DT_pagelength,
                           orderClasses = TRUE,
                           autoWidth=FALSE,
                           deferRender = TRUE,
                           bLengthChange = FALSE,
                           scrollX = 200,
                           scrollY = 200,
                           scroller = TRUE,
                           columns.searchable=F,
                           fixedColumns = list(leftColumns = 1),
                           columnDefs = list(list(columns.width=c("60px"),
                                                  columnDefs.targets=c(list(0),list(1),list(2)))))) %>%
      formatStyle(columns = 'Significant',
                  target = 'row',
                  background = styleEqual(1, 'lightblue'))
  } else {
    dat <- DT::datatable(as.data.frame(Biobase::fData(rv$current.obj)),
                         rownames=TRUE,
                         extensions = c('Scroller', 'Buttons', 'FixedColumns'),
                         options=list(initComplete = initComplete(),
                                      buttons = list('copy',
                                                     list(
                                                       extend = 'csv',
                                                       filename = 'feature metadata'
                                                     ),'print'),
                                      dom='Bfrtip',
                                      pageLength=DT_pagelength,
                                      deferRender = TRUE,
                                      bLengthChange = FALSE,
                                      scrollX = 200,
                                      scrollY = 600,
                                      scroller = TRUE,
                                      orderClasses = TRUE,
                                      autoWidth=FALSE,
                                      columns.searchable=F,
                                      fixedColumns = list(leftColumns = 1),
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
output$viewExprsMissValues <- DT::renderDataTable(server=TRUE, {
  req(rv$current.obj)
  dt <- DT::datatable(as.data.frame(cbind(ID = rownames(Biobase::fData(rv$current.obj)),
                                          Biobase::exprs(rv$current.obj))),
                      extensions = c('Scroller', 'Buttons'),
                      rownames = FALSE,
                      
                      options=list(
                        buttons = list('copy',
                                       list(
                                         extend = 'csv',
                                         filename = 'missing values'
                                       ),'print'),
                        dom='Bfrtip',
                        orderClasses = TRUE,
                        autoWidth=FALSE,
                        bLengthChange = FALSE,
                        scrollX = 200,
                        scrollY =600,
                        scroller = TRUE,
                        columns.searchable=F,
                        autoWidth=TRUE,
                        
                        pageLength = DT_pagelength,
                        #columnDefs = list(list(columns.width=c("60px"),columnDefs.targets=c(list(0),list(1),list(2))))
                        columnDefs = list(list(width='150px',targets= "_all"))
                      )
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
  else{gradient <- input$expGradientRate }
  
  isolate({
    rv$tempplot$corrMatrix <- wrapper.corrMatrixD_HC(rv$current.obj,gradient)
    rv$tempplot$corrMatrix
  })
  
})


observeEvent(input$distance,{rv$PlotParams$heatmap.distance <- input$distance})
observeEvent(input$linkage,{rv$PlotParams$heatmap.linkage <- input$linkage})

heatmap <- reactive({
  
  req(rv$current.obj)
  input$linkage
  input$distance
  
  isolate({  wrapper.heatmapD(rv$current.obj,
                              input$distance, 
                              input$linkage,
                              TRUE)
  })
  
})








output$DS_PlotHeatmap <- renderUI({
  req(rv$current.obj)
  if (nrow(rv$current.obj) > limitHeatmap){
    tags$p("The dataset is too big to compute the heatmap in a reasonable time.")
  }else {
    tagList(
      withProgress(message = 'Building plot',detail = '', value = 1, {
        plotOutput("heatmap", width = "900px", height = "600px")
      })
      
    )
  }
})



#################
output$table <- DT::renderDataTable(server=TRUE, {
  req(rv$current.obj)
  df <- getDataForExprs(rv$current.obj)
  print(head(df))
  dt <- datatable( df,
                   extensions = c('Scroller', 'Buttons'),
                   options = list(
                     buttons = list('copy',
                                    list(
                                      extend = 'csv',
                                      filename = 'quantitation data'
                                    ),'print'),
                     dom='Bfrtip',initComplete = initComplete(),
                     displayLength = 20,
                     deferRender = TRUE,
                     bLengthChange = FALSE,
                     scrollX = 200,
                     scrollY = 600,
                     scroller = TRUE,
                     ordering=FALSE,
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
output$heatmap <- renderImage({
  # A temp file to save the output. It will be deleted after renderImage
  # sends it, because deleteFile=TRUE.
  outfile <- tempfile(fileext='.png')
  
  # Generate a png
  png(outfile, width=900, height=600)
  heatmap()
  dev.off()
  
  # Return a list
  list(src = outfile,
       alt = "This is alternate text")
}, deleteFile = TRUE)


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







