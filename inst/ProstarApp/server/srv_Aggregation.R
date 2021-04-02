callModule(moduleStaticDataTable,"overview_Aggregation", table2show=reactive({GetDatasetOverview()}),
           filename='Aggregation_overview')

callModule(moduleProcess, "moduleProcess_Aggregation", 
           isDone = reactive({rvModProcess$moduleAggregationDone}), 
           pages = reactive({rvModProcess$moduleAggregation}),
           rstFunc = resetModuleAggregation,
           forceReset = reactive({rvModProcess$moduleAggregationForceReset })  )




callModule(modulePopover,"modulePopover_includeShared", 
           data = reactive(list(title="Include shared peptides",
                                content= HTML(paste0("<ul><li><strong>No:</strong> only protein-specific peptides</li><li><strong>Yes 1:</strong> shared peptides processed as protein specific</li><li><strong>Yes 2</strong>: proportional redistribution of shared peptides</li></ul>")
                                )
           )
           )
)




resetModuleAggregation <- reactive({  
  ## update widgets values (reactive values)
  resetModuleProcess("Aggregation")
  
  
  
  rv$widgets$aggregation$includeSharedPeptides <- "Yes2"
  rv$widgets$aggregation$operator <- "Mean"
  rv$widgets$aggregation$considerPeptides <- 'allPeptides'
  rv$widgets$aggregation$proteinId <- "None"
  rv$widgets$aggregation$topN <- 3
  rv$widgets$aggregation$filterProtAfterAgregation <- NULL
  rv$widgets$aggregation$columnsForProteinDataset.box <- NULL
  rv$widgets$aggregation$nbPeptides <- 0
  
  rvModProcess$moduleAggregationDone = rep(FALSE, 3)
  ##update dataset to put the previous one
  rv$current.obj <- rv$dataset[[input$datasets]]
  ## reset temp object
  rv$temp.aggregate <- NULL
  
})


observeEvent(input$radioBtn_includeShared,ignoreInit = TRUE,{
  rv$widgets$aggregation$includeSharedPeptides <- input$radioBtn_includeShared
})

observeEvent(input$AggregationOperator,ignoreInit = TRUE,{
  rv$widgets$aggregation$operator <- input$AggregationOperator
})

observeEvent(input$AggregationConsider,ignoreInit = TRUE,{
  rv$widgets$aggregation$considerPeptides <- input$AggregationConsider
})


observeEvent(req(input$proteinId),{
  rv$proteinId <- input$proteinId
  rv$matAdj <- ComputeAdjacencyMatrices()
  ComputeConnexComposants()
  rv$widgets$aggregation$proteinId <- input$proteinId
})


observeEvent(input$nTopn,{
  rv$widgets$aggregation$topN <- input$nTopn
})


observeEvent(input$filterProtAfterAgregation,ignoreInit = TRUE,{
  rv$widgets$aggregation$filterProtAfterAgregation <- input$filterProtAfterAgregation
})


observeEvent(input$columnsForProteinDataset.box,{
  rv$widgets$aggregation$columnsForProteinDataset.box <- input$columnsForProteinDataset.box
})



observeEvent(input$nbPeptides,ignoreInit = TRUE,{
  rv$widgets$aggregation$nbPeptides <- input$nbPeptides
})


#-----------------------------------------------------
#
#              SCREEN 1
#
#-----------------------------------------------------
output$screenAggregation1 <- renderUI({
  
  tagList(
    shinyjs::useShinyjs(),
    uiOutput("warningAgregationMethod"),
    div(
      div( style="display:inline-block; vertical-align: top;",
           uiOutput("chooseProteinId")),
      div( style="display:inline-block; vertical-align: top;",       
           modulePopoverUI("modulePopover_includeShared"),
           radioButtons("radioBtn_includeShared", NULL, choices=
                          c("No" = "No",
                            "Yes (as protein specific)"= "Yes1" ,
                            "Yes (redistribution)" = "Yes2" ),
                        selected=rv$widgets$aggregation$includeSharedPeptides)),
      div( style="display:inline-block; vertical-align: top; padding-right: 10px;",
           radioButtons("AggregationConsider", "Consider", 
                        choices=c('all peptides'="allPeptides", 
                                  "N most abundant"="onlyN"), 
                        selected=rv$widgets$aggregation$considerPeptides)),
      div( style="display:inline-block; vertical-align: top; padding-right: 10px;",
           uiOutput('nTopn_widget')),
      
      div( style="display:inline-block; vertical-align: top;",
           uiOutput("operatorChoice")
      )
    ),
    actionButton("perform.aggregation","Perform aggregation", class = actionBtnClass),
    uiOutput("ObserverAggregationDone"),
    
    hr(),
    div(
      div( style="display:inline-block; vertical-align: top;",
           uiOutput("specificPeptideBarplot")),
      div( style="display:inline-block; vertical-align: top; padding-right: 20px;",       
           uiOutput("allPeptideBarplot")),
      div( style="display:inline-block; vertical-align: top;",
           DT::dataTableOutput("aggregationStats"))
    )
    
  )
  
  
})


output$warningAgregationMethod <- renderUI({
  req(rv$current.obj)
  
  if (length(which(is.na(Biobase::exprs(rv$current.obj)))) > 0)
  {
    tags$p(style = "color: red;",
           tags$b('Warning:')," Your dataset contains missing values.
    For better results, you should impute them first")
  }
  
})

output$nTopn_widget <- renderUI({
  req(rv$widgets$aggregation$considerPeptides)
  if (rv$widgets$aggregation$considerPeptides!='onlyN'){return(NULL)}
  numericInput("nTopn", "N",value = rv$widgets$aggregation$topN, min = 0, step=1, width='100px')
})


output$operatorChoice <- renderUI({
  rv$widgets$aggregation$includeSharedPeptides
  
  choice <- NULL
  if (rv$widgets$aggregation$includeSharedPeptides %in% c("No", "Yes1")){
    choice <- c("Mean"="Mean","Sum"="Sum")
  } else {choice <- c("Mean"="Mean")}
  choice
  
  radioButtons("AggregationOperator", "Operator", 
               choices=choice, 
               selected=rv$widgets$aggregation$operator)
})


observeEvent(rv$widgets$aggregation$includeSharedPeptides, {
  if (rv$widgets$aggregation$includeSharedPeptides=='Yes2'){
    ch <- c("Mean"="Mean")  
  } else {
    ch <- c("Sum"='Sum', "Mean"="Mean")
  }
  #updateRadioButtons(session,"AggregationOperator", choices=ch, selected=rv$widgets$aggregation$operator)
})




output$specificPeptideBarplot <- renderUI({
  req(rv$matAdj)
  withProgress(message = 'Rendering plot, pleast wait...',detail = '', value = 1, {
    tagList(
      h4("Only specific peptides"),
      plotOutput("aggregationPlotUnique", width="400px")
    )
  })
})

output$allPeptideBarplot <- renderUI({
  req(rv$matAdj)
  withProgress(message = 'Rendering plot, pleast wait...',detail = '', value = 1, {
    tagList(
      h4("All (specific & shared) peptides"),
      plotOutput("aggregationPlotShared", width="400px")
    )
  })
})


output$displayNbPeptides <- renderUI({
  req(rv$widgets$aggregation$filterProtAfterAgregation)
  
  if (rv$widgets$aggregation$filterProtAfterAgregation) {
    numericInput("nbPeptides", "Nb of peptides defining a protein", 
                 value = 0, min =0, step=1,
                 width = "250px")
  }
})

########################################################
RunAggregation <- reactive({
  req(rv$matAdj)
  rv$widgets$aggregation$includeSharedPeptides
  rv$widgets$aggregation$operator
  rv$widgets$aggregation$considerPeptides
  rv$widgets$aggregation$topN
  
  withProgress(message = '',detail = '', value = 0, {
    incProgress(0.2, detail = 'loading foreach package')
    
    
    require(foreach)
    incProgress(0.5, detail = 'Aggregation in progress')
    
    ll.agg <- NULL
    if(rv$widgets$aggregation$includeSharedPeptides %in% c("Yes2", "Yes1")){
      X <- rv$matAdj$matWithSharedPeptides
      if (rv$widgets$aggregation$includeSharedPeptides == 'Yes1'){
        if (rv$widgets$aggregation$considerPeptides == 'allPeptides') {
          ll.agg <- do.call(paste0('aggregate',rv$widgets$aggregation$operator),
                            list( obj.pep = rv$current.obj,X=X))
        } else {
          ll.agg <- aggregateTopn(rv$current.obj, 
                                  X,
                                  rv$widgets$aggregation$operator, 
                                  n = as.numeric(rv$widgets$aggregation$topN))
        }
      } else {
        if (rv$widgets$aggregation$considerPeptides == 'allPeptides') {
          ll.agg <- aggregateIterParallel(rv$current.obj, 
                                          X,
                                          init.method='Sum', 
                                          method='Mean')
        } else {
          ll.agg <- aggregateIterParallel(rv$current.obj, 
                                          X, 
                                          init.method='Sum', 
                                          method='onlyN', 
                                          n = rv$widgets$aggregation$topN)
        }
      }
    } else {
      X <- rv$matAdj$matWithUniquePeptides
      if (rv$widgets$aggregation$considerPeptides == 'allPeptides') {
        ll.agg <- do.call(paste0('aggregate',rv$widgets$aggregation$operator),
                          list(obj.pep = rv$current.obj,
                               X = X))
      } else {
        ll.agg <- aggregateTopn(rv$current.obj, 
                                X, 
                                rv$widgets$aggregation$operator,
                                n = as.numeric(rv$widgets$aggregation$topN)
        )
      }
    }
  } )
  
  
  
  return(ll.agg)
  
})

#-----------------------------------------------------
#
#              SCREEN 2
#
#-----------------------------------------------------
output$screenAggregation2 <- renderUI({
  tagList(
    uiOutput(outputId = "progressSaveAggregation"),
    withProgress(message = '',detail = '', value = 0, {
      incProgress(0.5, detail = 'Aggregation in progress')
      uiOutput("Aggregation_Step2")
    })
  )
})






#-----------------------------------------------------
#
#              SCREEN 3
#
#-----------------------------------------------------
output$screenAggregation3 <- renderUI({
  tagList(
    shinyjs::useShinyjs(),
    h4("Once the saving operation is done, the new current dataset is a protein dataset.
       Prostar will automatically switch to the home page with the new dataset."),
    #shinyjs::disabled(
      actionButton("valid.aggregation",
                   "Save aggregation", 
                   class = actionBtnClass)
   # )
  )
})






##' -- Validate the aggregation ---------------------------------------
##' @author Samuel Wieczorek
observeEvent(input$valid.aggregation,{ 
  
  req(rv$matAdj)
  req(rv$temp.aggregate$obj.prot)
  req(length(rv$temp.aggregate$issues) == 0)
  
  isolate({
    withProgress(message = '',detail = '', value = 0, {
      
      X <- NULL
      if(rv$widgets$aggregation$includeSharedPeptides %in% c("Yes2", "Yes1")){
        X <- rv$matAdj$matWithSharedPeptides}
      else { X <- rv$matAdj$matWithUniquePeptides}
      
      total <- 60
      delta <- round(total / length(rv$widgets$aggregation$columnsForProteinDataset.box))
      cpt <- 10
      
      for(c in rv$widgets$aggregation$columnsForProteinDataset.box){
        newCol <- BuildColumnToProteinDataset(
          Biobase::fData(rv$current.obj), X, c, rownames(Biobase::fData(rv$temp.aggregate$obj.prot)))
        cnames <- colnames(Biobase::fData(rv$temp.aggregate$obj.prot))
        Biobase::fData(rv$temp.aggregate$obj.prot) <- 
          data.frame(Biobase::fData(rv$temp.aggregate$obj.prot), newCol)
        
        colnames(Biobase::fData(rv$temp.aggregate$obj.prot)) <- c(cnames, c)
        
        cpt <- cpt + delta
        incProgress(cpt/100, detail = paste0('Processing column ', c))
      }
      rv$current.obj <- rv$temp.aggregate$obj.prot
      

      rv$typeOfDataset <- rv$current.obj@experimentData@other$typeOfData
     # rv$current.obj <- DAPAR::addOriginOfValue(rv$current.obj, NULL)
      
      name <- paste0("Aggregated", ".", rv$typeOfDataset)
      rv$current.obj <- saveParameters(rv$current.obj, name,"Aggregation",build_ParamsList_Aggregation())
      
      rv$dataset[[name]] <- rv$current.obj
      rvModProcess$moduleAggregationDone[3] <- TRUE
      updateSelectInput(session, "datasets",  choices = names(rv$dataset), selected = name)
      
    })
  })
})


# observeEvent(rv$temp.aggregate$issues, {
#   
#   shinyjs::toggleState('valid.aggregation',
#                        condition = length(rv$temp.aggregate$issues) == 0)
# })


#-----------------------------------------------
output$ObserverAggregationDone <- renderUI({
  req(rv$temp.aggregate)
  #req(input$perform.aggregation)

  #browser()
  if (length(rv$temp.aggregate$issues) > 0){
    .style = "color: red;"
    txt <- 'The aggregation process did not succeed because some sets of peptides contains missing values and quantitative
       values at the same time.'
  }
  else {
    txt <- "Aggregation done"
    .style = ""
  }

 
  tags$h3(style = .style, txt)
})



output$aggregationStats <- DT::renderDataTable (server=TRUE,{
  req(rv$matAdj)
  if (is.null(rv$widgets$aggregation$proteinId) || rv$widgets$aggregation$proteinId == "None") {return(NULL)}
  
  res <- getProteinsStats(rv$matAdj$matWithSharedPeptides)
  #print(res)
  rv$AggregProtStats$nb <- c(res$nbPeptides,
                             res$nbSpecificPeptides,
                             res$nbSharedPeptides,
                             res$nbProt,
                             length(res$protOnlyUniquePep),
                             length(res$protOnlySharedPep),
                             length(res$protMixPep))
  
  df <- as.data.frame(rv$AggregProtStats)
  names(df) <- c('Description', 'Value')
  DT::datatable(df, 
                escape = FALSE,
                rownames= FALSE,
                extensions = c('Scroller', 'Buttons'),
                option=list(initComplete = initComplete(),
                            buttons = list('copy',
                                           list(
                                             extend = 'csv',
                                             filename = 'aggregation stats'
                                           ),'print'),
                            dom='Brt',
                            autoWidth=TRUE,
                            ordering=F,
                            columnDefs = list(list(width='150px',targets= 0),
                                              list(width='100px',targets= 1))
                )
  )
})

output$aggregationPlotShared <- renderPlot({
  req(rv$matAdj)
  GraphPepProt(rv$matAdj$matWithSharedPeptides)
})


output$aggregationPlotUnique <- renderPlot({
  req(rv$matAdj)
  
  GraphPepProt(rv$matAdj$matWithUniquePeptides)
})



###------------ Perform aggregation--------------------
observeEvent(input$perform.aggregation,{
  
  #isolate({
  #browser()
  rv$temp.aggregate <- RunAggregation()
 # browser()
  rvModProcess$moduleAggregationDone[1] <- length(rv$temp.aggregate$issues) == 0
  shinyjs::toggleState('valid.aggregation',
                       condition = length(rv$temp.aggregate$issues) == 0)
  #})
})



callModule(modulePopover,"modulePopover_colsForAggreg", 
           data = reactive(list(title= "Columns of the meta-data",
                                content= "Select the columns of the meta-data (related to proteins) that have to be recorded in the new protein dataset (e.g. the columns which contains the protein ID if you wish to perform a GO analysis.)")))


## -----------------------------------------------
## Second screen of aggregation tool
## -----------------------------------------------
output$Aggregation_Step2 <- renderUI({
  
  req(rv$current.obj)
  
  #if (rv$current.obj@experimentData@other$typeOfData == typePeptide) {
  choices <- colnames(Biobase::fData(rv$current.obj))
  names(choices) <- colnames(Biobase::fData(rv$current.obj))
  tagList(
    uiOutput("displayNbPeptides"),
    
    div(
      div(
        style="display:inline-block; vertical-align: middle; padding-right: 20px;",
        modulePopoverUI("modulePopover_colsForAggreg")
      ),
      div(
        style="display:inline-block; vertical-align: middle;",
        selectInput("columnsForProteinDataset.box",
                    label = "",
                    choices = choices,
                    multiple = TRUE, width='200px',
                    #size = 10,
                    selectize = TRUE)
      )
    )
  )
  
  # } else {
  #   tagList(
  #     h4("The peptide dataset has been aggregated into a protein dataset."),
  #     tags$div(style="align: center;",
  #              moduleStaticDataTableUI("overview_Aggregation")
  #     )
  #   )
  # }
  
})


observe({
  rv$widgets$aggregation$columnsForProteinDataset.box
  
  if (length(rv$widgets$aggregation$columnsForProteinDataset.box) > 0){
    rvModProcess$moduleAggregationDone[2] <- TRUE
  } else {
    rvModProcess$moduleAggregationDone[2] <- FALSE
  }
})







######################################################### 
output$columnsForProteinDataset <- renderUI({
  req(rv$current.obj)
  
  choices <- colnames(Biobase::fData(rv$current.obj))
  names(choices) <- colnames(Biobase::fData(rv$current.obj))
  
  selectInput("columnsForProteinDataset.box",
              label = "",
              choices = choices,
              multiple = TRUE, width='200px',
              size = 20,
              selectize = FALSE)
  
})






output$chooseProteinId <- renderUI({
  if (!is.null(rv$current.obj@experimentData@other$proteinId)) {return(NULL)}
  
  selectInput("proteinId", 
              "Choose the protein ID",
              choices = c("None",colnames(Biobase::fData(rv$current.obj))),
              selected = rv$widgets$aggregation$proteinId)
})
