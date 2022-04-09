# mod_staticDT_server("overview_Aggregation",
#                     data = reactive({GetDatasetOverview()}),
#                     filename = 'Aggregation_overview')

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
  # # Get back to previous dataset
  # if (length(grep("Aggregated", names(rv$dataset))) > 0){
  #   i <- grep("Aggregated", names(rv$dataset))
  #   rv$dataset <- rv$dataset[1:(i-1)]
  #   updateSelectInput(session, 
  #                     'datasets', 
  #                     choices = names(rv$dataset),
  #                     selected = names(rv$dataset)[length(names(rv$dataset))]
  #   )
  # }
  # 
  # rv$current.obj <- rv$dataset[[length(names(rv$dataset))]] 
  
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
  #browser()
  rv$proteinId <- input$proteinId
  rv$current.obj <- SetMatAdj(rv$current.obj, ComputeAdjacencyMatrices())
  rv$current.obj <- SetCC(rv$current.obj, ComputeConnectedComposants())
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
                        selected = rv$widgets$aggregation$includeSharedPeptides)),
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
    shinyjs::hidden(downloadButton('downloadAggregationIssues', 'Download issues', class = actionBtnClass)),
    
    hr(),
    div(
      div( style="display:inline-block; vertical-align: top;",
           uiOutput("specificPeptideBarplot")),
      div( style="display:inline-block; vertical-align: top; padding-right: 20px;",       
           uiOutput("allPeptideBarplot")),
      div( style="display:inline-block; vertical-align: top;",
           tagList(
             DT::dataTableOutput("aggregationStats")
           )
      )
    )
    
  )
  
  
})


output$warningAgregationMethod <- renderUI({
  req(rv$current.obj)
  
  m <- match.metacell(DAPAR::GetMetacell(rv$current.obj), 
                      pattern = "missing", 
                      level = 'peptide')
  #browser()
  if (length(which(m)) > 0)
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
  req(DAPAR::GetMatAdj(rv$current.obj))
  withProgress(message = 'Rendering plot, pleast wait...',detail = '', value = 1, {
    tagList(
      h4("Only specific peptides"),
      plotOutput("aggregationPlotUnique", width="400px")
    )
  })
})

output$allPeptideBarplot <- renderUI({
  req(DAPAR::GetMatAdj(rv$current.obj))
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
  if (! requireNamespace("foreach", quietly = TRUE)) {
    stop("Please install foreach: BiocManager::install('foreach')")
  }
  
  req(DAPAR::GetMatAdj(rv$current.obj))
  rv$widgets$aggregation$includeSharedPeptides
  rv$widgets$aggregation$operator
  rv$widgets$aggregation$considerPeptides
  rv$widgets$aggregation$topN
  
  withProgress(message = '',detail = '', value = 0, {
    incProgress(0.2, detail = 'loading foreach package')
    
    
    incProgress(0.5, detail = 'Aggregation in progress')

    ll.agg <- NULL
    if(rv$widgets$aggregation$includeSharedPeptides %in% c("Yes2", "Yes1")){
      X <- DAPAR::GetMatAdj(rv$current.obj)$matWithSharedPeptides
      if (rv$widgets$aggregation$includeSharedPeptides == 'Yes1'){
        if (rv$widgets$aggregation$considerPeptides == 'allPeptides') {
          ll.agg <- do.call(paste0('aggregate', rv$widgets$aggregation$operator),
                            list( obj.pep = rv$current.obj,X=X))
        } else {
          ll.agg <- aggregateTopn(rv$current.obj, 
                                  X,
                                  rv$widgets$aggregation$operator, 
                                  n = as.numeric(rv$widgets$aggregation$topN))
        }
      } else {
        if (rv$widgets$aggregation$considerPeptides == 'allPeptides') {
          ll.agg <- aggregateIterParallel(obj.pep = rv$current.obj, 
                                          X = X,
                                          init.method = 'Sum', 
                                          method = 'Mean')
        } else {
          ll.agg <- aggregateIterParallel(rv$current.obj, 
                                          X, 
                                          init.method = 'Sum', 
                                          method = 'onlyN', 
                                          n = rv$widgets$aggregation$topN)
        }
      }
    } else {
      X <- DAPAR::GetMatAdj(rv$current.obj)$matWithUniquePeptides
      if (rv$widgets$aggregation$considerPeptides == 'allPeptides') {
        ll.agg <- do.call(paste0('aggregate', rv$widgets$aggregation$operator),
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




###------------ Perform aggregation--------------------
observeEvent(input$perform.aggregation,{
  
  rv$temp.aggregate <- RunAggregation()
  rvModProcess$moduleAggregationDone[1] <- is.null(rv$temp.aggregate$issues)
  # shinyjs::toggleState('validAggregation',
  #                      condition = is.null(rv$temp.aggregate$issues))
  # shinyjs::toggle('downloadAggregationIssues', 
  #                 condition = !is.null(rv$temp.aggregate$issues) && length(rv$temp.aggregate$issues) > 0
  # )
})

observe({
  rvModProcess$moduleAggregationDone[1]
  # shinyjs::toggleState('validAggregation',
  #                      condition = rvModProcess$moduleAggregationDone[1])
  shinyjs::toggle('downloadAggregationIssues', 
                  condition = !rvModProcess$moduleAggregationDone[1] && length(rv$temp.aggregate$issues) > 0
  )
})

output$downloadAggregationIssues <- downloadHandler(
  
  filename = 'aggregation_issues.txt',
  content = function(file) {
    
    tmp.peptides <- lapply(rv$temp.aggregate$issues, function(x)paste0(x, collapse=","))
    df <- data.frame(Proteins=names(rv$temp.aggregate$issues), Peptides = as.data.frame(do.call(rbind, tmp.peptides)))
    colnames(df) <- c('Proteins', 'Peptides')
    write.table(df, file = file, row.names = FALSE, quote=FALSE, sep="\t")
  }
)


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
    h4("Once the saving operation is done, the new current dataset is a protein dataset.
       Prostar will automatically switch to the home page with the new dataset."),
    uiOutput("showValidAggregationBtn_ui")
  )
})


output$showValidAggregationBtn_ui <- renderUI({
  req(rvModProcess$moduleAggregationDone[1])
  actionButton("validAggregation",
               "Save aggregation", 
               class = actionBtnClass)
})



##' -- Validate the aggregation ---------------------------------------
##' @author Samuel Wieczorek
observeEvent(input$validAggregation,{ 
  
  req(DAPAR::GetMatAdj(rv$current.obj))
  req(rv$temp.aggregate$obj.prot)
  req(is.null(rv$temp.aggregate$issues))
  
  isolate({
    withProgress(message = '',detail = '', value = 0, {
      
      X <- NULL
      if(rv$widgets$aggregation$includeSharedPeptides %in% c("Yes2", "Yes1"))
        X <- DAPAR::GetMatAdj(rv$current.obj)$matWithSharedPeptides
      else
        X <- DAPAR::GetMatAdj(rv$current.obj)$matWithUniquePeptides
      
      total <- 60
      delta <- round(total / length(rv$widgets$aggregation$columnsForProteinDataset.box))
      cpt <- 10
      
      for(c in rv$widgets$aggregation$columnsForProteinDataset.box){
        newCol <- BuildColumnToProteinDataset(peptideData = fData(rv$current.obj), 
                                              matAdj = X,
                                              columnName = c, 
                                              proteinNames = rownames(fData(rv$temp.aggregate$obj.prot))
                                              )
        cnames <- colnames(fData(rv$temp.aggregate$obj.prot))
        fData(rv$temp.aggregate$obj.prot) <- 
          data.frame(fData(rv$temp.aggregate$obj.prot), newCol)
        
        colnames(fData(rv$temp.aggregate$obj.prot)) <- c(cnames, paste0('agg_',c))
        
        cpt <- cpt + delta
        incProgress(cpt/100, detail = paste0('Processing column ', c))
      }
      br
      rv$current.obj <- rv$temp.aggregate$obj.prot
      

      rv$typeOfDataset <- rv$current.obj@experimentData@other$typeOfData
     # rv$current.obj <- DAPAR::addOriginOfValue(rv$current.obj, NULL)
      
      name <- paste0("Aggregated", ".", rv$typeOfDataset)
      rv$current.obj <- saveParameters(rv$current.obj,
                                       name,
                                       "Aggregation",
                                       build_ParamsList_Aggregation())
      
      rv$dataset[[name]] <- rv$current.obj
      rvModProcess$moduleAggregationDone[3] <- TRUE
      updateSelectInput(session, "datasets",  choices = names(rv$dataset), selected = name)
      
    })
  })
})

#-----------------------------------------------
output$ObserverAggregationDone <- renderUI({
  req(rv$temp.aggregate)

  if (!is.null(rv$temp.aggregate$issues) && length(rv$temp.aggregate$issues) > 0){
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
  req(DAPAR::GetMatAdj(rv$current.obj))
  req(rv$widgets$aggregation$proteinId != "None")
  
  res <- getProteinsStats(DAPAR::GetMatAdj(rv$current.obj)$matWithSharedPeptides)

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
                extensions = c('Scroller'),
                option=list(initComplete = initComplete(),
                            dom = 'rt',
                            autoWidth = TRUE,
                            ordering = F,
                            columnDefs = list(list(width='150px', targets= 0),
                                              list(width='100px', targets= 1))
                )
  )
})

output$aggregationPlotShared <- renderPlot({
  req(DAPAR::GetMatAdj(rv$current.obj))
  GraphPepProt(DAPAR::GetMatAdj(rv$current.obj)$matWithSharedPeptides)
})


output$aggregationPlotUnique <- renderPlot({
  req(DAPAR::GetMatAdj(rv$current.obj))
  GraphPepProt(DAPAR::GetMatAdj(rv$current.obj)$matWithUniquePeptides)
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
  ind <-  match(rv$current.obj@experimentData@other$names_metacell, colnames(fData(rv$current.obj)))
  choices <- setNames(nm = colnames(fData(rv$current.obj))[-ind])

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
                    multiple = TRUE, 
                    width='200px',
                    #size = 10,
                    selectize = TRUE)
      )
    )
  )

})


observe({
  rv$widgets$aggregation$columnsForProteinDataset.box
  ll <- length(rv$widgets$aggregation$columnsForProteinDataset.box) > 0
  rvModProcess$moduleAggregationDone[2] <- ll
})







######################################################### 
output$columnsForProteinDataset <- renderUI({
  req(rv$current.obj)
  
  choices <- colnames(fData(rv$current.obj))
  names(choices) <- colnames(fData(rv$current.obj))
  
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
              choices = c("None", colnames(fData(rv$current.obj))),
              selected = rv$widgets$aggregation$proteinId)
})
