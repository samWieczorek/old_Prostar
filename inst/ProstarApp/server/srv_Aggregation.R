callModule(moduleStaticDataTable,"overview_Aggregation", table2show=reactive({GetDatasetOverview()}))




# output$Aggreg_Aggreg <- renderUI({
#   splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
#               uiOutput("AggregationSideBar_Step1"),
#               tagList(uiOutput("AggregationWellPanel_Step1")
#               )
#   )
# })


output$Aggreg_Valid <- renderUI({
  tagList(
    uiOutput(outputId = "progressSaveAggregation"),
    busyIndicator(WaitMsgCalc,wait = 0),
    uiOutput("Aggregation_Step2")
  )
})



observeEvent(input$AggregationConsider,{
  shinyjs::toggle('nTopn', condition=input$AggregationConsider=='onlyN')
})

observeEvent(input$radioBtn_includeShared, {
  if (input$radioBtn_includeShared=='Yes2'){
    ch <- c("Mean"="Mean")  
  } else {
      ch <- c("Sum"='Sum', "Mean"="Mean")
      }
  updateRadioButtons(session,"AggregationOperator", choices=ch, selected='Mean')
})

########################################################
RunAggregation <- reactive({
    req(rv$matAdj)
    input$radioBtn_includeShared
    input$AggregationOperator
    input$AggregationConsider
    input$nTopn
    
   require(foreach)
    obj.prot <- NULL
    if(input$radioBtn_includeShared %in% c("Yes2", "Yes1")){
      X <- rv$matAdj$matWithSharedPeptides
      if (input$radioBtn_includeShared == 'Yes1'){
          if (input$AggregationConsider == 'allPeptides') {
              obj.prot <- do.call(paste0('aggregate',input$AggregationOperator),list( obj.pep=rv$current.obj,X=X))
          } else {
            obj.prot <- aggregate.topn(X, rv$current.obj, n=as.numeric(input$nTopn), input$AggregationOperator)
          }
      } else {
        if (input$AggregationConsider == 'allPeptides') {
          obj.prot <- aggregateIterParallel(rv$current.obj, X,init.method='Sum', method='Mean')
        } else {
          obj.prot <- aggregateIterParallel(rv$current.obj, X, init.method='Sum', method='onlyN', n=input$nTopn)
        }
      }
    } else {
      X <- rv$matAdj$matWithUniquePeptides
      if (input$AggregationConsider == 'allPeptides') {
        obj.prot <- do.call(paste0('aggregate',input$AggregationOperator),list(obj.pep=rv$current.obj,X=X))
      } else {
        obj.prot <- aggregateTopn(rv$current.obj, X,n=input$nTopn, input$AggregationOperator)
      }
    }
        
    return(obj.prot)

})




##' -- Validate the aggregation ---------------------------------------
##' @author Samuel Wieczorek
observeEvent(input$valid.aggregation,{ 
  # input$nbPeptides
  #input$filterProtAfterAgregation
  #input$aggregationMethod
  #input$columnsForProteinDataset.box
  req(rv$matAdj)
  req(rv$temp.aggregate)
  
  isolate({
    X <- NULL
    if(input$radioBtn_includeShared %in% c("Yes2", "Yes1")){
      X <- rv$matAdj$matWithSharedPeptides}
    else { X <- rv$matAdj$matWithUniquePeptides}
    
   
    updateSelectInput(session, "proteinId",selected = input$proteinId)
    updateRadioButtons(session, "AggregationOperator",selected = input$AggregationOperator)
    updateRadioButtons(session, "AggregationConsider",selected = input$AggregationConsider)
    updateSelectInput(session, "nTopn",selected = input$nTopn)
    updateRadioButtons(session,"radioBtn_includeShared",input$radioBtn_includeShared)
    
    l.params <- list(includeSharedPeptides = input$radioBtn_includeShared,
                     operator = input$AggregationOperator,
                     considerPeptides = input$AggregationConsider,
                     proteinId = input$proteinId,
                     topN = input$nTopn
    )
    
    #total <- 60
    #delta <- round(total / length(input$columnsForProteinDataset.box))
    #cpt <- 10
    for(c in input$columnsForProteinDataset.box){
      newCol <- BuildColumnToProteinDataset(
        Biobase::fData(rv$current.obj), X, c, rownames(Biobase::fData(rv$temp.aggregate)))
      cnames <- colnames(Biobase::fData(rv$temp.aggregate))
      Biobase::fData(rv$temp.aggregate) <- 
        data.frame(Biobase::fData(rv$temp.aggregate), newCol)
      colnames(Biobase::fData(rv$temp.aggregate)) <- c(cnames, c)
      #cpt <- cpt + delta
      #updatePB(session,inputId="pb_SaveAggregation",value=cpt,text_value=paste(cpt," %", sep=""), striped = TRUE, active=TRUE)
    }
    
    rv$current.obj <- rv$temp.aggregate
    #rv$temp.aggregate <- NULL
    rv$current.obj@experimentData@other$Prostar_Version <- 
      installed.packages(lib.loc = Prostar.loc)["Prostar","Version"]
    rv$current.obj@experimentData@other$DAPAR_Version <- 
      installed.packages(lib.loc = DAPAR.loc)["DAPAR","Version"]
    rv$typeOfDataset <- rv$current.obj@experimentData@other$typeOfData
    
    name <- paste0("Aggregated", ".", rv$typeOfDataset)
    rv$current.obj <- saveParameters(rv$current.obj, name,"Aggregation",l.params)
    
    rv$dataset[[name]] <- rv$current.obj
    
    #updatePB(session,inputId="pb_SaveAggregation",value=70,text_value="70 %", striped = TRUE, active=TRUE)
    #updatePB(session,inputId="pb_SaveAggregation",value=90,text_value="90 %", striped = TRUE, active=TRUE)
    #}
    
    #updateNavbarPage (session, "navPage", selected = "Descriptive statistics")
    updateSelectInput(session, "datasets",  choices = names(rv$dataset), selected = name)
    BuildNavbarPage()
    
    rv$pageAggreg <- 2
    shinyjs::hide('prevBtnConvert')
    shinyjs::hide('nextBtnConvert')
    
    
  })
  
  
})



#-----------------------------------------------
output$ObserverAggregationDone <- renderUI({
  req(rv$temp.aggregate)
  req(input$perform.aggregation)
  isolate({
     h3("Aggregation done with the ")

  })
})





observeEvent(input$proteinId,{
  req(input$proteinId)
  if (input$proteinId == "None"){return(NULL)}
  rv$proteinId <-input$proteinId  
  rv$matAdj <- ComputeAdjacencyMatrices()
  })



output$aggregationStats <- renderDataTable ({
  req(input$proteinId)
  req(rv$current.obj)
  req(rv$matAdj)
  if ((input$proteinId == "None")) {return(NULL)}
  
  res <- getProteinsStats(rv$matAdj$matWithUniquePeptides, 
                          rv$matAdj$matWithSharedPeptides)
  
  rv$AggregProtStats$nb <- c(nrow(rv$matAdj$matWithSharedPeptides),
                             nrow(rv$matAdj$matWithUniquePeptides),
                             nrow(rv$matAdj$matWithSharedPeptides)-nrow(rv$matAdj$matWithUniquePeptides),
                             ncol(rv$matAdj$matWithSharedPeptides),
                             length(res$protOnlyUniquePep),
                             length(res$protOnlySharedPep),
                             length(res$protMixPep))
  
  df <- as.data.frame(rv$AggregProtStats)
  names(df) <- c('Description', 'Value')
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
  
  isolate({
      rv$temp.aggregate <- RunAggregation()

  })
})

output$AggregationWellPanel_Step1 <- renderUI({
  req(rv$current.obj)
  
  tagList(
    
    fluidRow(
      column(width=6, uiOutput("specificPeptideBarplot")),
      column(width=6, uiOutput("allPeptideBarplot"))
    ),
    dataTableOutput("aggregationStats"),
    uiOutput("ObserverAggregationDone")
  )
  
})



output$specificPeptideBarplot <- renderUI({
  req(rv$matAdj)
  tagList(
    h4("Only specific peptides"),
    plotOutput("aggregationPlotUnique") %>% withSpinner(type=spinnerType)
  )
})

output$allPeptideBarplot <- renderUI({
  req(rv$matAdj)
  tagList(
    h4("All (specific & shared) peptides"),
    plotOutput("aggregationPlotShared") %>% withSpinner(type=spinnerType)
  )
})


output$displayNbPeptides <- renderUI({
  req(input$filterProtAfterAgregation)
  
  if (input$filterProtAfterAgregation) {
    numericInput("nbPeptides", "Nb of peptides defining a protein", 
                 value = 0, min =0, step=1,
                 width = "250px")
  }
})


callModule(modulePopover,"modulePopover_colsForAggreg", 
           data = reactive(list(title=HTML(paste0("<strong><font size=\"4\">Columns of the meta-data</font></strong>")),
                                content= "Select the columns of the meta-data (related to proteins) that have to be recorded in the new protein dataset (e.g. the columns which contains the protein ID if you wish to perform a GO analysis.)")))



output$Aggregation_Step2 <- renderUI({
  
  req(rv$current.obj)
  
  if (rv$current.obj@experimentData@other$typeOfData == typePeptide) {
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
                      size = 10,
                      selectize = FALSE)
        )
      ) ,
      actionButton("valid.aggregation","Save aggregation", sclass = actionBtnClass)
    )
    
  } else {
    tagList(
      h4("The peptide dataset has been aggregated into a protein dataset."),
      tags$div(style="align: center;",
               moduleStaticDataTableUI("overview_Aggregation")
      )
    )
  }
  
})





output$warningAgregationMethod <- renderUI({
  rv$current.obj
  if (is.null(rv$current.obj)) {return (NULL)}
  
  if (length(which(is.na(Biobase::exprs(rv$current.obj)))) > 0)
  {
    text <- "<font color=\"red\"> Warning ! <br> 
    Your dataset contains missing values.
    <br> For better results, you should impute  <br> them first"
    HTML(text)
  }
  
})


buildWritableVector <- function(v){
  t <- "c("
  for (i in v){
    t <- paste(t, "\"", as.character(i), "\"", sep="")
    if (i == last(v)) {t <- paste(t, ")", sep="")}
    else {t <- paste(t, ",", sep="")}
  }
  return(t)
}



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




######################################################### 

output$chooseProteinId <- renderUI({
  if (!is.null(rv$current.obj@experimentData@other$proteinId)) {return(NULL)}
  
  selectInput("proteinId", 
              "Choose the protein ID",
              choices = c("None",colnames(Biobase::fData(rv$current.obj))))
})
