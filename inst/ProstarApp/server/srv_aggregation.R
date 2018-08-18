NUM_PAGES_AGGREG <- 2


output$checkAggregPanel <- renderUI({
  rv$pageAggreg
  color <- rep("lightgrey",NUM_PAGES_AGGREG)
  
  ##Step 1
  if (rv$pageAggreg >= 1){
    res <- !is.null(rv$temp.aggregate)
    ifelse(res, color[1] <- "green", color[1] <- "red")
  }
  
  ##Step 2: Choose data ID
  
  if (rv$pageAggreg >= 2){
    res <- length(grep("Aggregated",input$datasets))
    ifelse(res, color[2] <- "green", color[2] <- "red")
    
  } 
  
  txt <- c("Aggregation", "Validation")
  buildTable(txt, color)
})


observe({
  toggleState(id = "prevBtnAggreg", condition = rv$pageAggreg > 1)
  toggleState(id = "nextBtnAggreg", condition = rv$pageAggreg < NUM_PAGES_AGGREG)
  hide(selector = ".page")
})

navPageAggreg <- function(direction) {
  rv$pageAggreg <- rv$pageAggreg + direction
}

observeEvent(input$prevBtnAggreg, navPageAggreg(-1))
observeEvent(input$nextBtnAggreg, navPageAggreg(1))



output$Aggreg_Aggreg <- renderUI({
  if (rv$pageAggreg != 1){return()}
  splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
              uiOutput("AggregationSideBar_Step1"),
              tagList(uiOutput("AggregationWellPanel_Step1")
              )
  )
})

callModule(moduleDatasetOverview,"overview_Aggregation")

output$Aggreg_Valid <- renderUI({
  if (rv$pageAggreg != 2){return()}
  tagList(
    uiOutput(outputId = "progressSaveAggregation"),
  busyIndicator(WaitMsgCalc,wait = 0),
  uiOutput("Aggregation_Step2")
  )
})



########################################################
RunAggregation <- reactive({
    req(rv$matAdj)
    
    n <- NULL
    if (input$aggregationMethod == gAgregateMethod[["sum on top n"]]) { 
        n <- as.numeric(input$nTopn)
        }

    if (input$checkSharedPeptides){
                data <- pepAgregate(rv$current.obj, 
                                    input$proteinId,
                                    input$aggregationMethod, 
                                    rv$matAdj$matWithSharedPeptides, 
                                    n)
            }else{
                data <- pepAgregate(rv$current.obj, 
                                    input$proteinId,
                                    input$aggregationMethod, 
                                    rv$matAdj$matWithUniquePeptides
                                    , n)
            }
            
            return(data)

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
        m <- NULL
        if (input$checkSharedPeptides){ 
            m <- rv$matAdj$matWithSharedPeptides
        }else{
            m <-rv$matAdj$matWithUniquePeptides
        }

        updateSelectInput(session, "proteinId",selected = input$proteinId)
        updateSelectInput(session, "aggregationMethod",selected = input$aggregationMethod)
        updateSelectInput(session, "nTopn",selected = input$nTopn)
        updateCheckboxInput(session,"checkSharedPeptides",input$checkSharedPeptides)
                
        l.params <- list(withSharedPeptides = input$checkSharedPeptides,
                                 agregMethod = input$aggregationMethod,
                                 proteinId = input$proteinId,
                                 topN = input$nTopn
                    )
                
                #total <- 60
                #delta <- round(total / length(input$columnsForProteinDataset.box))
                #cpt <- 10
                for(c in input$columnsForProteinDataset.box){
                    newCol <- BuildColumnToProteinDataset(
                        Biobase::fData(rv$current.obj), m, c, rownames(Biobase::fData(rv$temp.aggregate)))
                    cnames <- colnames(Biobase::fData(rv$temp.aggregate))
                    Biobase::fData(rv$temp.aggregate) <- 
                        data.frame(Biobase::fData(rv$temp.aggregate), newCol)
                    colnames(Biobase::fData(rv$temp.aggregate)) <- c(cnames, c)
                    #cpt <- cpt + delta
                    #updatePB(session,inputId="pb_SaveAggregation",value=cpt,text_value=paste(cpt," %", sep=""), striped = TRUE, active=TRUE)
                }
                
                rv$current.obj <- rv$temp.aggregate
                #rv$temp.aggregate <- NULL
                rv$current.obj <- saveParameters(rv$current.obj, "Aggregation",l.params)
                rv$current.obj@experimentData@other$Prostar_Version <- 
                  installed.packages(lib.loc = Prostar.loc)["Prostar","Version"]
                rv$current.obj@experimentData@other$DAPAR_Version <- 
                  installed.packages(lib.loc = DAPAR.loc)["DAPAR","Version"]
                rv$typeOfDataset <- rv$current.obj@experimentData@other$typeOfData
                
                name <- paste ("Aggregated", " - ", rv$typeOfDataset, sep="")
                rv$dataset[[name]] <- rv$current.obj
                
                UpdateLog("Aggregation", l.params)
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


observeEvent(input$aggregationMethod,{
  
  toggle(id = "nTopn", 
         condition = input$aggregationMethod == gAgregateMethod[["sum on top n"]])
})



#-----------------------------------------------
output$ObserverAggregationDone <- renderUI({
    req(rv$temp.aggregate)
    req(input$perform.aggregation)
    isolate({
       if (input$aggregationMethod != "None"){
            h3(paste("Aggregation done with the ",input$aggregationMethod, " method.",  sep=""))
        }
    })
})





observeEvent(input$proteinId,{
    
    rv$current.obj
    
    result = tryCatch(
        {
            matSharedPeptides <- BuildAdjacencyMatrix(rv$current.obj,  input$proteinId,FALSE)
            matUniquePeptides <- BuildAdjacencyMatrix(rv$current.obj, input$proteinId,TRUE)
            
            rv$matAdj <- list(matWithSharedPeptides=matSharedPeptides, matWithUniquePeptides=matUniquePeptides)
            
        }
    , error = function(e) {
        shinyjs::info(paste("Build adjacency matrix:",
                            conditionMessage(e), 
                            sep=" "))
    }, finally = {
        #cleanup-code 
    })
    
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
  if (input$aggregationMethod != "None")
                {rv$temp.aggregate <- RunAggregation()
     }
     
    })
})









output$AggregationSideBar_Step1 <-  renderUI({
    
    wellPanel(id = "sidebar_Aggregation",
              height = "100%",
              tagList(
                  uiOutput("warningAgregationMethod"),
                  uiOutput("chooseProteinId"),
                  checkboxInput("checkSharedPeptides", "Include shared peptides", value = FALSE),
                  selectInput("aggregationMethod","Aggregation methods",choices =  gAgregateMethod),
                  hidden(numericInput("nTopn", "nTopn",value = NULL, min = 0)),
                  actionButton("perform.aggregation","Perform aggregation")
              )
    )
    
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
          actionButton("valid.aggregation","Save aggregation", styleclass = "primary")
                    )
    
    } else {
        tagList(
          h4("The peptide dataset has been aggregated into a protein dataset."),
          tags$div(style="align: center;",
                   moduleDatasetOverviewUI("overview_Aggregation")
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
    req(rv$current.obj)
    
    selectInput("proteinId", 
                "Choose the protein ID",
                choices = c("None",colnames(Biobase::fData(rv$current.obj))))
})

