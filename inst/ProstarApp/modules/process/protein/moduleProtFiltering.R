source(file.path(".", "modules/moduleNavigation.R"),  local = TRUE)$value
source(file.path(".", "modules/process/Common/moduleFilterStringBasedOptions.R"),  local = TRUE)$value
source(file.path(".", "modules/Plots/moduleGroupMVPlots.R"),  local = TRUE)$value




############# Definition of the module   #########################

moduleProtFilteringUI <- function(id){
  ns <- NS(id)
  tagList(
    br(),br(),br(),
    uiOutput(ns('bars')),
    hr(),
    uiOutput(ns('screens'))
    #moduleNavigationUI(ns("moduleProcess_Filtering"))
  )
}

moduleProtFiltering <- function(input, output, session, dataIn, screen.id, settings=NULL){
  ns <- session$ns
  
  #Global variables
  def.progress.saveFiltering <- c("Build Parameters list", "Save Parameters list", "Compte adjacency matrix", "Compute connex composants", "Save new dataset")
  gFiltersList <- c("None" = "None",
                    "Empty lines" = "EmptyLines",
                    "Whole matrix" = "wholeMatrix",
                    "For every condition" = "allCond",
                    "At least one condition" = "atLeastOneCond")
  gFilterNone <- gFiltersList[["None"]]
  gFilterEmptyLines <- gFiltersList[["Empty lines"]]
  gFilterWholeMat <- gFiltersList[["Whole matrix"]]
  gFilterAllCond <- gFiltersList[["For every condition"]]
  gFilterOneCond <- gFiltersList[["At least one condition"]]
  gFilterTextPrefix <- "Filtered with"
  
  GetFilterText <- function(type, seuil){
    return (
      paste(gFilterTextPrefix," ",type , " (threshold = ", seuil, " ).", sep=""))
  }
  
  
  
  
  
  ###### definition of RV for navigation process
  rvNavProcess <- reactiveValues(
    Done = rep(FALSE,5),
    def = list(name = "Filtering",
               stepsNames = c("MV filtering", "String-based filtering","Numerical filtering", "Summary", "Save"),
               isMandatory = rep(TRUE,5),
               ll.UI = list( screenStep1 = uiOutput(ns("screenFiltering1")),
                             screenStep2 = uiOutput(ns("screenFiltering2")),
                             screenStep3 = uiOutput(ns("screenFiltering3")),
                             screenStep4 = uiOutput(ns("screenFiltering4")),
                             screenStep5 = uiOutput(ns("screenFiltering5"))
               ),
               rstFunc = reactive({resetModuleFiltering()}))
  )
  
  
  
  ### appel du module de navigation
  observe({
    rv.filtering$nav2 <- callModule(moduleNavigation2, "moduleProcess_Filtering", 
               isDone = reactive({rvNavProcess$Done}), 
               pages = reactive({rvNavProcess$def}),
               rstFunc = resetModuleFiltering,
               type = reactive({'bubble'}))
  })
  
  
  ### Definition of rv for the filtering module
  rv.filtering <- reactiveValues(
    ## temporary current data in module
    obj =  NULL,
    nav2 = NULL,
    ## return result of the module
    dataOut = NULL, 
    name = "processFiltering",
    
    
    deleted = list(stringBased = NULL,
                  mvLines = NULL,
                  numeric = NULL),
    widgets = list(ChooseFilters = "None",
                   seuilNA = 0,
                   DT_filterSummary = data.frame(Filter=NULL, 
                                                      Prefix=NULL,
                                                      nbDeleted=NULL, 
                                                      Total=NULL, 
                                                      stringsAsFactors=F),
                   DT_numfilterSummary = data.frame(Filter=NULL, 
                                                         Condition=NULL,
                                                         nbDeleted=NULL, 
                                                         Total=NULL, 
                                                         stringsAsFactors=F)
                   )
    )
  
  ################################################
  
  
  resetModuleFiltering <- reactive({  
    ## update rv.filtering$widgets values (reactive values)
    #resetModuleProcess("Filtering")
    
    rv.filtering$widgets$seuilNA <- 0
    rv.filtering$deleted <- list(stringBased <- NULL,
                                mvLines <- NULL,
                               numeric = NULL)
    
    ## update rv.filtering$widgets in UI
    #updateSelectInput(session, "ChooseFilters", selected = rv.filtering$widgets$ChooseFilters)
   # updateSelectInput(session, "seuilNA", selected = rv.filtering$widgets$seuilNA)
    
    rvNavProcess$Done = rep(FALSE, 5)
    ##update dataset to put the previous one
    #rv.filtering$obj <- rv$dataset[[last(names(rv$dataset))]] 
    
  })
  
  
  ### initialisation de la variable globale du process
  observe({
    dataIn()
    rv.filtering$obj <- dataIn()
  })
  
  
  output$bars <- renderUI({
    rv.filtering$nav2()$bars
  })
  
  
  output$screens <- renderUI({
    rv.filtering$nav2()$screens
  })
  
  ################# END of definitino part   #############################
  #######################################################################
  
  
  
  
  
 
  
  
  
  callModule(missingValuesPlots,"MVPlots_filtering", data=reactive({list(obj = rv.filtering$obj,
                                                                         currentProcess = reactive({'Filtering'}))}))
  callModule(moduleFilterStringbasedOptions,"filteringStringBasedOptions")
  callModule(modulePopover,"modulePopover_keepVal", data = reactive(list(title=tags$b("Keep vals"),
                                                                         content= "The user-defined threshold allows to tune the minimum amount of non-NA values for each line to be kept in the dataset (the line is filtered out otherwise). The threshold either applies on the whole dataset, on each condition or on at least one condition.")))
  
  
  
  
  
  
  
  
  
  output$screenFiltering1 <- renderUI({
    #rv.filtering$widgets$ChooseFilters
    print("In output$screenFiltering1 <- renderUI")
    
    tagList(
      div(
        id = "screen1Filtering",
        # tags$div(
        div(style="display:inline-block; vertical-align: middle; padding-right: 40px;",
            selectInput(ns("ChooseFilters"),"Type",  
                        choices = gFiltersList, 
                        selected=rv.filtering$widgets$ChooseFilters,
                        width='200px')
        ),
        div( style="display:inline-block; vertical-align: middle;  padding-right: 40px;",
             uiOutput(ns("seuilNADelete"))
        ),
        div( style="display:inline-block; vertical-align: middle;",
             actionButton(ns("perform.filtering.MV"), "Perform MV filtering", class = actionBtnClass)
        ),
        hr(),
        missingValuesPlotsUI(ns("MVPlots_filtering"))
      )
      
    )
    
  })
  
  
  
  
  
  
  
  output$screenFiltering2 <- renderUI({
    print("In output$screenFiltering2 <- renderUI")
    tagList(
      h3("toto"),
      
      #   id = "screen2Filtering",
      tags$div(
        tags$div( style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                  selectInput(ns("symFilter_cname"), "Column name", choices = Get_symFilter_cname_choice())
        ),
        div( style="display:inline-block; vertical-align: middle;padding-right: 20px;",
             textInput(ns("symFilter_tagName"), "Prefix", value = "", width='50px')
        ),
        div( style="display:inline-block; vertical-align: middle;",
             p(""),actionButton(ns("btn_StringBasedFilter"), "Perform", class = actionBtnClass)
        )
      ),
      hr(),
      div(
        div( style="display:inline-block; vertical-align: middle; align: center;",
             DT::dataTableOutput(ns("FilterSummaryData"))
        )
      )
      
    )
  })
  
  
  
  output$screenFiltering3 <- renderUI({
    req(rv.filtering$obj)
    
    ll <- lapply(Biobase::fData(rv.filtering$obj), function(x){is.numeric(x)})
    choice <- c("None", colnames(Biobase::fData(rv.filtering$obj))[which(ll == TRUE)])
    
    tagList(
      tags$div(
        tags$div( style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                  selectInput(ns("numericFilter_cname"), "Column name", choices = choice)
        ),
        
        tags$div( style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                  selectInput(ns("numericFilter_operator"), "Operator", 
                              choices = c('None' = '',
                                          '==' = '==',
                                          '<=' = '<=',
                                          '<' = '<',
                                          '=>' = '=>',
                                          '>' = '>',
                                          '!=' = '!='), width='100px')
        ),
        tags$div( style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                  numericInput(ns("numericFilter_value"), "Value", value = "", width='100px')
        ),
        tags$div( style="display:inline-block; vertical-align: middle;",
                  p(""),actionButton(ns("btn_numFilter"), "Perform", class = actionBtnClass)
        )
      ),
      tags$hr(),
      tags$div(
        tags$div( style="display:inline-block; vertical-align: middle; align: center;",
                  DT::dataTableOutput(ns("numericalFilterSummaryData"))
        )
      )
      
    )
  })
  
  
  output$screenFiltering4 <- renderUI({
    
    tagList(
      fluidRow(
        column(width=3,radioButtons(ns("ChooseTabAfterFiltering"),  "Choose the data to display",
                                    choices= list("Quantitative data" = "quantiData", "Meta data" = "metaData"),selected=character(0))),
        column(width=3,radioButtons(ns("ChooseViewAfterFiltering"), "Type of filtered data", 
                                    choices= list("Deleted on missing values" = "MissingValues",
                                                  "Deleted string based" = "StringBased",
                                                  "Deleted numeric filter" = "Numerical"),
                                    selected=character(0))),
        column(width=3,uiOutput(ns("legendForExprsData2")))
      ),
      hr(),
      DT::dataTableOutput(ns("VizualizeFilteredData")),
      uiOutput(ns("helpTextMV"))
    )
  })
  
  
  
  
  output$screenFiltering5 <- renderUI({     
    
    tagList(
      actionButton(ns("ValidateFilters"),"Save filtered dataset",class = actionBtnClass)
    )
  })
  
  
  ##  ---------------------------------------------------------
  ## perform symbolic filter
  ## ----------------------------------------------------------
  observeEvent(input$btn_StringBasedFilter,{
    req(input$symFilter_cname)
    temp <- rv.filtering$obj
    
    print(input$symFilter_cname)
    if (input$symFilter_cname=="None"){return()}
    cname <- input$symFilter_cname
    tagName <- input$symFilter_tagName
    res <- StringBasedFiltering2(temp,cname, input$symFilter_tagName)
    nbDeleted <- 0
    
    if (!is.null(res[["deleted"]])){
      rv.filtering$deleted$stringBased <- rbindMSnset(rv.filtering$deleted$stringBased, res[["deleted"]])
      nbDeleted <-  nrow(res[["deleted"]])
    } else {
      nbDeleted <-  0
    }                          
    rv.filtering$obj <- res[["obj"]]
    rvNavProcess$Done[2] <- TRUE
    
    df <- data.frame(Filter=cname, Prefix=tagName, nbDeleted=nbDeleted, Total=nrow(rv.filtering$obj))
    rv.filtering$widgets$DT_filterSummary <- rbind(rv.filtering$widgets$DT_filterSummary , df)
  })
  
  
  
  
  ## Perform missing values filtering
  observeEvent(input$perform.filtering.MV,{
    print("In : observeEvent(input$perform.filtering.MV")
    
    if (input$ChooseFilters == gFilterNone){
      #rv.filtering$obj <- rv$dataset[[input$datasets]]
    } else {
      
      keepThat <- mvFilterGetIndices(rv.filtering$obj,
                                     input$ChooseFilters,
                                     as.integer(input$seuilNA))
      if (!is.null(keepThat))
      {
        rv.filtering$deleted$mvLines <- rv.filtering$obj[-keepThat]
        rv.filtering$obj <- mvFilterFromIndices(rv.filtering$obj,
                                              keepThat,
                                              GetFilterText(input$ChooseFilters, as.integer(input$seuilNA)))
        
        rvNavProcess$Done[1] <- TRUE
      }
    }
    updateSelectInput(session, "ChooseFilters", selected = input$ChooseFilters)
    updateSelectInput(session, "seuilNA", selected = input$seuilNA)
  })
  
  
  
  
  ## ----------------------------------------------
  # Perform numerical filtering
  observeEvent(input$btn_numFilter,{
    temp <- rv.filtering$obj
    
    if (input$numericFilter_cname=="None"){return()}
    cname <- input$numericFilter_cname
    tagValue <- input$numericFilter_value
    
    res <- NumericalFiltering(temp,cname, input$numericFilter_value,input$numericFilter_operator)
    nbDeleted <- 0
    
    if (!is.null(res[["deleted"]])){
      rv.filtering$deleted$numeric <- rbindMSnset(rv.filtering$deleted$numeric, res[["deleted"]])
      nbDeleted <-  nrow(res[["deleted"]])
    } else {
      nbDeleted <-  0
    }                          
    rv.filtering$obj <- res[["obj"]]
    rvNavProcess$Done[3] <- TRUE
    
    df <- data.frame(Filter=cname, 
                     Condition=paste0(input$numericFilter_operator,' ',tagValue), 
                     nbDeleted=nbDeleted, 
                     Total=nrow(rv.filtering$obj))
    rv.filtering$widgets$DT_numfilterSummary <- rbind(rv.filtering$widgets$DT_numfilterSummary, df)
    
  })
  
  
  
  ### ------------------------------------------------------------
  output$numericalFilterSummaryData <- DT::renderDataTable({
    req(rv.filtering$obj)
    req(rv.filtering$widgets$DT_numfilterSummary)
    
    if (nrow(rv.filtering$widgets$DT_numfilterSummary) == 0){
      df <- data.frame(Filter=NA, Condition=NA, nbDeleted=NA, Total=nrow(rv.filtering$obj), stringsAsFactors = FALSE)
      rv.filtering$widgets$DT_numfilterSummary <- rbind(rv.filtering$widgets$DT_numfilterSummary ,df)
    }
    
    DT::datatable(rv.filtering$widgets$DT_numfilterSummary,
                  extensions = c('Scroller', 'Buttons'),
                  rownames = FALSE,
                  options=list(initComplete = initComplete(),
                               dom = 'Brt',
                               deferRender = TRUE,
                               bLengthChange = FALSE
                  ))
  })
  
  
  output$FilterSummaryData <- DT::renderDataTable({
    req(rv.filtering$obj)
    req(rv.filtering$widgets$DT_filterSummary)
    
    if (nrow(rv.filtering$widgets$DT_filterSummary )==0){
      df <- data.frame(Filter="-", Prefix="-", nbDeleted=0, Total=nrow(rv.filtering$obj), stringsAsFactors = FALSE)
      rv.filtering$widgets$DT_filterSummary <- rbind(rv.filtering$widgets$DT_filterSummary ,df)
    }
    
    
    DT::datatable(rv.filtering$widgets$DT_filterSummary,
                  extensions = c('Scroller', 'Buttons'),
                  rownames = FALSE,
                  options=list(dom='Brt',
                               initComplete = initComplete(),
                               deferRender = TRUE,
                               bLengthChange = FALSE
                  ))
  })
  
  
  
  
  
  #############-------------------------
  
  
  observeEvent(input$ChooseFilters, {
    rv.filtering$widgets$ChooseFilters <- input$ChooseFilters
  })
  
  
  observeEvent(input$seuilNA, {
    rv.filtering$widgets$seuilNA <- input$seuilNA
  })
  
  
  output$ObserverNumericalFilteringDone <- renderUI({
    req(rv.filtering$obj)
    rvNavProcess$Done
    
    isolate({
      if (!(rvNavProcess$Done[3]) )
      {return(NULL)  }
      else {
        h3("Numerical filtering done")
      }
      
    })
  })
  
  
  
  Get_symFilter_cname_choice <- reactive({
    
    choice <- c("None", colnames(Biobase::fData(rv.filtering$obj)))
    choice
  })
  
  
  
  ## symbolic filtering event
  
  
  
  
  
  
  getDataForNumericalFiltered <- reactive({
    req(settings())
    rv.filtering$deleted$numeric
    table <- as.data.frame(round(Biobase::exprs(rv.filtering$deleted$numeric),digits=settings()$nDigits))
    table <- cbind(table, Biobase::fData(rv.filtering$deleted$numeric)[,rv.filtering$deleted$numeric@experimentData@other$OriginOfValues])
    
    table
  })
  
  
  
  getDataForMVFiltered <- reactive({
    req(settings())
    rv.filtering$deleted$mvLines
    print(settings())
    table <- as.data.frame(round(Biobase::exprs(rv.filtering$deleted$mvLines),digits=settings()$nDigits))
    table <- cbind(table, Biobase::fData(rv.filtering$deleted$mvLines)[,rv.filtering$deleted$mvLines@experimentData@other$OriginOfValues])
    
    table
  })
  
  
  callModule(moduleLegendColoredExprs, "ExprsColorLegend_Filtering")
  
  getDataForMVStringFiltered <- reactive({
    req(settings())
    rv.filtering$deleted$stringBased
    
    table <- as.data.frame(round(Biobase::exprs(rv.filtering$deleted$stringBased),digits=settings()$nDigits))
    table <- cbind(table, Biobase::fData(rv.filtering$deleted$stringBased)[,rv.filtering$deleted$stringBased@experimentData@other$OriginOfValues])
    
    table
  })
  
  
  output$legendForExprsData2 <- renderUI({
    req(input$ChooseTabAfterFiltering)
    
    if (input$ChooseTabAfterFiltering != "quantiData"){return(NULL)}
    
    moduleLegendColoredExprsUI("ExprsColorLegend_Filtering", settings()$colorsTypeMV)
    
  })
  
  
  #----------------------------------------------
  output$VizualizeFilteredData <- DT::renderDataTable({
   #$ req(settings()$nDigits)
    req(input$ChooseViewAfterFiltering)
    req(input$ChooseTabAfterFiltering)
    rv.filtering$deleted$stringBased
    rv.filtering$deleted$mvLines
    rv.filtering$deleted$numeric
    
    data <- NULL
    if ((input$ChooseViewAfterFiltering == "MissingValues") && !is.null(rv.filtering$deleted$mvLines))
    {
      switch(input$ChooseTabAfterFiltering,
             quantiData =  data <- getDataForMVFiltered(),
             metaData = data <- cbind(ID = rownames(Biobase::fData(rv.filtering$deleted$mvLines)), Biobase::fData(rv.filtering$deleted$mvLines))
      )
    } 
    
    else if ((input$ChooseViewAfterFiltering == "StringBased") && !is.null(rv.filtering$deleted$stringBased)) {
      
      switch(input$ChooseTabAfterFiltering,
             quantiData =  data <- getDataForMVStringFiltered(),
             metaData = data <- Biobase::fData(rv.filtering$deleted$stringBased)
      )
    }  else if ((input$ChooseViewAfterFiltering == "Numerical") && !is.null(rv.filtering$deleted$numeric)) {
      
      switch(input$ChooseTabAfterFiltering,
             quantiData =  data <- getDataForNumericalFiltered(),
             metaData = data <- Biobase::fData(rv.filtering$deleted$numeric)
      )
    }
    
    if (!is.null(data)){
      
      if(input$ChooseTabAfterFiltering =="quantiData"){
        dt <- DT::datatable( data,
                         extensions = c('Scroller', 'Buttons'),
                         options = list(dom = 'Brtip',
                                        initComplete = initComplete(),
                                        displayLength = 20,
                                        deferRender = TRUE,
                                        bLengthChange = FALSE,
                                        scrollX = 200,
                                        scrollY = 600,
                                        scroller = TRUE,
                                        ordering=FALSE,
                                        server = TRUE,
                                        columnDefs = list(list(targets = c(((ncol(data)/2)+1):ncol(data)), visible = FALSE),
                                                          list(width='150px',targets= "_all"))
                         )) %>%
          DT::formatStyle(
            colnames(data)[1:(ncol(data)/2)],
            colnames(data)[((ncol(data)/2)+1):ncol(data)],
            backgroundColor = DT::styleEqual(c("POV", "MEC"), c(settings()$colorsTypeMV$POV, settings()$colorsTypeMV$MEC))
          )
      } else {
        dt <- datatable( data,extensions = 'Scroller',
                         options = list(initComplete = initComplete(),
                                        displayLength = 20,
                                        deferRender = TRUE,
                                        bLengthChange = FALSE,
                                        scrollX = 200,
                                        scrollY = 600,
                                        scroller = TRUE,
                                        ordering=FALSE,
                                        server = TRUE)) 
      }
      
      dt
    }
  })
  
  
  #########################################################
  ##' Show the widget (slider input) for filtering
  ##' @author Samuel Wieczorek
  output$seuilNADelete <- renderUI({ 
    req(input$ChooseFilters)
    
    if ((input$ChooseFilters=="None") || (input$ChooseFilters==gFilterEmptyLines)) {return(NULL)   }
    print(rv.filtering$obj)
    choix <- getListNbValuesInLines(rv.filtering$obj, type=input$ChooseFilters)
    tagList(
      modulePopoverUI(ns("modulePopover_keepVal")),
      
      selectInput(ns("seuilNA"), NULL,
                  choices = choix,
                  selected = rv.filtering$widgets$seuilNA,
                  width='150px'))
    
  })
  
  
  
  
  
  
  disableActionButton <- function(id,session) {
    session$sendCustomMessage(type="jsCode",
                              list(code= paste("$('#",id,"').prop('disabled',true)"
                                               ,sep="")))
  }
  
  #-----------------------------------------------
  output$ObserverStringBasedFilteringDone <- renderUI({
    
    isolate({
      if (!rvNavProcess$Done[2]) 
      {return(NULL)  }
      else {
        h3("String-based filtering done")
      }
      
    })
  })
  
  # output$ObserverMVFilteringDone <- renderUI({
  #   
  #   #isolate({
  #     
  #     n <- 0
  #     if(!is.null(rv.filtering$deleted$mvLines)){n <- nrow(rv.filtering$deleted$mvLines)}
  #     if (!isDone[1]) 
  #     {return(NULL)  }
  #     else {
  #       h3(paste0("MV filtering done. ",n, " lines were deleted."))
  #     }
  #     
  #  # })
  # })
  
  
  #########################################################
  ##' Validation of the filters and modification on current object
  ##' @author Samuel Wieczorek
  observeEvent(input$ValidateFilters,ignoreInit = TRUE,{ 
    
    isolate({
      nSteps <- length(def.progress.saveFiltering)
      print(paste0("nSteps = ", nSteps))
      withProgress(message = '',detail = '', value = 0, {
        
        incProgress(1/nSteps, detail = def.progress.saveFiltering[1])
        
        
      if((input$ChooseFilters != gFilterNone) 
         || (nrow(rv.filtering$widgets$DT_filterSummary )>1)
         || (nrow(rv.filtering$widgets$DT_numfilterSummary )>1)){
        l.params <- build_ParamsList_Filtering()
        
        incProgress(1/nSteps, detail = def.progress.saveFiltering[2])
        
        typeOfDataset <- rv.filtering$obj@experimentData@other$typeOfData
        name <- paste0("Filtered", ".", typeOfDataset)
        rv.filtering$obj <- saveParameters(rv.filtering$obj,name,"Filtering",l.params)
        
        mat <- cc <- list()
        incProgress(1/nSteps, detail = def.progress.saveFiltering[3])
        mat <- ComputeAdjacencyMatrices(rv.filtering$obj)
        
        incProgress(1/nSteps, detail = def.progress.saveFiltering[4])
        #cc <- ComputeConnexComposants(mat)

        incProgress(1/nSteps, detail = def.progress.saveFiltering[5])
        
        ## mise a jour de la variable de retour du module
        rv.filtering$dataOut <- list(obj = rv.filtering$obj,
                                      AdjacencyMat = mat,
                                      ConnexComp = cc
        )

      }
      rvNavProcess$Done[5] <- TRUE
      })
    })
      

    
  })
  
  
  
  
  build_ParamsList_Filtering <- reactive({
    if (nrow(rv.filtering$widgets$DT_filterSummary) <=1) {
      df.string <- NULL
    } else {
      df.string <- rv.filtering$widgets$DT_filterSummary
    }
    
    if (nrow(rv.filtering$widgets$DT_numfilterSummary) <=1) {
      df.numeric <- NULL
    } else {
      df.numeric <- rv.filtering$widgets$DT_numfilterSummary}
    
    l.params <- list(mvFilterType = input$ChooseFilters,
                     mvThNA = as.numeric(input$seuilNA), 
                     stringFilter.df = df.string,
                     numericFilter.df = df.numeric)
    
    l.params
  })
  
  
  
  #########################################################
  ##' Show the widget for filters
  ##' @author Samuel Wieczorek
  output$choixFiltres <- renderUI({
    req(input$file)
    radioButtons("ChooseFilters","Filtering options",choices = gFiltersList)
    
  })
  
  
  
  output$helpTextMV <- renderUI({
    helpText("After checking the data, validate the filters.")
  })
  


  return(reactive({rv.filtering$dataOut}))

}




