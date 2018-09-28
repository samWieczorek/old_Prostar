callModule(missingValuesPlots,"MVPlots_filtering")
callModule(moduleFilterStringbasedOptions,"filteringStringBasedOptions")
callModule(modulePopover,"modulePopover_keepVal", data = reactive(list(title="Keep vals",
                                                                        content= "The user-defined threshold allows to tune the minimum amount of non-NA values for each line to be kept in the dataset (the line is filtered out otherwise). The threshold either applies on the whole dataset, on each condition or on at least one condition.")))







output$mv_Filtering <- renderUI({
  req(rv$current.obj)
  tag <- rv$current.obj@experimentData@other$mvFilter.method
  if (!is.null(tag)) { filter <- tag}
  
  tagList(
      tags$div(
        tags$div( style="display:inline-block; vertical-align: middle; padding-right: 40px;",
                  selectInput("ChooseFilters","Type",  choices = gFiltersList, width='150px')
        ),
        tags$div( style="display:inline-block; vertical-align: middle;",
                  uiOutput("seuilNADelete")
        ),
        tags$div( style="display:inline-block; vertical-align: middle; padding-right: 40px;",
                  actionButton("perform.filtering.MV", "Perform MV filtering")
        )
      ),
    tags$div(style="margin-bottom:200px;",
             tagList(
               missingValuesPlotsUI("MVPlots_filtering"),
             uiOutput("ObserverMVFilteringDone")
            )
    )
    
    )
  
})




output$stringBased_Filtering <- renderUI({
  req(rv$current.obj)
  req(rv$DT_filterSummary)
  
  if (nrow(rv$DT_filterSummary) <= 1) {
    choice <- c("None", colnames(fData(rv$current.obj)))
  } else {
    index <- match(rv$DT_filterSummary[-1,"Filter"], colnames(fData(rv$current.obj)))
    choice <- c("None", colnames(fData(rv$current.obj))[-index])
  }
  
  tagList(
    tags$div(
      tags$div( style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                selectInput("symFilter_cname", "Column name", choices = choice)
      ),
      tags$div( style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                textInput("symFilter_tagName", "Prefix", value = "", width='50px')
      ),
      tags$div( style="display:inline-block; vertical-align: middle;",
                p(""),actionButton("actionButtonFilter", "Perform")
      )
    ),
    tags$hr(),
    DT::dataTableOutput("FilterSummaryData")
  )
})



output$valid_Filtering <- renderUI({
  req(rv$current.obj)
  
  tagList(
    fluidRow(
      column(width=3,radioButtons("ChooseTabAfterFiltering",  "Choose the data to display",
                 choices= list("Quantitative data" = "quantiData", "Meta data" = "metaData"),selected=character(0))),
      column(width=3,radioButtons("ChooseViewAfterFiltering",   "Type of filtered data", 
                  choices= list("Deleted on missing values" = "MissingValues","Deleted string based" = "StringBased"),
                  selected=character(0))),
      column(width=3,uiOutput("legendForExprsData2")),
      column(width=3,actionButton("ValidateFilters","Save filtered dataset",styleclass = "primary"))
      ),
         tags$hr(),
         DT::dataTableOutput("VizualizeFilteredData"),
         uiOutput("helpTextMV")
         )
})











observeEvent(input$actionButtonFilter,{
  rv$current.obj
  temp <- rv$current.obj
  
  if (input$symFilter_cname=="None"){return()}
  
  cname <- input$symFilter_cname
  tagName <- input$symFilter_tagName
  res <- StringBasedFiltering2(temp,cname, input$symFilter_tagName)
  nbDeleted <- 0
  
  if (!is.null(res[["deleted"]])){
    rv$deleted.stringBased <- rbindMSnset(rv$deleted.stringBased, res[["deleted"]])
    nbDeleted <-  nrow(res[["deleted"]])
  } else {
    nbDeleted <-  0
  }                          
  rv$current.obj <- res[["obj"]]
  rv$stringBasedFiltering_Done = TRUE
  
  df <- data.frame(Filter=cname, Prefix=tagName, nbDeleted=nbDeleted, Total=nrow(rv$current.obj))
  rv$DT_filterSummary <- rbind(rv$DT_filterSummary , df)

  #colnames(rv$DT_filterSummary) <- c("Filter", "Prefix", "nbDeleted", "Total")
  
})


output$SymbolicFilterOptions <- renderUI({
  req(rv$current.obj)
  req(rv$DT_filterSummary)
  
  if (nrow(rv$DT_filterSummary) <= 1) {
    choice <- c("None", colnames(fData(rv$current.obj)))
  } else {
    index <- match(rv$DT_filterSummary[-1,"Filter"], colnames(fData(rv$current.obj)))
    choice <- c("None", colnames(fData(rv$current.obj))[-index])
  }
  tagList(
    selectInput("symFilter_cname", "Column name", choices = choice),
    textInput("symFilter_tagName", "Prefix", value = ""),
    actionButton("actionButtonFilter", "Perform")
  )
})


output$FilterSummaryData <- DT::renderDataTable({
  req(rv$current.obj)
  req(rv$DT_filterSummary)
  
  if (nrow(rv$DT_filterSummary )==0){
    df <- data.frame(Filter=NA, Prefix=NA, nbDeleted=NA, Total=nrow(rv$current.obj))
    rv$DT_filterSummary <- rbind(rv$DT_filterSummary ,df)
  }
  
  DT::datatable(rv$DT_filterSummary,extensions = 'Scroller',
                options=list(initComplete = initComplete(),
                             deferRender = TRUE,
                             bLengthChange = FALSE,
                             scrollX = 200,
                             scrollY = 600,
                             scroller = TRUE
                ))
})


observe({
  input$datasets
  if (length(grep("Filtered", input$datasets))==0 && rv$ValidFilteringClicked){
    rv$ValidFilteringClicked <- FALSE
    df <- data.frame(Filter=NA, Prefix=NA, nbDeleted=NA, Total=nrow(rv$current.obj))
    rv$DT_filterSummary <- df
  }
  
})


getDataForMVFiltered <- reactive({
  req(input$settings_nDigits)
  rv$deleted.mvLines
  
  table <- as.data.frame(round(Biobase::exprs(rv$deleted.mvLines),digits=input$settings_nDigits))
  table <- cbind(table, Biobase::fData(rv$deleted.mvLines)[,rv$deleted.mvLines@experimentData@other$OriginOfValues])
  
  table
})




getDataForMVStringFiltered <- reactive({
  req(input$settings_nDigits)
  rv$deleted.stringBased
  
  
  table <- as.data.frame(round(Biobase::exprs(rv$deleted.stringBased),digits=input$settings_nDigits))
  table <- cbind(table, Biobase::fData(rv$deleted.stringBased)[,rv$deleted.stringBased@experimentData@other$OriginOfValues])
  
  table
})


output$legendForExprsData2 <- renderUI({
  req(input$ChooseTabAfterFiltering)
  
  if (input$ChooseTabAfterFiltering != "quantiData"){return(NULL)}
  moduleLegendColoredExprsUI("FilterColorLegend_DS", rv$colorsTypeMV)
  
})


#----------------------------------------------
output$VizualizeFilteredData <- DT::renderDataTable({
  req(input$settings_nDigits)
  rv$deleted.mvLines
  req(input$ChooseViewAfterFiltering)
  req(input$ChooseTabAfterFiltering)
  rv$deleted.stringBased
  
  
  data <- NULL
  if ((input$ChooseViewAfterFiltering == "MissingValues") && !is.null(rv$deleted.mvLines))
  {
    switch(input$ChooseTabAfterFiltering,
           quantiData =  data <- getDataForMVFiltered(),
           metaData = data <- cbind(ID = rownames(Biobase::fData(rv$deleted.mvLines)), Biobase::fData(rv$deleted.mvLines))
    )
  } 
  
  else if ((input$ChooseViewAfterFiltering == "StringBased") && !is.null(rv$deleted.stringBased)) {
    
    switch(input$ChooseTabAfterFiltering,
           quantiData =  data <- getDataForMVStringFiltered(),
           metaData = data <- Biobase::fData(rv$deleted.stringBased)
    )
  } 
  
  if (!is.null(data)){
    
    if(input$ChooseTabAfterFiltering =="quantiData"){
      dt <- datatable( data,extensions = 'Scroller',
                       options = list(initComplete = initComplete(),
                                      displayLength = 20,
                                      deferRender = TRUE,
                                      bLengthChange = FALSE,
                                      scrollX = 200,
                                      scrollY = 600,
                                      scroller = TRUE,
                                      ordering=FALSE,
                                      server = TRUE,
                                      columnDefs = list(list(targets = c(((ncol(data)/2)+1):ncol(data)), visible = FALSE))
                       )) %>%
        formatStyle(
          colnames(data)[1:(ncol(data)/2)],
          colnames(data)[((ncol(data)/2)+1):ncol(data)],
          backgroundColor = styleEqual(c("POV", "MEC"), c(rv$colorsTypeMV$POV, rv$colorsTypeMV$MEC))
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
  
  choix <- getListNbValuesInLines(rv$current.obj, type=input$ChooseFilters)
  tagList(
    modulePopoverUI("modulePopover_keepVal"),
  selectInput("seuilNA", "", choices = choix, width='150px'))
  
})





#########################################################
UpdateFilterWidgets <- function(){
  
  isolate({
    rv$current.obj
    if (length(rv$current.obj@processingData@processing) > 0){
      
      val <- match (gReplaceAllZeros ,rv$current.obj@processingData@processing)
      updateCheckboxInput(session, "replaceAllZeros",value=val)
      
      val <- match (gLogTransform,rv$current.obj@processingData@processing)
      #updateCheckboxInput(session,"log2transform",value=val)
      
      r <- grep(pattern = gFilterTextPrefix, 
                rv$current.obj@processingData@processing, 
                fixed=TRUE, value=FALSE)
      if ( length(r) > 0)
      { 
        listMots <- unlist(strsplit(
          rv$current.obj@processingData@processing[r], split=" "))
        updateSliderInput(session, inputId = "seuilNA", value = listMots[6])
        updateRadioButtons(session, inputId = "ChooseFilters", selected = listMots[3])
      }
      else
      { 
        updateRadioButtons(session,
                           inputId = "ChooseFilters", 
                           selected = gFilterNone)
      }
    }
    else{
      updateCheckboxInput(session, "replaceAllZeros",value=F)
      updateRadioButtons(session,
                         inputId = "ChooseFilters", 
                         selected = gFilterNone)
    }
    updateSelectInput(session,"typeImputation",selected= c("None")) 
    updateSelectInput(session, "normalization.family",selected = c("None"))
  })
}





## Perform missing values filtering
observeEvent(input$perform.filtering.MV,{
  #if (input$perform.filtering.MV == 0){return()}
  
  isolate({
    
     if (input$ChooseFilters == gFilterNone){
      rv$current.obj <- rv$dataset[[input$datasets]]
    } else {
      
      keepThat <- mvFilterGetIndices(rv$dataset[[input$datasets]],
                                     input$ChooseFilters,
                                     as.integer(input$seuilNA))
      if (!is.null(keepThat))
      {
        rv$deleted.mvLines <- rv$dataset[[input$datasets]][-keepThat]
        rv$current.obj <- 
          mvFilterFromIndices(rv$dataset[[input$datasets]],
                              keepThat,
                              GetFilterText(input$ChooseFilters, as.integer(input$seuilNA)))
        
        rv$mvFiltering_Done <- TRUE
        updateSelectInput(session, "ChooseFilters", selected = input$ChooseFilters)
        updateSelectInput(session, "seuilNA", selected = input$seuilNA)
        
      }
    }

  })
})



disableActionButton <- function(id,session) {
  session$sendCustomMessage(type="jsCode",
                            list(code= paste("$('#",id,"').prop('disabled',true)"
                                             ,sep="")))
}

#-----------------------------------------------
output$ObserverStringBasedFilteringDone <- renderUI({
  req(rv$current.obj)
  rv$stringBasedFiltering_Done

  isolate({
    if (!rv$stringBasedFiltering_Done) 
    {return(NULL)  }
    else {
      h3("String-based filtering done")
    }
    
  })
})

output$ObserverMVFilteringDone <- renderUI({
  req(rv$current.obj)
  rv$MVFiltering_Done
  
  isolate({
    
    n <- 0
    if(!is.null(rv$deleted.mvLines)){n <- nrow(rv$deleted.mvLines)}
    if (!rv$mvFiltering_Done) 
    {return(NULL)  }
    else {
      h3(paste0("MV filtering done. ",n, " lines were deleted."))
    }
    
  })
})


#########################################################
##' Validation of the filters and modification on current object
##' @author Samuel Wieczorek
observeEvent(input$ValidateFilters,ignoreInit = TRUE,{ 
  req(rv$current.obj)
  if((input$ChooseFilters != gFilterNone) || (nrow(rv$DT_filterSummary )>1)){
    
    if (nrow(rv$DT_filterSummary) <=1) {
      df <- NULL
    } else {
      df <- rv$DT_filterSummary}
    
    l.params <- list(mvFilterType = input$ChooseFilters,
                     mvThNA = input$seuilNA, 
                     stringFilter.df = df)
    
    UpdateLog("Filtering", l.params)
    rv$current.obj <- saveParameters(rv$current.obj,GetCurrentDatasetName(),"Filtering",l.params)
    
    rv$ValidFilteringClicked <- TRUE
    rv$typeOfDataset <- rv$current.obj@experimentData@other$typeOfData
    name <- paste0("Filtered", ".", rv$typeOfDataset)
    rv$dataset[[name]] <- rv$current.obj
    
    if (rv$typeOfDataset == "peptide"  && !is.null(rv$proteinId)){
      ComputeAdjacencyMatrices()}
    updateSelectInput(session, "datasets", choices = names(rv$dataset), selected = name)
  }
  
  
})



#########################################################
##' Show the widget for filters
##' @author Samuel Wieczorek
output$choixFiltres <- renderUI({
  req(input$file)
  rv$current.obj
  radioButtons("ChooseFilters","Filtering options",choices = gFiltersList)
  
})



output$helpTextMV <- renderUI({
  req(rv$current.obj)
  helpText("After checking the data, validate the filters.")
})
