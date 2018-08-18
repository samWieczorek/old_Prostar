callModule(missingValuesPlots,"MVPlots_filtering")
callModule(moduleFilterStringbasedOptions,"filteringStringBasedOptions")




output$filteringDone <- renderUI({
  #input$datasets
  if( length(grep("Filtered", input$datasets))==0) {return()}
  
  shinyjs::hide('prevBtnFiltering')
  shinyjs::hide('nextBtnFiltering')
  
  tags$p(style="font-size: 24;",
         tags$b("The filtering has been processed."))
  
  
})

output$checkFilteringPanel <- renderUI({
  
  #rv$pageFiltering
  color <- rep("lightgrey",NUM_PAGES_FILTERING)
  
  ##Step 1
  if (rv$pageFiltering >= 1){
    res <- rv$mvFiltering_Done
    ifelse(res, color[1] <- "green", color[1] <- "red")
  }
  
  ##Step 2: Choose data ID
  
  if (rv$pageFiltering >= 2){
    res <- rv$stringBasedFiltering_Done
    ifelse(res, color[2] <- "green", color[2] <- "red")
    
  } 
  
  ## Step 3: Choose quantitative data
  if (rv$pageFiltering == 3){
    res <-   length(grep("Filtered", input$datasets))==1
    ifelse(res, color[3] <- "green", color[3] <- "red")
  }

  txt <- c("MV filtering", "String-based filtering", "Validate")
  buildTable(txt, color)
})



NUM_PAGES_FILTERING <- 3

observe({
  toggleState(id = "prevBtnFiltering", condition = rv$pageFiltering > 1)
  toggleState(id = "nextBtnFiltering", condition = rv$pageFiltering < NUM_PAGES_FILTERING)
  hide(selector = ".page")
 # show(paste0("step", rv$pageFiltering))
})

navPageFiltering <- function(direction) {
  rv$pageFiltering <- rv$pageFiltering + direction
  
  updateSelectInput(session, "ChooseFilters", selected = input$ChooseFilters)
  updateSelectInput(session, "seuilNA", selected = input$seuilNA)
  
  
}

observeEvent(input$prevBtnFiltering, navPageFiltering(-1))
observeEvent(input$nextBtnFiltering, navPageFiltering(1))



output$mv_Filtering <- renderUI({
  if (rv$pageFiltering != 1){return(NULL)}
  
  tagList(
    uiOutput("DP_sidebar_FilterTab1"),
   tags$p("The user-defined threshold allows it to tune the minimum amount of non-NA
                                                         values for each line to <br> be kept in the dataset
                                                         (the line is filtered out otherwise).
                                                         The threshold either applies on the whole  <br> dataset, on
                                                         each condition or on at least one condition."),
   tags$div(style="margin-bottom:200px;",
                  missingValuesPlotsUI("MVPlots_filtering")
                )
                
              )

})




output$stringBased_Filtering <- renderUI({
  if (rv$pageFiltering != 2){return(NULL)}
  
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
                  textInput("symFilter_tagName", "Prefix", value = "")
        ),
        tags$div( style="display:inline-block; vertical-align: middle;",
                  p(""),actionButton("actionButtonFilter", "Perform")
        )
      ),
      tags$hr(),
    DT::dataTableOutput("FilterSummaryData")
              )
})


## Perform missing values filtering
observeEvent(input$perform.filtering.MV,{
  
  # if (input$perform.filtering.MV == 0){return()}
  
   isolate({
  
  if (input$ChooseFilters == gFilterNone){
    rv$current.obj <- rv$dataset[[input$datasets]]
    rv$mvFiltering_Done <- FALSE
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





output$valid_Filtering <- renderUI({
  if (rv$pageFiltering != 3){return()}
  
  splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
              wellPanel(id = "sidebar_Filter3",
                        uiOutput("DP_sidebar_FilterTab3"),
                        actionButton("ValidateFilters","Save filtered dataset",styleclass = "primary"),
                        uiOutput("legendForExprsData2")
              ),
              tagList(
                DT::dataTableOutput("VizualizeFilteredData"),
                uiOutput("helpTextMV")
              )
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
                             scroller = TRUE,
                             ordering = F
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


callModule(modulePopover,"modulePopover_filteringTypeFilter", 
           data = reactive(list(title = tags$p(style="font-size:12;",tags$b("Type of filter")), 
                                content="xxxx")))


output$DP_sidebar_FilterTab1 <- renderUI({
  req(rv$current.obj)
  tag <- rv$current.obj@experimentData@other$mvFilter.method
  if (!is.null(tag)) { filter <- tag}
  
  tagList(
    tags$div(
      tags$div( style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                modulePopoverUI("modulePopover_filteringTypeFilter"),
                radioButtons("ChooseFilters","",  choices = gFiltersList)
      ),
      tags$div( style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                uiOutput("seuilNADelete")
      ),
      
      tags$div( style="display:inline-block; vertical-align: middle;",
                actionButton("perform.filtering.MV", "Perform MV filtering")
      )
    )
  )
  
  
})



#########################################################
##' Show the widget (slider input) for filtering
##' @author Samuel Wieczorek
output$seuilNADelete <- renderUI({ 
  input$ChooseFilters
  req(rv$current.obj)
  
  if ((input$ChooseFilters==gFilterNone) || (input$ChooseFilters==gFilterEmptyLines)) {return(NULL)   }
  
  choix <- getListNbValuesInLines(rv$current.obj, type=input$ChooseFilters)
  
  tagList(
    modulePopoverUI("modulePopover_filteringChooseFilter"),
    selectInput("seuilNA", "", choices = choix, width='100px')
  )
})




output$DP_sidebar_FilterTab3 <- renderUI({
   tagList(
     radioButtons("ChooseTabAfterFiltering",  "Choose the data to display",
                 choices= list("Quantitative data" = "quantiData", "Meta data" = "metaData"),selected=character(0))
    ,radioButtons("ChooseViewAfterFiltering",   "Type of filtered data", 
                  choices= list("Deleted on missing values" = "MissingValues","Deleted string based" = "StringBased"),
                  selected=character(0))
 )
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
  moduleLegendColoredExprsUI("FilterColorLegend_DS")
  
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
          backgroundColor = styleEqual(c("POV", "MEC"), c('lightblue', 'orange'))
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


callModule(modulePopover,"modulePopover_filteringChooseFilter", 
           data = reactive(list(title = tags$p(style="font-size:12;",tags$b("Keep ",tags$em("x"), " values / line")), 
                                content="Keep lines with at least x intensity values")))







#########################################################
# UpdateFilterWidgets <- function(){
#   
#   isolate({
#     rv$current.obj
#     if (length(rv$current.obj@processingData@processing) > 0){
#       
#       val <- match (gReplaceAllZeros ,
#                     rv$current.obj@processingData@processing)
#       updateCheckboxInput(session, "replaceAllZeros",value=val)
#       
#       val <- match (gLogTransform, 
#                     rv$current.obj@processingData@processing)
#       #updateCheckboxInput(session,"log2transform",value=val)
#       
#       r <- grep(pattern = gFilterTextPrefix, 
#                 rv$current.obj@processingData@processing, 
#                 fixed=TRUE, value=FALSE)
#       if ( length(r) > 0)
#       { 
#         listMots <- unlist(strsplit(
#           rv$current.obj@processingData@processing[r], split=" "))
#         updateSliderInput(session,
#                           inputId = "seuilNA", 
#                           value = listMots[6])
#         updateRadioButtons(session,
#                            inputId = "ChooseFilters", 
#                            selected = listMots[3])
#       }
#       else
#       { 
#         updateRadioButtons(session,
#                            inputId = "ChooseFilters", 
#                            selected = gFilterNone)
#       }
#     }
#     else{
#       updateCheckboxInput(session, "replaceAllZeros",value=F)
#       updateRadioButtons(session,
#                          inputId = "ChooseFilters", 
#                          selected = gFilterNone)
#     }
#     updateSelectInput(session,"typeImputation",selected= c("None")) 
#     updateSelectInput(session, "normalization.family",selected = c("None"))
#   })
# }








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
    
    rv$current.obj <- saveParameters(rv$current.obj,"Filtering",l.params)
    UpdateLog("Filtering", l.params)
    
    rv$ValidFilteringClicked <- TRUE
    rv$typeOfDataset <- rv$current.obj@experimentData@other$typeOfData
    name <- paste ("Filtered", " - ", rv$typeOfDataset, sep="")
    rv$dataset[[name]] <- rv$current.obj
    
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