

##---------------------------------------------------------------
##------------------------------------------------------------------





moduleProcessFiltering <- function(input, output, session, dataIn){
  ns <- session$ns
  
  observeEvent(dataIn(), {
    rvFiltering$current.obj <- dataIn()
    })
 
 
  isDone <- rep(FALSE, 4)
  stepsNames <- c("MV filtering", "String-based filtering", "Summary", "Save")
  isMandatory <- c(FALSE, FALSE, FALSE, TRUE)
  name <- "Filtering"
  current <- reactiveVal(1)
  nbSteps <- length(stepsNames)
  
  dataOut <- reactiveVal(NULL)
  
  rvFiltering <- reactiveValues(
    current.obj = NULL,
    widgets = list(ChooseFilters = "None",
         seuilNA = 0,
         DT_filterSummary = data.frame(Filtre=NULL, 
                                       Prefix=NULL,
                                       nbDeleted=NULL, 
                                       Total=NULL, 
                                       stringsAsFactors=F)
  )
  )
  
  
  callModule(missingValuesPlots,"MVPlots_filtering", data=reactive({rvFiltering$current.obj}))
  callModule(moduleFilterStringbasedOptions,"filteringStringBasedOptions")
  callModule(modulePopover,"modulePopover_keepVal", data = reactive(list(title=tags$b("Keep vals"),
                                                                         content= "The user-defined threshold allows to tune the minimum amount of non-NA values for each line to be kept in the dataset (the line is filtered out otherwise). The threshold either applies on the whole dataset, on each condition or on at least one condition.")))
  
  
  ##--------------------------------------------------------------
  ## Gestion des couleurs du slideshow
  ##--------------------------------------------------------------
  
  
  output$checkPanel <- renderUI({
    
  print(paste0("in output$checkPanel <- renderUI", nbSteps))
    color <- rep("lightgrey",nbSteps)
    colorForCursor <- rep("white",nbSteps)
    
    
    for (i in 1:nbSteps){
      status <- isDone[i]
      col <- ifelse(isMandatory[i], "red", orangeProstar)
      ifelse(status, color[i] <- "green", color[i] <- col)
    }
    
    colorForCursor[current()] <- "black"
    buildTable(stepsNames, color,colorForCursor)
    
  })
  
  
  observeEvent(input$rstBtn,{
    current(1)
   # rstFunc()
  })
  
  observe({
    toggle(id = "prevBtn", condition = (nbSteps >1))
    toggle(id = "nextBtn", condition = (nbSteps >1) )
    
    toggle(id = "rstBtn", condition = !(isDone[nbSteps])) 
    
    toggleState(id = "prevBtn", condition = current() > 1)
    toggleState(id = "nextBtn", condition = current() < nbSteps)
    hide(selector = ".page")
  })
  
  ##--------------------------------------------------------------
  ## Navigation dans le slideshow
  ##--------------------------------------------------------------
  
  navPage <- function(direction) {
    newValue <- current() + direction 
    current(newValue)
  }
  
  observeEvent(input$prevBtn,{navPage(-1)})
  observeEvent(input$nextBtn,{navPage(1)})
  

  observeEvent(current(),{
     for (i in 1:nbSteps){
      shinyjs::toggle(id = paste0("screen", i), condition = current() == i)
    }
  })







  resetModuleFiltering <- reactive({  
  ## update rvFiltering$widgets values (reactive values)
  resetModuleProcess("Filtering")
  
  rvFiltering$widgets$seuilNA <- 0
  rvFiltering$deleted.stringBased <- NULL
  rvFiltering$deleted.mvLines <- NULL

  
  ## update rvFiltering$widgets in UI
  updateSelectInput(session, "ChooseFilters", selected = rvFiltering$widgets$ChooseFilters)
  updateSelectInput(session, "seuilNA", selected = rvFiltering$widgets$seuilNA)
  
  rvModProcess$moduleFilteringDone = rep(FALSE, 4)
  ##update dataset to put the previous one
  rvFiltering$current.obj <- dataIn()
  
  })


  output$screenFiltering1 <- renderUI({
  #rv$widgets$ChooseFilters
  print("In output$screenFiltering1 <- renderUI")
  
  tagList(
   div(
      id = "tata",
     # tags$div(
        div(style="display:inline-block; vertical-align: middle; padding-right: 40px;",
                  selectInput(ns("ChooseFilters"),"Type",  
                              choices = gFiltersList, 
                              selected=rvFiltering$widgets$ChooseFilters,
                              width='200px')
        ),
        div( style="display:inline-block; vertical-align: middle;  padding-right: 40px;",
                  uiOutput(ns("seuilNADelete"))
        ),
        div( style="display:inline-block; vertical-align: middle;",
                  actionButton(ns("perform.filtering.MV"), "Perform MV filtering", class = actionBtnClass)
        ),
      hr(),
      missingValuesPlotsUI(ns("MVPlots_filtering")),
      uiOutput(ns("ObserverMVFilteringDone"))
      )
 
    )
  
})







output$screenFiltering2 <- renderUI({
  print("In output$screenFiltering2 <- renderUI")
  tagList(
    div(
      div( style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                selectInput(ns("symFilter_cname"), "Column name", choices = Get_symFilter_cname_choice())
      ),
      div( style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                textInput(ns("symFilter_tagName"), "Prefix", value = "", width='50px')
      ),
      div( style="display:inline-block; vertical-align: middle;",
                p(""),actionButton(ns("actionButtonFilter"), "Perform", class = actionBtnClass)
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
  
  tagList(
    fluidRow(
      column(width=3,radioButtons(ns("ChooseTabAfterFiltering"),  "Choose the data to display",
                 choices= list("Quantitative data" = "quantiData", "Meta data" = "metaData"),selected=character(0))),
      column(width=3,radioButtons(ns("ChooseViewAfterFiltering"), "Type of filtered data", 
                  choices= list("Deleted on missing values" = "MissingValues","Deleted string based" = "StringBased"),
                  selected=character(0))),
      column(width=3,uiOutput(ns("legendForExprsData2")))
      ),
      hr(),
      DT::dataTableOutput(ns("VizualizeFilteredData")),
      uiOutput(ns("helpTextMV"))
         )
})




output$screenFiltering4 <- renderUI({     
  
  tagList(
    actionButton(ns("ValidateFilters"),"Save filtered dataset",class = actionBtnClass)
    
  )
})




#############-------------------------


observeEvent(input$ChooseFilters, {
  rvFiltering$widgets$ChooseFilters <- input$ChooseFilters
})
observeEvent(input$seuilNA, {
  rvFiltering$widgets$seuilNA <- input$seuilNA
})



Get_symFilter_cname_choice <- reactive({
  req(rvFiltering$widgets$DT_filterSummary)
  
  if (nrow(rvFiltering$widgets$DT_filterSummary) <= 1) {
    choice <- c("None", colnames(fData(rvFiltering$current.obj)))
  } else {
    index <- match(rvFiltering$widgets$DT_filterSummary[-1,"Filter"], colnames(fData(rvFiltering$current.obj)))
    choice <- c("None", colnames(fData(rvFiltering$current.obj))[-index])
  }
  choice
})



## symbolic filtering event
observeEvent(input$actionButtonFilter,{
  temp <- rvFiltering$current.obj
  
  if (input$symFilter_cname=="None"){return()}
  
  cname <- input$symFilter_cname
  tagName <- input$symFilter_tagName
  res <- StringBasedFiltering2(temp,cname, input$symFilter_tagName)
  nbDeleted <- 0
  
  if (!is.null(res[["deleted"]])){
    rvFiltering$deleted.stringBased <- rbindMSnset(rvFiltering$deleted.stringBased, res[["deleted"]])
    nbDeleted <-  nrow(res[["deleted"]])
  } else {
    nbDeleted <-  0
  }                          
  rvFiltering$current.obj <- res[["obj"]]
  
  rvModProcess$moduleFilteringDone[2] <- TRUE
  
  df <- data.frame(Filter=cname, Prefix=tagName, nbDeleted=nbDeleted, Total=nrow(rvFiltering$current.obj))
  rvFiltering$widgets$DT_filterSummary <- rbind(rvFiltering$widgets$DT_filterSummary , df)

})



output$FilterSummaryData <- DT::renderDataTable({
  rvFiltering$widgets$DT_filterSummary
  
  print(rvFiltering$widgets$DT_filterSummary)
  if (nrow(rvFiltering$widgets$DT_filterSummary )==0){
    df <- data.frame(Filter="-", Prefix="-", nbDeleted=0, Total=nrow(rvFiltering$current.obj), stringsAsFactors = FALSE)
    rvFiltering$widgets$DT_filterSummary <- df
  }
  
  
   DT::datatable(rvFiltering$widgets$DT_filterSummary,
                extensions = c('Scroller', 'Buttons'),
                rownames = FALSE,
                options=list(dom='Brt',
                             initComplete = initComplete(),
                             deferRender = TRUE,
                             bLengthChange = FALSE
                             # columnDefs = list(list(width='150px',targets= c(1)),
                             #                  list(width='50px',targets= c(2:4)))

                ))
})



getDataForMVFiltered <- reactive({
  req(rv$settings_nDigits)
  rvFiltering$deleted.mvLines
  
  table <- as.data.frame(round(Biobase::exprs(rvFiltering$deleted.mvLines),digits=rv$settings_nDigits))
  table <- cbind(table, Biobase::fData(rvFiltering$deleted.mvLines)[,rvFiltering$deleted.mvLines@experimentData@other$OriginOfValues])
  
  table
})




getDataForMVStringFiltered <- reactive({
  req(rv$settings_nDigits)
  rvFiltering$deleted.stringBased
  
  
  table <- as.data.frame(round(Biobase::exprs(rvFiltering$deleted.stringBased),digits=rv$settings_nDigits))
  table <- cbind(table, Biobase::fData(rvFiltering$deleted.stringBased)[,rvFiltering$deleted.stringBased@experimentData@other$OriginOfValues])
  
  table
})


output$legendForExprsData2 <- renderUI({
  req(input$ChooseTabAfterFiltering)
  
  if (input$ChooseTabAfterFiltering != "quantiData"){return(NULL)}
  moduleLegendColoredExprsUI("FilterColorLegend_DS", rv$colorsTypeMV)
  
})


#----------------------------------------------
output$VizualizeFilteredData <- DT::renderDataTable({
  req(rv$settings_nDigits)
  rvFiltering$deleted.mvLines
  req(input$ChooseViewAfterFiltering)
  req(input$ChooseTabAfterFiltering)
  rvFiltering$deleted.stringBased
  
  
  data <- NULL
  if ((input$ChooseViewAfterFiltering == "MissingValues") && !is.null(rvFiltering$deleted.mvLines))
  {
    switch(input$ChooseTabAfterFiltering,
           quantiData =  data <- getDataForMVFiltered(),
           metaData = data <- cbind(ID = rownames(Biobase::fData(rvFiltering$deleted.mvLines)), Biobase::fData(rvFiltering$deleted.mvLines))
    )
  } 
  
  else if ((input$ChooseViewAfterFiltering == "StringBased") && !is.null(rvFiltering$deleted.stringBased)) {
    
    switch(input$ChooseTabAfterFiltering,
           quantiData =  data <- getDataForMVStringFiltered(),
           metaData = data <- Biobase::fData(rvFiltering$deleted.stringBased)
    )
  } 
  
  if (!is.null(data)){
    
    if(input$ChooseTabAfterFiltering =="quantiData"){
      dt <- datatable( data,
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
  
  choix <- getListNbValuesInLines(rvFiltering$current.obj, type=input$ChooseFilters)
  tagList(
    modulePopoverUI("modulePopover_keepVal"),
    
    selectInput("seuilNA", NULL,
                choices = choix,
                selected = rvFiltering$widgets$seuilNA,
                width='150px'))
  
})



## Perform missing values filtering
observeEvent(input$perform.filtering.MV,{
  #if (input$perform.filtering.MV == 0){return()}
  print("In : observeEvent(input$perform.filtering.MV")
  
  #isolate({
    
     if (input$ChooseFilters == gFilterNone){
      rvFiltering$current.obj <- dataIn()
    } else {
      
      keepThat <- mvFilterGetIndices(dataIn(),
                                     input$ChooseFilters,
                                     as.integer(input$seuilNA))
      if (!is.null(keepThat))
      {
        rvFiltering$deleted.mvLines <- dataIn()[-keepThat]
        rvFiltering$current.obj <- 
          mvFilterFromIndices(dataIn(),
                              keepThat,
                              GetFilterText(input$ChooseFilters, as.integer(input$seuilNA)))
        
         isDone[1] <- TRUE
        
      }
    }

 # })
  
  updateSelectInput(session, "ChooseFilters", selected = input$ChooseFilters)
  updateSelectInput(session, "seuilNA", selected = input$seuilNA)
  
})



disableActionButton <- function(id,session) {
  session$sendCustomMessage(type="jsCode",
                            list(code= paste("$('#",id,"').prop('disabled',true)"
                                             ,sep="")))
}

#-----------------------------------------------
output$ObserverStringBasedFilteringDone <- renderUI({
   
  isolate({
    if (!isDone[2]) 
    {return(NULL)  }
    else {
      h3("String-based filtering done")
    }
    
  })
})

output$ObserverMVFilteringDone <- renderUI({
  
  isolate({
    
    n <- 0
    if(!is.null(rvFiltering$deleted.mvLines)){n <- nrow(rvFiltering$deleted.mvLines)}
    if (!isDone[1]) 
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
  
  isolate({
    if((input$ChooseFilters != gFilterNone) || (nrow(rvFiltering$widgets$DT_filterSummary )>1)){
      l.params <- build_ParamsList_Filtering()
      
      rv$typeOfDataset <- rvFiltering$current.obj@experimentData@other$typeOfData
      name <- paste0("Filtered", ".", rv$typeOfDataset)
      rvFiltering$current.obj <- saveParameters(rv$current.obj,name,"Filtering",l.params)
      rv$dataset[[name]] <- rvFiltering$current.obj
      dataOut<- rvFiltering$current.obj
      isDone[4] <- TRUE
    
      if (rv$typeOfDataset == "peptide"  && !is.null(rv$proteinId)){
        ComputeAdjacencyMatrices()}
      updateSelectInput(session, "datasets", choices = names(rv$dataset), selected = name)
      }
  
  })
  
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



return(reactive({dataOut}))

}

callModule(moduleProcessFiltering, "moduleProcess_Filtering", dataIn = reactive({rv$current.obj}))


