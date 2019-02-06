
callModule(moduleProcess, "moduleProcess_Filtering", 
           isDone = reactive({rvModProcess$moduleFilteringDone}), 
           pages = reactive({rvModProcess$moduleFiltering}),
           rstFunc = resetModuleFiltering)

##---------------------------------------------------------------
##------------------------------------------------------------------


resetModuleFiltering <- reactive({  
  ## update widgets values (reactive values)
  rv$widgetsfiltering$ChooseFilters <- "None"
  rv$widgetsfiltering$seuilNA <- 0
  rv$widgetsfiltering$DT_filterSummary <- data.frame(Filtre=NULL, 
                                                   Prefix=NULL,
                                                   nbDeleted=NULL, 
                                                   Total=NULL, 
                                                   stringsAsFactors=F)
  
  
  ## update widgets in UI
  updateSelectInput(session, "ChooseFilters", selected = rv$widgetsfiltering$ChooseFilters)
  updateSelectInput(session, "seuilNA", selected = rv$widgets$filtering$seuilNA)
  
  ##update dataset to put the previous one
  rv$current.obj <- rv$dataset[[last(names(rv$dataset))]] 
  
  })


output$screenFiltering1 <- renderUI({
  req(rv$current.obj)
  #rv$widgets$filtering$ChooseFilters
  tagList(
      tags$div(
        tags$div( style="display:inline-block; vertical-align: middle; padding-right: 40px;",
                  selectInput("ChooseFilters","Type",  
                              choices = gFiltersList, 
                              selected=rv$widgets$filtering$ChooseFilters,
                              width='200px')
        ),
        tags$div( style="display:inline-block; vertical-align: middle;  padding-right: 40px;",
                  uiOutput("seuilNADelete")
        ),
        tags$div( style="display:inline-block; vertical-align: middle;",
                  actionButton("perform.filtering.MV", "Perform MV filtering", class = actionBtnClass)
        ),
      hr(),
      missingValuesPlotsUI("MVPlots_filtering"),
      uiOutput("ObserverMVFilteringDone")
      )
 
    )
  
})






observeEvent(input$ChooseFilters, {
  rv$widgets$filtering$ChooseFilters <- input$ChooseFilters
  })
observeEvent(input$seuilNA, {
  rv$widgets$filtering$seuilNA <- input$seuilNA
  })



output$screenFiltering2 <- renderUI({
  
  tagList(
    tags$div(
      tags$div( style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                selectInput("symFilter_cname", "Column name", choices = Get_symFilter_cname_choice())
      ),
      tags$div( style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                textInput("symFilter_tagName", "Prefix", value = "", width='50px')
      ),
      tags$div( style="display:inline-block; vertical-align: middle;",
                p(""),actionButton("actionButtonFilter", "Perform", class = actionBtnClass)
      )
    ),
    hr(),
    tags$div(
      tags$div( style="display:inline-block; vertical-align: middle; align: center;",
                DT::dataTableOutput("FilterSummaryData")
      )
    )
    
  )
})



output$screenFiltering3 <- renderUI({
  req(rv$current.obj)
  
  tagList(
    fluidRow(
      column(width=3,radioButtons("ChooseTabAfterFiltering",  "Choose the data to display",
                 choices= list("Quantitative data" = "quantiData", "Meta data" = "metaData"),selected=character(0))),
      column(width=3,radioButtons("ChooseViewAfterFiltering", "Type of filtered data", 
                  choices= list("Deleted on missing values" = "MissingValues","Deleted string based" = "StringBased"),
                  selected=character(0))),
      column(width=3,uiOutput("legendForExprsData2"))
      ),
         tags$hr(),
         DT::dataTableOutput("VizualizeFilteredData"),
         uiOutput("helpTextMV")
         )
})




output$screenFiltering4 <- renderUI({     
  
  tagList(
    actionButton("ValidateFilters","Save filtered dataset",class = actionBtnClass)
    
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
  
  rvModProcess$moduleFilteringDone[2] <- TRUE
  
  df <- data.frame(Filter=cname, Prefix=tagName, nbDeleted=nbDeleted, Total=nrow(rv$current.obj))
  rv$widgets$filtering$DT_filterSummary <- rbind(rv$widgets$filtering$DT_filterSummary , df)

  #colnames(rv$widgets$filtering$DT_filterSummary) <- c("Filter", "Prefix", "nbDeleted", "Total")
  
})



output$FilterSummaryData <- DT::renderDataTable({
  if (nrow(rv$widgets$filtering$DT_filterSummary )==0){
    df <- data.frame(Filter="-", Prefix="-", nbDeleted=0, Total=nrow(rv$current.obj), stringsAsFactors = FALSE)
    rv$widgets$filtering$DT_filterSummary <- df
  }
  
  
   DT::datatable(rv$widgets$filtering$DT_filterSummary,
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
  rv$deleted.mvLines
  
  table <- as.data.frame(round(Biobase::exprs(rv$deleted.mvLines),digits=rv$settings_nDigits))
  table <- cbind(table, Biobase::fData(rv$deleted.mvLines)[,rv$deleted.mvLines@experimentData@other$OriginOfValues])
  
  table
})




getDataForMVStringFiltered <- reactive({
  req(rv$settings_nDigits)
  rv$deleted.stringBased
  
  
  table <- as.data.frame(round(Biobase::exprs(rv$deleted.stringBased),digits=rv$settings_nDigits))
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
  req(rv$settings_nDigits)
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
  
  choix <- getListNbValuesInLines(rv$current.obj, type=input$ChooseFilters)
  tagList(
    modulePopoverUI("modulePopover_keepVal"),
    
    selectInput("seuilNA", NULL,
                choices = choix,
                selected = rv$widgets$filtering$seuilNA,
                width='150px'))
  
})



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
        

        rvModProcess$moduleFilteringDone[1] <- TRUE
        
      }
    }

  })
  
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
  #req(rv$current.obj)
  
  isolate({
    if (!rvModProcess$moduleFilteringDone[2]) 
    {return(NULL)  }
    else {
      h3("String-based filtering done")
    }
    
  })
})

output$ObserverMVFilteringDone <- renderUI({
  #req(rv$current.obj)
  
  isolate({
    
    n <- 0
    if(!is.null(rv$deleted.mvLines)){n <- nrow(rv$deleted.mvLines)}
    if (!rvModProcess$moduleFilteringDone[1]) 
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
  
  isolate({
    if((input$ChooseFilters != gFilterNone) || (nrow(rv$widgets$filtering$DT_filterSummary )>1)){

        l.params <- build_ParamsList_Filtering()
    

    
    #rv$ValidFilteringClicked <- TRUE
    rv$typeOfDataset <- rv$current.obj@experimentData@other$typeOfData
    name <- paste0("Filtered", ".", rv$typeOfDataset)
    rv$current.obj <- saveParameters(rv$current.obj,name,"Filtering",l.params)
    rv$dataset[[name]] <- rv$current.obj
    
    rvModProcess$moduleFilteringDone[4] <- TRUE
    
    if (rv$typeOfDataset == "peptide"  && !is.null(rv$proteinId)){
      ComputeAdjacencyMatrices()}
    updateSelectInput(session, "datasets", choices = names(rv$dataset), selected = name)
    #updateSelectInput(session, "seuilNA", selected= input$seuilNA)
  }
  
  })
  
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






