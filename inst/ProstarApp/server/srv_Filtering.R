

##---------------------------------------------------------------
##------------------------------------------------------------------




callModule(moduleProcess, "moduleProcess_Filtering", 
           isDone = reactive({rvModProcess$moduleFilteringDone}), 
           pages = reactive({rvModProcess$moduleFiltering}),
           rstFunc = resetModuleFiltering)


callModule(missingValuesPlots,"MVPlots_filtering",
           data=reactive({rv$current.obj}),
           palette = reactive({rv$PlotParams$paletteConditions})
           )
callModule(moduleFilterStringbasedOptions,"filteringStringBasedOptions")
callModule(modulePopover,"modulePopover_keepVal", data = reactive(list(title=tags$b("Keep vals"),
                                                                         content= "The user-defined threshold allows to tune the minimum amount of non-NA values for each line to be kept in the dataset (the line is filtered out otherwise). The threshold either applies on the whole dataset, on each condition or on at least one condition.")))
  
  
  ##--------------------------------------------------------------
  ## Gestion des couleurs du slideshow
  ##--------------------------------------------------------------
 
resetModuleFiltering <- reactive({  
    ## update rv$widgets values (reactive values)
    resetModuleProcess("Filtering")
  
    rv$widgets$seuilNA <- 0
    rv$deleted.stringBased <- NULL
    rv$deleted.mvLines <- NULL
    rv$deleted.numeric = NULL

    ## update rv$widgets in UI
    updateSelectInput(session, "ChooseFilters", selected = rv$widgets$ChooseFilters)
    updateSelectInput(session, "seuilNA", selected = rv$widgets$seuilNA)
  
    rvModProcess$moduleFilteringDone = rep(FALSE, 5)
    ##update dataset to put the previous one
    rv$current.obj <- rv$dataset[[last(names(rv$dataset))]] 

})
  



  output$screenFiltering1 <- renderUI({
  #rv$widgets$ChooseFilters
  print("In output$screenFiltering1 <- renderUI")
  
  tagList(
   div(
      id = "screen1Filtering",
     # tags$div(
        div(style="display:inline-block; vertical-align: middle; padding-right: 40px;",
                  selectInput("ChooseFilters","Type",  
                              choices = gFiltersList, 
                              selected=rv$widgets$ChooseFilters,
                              width='200px')
        ),
        div( style="display:inline-block; vertical-align: middle;  padding-right: 40px;",
                  uiOutput("seuilNADelete")
        ),
        div( style="display:inline-block; vertical-align: middle;",
                  actionButton("perform.filtering.MV", "Perform MV filtering", class = actionBtnClass)
        ),
      hr(),
      missingValuesPlotsUI("MVPlots_filtering")
      #uiOutput("ObserverMVFilteringDone")
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
                  selectInput("symFilter_cname", "Column name", choices = Get_symFilter_cname_choice())
        ),
       div( style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                 textInput("symFilter_tagName", "Prefix", value = "", width='50px')
       ),
       div( style="display:inline-block; vertical-align: middle;",
                 p(""),actionButton("actionButtonFilter", "Perform", class = actionBtnClass)
       )
     ),
     hr(),
     div(
       div( style="display:inline-block; vertical-align: middle; align: center;",
                 DT::dataTableOutput("FilterSummaryData")
       )
     )
    
  )
})



output$screenFiltering3 <- renderUI({
  req(rv$current.obj)
  
  ll <- lapply(fData(rv$current.obj), function(x){is.numeric(x)})
  choice <- c("None", colnames(fData(rv$current.obj))[which(ll == TRUE)])
  
  tagList(
    tags$div(
      tags$div( style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                selectInput("numericFilter_cname", "Column name", choices = choice)
      ),
      
      tags$div( style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                selectInput("numericFilter_operator", "Operator", 
                            choices = c('None' = '',
                                        '==' = '==',
                                        '<=' = '<=',
                                        '<' = '<',
                                        '>=' = '>=',
                                        '>' = '>',
                                        '!=' = '!='), width='100px')
      ),
      tags$div( style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                numericInput("numericFilter_value", "Value", value = "", width='100px')
      ),
      tags$div( style="display:inline-block; vertical-align: middle;",
                p(""),actionButton("btn_numFilter", "Perform", class = actionBtnClass)
      )
    ),
    tags$hr(),
    tags$div(
      tags$div( style="display:inline-block; vertical-align: middle; align: center;",
                DT::dataTableOutput("numericalFilterSummaryData")
      )
    )
    
  )
})


output$screenFiltering4 <- renderUI({
  
  tagList(
    fluidRow(
      column(width=3,radioButtons("ChooseTabAfterFiltering",  "Choose the data to display",
                 choices= list("Quantitative data" = "quantiData", "Meta data" = "metaData"),selected=character(0))),
      column(width=3,radioButtons("ChooseViewAfterFiltering", "Type of filtered data", 
                                  choices= list("Deleted on missing values" = "MissingValues",
                                                "Deleted string based" = "StringBased",
                                                "Deleted numeric filter" = "Numerical"),
                                  selected=character(0))),
      column(width=3,uiOutput("legendForExprsData2"))
      ),
      hr(),
      DT::dataTableOutput("VizualizeFilteredData"),
      uiOutput("helpTextMV")
         )
})


 

output$screenFiltering5 <- renderUI({     
  
  tagList(
    actionButton("ValidateFilters","Save filtered dataset",class = actionBtnClass)
  )
})


##  ---------------------------------------------------------
## perform symbolic filter
## ----------------------------------------------------------
observeEvent(input$actionButtonFilter,{
  req(input$symFilter_cname)
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
})




## Perform missing values filtering
observeEvent(input$perform.filtering.MV,{
  print("In : observeEvent(input$perform.filtering.MV")
  
  if (input$ChooseFilters == gFilterNone){
    #rv$current.obj <- rv$dataset[[input$datasets]]
  } else {
    keepThat <- mvFilterGetIndices(rv$current.obj,
                                   input$ChooseFilters,
                                   as.integer(input$seuilNA))
    if (!is.null(keepThat))
    {
      rv$deleted.mvLines <- rv$current.obj[-keepThat]
      rv$current.obj <- mvFilterFromIndices(rv$current.obj,
                            keepThat,
                            GetFilterText(input$ChooseFilters, as.integer(input$seuilNA)))
      
      rvModProcess$moduleFilteringDone[1] <- TRUE
    }
  }
  updateSelectInput(session, "ChooseFilters", selected = input$ChooseFilters)
  updateSelectInput(session, "seuilNA", selected = input$seuilNA)
})




## ----------------------------------------------
# Perform numerical filtering
observeEvent(input$btn_numFilter,{
  temp <- rv$current.obj
  
  if (input$numericFilter_cname=="None"){return()}
  cname <- input$numericFilter_cname
  tagValue <- input$numericFilter_value
  
  res <- NumericalFiltering(temp,cname, input$numericFilter_value,input$numericFilter_operator)
  nbDeleted <- 0
  
  if (!is.null(res[["deleted"]])){
    rv$deleted.numeric <- rbindMSnset(rv$deleted.numeric, res[["deleted"]])
    nbDeleted <-  nrow(res[["deleted"]])
  } else {
    nbDeleted <-  0
  }                          
  rv$current.obj <- res[["obj"]]
  rvModProcess$moduleFilteringDone[3] <- TRUE
  
  df <- data.frame(Filter=cname, 
                   Condition=paste0(input$numericFilter_operator,' ',tagValue), 
                   nbDeleted=nbDeleted, 
                   Total=nrow(rv$current.obj))
  rv$widgets$filtering$DT_numfilterSummary <- rbind(rv$widgets$filtering$DT_numfilterSummary, df)
  
})



### ------------------------------------------------------------
output$numericalFilterSummaryData <- DT::renderDataTable({
  req(rv$current.obj)
  req(rv$widgets$filtering$DT_numfilterSummary)
  
  if (nrow(rv$widgets$filtering$DT_numfilterSummary) == 0){
    df <- data.frame(Filter=NA, Condition=NA, nbDeleted=NA, Total=nrow(rv$current.obj), stringsAsFactors = FALSE)
    rv$widgets$filtering$DT_numfilterSummary <- rbind(rv$widgets$filtering$DT_numfilterSummary ,df)
  }
  
  
  DT::datatable(rv$widgets$filtering$DT_numfilterSummary,
                extensions = c('Scroller', 'Buttons'),
                rownames = FALSE,
                
                options=list(initComplete = initComplete(),
                             dom = 'Brt',
                             buttons = list('copy',
                                            list(
                                              extend = 'csv',
                                              filename = 'NumericalFiltering_summary'
                                            ),'print'),
                             deferRender = TRUE,
                             bLengthChange = FALSE
                ))
})





output$FilterSummaryData <- DT::renderDataTable({
  req(rv$current.obj)
  req(rv$widgets$filtering$DT_numfilterSummary)
  
  if (nrow(rv$widgets$filtering$DT_filterSummary )==0){
    df <- data.frame(Filter="-", Prefix="-", nbDeleted=0, Total=nrow(rv$current.obj), stringsAsFactors = FALSE)
    rv$widgets$filtering$DT_filterSummary <- df
    }
  
  
  DT::datatable(rv$widgets$filtering$DT_filterSummary,
                extensions = c('Scroller', 'Buttons'),
                rownames = FALSE,
                options=list(dom='Brt',
                             buttons = list('copy',
                                            list(
                                              extend = 'csv',
                                              filename = 'Filtering_summary'
                                            ),'print'),
                             initComplete = initComplete(),
                             deferRender = TRUE,
                             bLengthChange = FALSE
                ))
})





#############-------------------------


observeEvent(input$ChooseFilters, {
  rv$widgets$ChooseFilters <- input$ChooseFilters
})
observeEvent(input$seuilNA, {
  rv$widgets$seuilNA <- input$seuilNA
})


output$ObserverNumericalFilteringDone <- renderUI({
  req(rv$current.obj)
  rv$numericalFiltering_Done
  
  isolate({
    if (!rv$numericalFiltering_Done) 
    {return(NULL)  }
    else {
      h3("Numerical filtering done")
    }
    
  })
})



Get_symFilter_cname_choice <- reactive({
  
  choice <- c("None", colnames(fData(rv$current.obj)))
  choice
})



## symbolic filtering event






getDataForNumericalFiltered <- reactive({
  req(rv$settings_nDigits)
  rv$deleted.numeric
  table <- as.data.frame(round(Biobase::exprs(rv$deleted.numeric),digits=rv$settings_nDigits))
  table <- cbind(table, Biobase::fData(rv$deleted.numeric)[,rv$deleted.numeric@experimentData@other$OriginOfValues])
  
  table
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
  rv$deleted.numeric
  
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
  }  else if ((input$ChooseViewAfterFiltering == "Numerical") && !is.null(rv$deleted.numeric)) {
    
    switch(input$ChooseTabAfterFiltering,
           quantiData =  data <- getDataForNumericalFiltered(),
           metaData = data <- Biobase::fData(rv$deleted.numeric)
    )
  }
  
  if (!is.null(data)){
    
    if(input$ChooseTabAfterFiltering =="quantiData"){
      dt <- datatable( data,
                       extensions = c('Scroller', 'Buttons'),
                       options = list(dom = 'Brtip',
                                      options = list(buttons = list('copy',
                                                                    list(
                                                                      extend = 'csv',
                                                                      filename = 'Prostar_export'
                                                                    ),'print'),
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
  print(rv$current.obj)
  choix <- getListNbValuesInLines(rv$current.obj, type=input$ChooseFilters)
  tagList(
    modulePopoverUI("modulePopover_keepVal"),
    
    selectInput("seuilNA", NULL,
                choices = choix,
                selected = rv$widgets$seuilNA,
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
    if (!isDone[2]) 
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
#     if(!is.null(rv$deleted.mvLines)){n <- nrow(rv$deleted.mvLines)}
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
    if((input$ChooseFilters != gFilterNone) 
       || (nrow(rv$widgets$filtering$DT_filterSummary )>1)
       || (nrow(rv$widgets$filtering$DT_numfilterSummary )>1)){
      l.params <- build_ParamsList_Filtering()
      
      rv$typeOfDataset <- rv$current.obj@experimentData@other$typeOfData
      name <- paste0("Filtered", ".", rv$typeOfDataset)
      rv$current.obj <- saveParameters(rv$current.obj,name,"Filtering",l.params)
      rv$dataset[[name]] <- rv$current.obj
      dataOut<- rv$current.obj
      rvModProcess$moduleNormalizationDone[5] <- TRUE
    
      if (rv$typeOfDataset == "peptide"  && !is.null(rv$proteinId)){
        ComputeAdjacencyMatrices()
        ComputeConnexComposants()
      }
      updateSelectInput(session, "datasets", choices = names(rv$dataset), selected = name)
      }
    rvModProcess$moduleFilteringDone[5] <- TRUE
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

# 
# 
# return(reactive({dataOut}))
# 
# }


