source(file.path("server", "mod_filtering_example.R"),  local = TRUE)$value
source(file.path("server", "mod_query_metacell.R"),  local = TRUE)$value


callModule(moduleProcess, "moduleProcess_Filtering",
           isDone = reactive({rvModProcess$moduleFilteringDone}),
           pages = reactive({rvModProcess$moduleFiltering}),
           rstFunc = resetModuleFiltering,
           forceReset = reactive({rvModProcess$moduleFilteringForceReset })  )



resetModuleFiltering <- reactive({
  #req(input$datasets)
  ## update rv$widgets values (reactive values)
  resetModuleProcess("Filtering")
  
  
  rv$widgets$filtering$DT_filterSummary <- data.frame(Filter=NULL,
                                                      Prefix=NULL,
                                                      nbDeleted=NULL,
                                                      Total=NULL,
                                                      stringsAsFactors=F)
  rv$widgets$filtering$DT_numfilterSummary <- data.frame(Filter=NULL,
                                                         Condition=NULL,
                                                         nbDeleted=NULL,
                                                         Total=NULL,
                                                         stringsAsFactors=F)
  
  rv$widgets$filtering$metacell_Filter_SummaryDT <- data.frame(query = NULL,
                                                               nbDeleted=NULL,
                                                               Total=NULL,
                                                               stringsAsFactors=F)
  
  rv$widgets$filtering$MetacellTag <- "None"
  rv$widgets$filtering$MetacellFilters <- "None"
  rv$widgets$filtering$KeepRemove <- 'delete'
  rv$widgets$filtering$metacell_value_th <- 0
  rv$widgets$filtering$choose_metacell_percent_th <- 0
  rv$widgets$filtering$metacell_value_percent <- 0
  rv$widgets$filtering$val_vs_percent <- 'Value'
  rv$widgets$filtering$metacellFilter_operator <- '<='
  
  
  
  rv$deleted.stringBased <- NULL
  #rv$deleted.mvLines <- NULL
  rv$deleted.metacell <- NULL
  #rv$deleted.byMSMSLines <- NULL
  rv$deleted.numeric <- NULL
  
  rv$current.obj <- rv$dataset[[input$datasets]]
  rvModProcess$moduleFilteringDone = rep(FALSE, length(rvModProcess$moduleFiltering$stepsNames))
  
})


#########################################################################################
##
##                    SCREEN 1
## 
############################################################################



output$screenFiltering1 <- renderUI({
  
  
    tagList(
      div(
       # id = "screen1Filtering",
        
        div(
          mod_query_metacell_ui('query'),
          div( style="display:inline-block; vertical-align: middle;",
               shinyjs::disabled(
                 actionButton("performMetacellFiltering", 
                                              "Perform metacell filtering", 
                                              class = actionBtnClass)
                                 )
          )
        ),
        tags$hr(),
        div( style="display:inline-block; vertical-align: middle; align: center;",
             DT::dataTableOutput("metacell_Filter_SummaryDT")
        ),
        
        hr(),
        ################## Plots section #############################
        mod_plotsMetacellHistos_ui("MVPlots_filtering")
      )
      
    )
  
})


indices <- mod_query_metacell_server(id = 'query',
                                  obj = reactive({rv$current.obj}),
                                  list_tags = reactive({c('None' = 'None',
                                                          DAPAR::metacell.def(GetTypeofData(rv$current.obj))$node
                                  )}),
                                  keep_vs_remove = reactive({setNames(nm = c("delete", "keep"))}),
                                  filters = reactive({c("None" = "None",
                                                        "Whole Line" = "WholeLine",
                                                        "Whole matrix" = "WholeMatrix",
                                                        "For every condition" = "AllCond",
                                                        "At least one condition" = "AtLeastOneCond")}),
                                  val_vs_percent = reactive({setNames(nm=c('Count', 'Percentage'))}),
                                  operator = reactive({setNames(nm=DAPAR::SymFilteringOperators())})
)



 observeEvent(req(indices()$params$MetacellTag), {
   shinyjs::toggleState("performMetacellFiltering",
                         condition = indices()$params$MetacellTag != 'None')
 })


mod_plotsMetacellHistos_server(id = "MVPlots_filtering", 
                               obj = reactive({rv$current.obj}),
                               pal = reactive({rv$PlotParams$paletteForConditions}),
                               pattern = reactive({rv$widgets$filtering$MetacellTag})
)



## Perform filtration
observeEvent(input$performMetacellFiltering, ignoreInit = TRUE,{

  nbDeleted <- 0
  rv$widgets$filtering$MetacellTag <- indices()$params$MetacellTag
  rv$widgets$filtering$KeepRemove <- indices()$params$KeepRemove
  rv$widgets$filtering$MetacellFilters <- indices()$params$MetacellFilters
  rv$widgets$filtering$metacell_percent_th  <- indices()$params$metacell_percent_th
  rv$widgets$filtering$metacell_value_th  <- indices()$params$metacell_value_th
  rv$widgets$filtering$val_vs_percent  <- indices()$params$val_vs_percent
  rv$widgets$filtering$metacellFilter_operator  <- indices()$params$metacellFilter_operator

  obj.tmp <-  MetaCellFiltering(obj = rv$current.obj,
                                indices = indices()$indices,
                                cmd = rv$widgets$filtering$KeepRemove)
    
    rv$deleted.metacell <- obj.tmp$deleted
    rv$current.obj <- obj.tmp$new
    nbDeleted <- nrow(rv$deleted.metacell)

  
  df <- data.frame(query =  indices()$query,
                   nbDeleted = nbDeleted,
                   Total = nrow(rv$current.obj))
  
  rv$widgets$filtering$metacell_Filter_SummaryDT <- rbind(rv$widgets$filtering$metacell_Filter_SummaryDT , df)
  
  rvModProcess$moduleFilteringDone[1] <- TRUE
}, priority = 900)



output$metacell_Filter_SummaryDT <- DT::renderDataTable(server=TRUE,{
  req(rv$current.obj)
  req(rv$widgets$filtering$metacell_Filter_SummaryDT)
  isolate({
    
    if (nrow(rv$widgets$filtering$metacell_Filter_SummaryDT )==0){
      df <- data.frame(query="-",
                       nbDeleted=0,
                       Total=nrow(rv$current.obj),
                       stringsAsFactors = FALSE)
      rv$widgets$filtering$metacell_Filter_SummaryDT <- df
    }
    
    
    DT::datatable(rv$widgets$filtering$metacell_Filter_SummaryDT,
                  extensions = c('Scroller'),
                  rownames = FALSE,
                  options=list(
                               dom='rt',
                               initComplete = initComplete(),
                               deferRender = TRUE,
                               bLengthChange = FALSE
                  ))
  })
})



#########################################################################################
##
##                    SCREEN 2
## 
###########################################################################################



output$screenFiltering2 <- renderUI({
  tagList(
    
   tags$div(
      tags$div( style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                selectInput("symFilter_cname", "Column name", choices = Get_symFilter_cname_choice())
      ),
      div( style="display:inline-block; vertical-align: middle;padding-right: 20px;",
           textInput("symFilter_tagName", "Prefix", value = "", width='50px')
      ),
      div( style="display:inline-block; vertical-align: middle;",
           p(""),actionButton("perform.text.filtering", "Perform", class = actionBtnClass)
      )
    ),
   uiOutput('explainSymFilter_ui'),
    hr(),
    div(
      div( style="display:inline-block; vertical-align: middle; align: center;",
            DT::dataTableOutput("FilterSummaryData")
      )
    )
    
  )
})


output$explainSymFilter_ui <- renderUI({
  req(input$symFilter_cname != 'None')
  req(input$symFilter_tagName != '')
  txt <- paste0("You are going to delete lines in the column '", 
                input$symFilter_cname, "' which begin with '", 
                input$symFilter_tagName, 
                "'.")
  p(txt)
})

##  ---------------------------------------------------------
## perform symbolic filter
## ----------------------------------------------------------
observeEvent(input$perform.text.filtering,{
  req(input$symFilter_cname)
  req(input$symFilter_cname != "None")
  temp <- rv$current.obj
  
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




output$FilterSummaryData <- DT::renderDataTable(server=TRUE,{
  req(rv$current.obj)
  req(rv$widgets$filtering$DT_numfilterSummary)
  isolate({
    
    if (nrow(rv$widgets$filtering$DT_filterSummary )==0){
      df <- data.frame(Filter="-", 
                       Prefix="-", 
                       nbDeleted=0, 
                       Total=nrow(rv$current.obj), 
                       stringsAsFactors = FALSE)
       rv$widgets$filtering$DT_filterSummary <- df
    }
    
    
    DT::datatable(rv$widgets$filtering$DT_filterSummary,
                  extensions = c('Scroller'),
                  rownames = FALSE,
                  options=list(dom = 'rt',
                               initComplete = initComplete(),
                               deferRender = TRUE,
                               bLengthChange = FALSE
                               )
                  )
    })
})


#########################################################################################
##
##                    SCREEN 3
## 
###########################################################################################

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
                            choices = setNames(nm=DAPAR::SymFilteringOperators()),
                            width='100px')
      ),
      tags$div( style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                numericInput("numericFilter_value", "Value", value = "", width='100px')
      ),
      tags$div( style="display:inline-block; vertical-align: middle;",
                p(""),actionButton("btn_numFilter", "Perform", class = actionBtnClass)
      )
    ),
    uiOutput('explainNumFilter_ui'),
    tags$hr(),
    tags$div(
      tags$div( style="display:inline-block; vertical-align: middle; align: center;",
                DT::dataTableOutput("numericalFilterSummaryData")
      )
    )
    
  )
})


output$explainNumFilter_ui <- renderUI({
  req(input$numericFilter_cname != 'None')
  req(input$numericFilter_value != '')
  
  txt <- paste0("You are going to delete lines where ", 
                input$numericFilter_cname, " ", 
                input$numericFilter_operator, 
                " ", input$numericFilter_value, ".")
  p(txt)
})

## ----------------------------------------------
# Perform numerical filtering
observeEvent(input$btn_numFilter,ignoreInit=TRUE,{
  temp <- rv$current.obj
  
  req(input$numericFilter_cname != "None")
  
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



Get_symFilter_cname_choice <- reactive({
  req(rv$current.obj)
  choice <- c("None", colnames(fData(rv$current.obj)))
  choice
})


### ------------------------------------------------------------
output$numericalFilterSummaryData <- DT::renderDataTable(server=TRUE,{
  req(rv$current.obj)
  req(rv$widgets$filtering$DT_numfilterSummary)
  
  isolate({
    if (nrow(rv$widgets$filtering$DT_numfilterSummary) == 0){
      df <- data.frame(Filter=NA, Condition=NA, nbDeleted=NA, Total=nrow(rv$current.obj), stringsAsFactors = FALSE)
      rv$widgets$filtering$DT_numfilterSummary <- rbind(rv$widgets$filtering$DT_numfilterSummary ,df)
    }
    
    
    DT::datatable(rv$widgets$filtering$DT_numfilterSummary,
                  extensions = c('Scroller'),
                  rownames = FALSE,
                  
                  options=list(initComplete = initComplete(),
                               dom = 'rt',
                               deferRender = TRUE,
                               bLengthChange = FALSE
                  ))
  })
  
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


#########################################################################################
##
##                    SCREEN 4
## 
###########################################################################################

output$screenFiltering4 <- renderUI({
  
  tagList(
    fluidRow(
      column(width=3,radioButtons("ChooseTabAfterFiltering",  "Choose the data to display",
                                  choices= list("Quantitative data" = "quantiData", "Meta data" = "metaData"),selected=character(0))),
      column(width=3,radioButtons("ChooseViewAfterFiltering", "Type of filtered data",
                                  choices= list("Deleted on quant. metadata" = "Metacell",
                                                "Deleted string based" = "StringBased",
                                                "Deleted numeric filter" = "Numerical"),
                                  selected=character(0))),
      column(width=3,uiOutput("legendForExprsData2"))
    ),
    hr(),
    uiOutput("helpTextMV"),
    mod_download_btns_ui('VizualizeFilteredData_DL_btns'),
    DT::dataTableOutput("VizualizeFilteredData")
    
  )
})


getDataForMetacellFiltered <- reactive({
  req(rv$settings_nDigits)
  rv$deleted.metacell
  
 # browser()
  table <- as.data.frame(round(Biobase::exprs(rv$deleted.metacell),
                               digits = rv$settings_nDigits))
  table <- cbind(id = Biobase::fData(rv$deleted.metacell)[, GetKeyId(rv$deleted.metacell)],
                 table,
                 DAPAR::GetMetacell(rv$deleted.metacell)
  )
  table
})

getDataForNumericalFiltered <- reactive({
  req(rv$settings_nDigits)
  rv$deleted.numeric
  table <- as.data.frame(round(Biobase::exprs(rv$deleted.numeric),
                               digits = rv$settings_nDigits))
  table <- cbind(id = Biobase::fData(rv$deleted.numeric)[, GetKeyId(rv$deleted.numeric)],
                 table, 
                 DAPAR::GetMetacell(rv$deleted.numeric))
  
  table
})


getDataForMVStringFiltered <- reactive({
  req(rv$settings_nDigits)
  rv$deleted.stringBased
  id <- 
  table <- as.data.frame(round(Biobase::exprs(rv$deleted.stringBased),
                               digits=rv$settings_nDigits))
  table <- cbind(id = Biobase::fData(rv$deleted.stringBased)[, GetKeyId(rv$deleted.stringBased)],
                 table, 
                 DAPAR::GetMetacell(rv$deleted.stringBased))
  
  table
})



GetDataFor_VizualizeFilteredData <- reactive({
  
  rv$deleted.metacell
  req(input$ChooseViewAfterFiltering)
  req(input$ChooseTabAfterFiltering)
  rv$deleted.stringBased
  rv$deleted.numeric
  
  data <- NULL
  data <- switch(input$ChooseViewAfterFiltering,
                 Metacell = if(!is.null(rv$deleted.metacell))
                   switch(input$ChooseTabAfterFiltering,
                          quantiData =  getDataForMetacellFiltered(),
                          metaData = Biobase::fData(rv$deleted.metacell)
                          ),
                 StringBased = if(!is.null(rv$deleted.stringBased))
                   switch(input$ChooseTabAfterFiltering,
                          quantiData = getDataForMVStringFiltered(),
                          metaData = Biobase::fData(rv$deleted.stringBased)
                          ),
                 Numerical = if(!is.null(rv$deleted.numeric))
                   switch(input$ChooseTabAfterFiltering,
                          quantiData = getDataForNumericalFiltered(),
                          metaData = Biobase::fData(rv$deleted.numeric)
                          )
                 )
  data
})



mod_download_btns_server(id = 'VizualizeFilteredData_DL_btns',
                         df.data = reactive({
                           
                           if (input$ChooseTabAfterFiltering == 'quantiData'){
                             len <- ncol(GetDataFor_VizualizeFilteredData())
                             GetDataFor_VizualizeFilteredData()[, 1:(1+len/2)]
                           } else GetDataFor_VizualizeFilteredData()
                         }), 
                         name = reactive({'ViewFilteredData'}), 
                         colors = reactive({
                           if (input$ChooseTabAfterFiltering == 'quantiData')
                             list('missing POV' = "lightblue",
                                  'missing MEC' = "orange",
                                  'recovered' = "lightgrey",
                                  'identified' = "white",
                                  'combined' = "red")
                           else NULL}),
                         df.tags = reactive({
                           if (input$ChooseTabAfterFiltering == 'quantiData'){
                             len <- ncol(GetDataFor_VizualizeFilteredData())
                             GetDataFor_VizualizeFilteredData()[, (2 + (len-1)/2):len]
                           } else GetDataFor_VizualizeFilteredData()})
)


#----------------------------------------------
output$VizualizeFilteredData <- DT::renderDataTable(server=TRUE,{
  input$ChooseTabAfterFiltering
  req(GetDataFor_VizualizeFilteredData())
  dt <- NULL
  data <- GetDataFor_VizualizeFilteredData()
  c.tags <- BuildColorStyles(rv$current.obj, rv$colorsTypeMV)$tags
  c.colors <-  BuildColorStyles(rv$current.obj, rv$colorsTypeMV)$colors
  

  if(input$ChooseTabAfterFiltering =="quantiData"){
    dt <- DT::datatable( data,
                         extensions = c('Scroller'),
                         options = list(
                           dom = 'rtip',
                           initComplete = initComplete(),
                           displayLength = 20,
                           deferRender = TRUE,
                           bLengthChange = FALSE,
                           scrollX = 200,
                           scrollY = 600,
                           scroller = TRUE,
                           ordering = FALSE,
                           columnDefs = list(list(targets = c((( 2 + (ncol(data)-1)/2)):ncol(data)), visible = FALSE),
                                             list(width='150px', targets= "_all"))
                         )
    ) %>%
      formatStyle(
        colnames(data)[2:(1 + (ncol(data)-1)/2)],
        colnames(data)[((2 + (ncol(data)-1)/2)):ncol(data)],
        backgroundColor = styleEqual(c.tags, c.colors)
      )
  } else {
    dt <- DT::datatable( data,
                         extensions = 'Scroller',
                         options = list(initComplete = initComplete(),
                                        displayLength = 20,
                                        deferRender = TRUE,
                                        bLengthChange = FALSE,
                                        scrollX = 200,
                                        scrollY = 600,
                                        scroller = TRUE,
                                        ordering = FALSE)) 
  }
  # }
  dt
  
})



#########################################################################################
##
##                    SCREEN 5
## 
###########################################################################################

output$screenFiltering5 <- renderUI({
  
  tagList(
    shinyjs::hidden(div(id = "msg_empty_dataset",
                        p("Please note that the validate button is disabled because
                      the dataset is empty. You should rerun the filtering tool."))),
    actionButton("ValidateFilters","Save filtered dataset",class = actionBtnClass)
  )
})

observe({
  shinyjs::toggleState("ValidateFilters",
                       condition = nrow(rv$current.obj) >0)
  shinyjs::toggle("msg_empty_dataset", condition = nrow(rv$current.obj) == 0)
})

#########################################################
##' Validation of the filters and modification on current object
##' @author Samuel Wieczorek
observeEvent(input$ValidateFilters, ignoreInit = TRUE,{
  
  if (rv$typeOfDataset == "peptide")
    nSteps <- 5
  else 
    nSteps <- 3

  isolate({
    if((nrow(rv$widgets$filtering$metacell_Filter_SummaryDT) > 1)
       || (nrow(rv$widgets$filtering$DT_filterSummary ) > 1)
       || (nrow(rv$widgets$filtering$DT_numfilterSummary ) > 1)){
      
      withProgress(message = 'Save filtered dataset',detail = '', value = 0, {
        incProgress(1/nSteps, detail = 'Building parameters list')
        l.params <- build_ParamsList_Filtering()
        
        incProgress(2/nSteps, detail = 'Saving parameters')
        rv$typeOfDataset <- rv$current.obj@experimentData@other$typeOfData
        name <- paste0("Filtered", ".", rv$typeOfDataset)
        rv$current.obj <- saveParameters(rv$current.obj,name,"Filtering",l.params)
        
        
        
        if (rv$typeOfDataset == "peptide"  && !is.null(rv$proteinId)){
          incProgress(3/nSteps, detail = 'Computing adjacency matrices')
          ComputeAdjacencyMatrices()
        }
        
        if (rv$typeOfDataset == "peptide"  && !is.null(rv$proteinId)){
          incProgress(4/nSteps, detail = 'Computing connected components')
          ComputeConnectedComposants()
        }
      })

      UpdateDatasetWidget(rv$current.obj, name)
    }
    dataOut<- rv$current.obj
    rvModProcess$moduleFilteringDone[5] <- TRUE
    
  })
  
})


mod_LegendColoredExprs_server(id = 'FilterColorLegend_DS')


output$legendForExprsData2 <- renderUI({
  req(input$ChooseTabAfterFiltering)
  req(input$ChooseTabAfterFiltering == "quantiData")
  
  mod_LegendColoredExprs_ui(id = 'FilterColorLegend_DS')
  
})








# disableActionButton <- function(id,session) {
#   session$sendCustomMessage(type="jsCode",
#                             list(code= paste("$('#",id,"').prop('disabled',true)"
#                                              ,sep="")))
# }

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




output$helpTextMV <- renderUI({
  helpText("After checking the data, validate the filters.")
})

# 
# 
# return(reactive({dataOut}))
# 
# }