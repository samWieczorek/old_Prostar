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

  
  rv$widgets$filtering$MetacellTag <- "None"
  rv$widgets$filtering$MetacellFilters <- "None"
  rv$widgets$filtering$KeepRemove <- 'delete'
  rv$widgets$filtering$metacell_value_th <- 0
  rv$widgets$filtering$metacell_value_percent <- 0
  rv$widgets$filtering$val_vs_percent <- 'Value'
  rv$widgets$filtering$metacellFilter_operator <- '<='
  rv$widgets$filtering$metacell_Filter_SummaryDT <- data.frame(Label=NULL,
                                                           Remove=NULL,
                                                           Condition=NULL,#condition
                                                           Threshold=NULL,#operator+th
                                                           nbDeleted=NULL,#nb line removed
                                                           Total=NULL,# sum of lines deleted multiple filters
                                                           stringsAsFactors=F)
  
  
  rv$deleted.stringBased <- NULL
  #rv$deleted.mvLines <- NULL
  rv$deleted.metacell <- NULL
  #rv$deleted.byMSMSLines <- NULL
  rv$deleted.numeric <- NULL
  
  rv$current.obj <- rv$dataset[[input$datasets]]
  rvModProcess$moduleFilteringDone = rep(FALSE, 6)
  
})






#########################################################################################
##
##                    SCREEN 1
## 
############################################################################

output$screenFiltering1 <- renderUI({
  
  isolate({
    tagList(
      div(
        id = "screen1Filtering",
        
        ################## Enora's section ###################################
        div(
          #style="border: 1px black solid; height: auto; padding: 10px",
            #div(HTML("Empty Lines")),
            fluidRow(
              column(2,
                     selectInput("chooseMetacellTag",
                                 "Nature of data to filter",
                                 choices = DAPAR::metacell.def(rv$current.obj@experimentData@other$typeOfData),
                                 width='200px')
              ),
              column(2,
                     uiOutput("Choose_keepOrRemove_ui")
              ),
              column(2,
                     selectInput("ChooseMetacellFilters",
                                 "Scope of filter operation.",
                                 choices = c(gFiltersList[1],
                                             "Whole Line"="WholeLine",
                                             gFiltersList[3:length(gFiltersList)]),
                                 selected = rv$widgets$filtering$MetacellFilters,
                                 width='200px')
              ),
              column(6,
                     uiOutput("MetacellFilters_widgets_set2_ui")
              )
            ),
            uiOutput('metacellFilter_request_ui'),
            div( style="display:inline-block; vertical-align: middle;",
                 actionButton("perform.metacell.filtering", 
                              "Perform metacell filtering", 
                              class = actionBtnClass)
            ),
        ),
        tags$hr(),
        div( style="display:inline-block; vertical-align: middle; align: center;",
             DT::dataTableOutput("metacell_Filter_SummaryDT")
        ),
        # uiOutput("temp.ObserverMVFilteringDone_ui"),
        
        tags$hr(style="border-top: 3px double black;"),
        
       
        hr(),
        ################## PLots section ###################################
        mod_plots_mv_histo_ui("MVPlots_filtering")
        #uiOutput("ObserverMVFilteringDone")
      )
      
    )
  })
  
})


callModule(mod_plots_mv_histo_server, "MVPlots_filtering", 
           data = reactive({rv$current.obj}),
           pal = reactive({rv$PlotParams$paletteForConditions})
)

callModule(modulePopover,"modulePopover_Help_NA_Filtering", 
           data = reactive(list(title = HTML("<strong>Type</strong>"),
                                content= HTML(paste0("To filter the missing values, the choice of the lines to be kept is made by different options:"),
                                              ("<ul>"),
                                              ("<li><strong>None</strong>: No filtering, the quantitative data is left unchanged.</li>"),
                                              ("<li><strong>(Remove) Empty lines</strong>: All the lines with 100% of missing values are filtered out.</li>"),
                                              ("<li><strong>Whole Matrix</strong>: The lines (across all conditions) which contain less quantitative value than a user-defined threshold are kept;</li>"),
                                              ("<li><strong>For every condition</strong>: The lines for which each condition contain less quantitative value than a user-defined threshold are deleted;</li>"),
                                              ("<li><strong>At least one condition</strong>: The lines for which at least one condition contain less quantitative value than a user-defined threshold are deleted.</li>"),
                                              ("</ul>")
                                )
           )
           )
)


#####################################################################################################################
############### BEGINNING  OF ENORA'S PART #########################

output$Choose_keepOrRemove_ui <- renderUI({
  
  # text <- paste("Type of filtering operation",
  #               rv$widgets$filtering$MetacellTag,
  #               " data.")
  radioButtons("ChooseKeepRemove",
               "Type of filter operation",
               choices = setNames(nm = c("delete", "keep")),
               selected = rv$widgets$filtering$KeepRemove)
  
})


output$MetacellFilters_widgets_set2_ui <- renderUI({
  req(!(rv$widgets$filtering$MetacellFilters %in% c("None", "WholeLine")))
  
  # if (rv$widgets$filtering$MetacellFilters %in% c("None", "WholeLine"))
  #   {return(NULL)}
  # 
  
  fluidRow(
    column(4,
           radioButtons('choose_val_vs_percent',
                        paste("#/% of values to ", rv$widgets$filtering$KeepRemove),
                        choices = setNames(nm=c('Value', 'Percentage')),
                        selected = rv$widgets$filtering$val_vs_percent
                        )
           ),
    column(4,
           selectInput("choose_metacellFilter_operator",
                       "Choose operator",
                       choices = setNames(nm=DAPAR::SymFilteringOperators()),
                       selected = rv$widgets$filtering$metacellFilter_operator,
                       width='150px')
           ),
    column(4,
           uiOutput('choose_value_ui'),
           uiOutput('choose_percentage_ui')
           )
    )
})


output$choose_value_ui <- renderUI({
  req(rv$widgets$filtering$val_vs_percent == 'Value')
  
  # if (rv$widgets$filtering$val_vs_percent != 'Value') {return(NULL)}
  
  tagList(
    modulePopoverUI("modulePopover_keepVal"),
    selectInput("choose_metacell_value_th", "Choose threshold",
                choices =  getListNbValuesInLines(rv$current.obj, 
                                                  type = rv$widgets$filtering$MetacellFilters),
                selected = rv$widgets$filtering$metacell_value_th,
                width='150px')
  )
})



output$choose_percentage_ui <- renderUI({
  req(rv$widgets$filtering$val_vs_percent == 'Percentage')
  
  #if (rv$widgets$filtering$temp.val_vs_percent != 'Percentage') {return(NULL)}
  
  tagList(
    modulePopoverUI("modulePopover_keepVal_percent"),
    numericInput("choose_metacell_percent_th", 
                 "Choose percentage",
                 min = 0,
                 max = 100,
                 value = rv$widgets$filtering$metacell_percent_th,
                 width='150px')
  )
})



output$metacellFilter_request_ui <- renderUI({
  ###@ req ? ###
  
  if (rv$widgets$filtering$MetacellFilters == "None"){
    txt_summary <- "No filtering is processed on your dataset."
  } else if (rv$widgets$filtering$MetacellFilters == "WholeLine") {
    txt_summary <- paste("You are going to ",
                         rv$widgets$filtering$KeepRemove,
                         "lines that contain only",
                         rv$widgets$filtering$MetacellTag,
                         " data.")
  } else {
    switch(rv$widgets$filtering$metacellFilter_operator,
           '<=' = text_operator <- "less or equal than",
           '<' = text_operator <- "less",
           '>=' = text_operator <- "greater or equal than ",
           '>' = text_operator <- "greater than",
           '==' = text_operator <- "equal",
           '!=' = text_operator <- "different")
    
    switch(rv$widgets$filtering$MetacellFilters,
           "WholeMatrix" = text_method <- "all the matrix.",
           "AllCond" = text_method <- "every condition.",
           "AtLeastOneCond" = text_method <- "at least one condition.")
    
    if(rv$widgets$filtering$val_vs_percent == 'Value'){
      text_threshold <- rv$widgets$filtering$metacell_value_th
    } else {
      text_threshold <- paste(rv$widgets$filtering$metacell_percent_th*100,
                              "%", sep="")
    }
    
    txt_summary <- paste("You are going to ",
                         rv$widgets$filtering$KeepRemove,
                         " lines where number of ",
                         rv$widgets$filtering$MetacellTag,
                         " data is ",
                         text_operator,
                         " to ",
                         text_threshold,
                         " in ",
                         text_method)
  }
  
  
  tags$p(txt_summary, style = "font-size: small; text-align : center; color: purple;")
  
})


## Perform filtration
observeEvent(input$perform.metacell.filtering, ignoreInit=TRUE,{
  rv$widgets$filtering$MetacellTag
  rv$widgets$filtering$KeepRemove
  rv$widgets$filtering$MetacellFilters
  rv$widgets$filtering$metacell_value_th
  rv$widgets$filtering$metacell_percent_th
  rv$widgets$filtering$val_vs_percent
  rv$widgets$filtering$metacellFilter_operator
  
 
  th <- NULL
  if (rv$widgets$filtering$val_vs_percent == 'Percentage') {
    th <- as.numeric(rv$widgets$filtering$metacell_percent_th)
  } else {
    th <- as.integer(rv$widgets$filtering$metacell_value_th)
  }
  
  
  level <- rv$current.obj@experimentData@other$typeOfData
  pattern <- rv$widgets$filtering$MetacellTag
  type <- rv$widgets$filtering$MetacellFilters
  percent <- rv$widgets$filtering$val_vs_percent == 'Percentage'
  op <- rv$widgets$filtering$metacellFilter_operator
  conds <-  Biobase::pData(rv$current.obj)$Condition
  
  mask <- match.metacell(metadata=GetMetacell(rv$current.obj), 
                                  pattern=pattern, 
                                  level=level)

  indices <- switch(rv$widgets$filtering$MetacellFilters,
                    WholeMatrix = GetIndices_WholeMatrix(metacell.mask = mask,
                                                         op = op, 
                                                         percent = percentt, 
                                                         th = th),
                    WholeLine = GetIndices_WholeLine(metacell.mask = mask),
                    AllCond = GetIndices_OnConditions(metacell.mask = mask, 
                                                      type = type, 
                                                      conds = conds, 
                                                      percent = percentt, 
                                                      op = op, 
                                                      th = th),
                    AtLeastOneCond = GetIndices_OnConditions(metacell.mask = mask, 
                                                             type = type,
                                                             conds = conds, 
                                                             percent = percent,
                                                             op = op, 
                                                             th = th)
  )

  browser()
  if (!is.null(indices)) {
    if (rv$widgets$filtering$KeepRemove == 'delete'){
      rv$deleted.metacell <- rv$current.obj[indices]
      rv$current.obj <- rv$current.obj[-indices]
    } else { 
      rv$deleted.metacell <- rv$current.obj[-indices]
      rv$current.obj <- rv$current.obj[indices]
    }
  }
  
  
  rvModProcess$moduleFilteringDone[1] <- TRUE
  
  if (rv$widgets$filtering$MetacellFilters == "WholeLine") {
    df <- data.frame(Label = rv$widgets$filtering$MetacellTag,
                     Remove = rv$widgets$filtering$KeepRemove == 'delete',
                     Condition = rv$widgets$filtering$MetacellFilters,
                     Threshold ='** \'-\' or \'== 100%\'? **',
                     nbDeleted = nrow(rv$deleted.metacell),
                     Total = nrow(rv$current.obj))
  } else {
    df <- data.frame(Label = rv$widgets$filtering$MetacellTag,
                     Remove = rv$widgets$filtering$KeepRemove == 'delete',
                     Condition = rv$widgets$filtering$MetacellFilters,
                     Threshold = paste(rv$widgets$filtering$metacellFilter_operator,
                                     if (rv$widgets$filtering$val_vs_percent == 'Percentage') {paste0(th*100,"%")}
                                     else {th}
                                     ),
                     nbDeleted = nrow(rv$deleted.metacell),
                     Total = nrow(rv$current.obj))
  }
  rv$widgets$filtering$metacell_Filter_SummaryDT <- rbind(rv$widgets$filtering$metacell_Filter_SummaryDT , df)
  
})



output$metacell_Filter_SummaryDT <- DT::renderDataTable(server=TRUE,{
  req(rv$current.obj)
  req(rv$widgets$filtering$metacell_Filter_SummaryDT)
  isolate({
    
    if (nrow(rv$widgets$filtering$metacell_Filter_SummaryDT )==0){
      df <- data.frame(Label="-",
                       Remove="-",
                       Condition="-",
                       Threshold="-",
                       nbDeleted=0,
                       Total=nrow(rv$current.obj),
                       stringsAsFactors = FALSE)
      rv$widgets$filtering$metacell_Filter_SummaryDT <- df
    }
    
    
    DT::datatable(rv$widgets$filtering$metacell_Filter_SummaryDT,
                  extensions = c('Scroller', 'Buttons'),
                  rownames = FALSE,
                  options=list(buttons = list('copy',
                                              list(
                                                extend = 'csv',
                                                filename = 'Metacell_Filtering_summary'
                                              ),'print'),
                               dom='Brt',
                               initComplete = initComplete(),
                               deferRender = TRUE,
                               bLengthChange = FALSE
                  ))
  })
})



observeEvent(input$chooseMetacellTag,{
  rv$widgets$filtering$MetacellTag <- input$chooseMetacellTag
})


observeEvent(input$ChooseKeepRemove, {
  rv$widgets$filtering$KeepRemove <- input$ChooseKeepRemove
})


observeEvent(input$ChooseMetacellFilters,{
  rv$widgets$filtering$MetacellFilters <- input$ChooseMetacellFilters
})


observeEvent(input$choose_metacell_value_th, ignoreNULL = TRUE, ignoreInit = TRUE, {
  rv$widgets$filtering$metacell_value_th <- input$choose_metacell_value_th
})


observeEvent(input$choose_metacell_percent_th, ignoreNULL = TRUE, ignoreInit = TRUE, {
  rv$widgets$filtering$metacell_percent_th <- input$choose_metacell_percent_th
})


observeEvent(input$choose_val_vs_percent, {
  rv$widgets$filtering$val_vs_percent <- input$choose_val_vs_percent
})


observeEvent(input$choose_metacellFilter_operator,{
  rv$widgets$filtering$metacellFilter_operator <- input$choose_metacellFilter_operator
})

############### END OF ENORA'S PART #########################





#########################################################################################
##
##                    SCREEN 2
## 
###########################################################################################



output$screenFiltering2 <- renderUI({
  print("In output$screenFiltering2 <- renderUI")
  tagList(
    
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
  rvModProcess$moduleFilteringDone[3] <- TRUE
  
  df <- data.frame(Filter=cname, Prefix=tagName, nbDeleted=nbDeleted, Total=nrow(rv$current.obj))
  rv$widgets$filtering$DT_filterSummary <- rbind(rv$widgets$filtering$DT_filterSummary , df)
})





output$FilterSummaryData <- DT::renderDataTable(server=TRUE,{
  req(rv$current.obj)
  req(rv$widgets$filtering$DT_numfilterSummary)
  isolate({
    
    if (nrow(rv$widgets$filtering$DT_filterSummary )==0){
      df <- data.frame(Filter="-", Prefix="-", nbDeleted=0, Total=nrow(rv$current.obj), stringsAsFactors = FALSE)
      #rv$widgets$filtering$DT_filterSummary <- rbind(rv$widgets$filtering$DT_numfilterSummary ,df)
      rv$widgets$filtering$DT_filterSummary <- df
    }
    
    
    DT::datatable(rv$widgets$filtering$DT_filterSummary,
                  extensions = c('Scroller', 'Buttons'),
                  rownames = FALSE,
                  options=list(buttons = list('copy',
                                              list(
                                                extend = 'csv',
                                                filename = 'Filtering_summary'
                                              ),'print'),
                               dom='Brt',
                               initComplete = initComplete(),
                               deferRender = TRUE,
                               bLengthChange = FALSE
                  ))
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
    tags$hr(),
    tags$div(
      tags$div( style="display:inline-block; vertical-align: middle; align: center;",
                DT::dataTableOutput("numericalFilterSummaryData")
      )
    )
    
  )
})



## ----------------------------------------------
# Perform numerical filtering
observeEvent(input$btn_numFilter,ignoreInit=TRUE,{
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
  rvModProcess$moduleFilteringDone[4] <- TRUE
  
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
                  extensions = c('Scroller', 'Buttons'),
                  rownames = FALSE,
                  
                  options=list(initComplete = initComplete(),
                               buttons = list('copy',
                                              list(
                                                extend = 'csv',
                                                filename = 'NumericalFiltering_summary'
                                              ),'print'),
                               dom='Brt',
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
                                  choices= list("Deleted on metacell" = "Metacell",
                                                "Deleted string based" = "StringBased",
                                                "Deleted numeric filter" = "Numerical"),
                                  selected=character(0))),
      column(width=3,uiOutput("legendForExprsData2"))
    ),
    hr(),
    uiOutput("helpTextMV"),
    uiOutput("Warning_VizualizeFilteredData"),
    DT::dataTableOutput("VizualizeFilteredData")
    
  )
})


getDataForMetacellFiltered <- reactive({
  req(rv$settings_nDigits)
  rv$deleted.metacell
  table <- as.data.frame(round(Biobase::exprs(rv$deleted.metacell),digits=rv$settings_nDigits))
  table <- cbind(table, DAPAR::GetMetacell(rv$deleted.metacell)
  )
  table
})

getDataForNumericalFiltered <- reactive({
  req(rv$settings_nDigits)
  rv$deleted.numeric
  table <- as.data.frame(round(Biobase::exprs(rv$deleted.numeric),digits=rv$settings_nDigits))
  table <- cbind(table, DAPAR::GetMetacell(rv$deleted.numeric))
  
  table
})


getDataForMVStringFiltered <- reactive({
  req(rv$settings_nDigits)
  rv$deleted.stringBased
  table <- as.data.frame(round(Biobase::exprs(rv$deleted.stringBased),digits=rv$settings_nDigits))
  table <- cbind(table, DAPAR::GetMetacell(rv$deleted.stringBased))
  
  table
})



output$Warning_VizualizeFilteredData <- renderUI({
  if (length(GetDataFor_VizualizeFilteredData())==0)
  {return(NULL)}
  if (nrow(GetDataFor_VizualizeFilteredData())>153) 
    p(MSG_WARNING_SIZE_DT)
  
})



GetDataFor_VizualizeFilteredData <- reactive({
 # req(rv$settings_nDigits)
 print('toto')
  browser()
  rv$deleted.metacell
  req(input$ChooseViewAfterFiltering)
  req(input$ChooseTabAfterFiltering)
  rv$deleted.stringBased
  rv$deleted.numeric
  #print("DANS REACTIVE : GetDataFor_VizualizeFilteredData")
  
  data <- NULL
  if ((input$ChooseViewAfterFiltering == "Metacell") && !is.null(rv$deleted.metacell))
  {
    #print("DANS REACTIVE : If 1")
    #print(dim(getDataForMVFiltered()))
    switch(input$ChooseTabAfterFiltering,
           quantiData =  data <- getDataForMetacellFiltered(),
           metaData = data <- cbind(ID = rownames(Biobase::fData(rv$deleted.metacell)), 
                                    Biobase::fData(rv$deleted.metacell))
    )
  } 
  
  else if ((input$ChooseViewAfterFiltering == "StringBased") && !is.null(rv$deleted.stringBased)) {
    
    #print("DANS REACTIVE : If 2")
    switch(input$ChooseTabAfterFiltering,
           quantiData =  data <- getDataForMVStringFiltered(),
           metaData = data <- Biobase::fData(rv$deleted.stringBased)
    )
  }  else if ((input$ChooseViewAfterFiltering == "Numerical") && !is.null(rv$deleted.numeric)) {
    #print("DANS REACTIVE : If 3")
    switch(input$ChooseTabAfterFiltering,
           quantiData =  data <- getDataForNumericalFiltered(),
           metaData = data <- Biobase::fData(rv$deleted.numeric)
    )
  }
  
  # print("END OF REACTIVE")
  #print(data)
  data
})



#----------------------------------------------
output$VizualizeFilteredData <- DT::renderDataTable(server=TRUE,{
  input$ChooseTabAfterFiltering
  req(GetDataFor_VizualizeFilteredData())
  dt <- NULL
  data <- GetDataFor_VizualizeFilteredData()
  browser()
  if(input$ChooseTabAfterFiltering =="quantiData"){
    dt <- DT::datatable( data,
                         extensions = c('Scroller', 'Buttons'),
                         options = list(
                           buttons = list('copy',
                                          list(
                                            extend = 'csv',
                                            filename = 'Prostar_export'),
                                          'print'),
                           dom='Brtip',
                           initComplete = initComplete(),
                           displayLength = 20,
                           deferRender = TRUE,
                           bLengthChange = FALSE,
                           scrollX = 200,
                           scrollY = 600,
                           scroller = TRUE,
                           ordering=FALSE,
                           columnDefs = list(list(targets = c(((ncol(data)/2)+1):ncol(data)), visible = FALSE),
                                             list(width='150px',targets= "_all"))
                         )
    ) %>%
      formatStyle(
        colnames(data)[1:(ncol(data)/2)],
        colnames(data)[((ncol(data)/2)+1):ncol(data)],
        backgroundColor = styleEqual(BuildColorStyles()$tags, BuildColorStyles()$colors)
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
                                        ordering=FALSE)) 
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
    actionButton("ValidateFilters","Save filtered dataset",class = actionBtnClass)
  )
})



#########################################################
##' Validation of the filters and modification on current object
##' @author Samuel Wieczorek
observeEvent(input$ValidateFilters,ignoreInit = TRUE,{
  
  isolate({
    if((nrow(rv$widgets$filtering$metacell_Filter_SummaryDT) > 1)
       || (nrow(rv$widgets$filtering$DT_filterSummary )>1)
       || (nrow(rv$widgets$filtering$DT_numfilterSummary )>1)){
      l.params <- build_ParamsList_Filtering()
      
      rv$typeOfDataset <- rv$current.obj@experimentData@other$typeOfData
      name <- paste0("Filtered", ".", rv$typeOfDataset)
      rv$current.obj <- saveParameters(rv$current.obj,name,"Filtering",l.params)
      
      dataOut<- rv$current.obj
      rvModProcess$moduleFilteringDone[6] <- TRUE
      
      if (rv$typeOfDataset == "peptide"  && !is.null(rv$proteinId)){
        ComputeAdjacencyMatrices()
        ComputeConnexComposants()
      }
      UpdateDatasetWidget(rv$current.obj, name)
    }
    rvModProcess$moduleFilteringDone[6] <- TRUE
  })
  
})







output$legendForExprsData2 <- renderUI({
  req(input$ChooseTabAfterFiltering)
  
  if (input$ChooseTabAfterFiltering != "quantiData"){return(NULL)}
  moduleLegendColoredExprsUI("FilterColorLegend_DS", rv$colorsTypeMV)
  
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




output$helpTextMV <- renderUI({
  helpText("After checking the data, validate the filters.")
})

# 
# 
# return(reactive({dataOut}))
# 
# }