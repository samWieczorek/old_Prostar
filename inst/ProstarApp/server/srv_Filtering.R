callModule(moduleProcess, "moduleProcess_Filtering",
           isDone = reactive({rvModProcess$moduleFilteringDone}),
           pages = reactive({rvModProcess$moduleFiltering}),
           rstFunc = resetModuleFiltering,
           forceReset = reactive({rvModProcess$moduleFilteringForceReset })  )



resetModuleFiltering <- reactive({
  #req(input$datasets)
  ## update rv$widgets values (reactive values)
  resetModuleProcess("Filtering")
  
  
  rv$widgets$filtering$ChooseFilters <- "None"
  rv$widgets$filtering$seuilNA <- 0
  rv$widgets$filtering$seuilNA_percent <- 0
  rv$widgets$filtering$val_vs_percent <- 'Value'
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
  
  rv$widgets$filtering$ChooseFilters_byMSMS <- "None"
  rv$widgets$filtering$seuil_byMSMS <- 0
  rv$widgets$filtering$seuil_percent_byMSMS <- 0
  rv$widgets$filtering$val_vs_percent_byMSMS <- 'Value'
  
  rv$widgets$filtering$temp.dataClass <- "Missing"
  rv$widgets$filtering$temp.ChooseFilters <- "WholeMatrix"
  rv$widgets$filtering$temp.remove <- 'Keep'
  rv$widgets$filtering$temp.seuilNA <- 0
  rv$widgets$filtering$temp.seuilNA_percent <- 0
  rv$widgets$filtering$temp.val_vs_percent <- 'Value'
  rv$widgets$filtering$temp.numericFilter_operator <- '<='
  rv$widgets$filtering$temp.DT_numfilterSummary <- data.frame(Filter=NULL,
                                                              Condition=NULL,
                                                              nbDeleted=NULL,
                                                              Total=NULL,
                                                              stringsAsFactors=F)
  
  
  rv$deleted.stringBased <- NULL
  rv$deleted.mvLines <- NULL
  rv$deleted.byMSMSLines <- NULL
  rv$deleted.numeric <- NULL
  
  rv$current.obj <- rv$dataset[[input$datasets]]
  rvModProcess$moduleFilteringDone = rep(FALSE, 6)
  
})






#########################################################################################
##
##                    SCREEN 1
## 
###########################################################################################

output$screenFiltering1 <- renderUI({
  
  isolate({
    tagList(
      div(
        id = "screen1Filtering",
        
        ############################################################
        div(style="border: 1px black solid; height: auto; padding: 5px",
            #div(HTML("Empty Lines")),
            fluidRow(
              column(1,
                     p(style = "font-size: xx-small ; text-align: center ;",
                       HTML("Empty Lines"))
              ),
              column(2,
                     # 1) Among M, O, R, I and U (last four can be combined or taken separatly)
                     selectInput("temp.dataClass",
                                 "Choose the class of the quantitative data",
                                 choices = c("quanti", "missing", "imputed", "combined"),
                                 # get dynamic, see depth of the label tree, if pept or prot ...
                                 width='200px')
              ),
              column(2,
                     p(style = "font-size: xx-small ; text-align: center ;",
                       HTML("Keep or remove")),
                     # 2) Include or exclude lines according to M or [O, R, I, U]
                     uiOutput("temp.keepOrRemove_ui")
              ),
              column(2,
                     p(style = "font-size: xx-small ; text-align: center ;",
                       HTML("According to conditions")),
                     # 3) According to conditions: Whole Matrix, All Cond or At least one cond
                     selectInput("temp.ChooseFilters","",
                                 choices = gFiltersList[-c(1,2)],
                                 selected = rv$widgets$filtering$temp.ChooseFilters,
                                 width='200px')
              ),
              column(2,
                     p(style = "font-size: xx-small ; text-align: center ;",
                       HTML("Threshold")),
                     # 4.1) Threshold in percent or absolute ?
                     uiOutput("temp.seuilNADelete_ui")
              ),
              column(2,
                     p(style = "font-size: xx-small ; text-align: center ;",
                       HTML("Threshold Value")),
                     # 4.2) Value of the threshold
                     fluidRow(
                       column(6,
                              selectInput("temp.numericFilter_operator", #btn_numFilter
                                          NULL,
                                          choices = c('<=' = '<=',
                                                      '<' = '<',
                                                      '>=' = '>=',
                                                      '>' = '>'),
                                          width='100px')
                       ),
                       column(6,
                              uiOutput('temp.keepVal_ui'),
                              uiOutput('temp.keepVal_percent_ui'))),
                     uiOutput('temp.keep_helptext')
              ),
              column(1,
                     p(style = "font-size: xx-small ; text-align: center ;",
                       HTML("\'+\' if want supp filtration"))
              )
            ),
            div( style="display:inline-block; vertical-align: middle;",
                 actionButton("temp.perform.filtering", "temp Perform MV filtering", class = actionBtnClass)
            ),
        ),
        tags$hr(),
        # div( style="display:inline-block; vertical-align: middle; align: center;",
        #      DT::dataTableOutput("temp.FilterSummaryData")
        # ),
        uiOutput("temp.ObserverMVFilteringDone_ui"),
        
        tags$hr(style="border-top: 3px double black;"),
        
        #####################################################################################################################
        # tags$div(
        div(style="display:inline-block; vertical-align: middle; padding-right: 40px;",
            modulePopoverUI("modulePopover_Help_NA_Filtering"),
            selectInput("ChooseFilters","",
                        choices = gFiltersList,
                        selected=rv$widgets$filtering$ChooseFilters,
                        width='200px')
            
        ),
        div( style="display:inline-block; vertical-align: middle;  padding-right: 40px;",
             uiOutput("seuilNADelete")
        ),
        div( style="display:inline-block; vertical-align: middle;",
             actionButton("perform.filtering.MV", "Perform MV filtering", class = actionBtnClass)
        ),
        hr(),
        mod_plots_mv_histo_ui("MVPlots_filtering"),
        uiOutput("ObserverMVFilteringDone")
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
output$temp.keepOrRemove_ui <- renderUI({
  
  text <- paste("Choose either to keep or remove lines containing: ",rv$widgets$filtering$temp.dataClass," data.")
  radioButtons("temp.remove",
               text,
               choices = c("Keep" = "Keep", "Remove" = "Remove"),
               selected = rv$widgets$filtering$temp.remove)
  
})


output$temp.seuilNADelete_ui <- renderUI({
  
  text <- paste("#/% of values to ",rv$widgets$filtering$temp.remove)
  radioButtons('temp.val_vs_percent',
               text, 
               choices = c('Value'='Value', 'Percentage'='Percentage'),
               selected = rv$widgets$filtering$temp.val_vs_percent
  )
  
})


output$temp.keepVal_ui <- renderUI({
  req(rv$widgets$filtering$temp.val_vs_percent)
  if (rv$widgets$filtering$temp.val_vs_percent != 'Value') {return(NULL)}
  
  tagList(
    modulePopoverUI("modulePopover_keepVal"),
    selectInput("temp.seuilNA", NULL,
                choices =  getListNbValuesInLines(rv$current.obj, 
                                                  type = rv$widgets$filtering$temp.ChooseFilters),
                selected = rv$widgets$filtering$temp.seuilNA,
                width='150px')
  )
})



output$temp.keepVal_percent_ui <- renderUI({
  req(rv$widgets$filtering$temp.val_vs_percent)
  if (rv$widgets$filtering$temp.val_vs_percent != 'Percentage') {return(NULL)}
  
  tagList(
    modulePopoverUI("modulePopover_keepVal_percent"),
    numericInput("temp.seuilNA_percent", NULL,
                 min = 0,
                 max = 100,
                 value = rv$widgets$filtering$temp.seuilNA_percent,
                 width='150px')
  )
})



output$temp.keep_helptext <- renderUI({
  rv$widgets$filtering$temp.ChooseFilters
  
  switch(rv$widgets$filtering$temp.numericFilter_operator,
         '<=' = text_operator <- "inferior or equal",
         '<' = text_operator <- "inferior",
         '>=' = text_operator <- "superior or equal",
         '>' = text_operator <- "superior")
  
  switch(rv$widgets$filtering$temp.ChooseFilters,
         "WholeMatrix" = text_method <- "all the matrix.",
         "AllCond" = text_method <- "every condition.",
         "AtLeastOneCond" = text_method <- "at least one condition.")
  
  if(rv$widgets$filtering$temp.val_vs_percent == 'Value'){
    text_threshold <- rv$widgets$filtering$temp.seuilNA
  } else {
    text_threshold <- paste(rv$widgets$filtering$temp.seuilNA_percent*100,"%", sep="")
  }
  
  
  txt_summary <- paste("You are going to ",
                       rv$widgets$filtering$temp.remove,
                       " lines where number of ",
                       rv$widgets$filtering$temp.dataClass,
                       " data are ",
                       text_operator,
                       " to ",
                       text_threshold,
                       " in ",
                       text_method)
  
  tags$p(txt_summary, style = "font-size: small; text-align : center; color: purple;")
  
})


## Perform filtration
observeEvent(input$temp.perform.filtering, ignoreInit=TRUE,{
  rv$widgets$filtering$temp.dataClass
  rv$widgets$filtering$temp.remove
  rv$widgets$filtering$temp.ChooseFilters
  rv$widgets$filtering$temp.seuilNA
  rv$widgets$filtering$temp.seuilNA_percent
  rv$widgets$filtering$temp.val_vs_percent
  rv$widgets$filtering$temp.numericFilter_operator
  
  th <- NULL
  if (rv$widgets$filtering$temp.val_vs_percent == 'Percentage')
    th <- as.numeric(rv$widgets$filtering$temp.seuilNA_percent)
  else
    th <- as.integer(rv$widgets$filtering$temp.seuilNA)
  
  
  # print("rv$current.obj")
  # print(rv$current.obj)
  # print("rv$widgets$filtering$temp.dataClass")
  # print(rv$widgets$filtering$temp.dataClass)
  # print("rv$widgets$filtering$temp.remove")
  # print(rv$widgets$filtering$temp.remove == 'Remove')
  # print("rv$widgets$filtering$temp.ChooseFilters")
  # print(rv$widgets$filtering$temp.ChooseFilters)
  # print("rv$widgets$filtering$temp.val_vs_percent")
  # print(rv$widgets$filtering$temp.val_vs_percent == 'Percentage')
  # print("rv$widgets$filtering$temp.numericFilter_operator")
  # print(rv$widgets$filtering$temp.numericFilter_operator)
  # print("th")
  # print(th)
  # print("level")
  # print(rv$current.obj@experimentData@other$typeOfData)
  
  
  keepThat <- DAPAR::filterGetIndices(obj = rv$current.obj,
                                      metacell = rv$widgets$filtering$temp.dataClass,
                                      remove = rv$widgets$filtering$temp.remove == 'Remove',
                                      condition = rv$widgets$filtering$temp.ChooseFilters,
                                      percent = rv$widgets$filtering$temp.val_vs_percent == 'Percentage',
                                      operator = rv$widgets$filtering$temp.numericFilter_operator,
                                      threshold = th)
  
  
  
  if (!is.null(keepThat)) {
    rv$temp.deleted.mvLines <- rv$current.obj[-keepThat]
    rv$current.obj <- mvFilterFromIndices(rv$current.obj,
                                          keepThat,
                                          GetFilterText(rv$widgets$filtering$temp.ChooseFilters,
                                                        th)
    )
  }
  
  View(fData(rv$temp.deleted.mvLines))
  View(fData(rv$current.obj))
  rvModProcess$moduleFilteringDone[1] <- TRUE
  
  ######
  ######
  ######
  ######
  ######
  # ########################
  # 
  # observeEvent(input$btn_numFilter,ignoreInit=TRUE,{
  #   temp <- rv$current.obj
  #   
  #   cname <- colnames(exprs(temp))
  #   print(cname)
  #   print("cname")
  #   tagValue <- th
  #   print(th)
  #   print("th")
  #   
  #   #############################
  #   # moche, a changer dans DAPAR
  #   ind <- vector()
  #   for (i in 1:length(cname)) {
  #     ind_col <- NumericalgetIndicesOfLinesToRemove(temp,cname[i], value, operator)
  #     print(i)
  #     print(cname[i])
  #     print(ind_col)
  #     ind[,ncol(ind)+1] <- ind_col
  #   }
  #   ind <- unique(ind)# lignes a supprimer de part la matrice
  #   #############################
  #   
  #   deleted <- temp[ind]
  #   
  #   temp <- deleteLinesFromIndices(temp, ind,
  #                                  paste("\"",
  #                                        length(ind),
  #                                        " lines were removed from dataset.\"",
  #                                        sep=""))
  #   nbDeleted <- 0
  #   
  #   
  #   if (!is.null(deleted)){
  #     rv$deleted.numeric <- rbindMSnset(rv$deleted.numeric, deleted)
  #     nbDeleted <-  nrow(deleted)
  #   } else {
  #     nbDeleted <-  0
  #   }
  #   rv$current.obj <- telmp
  #   
  #   df <- data.frame(Filter=cname,
  #                    Condition=paste0(input$numericFilter_operator,' ',tagValue),
  #                    nbDeleted=nbDeleted,
  #                    Total=nrow(rv$current.obj))
  #   rv$widgets$filtering$DT_numfilterSummary <- rbind(rv$widgets$filtering$DT_numfilterSummary, df)
  #   
  # })
  
})



output$temp.ObserverMVFilteringDone_ui <- renderUI({
  req(rv$temp.deleted.mvLines)
  
  n <- 0
  if(!is.null(rv$temp.deleted.mvLines)){n <- nrow(rv$temp.deleted.mvLines)}
  if (!rvModProcess$moduleFilteringDone[1])
  {return(NULL)  }
  else {
    h5(paste0("Missing values filtering done. ",n, " lines were deleted."))
  }
})

######
######
######
######
######
# output$temp.FilterSummaryData <- DT::renderDataTable(server=TRUE,{
#   req(rv$current.obj)
#   req(rv$widgets$filtering$temp.DT_numfilterSummary)
#   isolate({
#     
#     if (nrow(rv$widgets$filtering$temp.DT_filterSummary )==0){
#       df <- data.frame(Filter="-",
#                        Prefix="-",
#                        nbDeleted=0,
#                        Total=nrow(rv$current.obj),
#                        stringsAsFactors = FALSE)
#       rv$widgets$filtering$temp.DT_filterSummary <- df
#     }
#     
#     
#     DT::datatable(rv$widgets$filtering$temp.DT_filterSummary,
#                   extensions = c('Scroller', 'Buttons'),
#                   rownames = FALSE,
#                   options=list(buttons = list('copy',
#                                               list(
#                                                 extend = 'csv',
#                                                 filename = 'Filtering_summary'
#                                               ),'print'),
#                                dom='Brt',
#                                initComplete = initComplete(),
#                                deferRender = TRUE,
#                                bLengthChange = FALSE
#                   ))
#   })
# })



observeEvent(input$temp.dataClass,{
  rv$widgets$filtering$temp.dataClass <- input$temp.dataClass
})


observeEvent(input$temp.remove, {
  rv$widgets$filtering$temp.remove <- input$temp.remove
})


observeEvent(input$temp.ChooseFilters,{
  rv$widgets$filtering$temp.ChooseFilters <- input$temp.ChooseFilters
})


observeEvent(input$temp.seuilNA, ignoreNULL = TRUE, ignoreInit = TRUE, {
  rv$widgets$filtering$temp.seuilNA <- input$temp.seuilNA
})


observeEvent(input$temp.seuilNA_percent, ignoreNULL = TRUE, ignoreInit = TRUE, {
  rv$widgets$filtering$temp.seuilNA_percent <- input$temp.seuilNA_percent
})


observeEvent(input$temp.val_vs_percent, {
  rv$widgets$filtering$temp.val_vs_percent <- input$temp.val_vs_percent
})


observeEvent(input$temp.numericFilter_operator,{
  rv$widgets$filtering$temp.numericFilter_operator <- input$temp.numericFilter_operator
})



##############################################################################################






#########################################################
##' Show the widget (slider input) for filtering
##' @author Samuel Wieczorek
output$seuilNADelete <- renderUI({
  req(rv$widgets$filtering$ChooseFilters)
  
  if ((rv$widgets$filtering$ChooseFilters=="None") || (rv$widgets$filtering$ChooseFilters==gFilterEmptyLines)) {
    return(NULL)   
  }
  
  tagList(
    shinyjs::useShinyjs(),
    radioButtons('val_vs_percent', '#/% of values to keep', 
                 choices = c('Value'='Value', 'Percentage'='Percentage'),
                 selected = rv$widgets$filtering$val_vs_percent
    ),
    
    uiOutput('keepVal_ui'),
    uiOutput('keepVal_percent_ui'),
    uiOutput('keep_helptext')
  )
})


output$keepVal_ui <- renderUI({
  req(rv$widgets$filtering$val_vs_percent)
  if (rv$widgets$filtering$val_vs_percent != 'Value') {return(NULL)}
  if (rv$widgets$filtering$ChooseFilters %in% c('None', 'Emptylines')) {return(NULL)}
  #browser()
  tagList(
    modulePopoverUI("modulePopover_keepVal"),
    selectInput("seuilNA", NULL,
                choices =  getListNbValuesInLines(rv$current.obj, 
                                                  type=rv$widgets$filtering$ChooseFilters),
                selected = rv$widgets$filtering$seuilNA,
                width='150px')
  )
})




output$keepVal_percent_ui <- renderUI({
  req(rv$widgets$filtering$val_vs_percent)
  if (rv$widgets$filtering$val_vs_percent != 'Percentage') {return(NULL)}
  
  tagList(
    modulePopoverUI("modulePopover_keepVal_percent"),
    numericInput("seuilNA_percent", NULL,
                 min = 0,
                 max = 100,
                 value = rv$widgets$filtering$seuilNA_percent,
                 width='150px')
  )
})


output$keep_helptext <- renderUI({
  rv$widgets$filtering$ChooseFilters
  txt <- NULL
  switch(rv$widgets$filtering$ChooseFilters,
         None = txt <-"All lines will be kept",
         EmptyLines = txt <-"All lines containing only missing values are removed.",
         WholeMatrix = {
           if (rv$widgets$filtering$val_vs_percent == 'Value')
             txt <- paste0("Only the lines (across all conditions) which contain at least ",
                           rv$widgets$filtering$seuilNA, 
                           " quantitative value are kept.")
           else if (rv$widgets$filtering$val_vs_percent == 'Percentage')
             txt <- paste0("The lines (across all conditions) which contain at least ",
                           rv$widgets$filtering$seuilNA_percent, 
                           "% of quantitative value are kept.")
         },
         AtLeastOneCond = {
           if (rv$widgets$filtering$val_vs_percent == 'Value')
             txt <- paste0("The lines which contain at least ",
                           rv$widgets$filtering$seuilNA, 
                           " quantitative value in at least one condition, are kept.")
           else if (rv$widgets$filtering$val_vs_percent == 'Percentage')
             txt <- paste0("The lines which contain at least ",
                           rv$widgets$filtering$seuilNA_percent, 
                           "% of quantitative value in at least one condition, are kept.")
         },
         AllCond = {
           if (rv$widgets$filtering$val_vs_percent == 'Value')
             txt <- paste0("The lines which contain at least ",
                           rv$widgets$filtering$seuilNA, 
                           " quantitative value in each condition are kept.")
           else if (rv$widgets$filtering$val_vs_percent == 'Percentage')
             txt <- paste0("The lines which contain at least ",
                           rv$widgets$filtering$seuilNA_percent, 
                           "% of quantitative value in each condition are kept.")
         }
  )
  tagList(
    tags$p(txt)
  )
})





observeEvent(input$val_vs_percent, {
  
  rv$widgets$filtering$val_vs_percent <- input$val_vs_percent
  #shinyjs::toggle('keepVal', condition = rv$widgets$filtering$val_vs_percent == 'Value')
  #shinyjs::toggle('keepVal_percent', condition = rv$widgets$filtering$val_vs_percent == 'Percentage')
})



observeEvent(input$ChooseFilters,{
  rv$widgets$filtering$ChooseFilters <- input$ChooseFilters
})

observeEvent(input$seuilNA, ignoreNULL = TRUE, ignoreInit = TRUE, {
  rv$widgets$filtering$seuilNA <- input$seuilNA
})


observeEvent(input$seuilNA_percent, ignoreNULL = TRUE, ignoreInit = TRUE, {
  rv$widgets$filtering$seuilNA_percent <- input$seuilNA_percent
})


##
## Perform missing values filtering
observeEvent(input$perform.filtering.MV, ignoreInit=TRUE,{
  rv$widgets$filtering$ChooseFilters
  rv$widgets$filtering$seuilNA
  rv$widgets$filtering$seuilNA_percent
  rv$widgets$filtering$val_vs_percent
  
  
  if (rv$widgets$filtering$ChooseFilters == gFilterNone){
    #rv$current.obj <- rv$dataset[[input$datasets]]
  } else {
    
    th <- NULL
    if (rv$widgets$filtering$val_vs_percent == 'Percentage')
      th <- as.numeric(rv$widgets$filtering$seuilNA_percent)/100
    else
      th <-  as.integer(rv$widgets$filtering$seuilNA)
    
    keepThat <- mvFilterGetIndices(obj = rv$current.obj,
                                   percent = rv$widgets$filtering$val_vs_percent == 'Percentage',
                                   condition = rv$widgets$filtering$ChooseFilters,
                                   threshold = th)
    
    if (!is.null(keepThat))
    {
      rv$deleted.mvLines <- rv$current.obj[-keepThat]
      rv$current.obj <- mvFilterFromIndices(rv$current.obj,
                                            keepThat,
                                            GetFilterText(rv$widgets$filtering$ChooseFilters, 
                                                          th)
      )
    }
  }
  
  rvModProcess$moduleFilteringDone[1] <- TRUE
  #updateSelectInput(session, "ChooseFilters", selected = input$ChooseFilters)
  #updateSelectInput(session, "seuilNA", selected = input$seuilNA)
})



output$ObserverMVFilteringDone <- renderUI({
  req(rv$deleted.mvLines)
  #isolate({
  
  n <- 0
  if(!is.null(rv$deleted.mvLines)){n <- nrow(rv$deleted.mvLines)}
  if (!rvModProcess$moduleFilteringDone[1])
  {return(NULL)  }
  else {
    h5(paste0("Missing values filtering done. ",n, " lines were deleted."))
  }
  
  # })
})

# switch(rv$widgets$filtering$val_vs_percent,
#        Value = {
#          keepThat <- mvFilterGetIndices(rv$current.obj,
#                                         rv$widgets$filtering$ChooseFilters,
#                                         as.integer(rv$widgets$filtering$seuilNA))
#          if (!is.null(keepThat))
#          {
#            rv$deleted.mvLines <- rv$current.obj[-keepThat]
#            rv$current.obj <- mvFilterFromIndices(rv$current.obj,
#                                                  keepThat,
#                                                  GetFilterText(rv$widgets$filtering$ChooseFilters, as.integer(input$seuilNA)))
#          }
#        },
#        Percentage = {
#          rv$current.obj <- filterByProportion(obj = rv$current.obj,
#                                               intensities_proportion = as.numeric(rv$widgets$filtering$seuilNA_percent)/100,
#                                               mode = rv$widgets$filtering$ChooseFilters
#          )
#          
#        }
#        
# )
#   }
#   r
# })


#########################################################
##' Show the widget for filters
##' @author Samuel Wieczorek
output$choixFiltres <- renderUI({
  req(input$file)
  radioButtons("ChooseFilters","Filtering options",choices = gFiltersList)
  
})



#########################################################################################
##
##                    SCREEN xxx - valeurs d'abondance par 'MS/MS'
## 
###########################################################################################

output$screenFilteringxxx <- renderUI({
  
  isolate({
    tagList(
      div(
        id = "screenxxxFiltering",
        uiOutput("warning_byMSMS"),
        div( style="display:inline-block; vertical-align: middle; padding-right: 40px;",
             modulePopoverUI("modulePopover_Help_Filtering_byMSMS"),
             selectInput("ChooseFilters_byMSMS","",
                         choices = gFiltersList,
                         selected=rv$widgets$filtering$ChooseFilters_byMSMS,
                         width='200px')
             
        ),
        div( style="display:inline-block; vertical-align: middle;  padding-right: 40px;",
             uiOutput("seuilDelete_byMSMS")
        ),
        div( style="display:inline-block; vertical-align: middle;",
             actionButton("perform.filtering_byMSMS", "Perform Identification filtering", class = actionBtnClass)
        ),
        hr(),
        mod_plots_mv_histo_ui("MVPlots_filtering_byMSMS"),
        uiOutput("ObserverMVFilteringDone_byMSMS")
      )
      
    )
  })
  
})


callModule(mod_plots_mv_histo_server, "MVPlots_filtering_byMSMS", 
           data = reactive({rv$current.obj}),
           pal = reactive({rv$PlotParams$paletteForConditions})
)

callModule(modulePopover,"modulePopover_Help_Filtering_byMSMS", 
           data = reactive(list(title = HTML("<strong>Type</strong>"),
                                content= HTML(paste0("To filter the features according to their identification method (by MS/MS), the choice of the lines to be kept is made by different options:"),
                                              ("<ul>"),
                                              ("<li><strong>None</strong>: No filtering, the quantitative data is left unchanged.</li>"),
                                              ("<li><strong>(Remove) Empty lines</strong>: All the lines with no &quotby MS/MS&quot value are removed.</li>"),
                                              ("<li><strong>Whole Matrix</strong>: The lines (across all conditions) which contain less &quotby MS/MS&quot value than a user-defined threshold are kept;</li>"),
                                              ("<li><strong>For every condition</strong>: The lines for which each condition contain less &quotby MS/MS&quot value than a user-defined threshold are deleted;</li>"),
                                              ("<li><strong>At least one condition</strong>: The lines for which at least one condition contain less &quotby MS/MS&quot value than a user-defined threshold are deleted.</li>"),
                                              ("</ul>")
                                )
           )
           )
)


output$warning_byMSMS <- renderUI({
  
  fData <- fData(rv$current.obj)[,rv$current.obj@experimentData@other$OriginOfValues]
  IdentificationData <- fData[DAPAR::is.byMSMS(fData)]
  
  if(length(IdentificationData) == 0){
    txt <- paste0("Warning ! Your dataset contains no 'by MS/MS' values. 
                  If you filter the lines without 'by MS/MS', your dataset will be blank.")
    div(style="display:inline-block; vertical-align: middle; padding-right: 40px;",
        tagList(
          tags$p(txt, style= 'color: red ; font-size: 1.2em ;font-weight: bold ;')
        ))
  }
})


output$seuilDelete_byMSMS <- renderUI({
  req(rv$widgets$filtering$ChooseFilters_byMSMS)
  
  if ((rv$widgets$filtering$ChooseFilters_byMSMS=="None") || (rv$widgets$filtering$ChooseFilters_byMSMS==gFilterEmptyLines)) {
    return(NULL)   
  }
  
  tagList(
    shinyjs::useShinyjs(),
    radioButtons('val_vs_percent_byMSMS', '#/% of values to keep', 
                 choices = c('Value'='Value', 'Percentage'='Percentage'),
                 selected = rv$widgets$filtering$val_vs_percent_byMSMS
    ),
    
    uiOutput('keepVal_ui_byMSMS'),
    uiOutput('keepVal_percent_ui_byMSMS'),
    uiOutput('keep_helptext_byMSMS')
  )
})



output$keepVal_ui_byMSMS <- renderUI({
  req(rv$widgets$filtering$val_vs_percent_byMSMS)
  if (rv$widgets$filtering$val_vs_percent_byMSMS != 'Value') {return(NULL)}
  if (rv$widgets$filtering$ChooseFilters_byMSMS %in% c('None', 'Emptylines')) {return(NULL)}
  tagList(
    modulePopoverUI("modulePopover_keepVal_byMSMS"),
    selectInput("seuil_byMSMS", NULL,
                choices =  getListNbValuesInLines(rv$current.obj, 
                                                  type=rv$widgets$filtering$ChooseFilters_byMSMS),
                selected = rv$widgets$filtering$seuil_byMSMS,
                width='150px')
  )
})

output$keepVal_percent_ui_byMSMS <- renderUI({
  req(rv$widgets$filtering$val_vs_percent_byMSMS)
  if (rv$widgets$filtering$val_vs_percent_byMSMS != 'Percentage') {return(NULL)}
  
  tagList(
    modulePopoverUI("modulePopover_keepVal_percent_byMSMS"),
    numericInput("seuil_percent_byMSMS", NULL,
                 min = 0,
                 max = 100,
                 value = rv$widgets$filtering$seuil_percent_byMSMS,
                 width='150px')
  )
})



output$keep_helptext_byMSMS <- renderUI({
  rv$widgets$filtering$ChooseFilters_byMSMS
  txt <- NULL
  switch(rv$widgets$filtering$ChooseFilters_byMSMS,
         None = txt <-"All lines will be kept",
         EmptyLines = txt <-"All lines containing 'by MS/MS' are removed.",
         WholeMatrix = {
           if (rv$widgets$filtering$val_vs_percent_byMSMS == 'Value')
             txt <- paste0("Only the lines (across all conditions) which contain at least ",
                           rv$widgets$filtering$seuil_byMSMS, 
                           " 'by MS/MS' value are kept.")
           else if (rv$widgets$filtering$val_vs_percent_byMSMS == 'Percentage')
             txt <- paste0("The lines (across all conditions) which contain at least ",
                           rv$widgets$filtering$seuil_percent_byMSMS, 
                           "% of 'by MS/MS' value are kept.")
         },
         AtLeastOneCond = {
           if (rv$widgets$filtering$val_vs_percent_byMSMS == 'Value')
             txt <- paste0("The lines which contain at least ",
                           rv$widgets$filtering$seuil_byMSMS, 
                           " 'by MS/MS' value in at least one condition, are kept.")
           else if (rv$widgets$filtering$val_vs_percent_byMSMS == 'Percentage')
             txt <- paste0("The lines which contain at least ",
                           rv$widgets$filtering$seuil_percent_byMSMS, 
                           "% of 'by MS/MS' value in at least one condition, are kept.")
         },
         AllCond = {
           if (rv$widgets$filtering$val_vs_percent_byMSMS == 'Value')
             txt <- paste0("The lines which contain at least ",
                           rv$widgets$filtering$seuil_byMSMS, 
                           " 'by MS/MS' value in each condition are kept.")
           else if (rv$widgets$filtering$val_vs_percent_byMSMS == 'Percentage')
             txt <- paste0("The lines which contain at least ",
                           rv$widgets$filtering$seuil_percent_byMSMS, 
                           "% of 'by MS/MS' value in each condition are kept.")
         }
  )
  tagList(
    tags$p(txt)
  )
})


observeEvent(input$val_vs_percent_byMSMS, {
  
  rv$widgets$filtering$val_vs_percent_byMSMS <- input$val_vs_percent_byMSMS
})



observeEvent(input$ChooseFilters_byMSMS,{
  rv$widgets$filtering$ChooseFilters_byMSMS <- input$ChooseFilters_byMSMS
})

observeEvent(input$seuil_byMSMS, ignoreNULL = TRUE, ignoreInit = TRUE, {
  rv$widgets$filtering$seuil_byMSMS <- input$seuil_byMSMS
})

observeEvent(input$seuil_percent_byMSMS, ignoreNULL = TRUE, ignoreInit = TRUE, {
  rv$widgets$filtering$seuil_percent_byMSMS <- input$seuil_percent_byMSMS
})


## Perform missing values filtering
observeEvent(input$perform.filtering_byMSMS, ignoreInit=TRUE,{
  rv$widgets$filtering$ChooseFilters_byMSMS
  rv$widgets$filtering$seuil_byMSMS
  rv$widgets$filtering$seuil_percent_byMSMS
  rv$widgets$filtering$val_vs_percent_byMSMS
  
  
  if (rv$widgets$filtering$ChooseFilters_byMSMS == gFilterNone){
    #rv$current.obj <- rv$dataset[[input$datasets]]
  } else {
    
    th <- NULL
    if (rv$widgets$filtering$val_vs_percent_byMSMS == 'Percentage')
      th <- as.numeric(rv$widgets$filtering$seuil_percent_byMSMS)/100
    else
      th <-  as.integer(rv$widgets$filtering$seuil_byMSMS)
    
    keepThat <- DAPAR::mvFilterGetIndices_Marianne(obj = rv$current.obj,
                                                   percent = rv$widgets$filtering$val_vs_percent_byMSMS == 'Percentage',
                                                   condition = rv$widgets$filtering$ChooseFilters_byMSMS,
                                                   threshold = th)
    
    
    if (length(keepThat) != 0) {
      rv$deleted.byMSMSLines <- rv$current.obj[-keepThat]
      rv$current.obj <- mvFilterFromIndices(obj = rv$current.obj,
                                            keepThat = keepThat,
                                            processText = GetFilterText(rv$widgets$filtering$ChooseFilters_byMSMS, th)
      )
    } else {
      browser()
      rv$deleted.byMSMSLines <- rv$current.obj
      rv$current.obj <- rv$current.obj[-(1:nrow(rv$current.obj))]
    }
  }
  
  # browser()
  rvModProcess$moduleFilteringDone[2] <- TRUE
  
  
})

output$ObserverMVFilteringDone_byMSMS <- renderUI({
  req(rv$deleted.byMSMSLines)
  #isolate({
  
  browser()
  
  n <- 0
  if(!is.null(rv$deleted.byMSMSLines)){n <- nrow(rv$deleted.byMSMSLines)}
  if (!rvModProcess$moduleFilteringDone[2]) {
    return(NULL)
  } else {
    if (nrow(rv$current.obj) == 0) {
      p("Empty dataset because no 'by MS/MS' information.", style= 'color: red ; font-size: 1.2em ;font-weight: bold ;')
    }else{
      h5(paste0("Identification filtering done. ",n, " lines were deleted."))
    }
  }
  
  # })
})




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
                                  choices= list("Deleted on missing values" = "MissingValues",
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




getDataForNumericalFiltered <- reactive({
  req(rv$settings_nDigits)
  rv$deleted.numeric
  table <- as.data.frame(round(Biobase::exprs(rv$deleted.numeric),digits=rv$settings_nDigits))
  table <- cbind(table, Biobase::fData(rv$deleted.numeric)[,rv$deleted.numeric@experimentData@other$OriginOfValues])
  
  table
})


getDataForMVStringFiltered <- reactive({
  req(rv$settings_nDigits)
  rv$deleted.stringBased
  table <- as.data.frame(round(Biobase::exprs(rv$deleted.stringBased),digits=rv$settings_nDigits))
  table <- cbind(table, Biobase::fData(rv$deleted.stringBased)[,rv$deleted.stringBased@experimentData@other$OriginOfValues])
  
  table
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
    if((rv$widgets$filtering$ChooseFilters != gFilterNone)
       || (rv$widgets$filtering$ChooseFilters_byMSMS != gFilterNone)
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
















## symbolic filtering event










getDataForMVFiltered <- reactive({
  req(rv$settings_nDigits)
  rv$deleted.mvLines
  
  table <- as.data.frame(round(Biobase::exprs(rv$deleted.mvLines),digits=rv$settings_nDigits))
  table <- cbind(table, Biobase::fData(rv$deleted.mvLines)[,rv$deleted.mvLines@experimentData@other$OriginOfValues])
  
  table
})





output$legendForExprsData2 <- renderUI({
  req(input$ChooseTabAfterFiltering)
  
  if (input$ChooseTabAfterFiltering != "quantiData"){return(NULL)}
  moduleLegendColoredExprsUI("FilterColorLegend_DS", rv$colorsTypeMV)
  
})



output$Warning_VizualizeFilteredData <- renderUI({
  if (length(GetDataFor_VizualizeFilteredData())==0){return(NULL)}
  if (nrow(GetDataFor_VizualizeFilteredData())>153) 
    p(MSG_WARNING_SIZE_DT)
  
})



GetDataFor_VizualizeFilteredData <- reactive({
  req(rv$settings_nDigits)
  rv$deleted.mvLines
  req(input$ChooseViewAfterFiltering)
  req(input$ChooseTabAfterFiltering)
  rv$deleted.stringBased
  rv$deleted.numeric
  #print("DANS REACTIVE : GetDataFor_VizualizeFilteredData")
  
  
  
  
  data <- NULL
  if ((input$ChooseViewAfterFiltering == "MissingValues") && !is.null(rv$deleted.mvLines))
  {
    #print("DANS REACTIVE : If 1")
    #print(dim(getDataForMVFiltered()))
    switch(input$ChooseTabAfterFiltering,
           quantiData =  data <- getDataForMVFiltered(),
           metaData = data <- cbind(ID = rownames(Biobase::fData(rv$deleted.mvLines)), Biobase::fData(rv$deleted.mvLines))
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