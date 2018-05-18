


observeEvent(input$actionButtonFilter,{
    rv$current.obj
  temp <- rv$current.obj
  
  if (input$symFilter_cname=="None"){return()}
  
  cname <- input$symFilter_cname
  tagName <- input$symFilter_tagName
  res <- StringBasedFiltering2(temp,cname, input$symFilter_tagName)
  nbDeleted <- 0
  
  if (!is.null(res[["deleted"]])){
  #rv$deleted.stringBased.exprsData <- rbind(rv$deleted.stringBased.exprsData,Biobase::exprs(res[["deleted"]]))
  # rv$deleted.stringBased.fData <- rbind(rv$deleted.stringBased.fData, Biobase::fData(res[["deleted"]])) 
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
                  options=list(initComplete = JS(
                      "function(settings, json) {",
                      "$(this.api().table().header()).css({'background-color': 'darkgrey', 'color': 'black'});",
                      "}"),
                      deferRender = TRUE,
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


output$DP_sidebar_FilterTab1 <- renderUI({
    rv$current.obj
    if (is.null(rv$current.obj)){return()}
   # filter <- NULL
    tag <- rv$current.obj@experimentData@other$mvFilter.method
    if (!is.null(tag)) { filter <- tag}
    
    tagList(
        h4("Options")
        #,bsTooltip(id = "button1", title = "Button 1 Explanation", placement = "right", trigger = "click")
        #,hr()
        ,radioButtons("ChooseFilters","",  choices = gFiltersList),
        uiOutput("seuilNADelete"),
        actionButton("perform.filtering.MV", "Perform MV filtering")
  )
    
   
})


output$DP_sidebar_FilterTab3 <- renderUI({
    
    rv$current.obj
    if (is.null(rv$current.obj)){return()}
    tagList(
        #h4("Filtered data display")
                     #,hr()
                     radioButtons("ChooseTabAfterFiltering", 
                                   "Choose the data to display",
                                   choices=
                                       list("Quantitative data" = "quantiData",
                                            "Meta data" = "MetaData"))
                     ,radioButtons("ChooseViewAfterFiltering", 
                                   "Type of filtered data", 
                            choices=
                            list("Deleted on missing values" = "MissingValues",
                            "Deleted string based" = "StringBased"))
                     ,br(),br()
                     ,checkboxInput("nDigitsMV", 
                                    "Show full length intensities"
                                    , value = FALSE)
    )
})



getDataForMVFiltered <- reactive({
  input$nDigits
  rv$deleted.mvLines
  
  if (!is.null(input$nDigits) && isTRUE(input$nDigits)){nDigits = 1e100} else {nDigits = 3}
  
  table <- as.data.frame(round(Biobase::exprs(rv$deleted.mvLines),digits=nDigits))
  table <- cbind(table, Biobase::fData(rv$deleted.mvLines)[,rv$deleted.mvLines@experimentData@other$OriginOfValues])
  
  table
})




getDataForMVStringFiltered <- reactive({
  input$nDigits
  rv$deleted.stringBased
  
  if (!is.null(input$nDigits) && isTRUE(input$nDigits)){nDigits = 1e100} else {nDigits = 3}
  
  table <- as.data.frame(round(Biobase::exprs(rv$deleted.stringBased),digits=nDigits))
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
     rv$current.obj
     input$nDigitsMV
     rv$deleted.mvLines
     input$ChooseViewAfterFiltering
     input$ChooseTabAfterFiltering
     #rv$deleted.stringBased.exprsData
     #rv$deleted.stringBased.fData
     rv$deleted.stringBased
      
     if (is.null(input$ChooseTabAfterFiltering)
         ||is.null(input$ChooseViewAfterFiltering) 
         ||is.null(input$nDigitsMV) ){
         return(NULL)
         }
     
    if (is.null(input$nDigitsMV)){nDigits = 1e100}
     else {nDigitsMV = 3}
    
    data <- NULL
   if ((input$ChooseViewAfterFiltering == "MissingValues") && !is.null(rv$deleted.mvLines))
        {
        if(input$ChooseTabAfterFiltering == "quantiData" )
            {
          data <- getDataForMVFiltered()
          #data <- cbind(ID = rownames(Biobase::fData(obj)),round(Biobase::exprs(obj), digits=nDigitsMV))
        }else {
            data <- cbind(ID = rownames(Biobase::fData(rv$deleted.mvLines)), Biobase::fData(rv$deleted.mvLines))
            }
    } 
    
    else if ((input$ChooseViewAfterFiltering == "StringBased") && !is.null(rv$deleted.stringBased)) {
        
        if(input$ChooseTabAfterFiltering == "quantiData" )
        {
           data <- getDataForMVStringFiltered()
         # data <-  round(rv$deleted.stringBased.exprsData, digits=nDigitsMV)
        }else {
            data <- Biobase::fData(rv$deleted.stringBased)
            }
    } 
 
    
    
    if (!is.null(data)){
        
        if(input$ChooseTabAfterFiltering =="quantiData"){
                dt <- datatable( data,extensions = 'Scroller',
                     options = list(initComplete = JS(
                         "function(settings, json) {",
                         "$(this.api().table().header()).css({'background-color': 'darkgrey', 'color': 'black'});",
                         "}"),
                         displayLength = 20,
                         deferRender = TRUE,
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
                             options = list(displayLength = 20,
                                            deferRender = TRUE,
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
    input$ChooseFilters
    
    if (is.null(rv$current.obj)) {return(NULL)   }
    if ((input$ChooseFilters==gFilterNone) || (input$ChooseFilters==gFilterEmptyLines)) {return(NULL)   }
    
  choix <- getListNbValuesInLines(rv$current.obj, type=input$ChooseFilters)
   selectInput("seuilNA", 
                "Keep lines with at least x intensity values", 
                choices = choix)
    
})

# 
# GlobalPieChart <- reactive({
#     rv$current.obj
#     rv$nbContaminantsDeleted
#     rv$nbReverseDeleted
#     rv$nbBothDeleted
#     #input$idBoxContaminants
#     #input$prefixContaminants
#     #input$idBoxReverse
#    # input$prefixReverse
#    # if (is.null(rv$current.obj)) {return()}
#    # if (is.null(rv$nbContaminantsDeleted) || is.null(rv$nbReverseDeleted)){return(NULL)}
#    
#     # result = tryCatch(
#     #     {
#     #         
#     isolate({
#         proportionConRev_HC(rv$nbBothDeleted,
#                             rv$nbContaminantsDeleted, 
#                             rv$nbReverseDeleted, 
#                             nrow(rv$current.obj))
#       
#     })
#     
# })
# 
# output$GlobalPieChart <- renderHighchart({
#   rv$current.obj
#     rv$nbBothDeleted
#     rv$nbContaminantsDeleted
#   rv$nbReverseDeleted
#   input$idBoxContaminants
#   input$prefixContaminants
#   input$idBoxReverse
#   input$prefixReverse
#   
#   if (is.null(rv$current.obj)) {return()}
#   #if (is.null(rv$nbContaminantsDeleted) || is.null(rv$nbReverseDeleted)){return(NULL)}
#   if (is.null(input$idBoxContaminants) || (input$idBoxContaminants == "") ||
#       is.null(input$idBoxReverse) || (input$idBoxReverse == "") ||
#       is.null(input$prefixContaminants) || (input$prefixContaminants == "") ||
#       is.null(input$prefixReverse) || (input$prefixReverse == "")
#   ) {return(NULL)}
#   
# 
#   
#     GlobalPieChart()
# })









#########################################################
UpdateFilterWidgets <- function(){
    
    isolate({
        rv$current.obj
        if (length(rv$current.obj@processingData@processing) > 0){
            
            val <- match (gReplaceAllZeros ,
                          rv$current.obj@processingData@processing)
            updateCheckboxInput(session, "replaceAllZeros",value=val)
            
            val <- match (gLogTransform, 
                          rv$current.obj@processingData@processing)
            #updateCheckboxInput(session,"log2transform",value=val)
            
            r <- grep(pattern = gFilterTextPrefix, 
                      rv$current.obj@processingData@processing, 
                      fixed=TRUE, value=FALSE)
            if ( length(r) > 0)
            { 
                listMots <- unlist(strsplit(
                    rv$current.obj@processingData@processing[r], split=" "))
                updateSliderInput(session,
                                  inputId = "seuilNA", 
                                  value = listMots[6])
                updateRadioButtons(session,
                                   inputId = "ChooseFilters", 
                                   selected = listMots[3])
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






######################################
##' Function to compute the maximum value for the filter
##' @author Samuel Wieczorek
# GetMaxValueThresholdFilter <- function(){
#     input$ChooseFilters
#     vMax <- 0
#     choix <- list()
#     
#     result = tryCatch(
#         {
#             isolate({
#                 if (input$ChooseFilters == gFilterWholeMat) { 
#                     vMax <- ncol(Biobase::exprs(rv$current.obj))
#                     choix <- getListNbValuesInLines(rv$current.obj, type="wholeMatrix")
#                     }
#                 else if (input$ChooseFilters == gFilterAllCond 
#                          || input$ChooseFilters == gFilterOneCond){ 
#                     ll <- NULL
#                     for (i in 1:length(unique(Biobase::pData(rv$current.obj)$Label))){
#                         ll <- c(ll, length(which(
#                             Biobase::pData(rv$current.obj)$Label==
#                                 unique(Biobase::pData(rv$current.obj)$Label)[i])))
#                     }
#                     
#                     vMax <- min(ll)
#                     choix[[1]] <- 0
#                     for (i in 2:(vMax+1)){
#                       choix[[i]] <- i-1
#                     }
#                 }
#                 return(choix)
#             })
#         }
#         , warning = function(w) {
#             shinyjs::info(conditionMessage(w))
#         }, error = function(e) {
#             shinyjs::info(paste(match.call()[[1]],":",
#                                 conditionMessage(e), 
#                                 sep=" "))
#         }, finally = {
#             #cleanup-code 
#         })
#     
#     
#     
#     
# }


## Perform missing values filtering
observeEvent(input$perform.filtering.MV,{
    if (is.null(input$perform.filtering.MV) ){return()}
    if (input$perform.filtering.MV == 0){return()}
    
    isolate({
        
        result = tryCatch(
            {
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
                                GetFilterText(input$ChooseFilters, 
                                              as.integer(input$seuilNA)))
                        
                        
                        #write command log
                        # l <- paste(keepThat,",", collapse="")
                        # writeToCommandLogFile(
                        #     paste("keepThat <- ",
                        #         findSequences(keepThat),
                        #     sep="")
                        # )
                        
                        
                        #if (input$showCommandLog){
                        txt <- paste("keepThat <- mvFilterGetIndices(dataset[['",
                                     input$datasets, 
                                     "']], '",
                                     input$ChooseFilters, "', '",
                                     input$seuilNA, "')","\n",
                                     "deleted.mv <- current.obj[-keepThat]","\n",
                                    "txt <- '",GetFilterText(input$ChooseFilters,input$seuilNA),
                                                    "'","\n",
                                    "current.obj <- mvFilterFromIndices(",
                                    "current.obj, keepThat, '",
                                    GetFilterText(input$ChooseFilters,
                                                  input$seuilNA),
                                    "')",              
                                     sep="")

                       
                        writeToCommandLogFile(txt)
                   # }
                    
                    updateSelectInput(session, "ChooseFilters", 
                                      selected = input$ChooseFilters)
                    updateSelectInput(session, "seuilNA", 
                                      selected = input$seuilNA)
                    
                }
            }
            #, warning = function(w) {
            #    shinyjs::info(conditionMessage(w))
            }
            , error = function(e) {
                shinyjs::info(paste("Perform missing values filtering",":",
                                    conditionMessage(e), 
                                    sep=" "))
            }, finally = {
                #cleanup-code 
            })
    })
})


# observe({
#     input$idBoxContaminants
#     input$prefixContaminants
#     input$idBoxReverse
#     input$prefixReverse
#     
#     if (is.null(input$idBoxContaminants) || is.null(input$idBoxReverse) ||
#         is.null(input$prefixContaminants) || is.null(input$prefixReverse) ||
#         (input$idBoxContaminants == "") ||  (input$idBoxReverse == "") ||
#         (input$prefixContaminants == "") || (input$prefixReverse == ""))
#     {
#         shinyjs::disable("performFilteringContaminants")
#         rv$nbContaminantsDeleted <- NULL
#         rv$nbReverseDeleted <- NULL
#         rv$nbBothDeleted <- NULL
#         
#         return(NULL)
#     }
#     else {
#         shinyjs::enable("performFilteringContaminants")
#         
#     }
# })



# 
# majPropContaminants <- reactive({
#     input$performFilteringContaminants
#     
#   input$idBoxContaminants
#   input$prefixContaminants
#   input$idBoxReverse
#   input$prefixReverse
# 
#     if (is.null(input$performFilteringContaminants)){return (NULL)}
#   if (is.null(rv$current.obj)){return (NULL)}
#   if (is.null(input$idBoxContaminants) || is.null(input$idBoxReverse) ||
#        is.null(input$prefixContaminants) || is.null(input$prefixReverse) ||
#        (input$idBoxContaminants == "") ||  (input$idBoxReverse == "") ||
#       (input$prefixContaminants == "") || (input$prefixReverse == ""))
#   {
#       return(NULL)
#       }
#     
#   isolate({
#   l <- length(rv$dataset)
#         if (l ==1){ #Original dataset
#             obj <- rv$dataset[[1]]
#         } else {
#             dname <- unlist(strsplit(names(rv$dataset)[l], " - "))[1]
#             if (dname == "Filtered") {
#                 obj <- rv$dataset[[l - 1]]
#             } else {
#                 obj <- rv$dataset[[l]]
#             }
#         }
#   
#   
#    ind <- getIndicesOfLinesToRemove(obj,
#                                    input$idBoxContaminants,
#                                    input$prefixContaminants)
#   if (!is.null(ind)){ rv$nbContaminantsDeleted <- length(ind)}
# 
#   ind <- getIndicesOfLinesToRemove(obj,
#                                    input$idBoxReverse,
#                                    input$prefixReverse)
#   if (!is.null(ind)){rv$nbReverseDeleted <- length(ind)}
#   
#   
#   
# 
#   })
# })




#########################
# observeEvent(input$performFilteringContaminants,{
# 
#   rv$current.obj
#   #input$idBoxContaminants
#   #input$prefixContaminants
#   #input$idBoxReverse
#   #input$prefixReverse
#   
#   if (is.null(rv$current.obj)){return (NULL)}
#   if (is.null(input$idBoxContaminants) || (input$idBoxContaminants == "") ||
#       is.null(input$idBoxReverse) || (input$idBoxReverse == "") ||
#       is.null(input$prefixContaminants) || (input$prefixContaminants == "") ||
#       is.null(input$prefixReverse) || (input$prefixReverse == "")
#   ) {return(NULL)}
#     #if (is.null(input$perform.filtering.Contaminants) ){return()}
#     #if (input$perform.filtering.Contaminants == 0){return()}
#     
#     isolate({
#         
#         result = tryCatch(
#             {
#                 temp <- rv$current.obj
#                 
#                 res <- StringBasedFiltering(temp,
#                                             input$idBoxContaminants, 
#                                             input$prefixContaminants,
#                                             input$idBoxReverse,
#                                             input$prefixReverse
#                                             )
#                 
#                 
#                 rv$deleted.both <- res[["deleted.both"]]
#                 rv$deleted.contaminants <-res[["deleted.contaminants"]]
#                 rv$deleted.reverse <-res[["deleted.reverse"]]
#                 
#                 # majPropContaminants()
#                 rv$nbReverseDeleted <- nrow(rv$deleted.reverse)
#                 rv$nbContaminantsDeleted <- nrow(rv$deleted.contaminants)
#                 rv$nbBothDeleted <- nrow(rv$deleted.both)
#                 
#                 rv$current.obj <- res[["obj"]]
#                 rv$stringBasedFiltering_Done = TRUE
#                 
#                 updateSelectInput(session, "idBoxReverse", selected = input$idBoxReverse)
#                 updateSelectInput(session, "idBoxContaminants", selected = input$idBoxContaminants)
#                 updateSelectInput(session, "prefixContaminants",  selected = input$prefixContaminants)
#                 updateSelectInput(session,  "prefixReverse", selected = input$prefixReverse)
#                 
#                 updateTabsetPanel(session, "tabFilter",  selected = "FilterContaminants")
#                 
#                 #disableActionButton("resetFilterParamsButton", session)
#                 #createPNG_Filtering()
#             }
#             #, warning = function(w) {
#             #    shinyjs::info(conditionMessage(w))
#             # }
#             , error = function(e) {
#                 shinyjs::info(paste("Perform contaminants filtering",":",
#                                     conditionMessage(e), 
#                                     sep=" "))
#             }, finally = {
#                 #cleanup-code 
#             })
#         
#         
#         
#         
#         
#         
#     })
# })



disableActionButton <- function(id,session) {
    session$sendCustomMessage(type="jsCode",
                              list(code= paste("$('#",id,"').prop('disabled',true)"
                                               ,sep="")))
}

#-----------------------------------------------
output$ObserverStringBasedFilteringDone <- renderUI({
  rv$current.obj
  rv$stringBasedFiltering_Done
  if (is.null(rv$current.obj)) {return(NULL)}
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
    
    #if( (input$ValidateFilters == 0)) {return(NULL)}
    if(is.null(rv$current.obj)) {return(NULL)}
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
                    
                     
                    ###### write to commandLog File
                    #if (input$showCommandLog){
                        writeToCommandLogFile(  
                        paste("dataset[['",name, "']] <- current.obj", sep=""))
                    #}
                    ###### end write to command log file
                    
                    
                    updateSelectInput(session, "datasets", 
                                      paste("Dataset versions of",
                                            rv$current.obj.name, sep=" "),
                                      choices = names(rv$dataset), 
                                      selected = name)

                    
                    ## Add the necessary text to the Rmd file
                    #txt2Rmd <- readLines("Rmd_sources/filtering_Rmd.Rmd")
                    #filename <- paste(tempdir(), sessionID, 'report.Rmd',sep="/")
                    #write(txt2Rmd, file = filename,append = TRUE, sep = "\n")
                    #createPNG_Filtering()
                    
                }
                

})



# observeEvent(input$resetFilterParamsButton, ignoreInit = TRUE,{
#   updateSelectInput(session, "idBoxContaminants", selected="")
#   updateSelectInput(session, "idBoxReverse", selected="")
#   updateTextInput(session, "prefixContaminants", value="")
#   updateTextInput(session, "prefixReverse", value="")
#   
#   #rv$nbReverseDeleted <- NULL
#   #rv$nbContaminantsDeleted <- NULL
# })


# 
# output$choosePrefixContaminants <- renderUI({
#     rv$current.obj
#     input$idBoxContaminants
#     if (is.null(rv$current.obj)) {return(NULL)  }
#    # if (input$idBoxContaminants=="") {return(NULL)  }
#     if (is.null(input$idBoxContaminants) ) {return(NULL)  }
#     
#    # if (input$idBoxContaminants != ""){
#       textInput("prefixContaminants", label = "Choose prefix",value = "")
#    # }
# })


# output$choosePrefixReverse <- renderUI({
#     rv$current.obj
#     input$idBoxReverse
#     if (is.null(rv$current.obj)) {return(NULL)  }
#     #if (input$idBoxReverse == "") {return(NULL)  }
#     if (is.null(input$idBoxReverse) ) {return(NULL)  }
#     
#    # if (input$idBoxReverse != ""){
#       textInput("prefixReverse", label = "Choose prefix",value = "")
#    # }
# })
# 

# 
# output$id_Contaminants <- renderUI({
#     rv$current.obj
#     if (is.null(rv$current.obj)) {return(NULL)  }
#     
#     .choices <- c("",colnames(Biobase::fData(rv$current.obj)))
#     names(.choices) <- c("",colnames(Biobase::fData(rv$current.obj)))
#     selectInput("idBoxContaminants", 
#                 label = "Choose column", 
#                 choices = .choices , 
#                 selected = "")
# })

# 
# output$id_Reverse <- renderUI({
#     rv$current.obj
#     if (is.null(rv$current.obj)) {return(NULL)  }
#     
#     .choices <- c(G_emptyStr,colnames(Biobase::fData(rv$current.obj)))
#     names(.choices) <- c(G_emptyStr,colnames(Biobase::fData(rv$current.obj)))
#     selectInput("idBoxReverse", 
#                 label = "Choose column", 
#                 choices = .choices , 
#                 selected = "")
# })





#########################################################
##' Show the widget for filters
##' @author Samuel Wieczorek
output$choixFiltres <- renderUI({
    input$file
    if (is.null(input$file)) {return(NULL)}
    rv$current.obj
    radioButtons("ChooseFilters","Filtering options",choices = gFiltersList)
    
})



output$helpTextMV <- renderUI({
    rv$current.obj
    if (is.null(rv$current.obj)) {return(NULL)}
    helpText("After checking the data, validate the filters.")
})


