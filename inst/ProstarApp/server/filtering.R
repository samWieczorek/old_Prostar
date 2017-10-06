output$DP_sidebar_FilterTab1 <- renderUI({
    rv$current.obj
    if (is.null(rv$current.obj)){return()}
    filter <- NULL
    tag <- rv$current.obj@experimentData@other$mvFilter.method
    if (!is.null(tag)) { filter <- tag}
    tagList(
        h4("Missing values filtering options")
                     ,hr()
                     ,radioButtons("ChooseFilters","", 
                                   choices = gFiltersList,
                                   selected = filter)
                     ,conditionalPanel(
                         condition='input.ChooseFilters != "None"',
                         uiOutput("seuilNADelete"))
                     ,actionButton("perform.filtering.MV", 
                                   "Perform MV filtering")
    )
})

output$DP_sidebar_FilterTab2 <- renderUI({
    rv$current.obj
    if (is.null(rv$current.obj)){return()}
    
    tagList(
        h4("String based filtering options")
                     ,hr()
                     ,h4("Filter contaminants"),
                     uiOutput("id_Contaminants"),
                     uiOutput("choosePrefixContaminants"),
                     br(),
                     h4("Filter reverse"),
                     uiOutput("id_Reverse"),
                     uiOutput("choosePrefixReverse"),
                     br(),
                     actionButton("perform.filtering.Contaminants",
                                  "Perform string-based filtering")
    )
})


output$DP_sidebar_FilterTab3 <- renderUI({
    
    rv$current.obj
    if (is.null(rv$current.obj)){return()}
    tagList(
        h4("Filtered data display")
                     ,hr()
                     ,radioButtons("ChooseTabAfterFiltering", 
                                   "Choose the data to display",
                                   choices=
                                       list("Quantitative data" = "quantiData",
                                            "Meta data" = "MetaData"))
                     ,radioButtons("ChooseViewAfterFiltering", 
                                   "Choose the type of filtered data", 
                            choices=
                            list("Deleted on missing values" = "MissingValues",
                            "Deleted contaminants" = "Contaminants",
                            "Deleted reverse" = "Reverse"))
                     ,br(),br()
                     ,checkboxInput("nDigitsMV", 
                                    "Show full length intensities"
                                    , value = FALSE)
    )
})



#----------------------------------------------
output$VizualizeFilteredData <- DT::renderDataTable({
     rv$current.obj
     input$nDigitsMV
     input$ChooseViewAfterFiltering
     input$ChooseTabAfterFiltering
     
     if (is.null(input$ChooseTabAfterFiltering)
         ||is.null(input$ChooseViewAfterFiltering) 
         ||is.null(input$nDigitsMV) 
         ||(is.null(rv$current.obj))) {return()}
     
     
    if (is.null(input$nDigitsMV)){nDigits = 1e100}
     else {nDigitsMV = 3}
    
    data <- NULL
    if ((input$ChooseViewAfterFiltering == "MissingValues") 
        && !is.null(rv$deleted.mvLines))
        {
        obj <- rv$deleted.mvLines
        if(input$ChooseTabAfterFiltering == "quantiData" )
            {
            data <- cbind(ID = rownames(Biobase::fData(obj)),
                          round(Biobase::exprs(obj), digits=nDigitsMV))
        }else {data <- cbind(ID = rownames(Biobase::fData(obj)),
                             Biobase::fData(obj))}
    } else if ((input$ChooseViewAfterFiltering == "Contaminants") 
               && !is.null(rv$deleted.contaminants)) { 
        obj <- rv$deleted.contaminants
        if(input$ChooseTabAfterFiltering == "quantiData" )
        {data <- cbind(ID = rownames(Biobase::fData(obj)),
                       round(Biobase::exprs(obj), digits=nDigitsMV))
        }else {data <- cbind(ID = rownames(Biobase::fData(obj)),
                             Biobase::fData(obj))}
    } else if ((input$ChooseViewAfterFiltering == "Reverse") 
               && !is.null(rv$deleted.reverse)){
        obj <- rv$deleted.reverse
        if(input$ChooseTabAfterFiltering == "quantiData" )
        {data <- cbind(ID = rownames(Biobase::fData(obj)),
                       round(Biobase::exprs(obj), digits=nDigitsMV))
        }else {data <- cbind(ID = rownames(Biobase::fData(obj)),
                             Biobase::fData(obj))}
    }
    
    
    #if (!is.null(data)){
        DT::datatable(data, 
                         options=list(pageLength=DT_pagelength,
                                      orderClasses = TRUE,
                                      autoWidth=FALSE)
    )
    
    #dat
    #}
})


#########################################################
##' Show the widget (slider input) for filtering
##' @author Samuel Wieczorek
output$seuilNADelete <- renderUI({ 
    input$ChooseFilters
    
    if (is.null(rv$current.obj)) {return(NULL)   }
    if (input$ChooseFilters==gFilterNone) {return(NULL)   }
    
    choix <- list()
    vMax <- GetMaxValueThresholdFilter()
    choix[[1]] <- 0
    for (i in 2:(vMax+1)){
        choix[[i]] <- i-1
    }
    ch <- NULL
    tag <- rv$current.obj@experimentData@other$mvFilter.threshold
    
    if (!is.null(tag)) { ch <- tag}
    else {ch <- choix[[1]]}
    selectInput("seuilNA", 
                "Keep lines with at least x intensity values", 
                choices = choix, 
                selected = ch)
    
})


GlobalPieChart <- reactive({
    rv$current.obj
    rv$nbContaminantsDeleted
    rv$nbReverseDeleted
    if (is.null(rv$current.obj)) {return()}
    if (is.null(rv$nbContaminantsDeleted) || is.null(rv$nbReverseDeleted)){return(NULL)}
    
    # p <- rep("",4)
    # if (is.null(input$idBoxContaminants)) {p[1] <- ""}
    # else {p[1] <-input$idBoxContaminants}
    # 
    # if (is.null(input$idBoxReverse)) {p[2] <- ""}
    # else {p[2] <-input$idBoxReverse}
    # 
    # if (is.null(input$prefixContaminants)) {p[3] <- ""}
    # else {p[3] <-input$prefixContaminants}
    # 
    # if (is.null(input$prefixReverse)) {p[4] <- ""}
    # else {p[4] <-input$prefixReverse}
    
   # isolate({
    result = tryCatch(
        {
            #proportionConRev_HC(rv$current.obj,p[1], p[3], p[2],p[4])
          proportionConRev_HC(rv$nbContaminantsDeleted, rv$nbReverseDeleted, nrow(rv$current.obj))
            
        }
        #, warning = function(w) {
        #     shinyjs::info(conditionMessage(w))
        #}
        , error = function(e) {
            shinyjs::info(paste(match.call()[[1]],":",
                                conditionMessage(e), 
                                sep=" "))
        }, finally = {
            #cleanup-code 
       # })
    })
    
})

output$GlobalPieChart <- renderHighchart({
  rv$current.obj
  rv$nbContaminantsDeleted
  rv$nbReverseDeleted
  if (is.null(rv$current.obj)) {return()}
  if (is.null(rv$nbContaminantsDeleted==0) || is.null(rv$nbReverseDeleted)){return(NULL)}
  
    GlobalPieChart()
})









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
        updateSelectInput(session,"typeImputation",selected= c("none")) 
        updateSelectInput(session, "normalization.family",selected = c("None"))
    })
}






######################################
##' Function to compute the maximum value for the filter
##' @author Samuel Wieczorek
GetMaxValueThresholdFilter <- function(){
    input$ChooseFilters
    vMax <- 0
    
    
    result = tryCatch(
        {
            isolate({
                if (input$ChooseFilters == gFilterWholeMat) { 
                    vMax <- ncol(Biobase::exprs(rv$current.obj))}
                else if (input$ChooseFilters == gFilterAllCond 
                         || input$ChooseFilters == gFilterOneCond){ 
                    ll <- NULL
                    for (i in 1:length(unique(Biobase::pData(rv$current.obj)$Label))){
                        ll <- c(ll, length(which(
                            Biobase::pData(rv$current.obj)$Label==
                                unique(Biobase::pData(rv$current.obj)$Label)[i])))
                    }
                    
                    vMax <- min(ll)
                }
                
                return(vMax)
            })
        }
        , warning = function(w) {
            shinyjs::info(conditionMessage(w))
        }, error = function(e) {
            shinyjs::info(paste(match.call()[[1]],":",
                                conditionMessage(e), 
                                sep=" "))
        }, finally = {
            #cleanup-code 
        })
    
    
    
    
}


## Perform missing values filtering
observeEvent(input$perform.filtering.MV,{
    
    if (is.null(input$perform.filtering.MV) ){return()}
    if (input$perform.filtering.MV == 0){return()}
    
    isolate({
        
        result = tryCatch(
            {
                
                createPNG_BeforeFiltering()
                
                
                if (input$ChooseFilters == gFilterNone){
                    rv$current.obj <- rv$dataset[[input$datasets]]
                } else {
                    
                    
                    keepThat <- mvFilterGetIndices(rv$dataset[[input$datasets]],
                                                   input$ChooseFilters,
                                                   as.integer(input$seuilNA))
                    
                    if (!is.null(keepThat))
                    {
                        rv$deleted.mvLines <- 
                            rv$dataset[[input$datasets]][-keepThat]
                        
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
                        
                        
                        if (input$showCommandLog){
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
                    }
                    
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



observe({
  rv$current.obj
  input$idBoxContaminants
  input$prefixContaminants
  input$idBoxReverse
  input$prefixReverse

  if (is.null(rv$current.obj)){return (NULL)}
  if (is.null(input$idBoxContaminants) || (input$idBoxContaminants == "") ||
      is.null(input$idBoxReverse) || (input$idBoxReverse == "") ||
      is.null(input$prefixContaminants) || (input$prefixContaminants == "") ||
      is.null(input$prefixReverse) || (input$prefixReverse == "")
       ) {return(NULL)}
if (!is.null(rv$nbReverseDeleted) || !is.null(rv$nbContaminantsDeleted)){return (NULL)}
  
  
  ind <- getIndicesOfLinesToRemove(rv$current.obj,
                                   input$idBoxContaminants,
                                   input$prefixContaminants)
  if (!is.null(ind)){ rv$nbContaminantsDeleted <- length(ind)}
  
  ind <- getIndicesOfLinesToRemove(rv$current.obj,
                                   input$idBoxReverse,
                                   input$prefixReverse)
  if (!is.null(ind)){rv$nbReverseDeleted <- length(ind)}
  
})



#########################
observeEvent(input$perform.filtering.Contaminants,{
  rv$current.obj
  input$idBoxContaminants
  input$prefixContaminants
  input$idBoxReverse
  input$prefixReverse
  
  if (is.null(rv$current.obj)){return (NULL)}
  if (is.null(input$idBoxContaminants) || (input$idBoxContaminants == "") ||
      is.null(input$idBoxReverse) || (input$idBoxReverse == "") ||
      is.null(input$prefixContaminants) || (input$prefixContaminants == "") ||
      is.null(input$prefixReverse) || (input$prefixReverse == "")
  ) {return(NULL)}
    if (is.null(input$perform.filtering.Contaminants) ){return()}
    if (input$perform.filtering.Contaminants == 0){return()}
    
    isolate({
        result = tryCatch(
            {
                temp <- rv$current.obj
                if (!is.null(input$idBoxContaminants)
                    || (input$idBoxContaminants != "")) {
                    ind <- getIndicesOfLinesToRemove(temp,
                                                     input$idBoxContaminants, 
                                                     input$prefixContaminants)
                    
                    if (!is.null(ind)){
                        rv$nbContaminantsDeleted = length(ind)
                        if (length(ind) > 0)  {
                            rv$deleted.contaminants <- temp[ind]
                            
                            temp <- deleteLinesFromIndices(temp, ind, 
                                paste("\"", 
                                length(ind), 
                                " contaminants were removed from dataset.\"",
                                sep="")
                            )
                            
                            #write command log
                            if (input$showCommandLog){
                                txt <- paste("indContaminants <- getIndicesOfLinesToRemove(current.obj,\"", 
                                         input$idBoxContaminants,
                                         "\", \"",input$prefixContaminants,"\")","\n",
                                         "deleted.contaminants <- current.obj[indContaminants]","\n",
                                         "txt <- \"",length(ind), " contaminants were removed from dataset.\"","\n",
                                         "current.obj <- deleteLinesFromIndices(current.obj, indContaminants, txt)",
                                         sep=""
                                         )
                            writeToCommandLogFile(txt)
                        }
                        }
                    }
                }
                
                
                if (!is.null(input$idBoxReverse) 
                    || (input$idBoxReverse != "")){
                    ind <- getIndicesOfLinesToRemove(temp,
                                                     input$idBoxReverse,
                                                     input$prefixReverse)
                    
                    if (!is.null(ind)){
                        rv$nbReverseDeleted = length(ind)
                        if(length(ind) >0)  {
                            rv$deleted.reverse <- temp[ind]
                            temp <- deleteLinesFromIndices(
                                temp, ind, 
                                paste(length(ind), 
                                    " reverse were removed from dataset",
                                    sep="")
                            )
                            
                            if (input$showCommandLog){
                                txt <- paste("indReverse <- getIndicesOfLinesToRemove(current.obj, \"", 
                                         input$idBoxReverse,
                                         "\", \"",input$prefixReverse,"\")", "\n",
                                         "deleted.reverse <- current.obj[indReverse]", "\n",
                                         "txt <- \"",length(ind)," reverse were removed from dataset.\"", "\n",
                                         "current.obj <- deleteLinesFromIndices(current.obj, indReverse, txt)", sep="")
                            

                            writeToCommandLogFile(txt)
                        }
                        }
                    }
                }
                rv$current.obj <- temp
                rv$stringBasedFiltering_Done = TRUE
                
                updateSelectInput(session, 
                                  "idBoxReverse",
                                  selected = input$idBoxReverse)
                updateSelectInput(session, 
                                  "idBoxContaminants",
                                  selected = input$idBoxContaminants)
                updateSelectInput(session, 
                                  "prefixContaminants", 
                                  selected = input$prefixContaminants)
                updateSelectInput(session, 
                                  "prefixReverse",
                                  selected = input$prefixReverse)
                
                updateTabsetPanel(session, 
                                  "tabFilter", 
                                  selected = "FilterContaminants")
                
                
                createPNG_Filtering()
            }
            #, warning = function(w) {
            #    shinyjs::info(conditionMessage(w))
            # }
            , error = function(e) {
                shinyjs::info(paste("Perform contaminants filtering",":",
                                    conditionMessage(e), 
                                    sep=" "))
            }, finally = {
                #cleanup-code 
            })
        
        
        
        
        
        
    })
})



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
observeEvent(input$ValidateFilters,{ 
    
    if(is.null(input$ChooseFilters) || (input$ValidateFilters == 0)) 
    {return(NULL)}
    if(is.null(rv$current.obj)) {return(NULL)}
    
    isolate({
        
        
        result = tryCatch(
            {
                
                if((input$ChooseFilters != gFilterNone) 
                   || !is.null(input$idBoxContaminants) 
                   || !is.null(input$idBoxReverse)){
                    
                    rv$typeOfDataset <- rv$current.obj@experimentData@other$typeOfData
                    name <- paste ("Filtered", " - ", rv$typeOfDataset, sep="")
                    rv$dataset[[name]] <- rv$current.obj
                    
                    ###### write to commandLog File
                    if (input$showCommandLog){
                        writeToCommandLogFile(  
                        paste("dataset[['",name, "']] <- current.obj", sep=""))
                    }
                    ###### end write to command log file
                    
                    
                    updateSelectInput(session, "datasets", 
                                      paste("Dataset versions of",
                                            rv$current.obj.name, sep=" "),
                                      choices = names(rv$dataset), 
                                      selected = name)
                    txtFilterMV <- paste("Filtering :",
                                         GetFilterText(input$ChooseFilters, 
                                                       input$seuilNA), 
                                         sep="")
                    txt <- paste(txtFilterMV, "Contaminants deleted", 
                                 "Reverse deleted", 
                                 sep=" ")
                    UpdateLog(txt,name)
                    
                    
                    ## Add the necessary text to the Rmd file
                    #txt2Rmd <- readLines("Rmd_sources/filtering_Rmd.Rmd")
                    #filename <- paste(tempdir(), sessionID, 'report.Rmd',sep="/")
                    #write(txt2Rmd, file = filename,append = TRUE, sep = "\n")
                    createPNG_Filtering()
                    
                }
                
            }
            , warning = function(w) {
                shinyjs::info(conditionMessage(w))
            }, error = function(e) {
                shinyjs::info(paste("Validate filters",":",
                                    conditionMessage(e), 
                                    sep=" "))
            }, finally = {
                #cleanup-code 
            })
        
        
    })
    
})







output$choosePrefixContaminants <- renderUI({
    rv$current.obj
    input$idBoxContaminants
    if (is.null(rv$current.obj)) {return(NULL)  }
    if (is.null(input$idBoxContaminants)) {return(NULL)  }
    
    textInput("prefixContaminants", label = "Choose prefix",value = "")
})


output$choosePrefixReverse <- renderUI({
    rv$current.obj
    input$idBoxReverse
    if (is.null(rv$current.obj)) {return(NULL)  }
    if (is.null(input$idBoxReverse)) {return(NULL)  }
    
    textInput("prefixReverse", label = "Choose prefix", value = "" )
    
})



output$id_Contaminants <- renderUI({
    rv$current.obj
    if (is.null(rv$current.obj)) {return(NULL)  }
    
    .choices <- c("",colnames(Biobase::fData(rv$current.obj)))
    names(.choices) <- c("",colnames(Biobase::fData(rv$current.obj)))
    selectInput("idBoxContaminants", 
                label = "Choose column", 
                choices = .choices , 
                selected = NULL)
})


output$id_Reverse <- renderUI({
    rv$current.obj
    if (is.null(rv$current.obj)) {return(NULL)  }
    
    .choices <- c(G_emptyStr,colnames(Biobase::fData(rv$current.obj)))
    names(.choices) <- c(G_emptyStr,colnames(Biobase::fData(rv$current.obj)))
    selectInput("idBoxReverse", 
                label = "Choose column", 
                choices = .choices , 
                selected = NULL)
})





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

