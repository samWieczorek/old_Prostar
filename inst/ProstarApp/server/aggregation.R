observe({
    rv$current.obj
    if(is.null(rv$current.obj)) {return (NULL)}
    
    if (rv$current.obj@experimentData@other$typeOfData == typeProtein)
    { 
        hideTab(inputId ="navPage", target = "Aggregation")
    } else {
        showTab(inputId ="navPage", target = "Aggregation")
    }
})




########################################################
RunAggregation <- reactive({
    rv$matAdj
    if (is.null(rv$matAdj)) { return (NULL)}
    
    n <- NULL
    if (input$aggregationMethod == gAgregateMethod[["sum on top n"]]) { 
        n <- as.numeric(input$nTopn)
        }
    
    
    tryCatch (
        {
            if (input$checkSharedPeptides){
                data <- pepAgregate(rv$current.obj, 
                                    input$proteinId,
                                    input$aggregationMethod, 
                                    rv$matAdj$matWithSharedPeptides, 
                                    n)
                #if (input$showCommandLog){
                    txt <- paste(
                    "data <- pepAgregate(current.obj, '",
                    input$proteinId, "', '",
                    input$aggregationMethod, 
                    "', mat$matWithSharedPeptides,",n,")",
                    sep=""
                )
                writeToCommandLogFile(txt)
               # }
                
            }else{
                data <- pepAgregate(rv$current.obj, 
                                    input$proteinId,
                                    input$aggregationMethod, 
                                    rv$matAdj$matWithUniquePeptides
                                    , n)
                #if (input$showCommandLog){
                    writeToCommandLogFile(
                    paste(
                        "data <- pepAgregate(current.obj, '",
                        input$proteinId, "', '",
                        input$aggregationMethod, 
                        "', mat$matWithUniquePeptides,",n,")",
                        sep=""
                    )
                )
               # }
            }
            
            return(data)
        },
        err=function(errorCondition) {
            cat("in err handler")
            message(errorCondition)
        })
    
    
    
})




##' -- Validate the aggregation ---------------------------------------
##' @author Samuel Wieczorek
observeEvent(input$valid.aggregation,{ 
    input$nbPeptides
    input$filterProtAfterAgregation
    input$aggregationMethod
    input$columnsForProteinDataset.box
    rv$matAdj
    
    if (is.null(input$valid.aggregation) 
        || (input$valid.aggregation == 0)
        || is.null(rv$matAdj) || is.null(rv$temp.aggregate)) 
    {return(NULL)}
    
    
    result = tryCatch(
        {
            
            isolate({
                
                ##concatenation des informations
                m <- NULL
                if (input$checkSharedPeptides){ 
                    m <- rv$matAdj$matWithSharedPeptides
                    #if (input$showCommandLog){
                        writeToCommandLogFile("m <- mat$matWithSharedPeptides")
                        #}
                }else{ m <-rv$matAdj$matWithUniquePeptides
                #if (input$showCommandLog){
                  writeToCommandLogFile("m <- mat$matWithUniquePeptides")
                 # }
                }
                #updatePB(session,inputId="pb_SaveAggregation",value=10,text_value="10 %", striped = TRUE, active=TRUE)
                
                
                #total <- 60
                #delta <- round(total / length(input$columnsForProteinDataset.box))
                #cpt <- 10
                for(c in input$columnsForProteinDataset.box){
                    newCol <- BuildColumnToProteinDataset_par(
                        Biobase::fData(rv$current.obj), m, c, rownames(Biobase::fData(rv$temp.aggregate)))
                    cnames <- colnames(Biobase::fData(rv$temp.aggregate))
                    Biobase::fData(rv$temp.aggregate) <- 
                        data.frame(Biobase::fData(rv$temp.aggregate), newCol)
                    colnames(Biobase::fData(rv$temp.aggregate)) <- c(cnames, c)
                    #cpt <- cpt + delta
                    #updatePB(session,inputId="pb_SaveAggregation",value=cpt,text_value=paste(cpt," %", sep=""), striped = TRUE, active=TRUE)
                    
                }
                
                #if (input$filterProtAfterAgregation){
                #    rv$temp.aggregate <- FilterProteinWithFewPeptides(rv$temp.aggregate, input$nbPeptides)
                #}
                
                rv$current.obj <- rv$temp.aggregate
                rv$typeOfDataset <-
                    rv$current.obj@experimentData@other$typeOfData
                name <- paste ("Aggregated", " - ", rv$typeOfDataset, sep="")
                rv$dataset[[name]] <- rv$current.obj
                
                #updatePB(session,inputId="pb_SaveAggregation",value=70,text_value="70 %", striped = TRUE, active=TRUE)
                
                
                
                ######
                #if (input$showCommandLog){
                    l <- buildWritableVector(input$columnsForProteinDataset.box)
                writeToCommandLogFile(
                    paste("columnsForProteinDataset <- ",l, sep="") )
                
                writeToCommandLogFile("for (c in columnsForProteinDataset) {")
                writeToCommandLogFile(
                "newCol <- BuildColumnToProteinDataset(fData(current.obj), m, c, rownames(Biobase::fData(temp.aggregate)))")
                writeToCommandLogFile("cnames <- colnames(fData(temp.aggregate))")
                writeToCommandLogFile("fData(temp.aggregate) <-
                                      data.frame(fData(temp.aggregate), newCol)")
                writeToCommandLogFile("colnames(fData(temp.aggregate)) <- c(cnames, c)")
                writeToCommandLogFile("}")
                writeToCommandLogFile("current.obj <- temp.aggregate")
                writeToCommandLogFile(
                    paste("dataset[['",name, "']] <- current.obj", sep="")
                )
                #updatePB(session,inputId="pb_SaveAggregation",value=90,text_value="90 %", striped = TRUE, active=TRUE)
                #}
                
                updateNavbarPage (session, "navPage", selected = "Descriptive statistics")
                #updateTabsetPanel(session, "Aggregation", selected = "configureProteinDataset")
                
                updateSelectInput(session, "datasets", 
                                  paste("Dataset versions of",
                                        rv$current.obj.name, sep=" "),
                                  choices = names(rv$dataset),
                                  selected = name)
                UpdateLog(
                    paste("Aggregation : peptides were aggregated into 
                          proteins with method =",
                          input$aggregationMethod,
                          ", include Shared Peptides = ", 
                          input$checkSharedPeptides,
                          ", protein id = ", input$proteinId, sep=" "),
                    name)
                rv$temp.aggregate <- NULL
                #updatePB(session,inputId="pb_SaveAggregation",value=100,text_value="100 %", striped = TRUE, active=TRUE)
                
            } )
            
        }
        , warning = function(w) {
            shinyjs::info(conditionMessage(w))
        }, error = function(e) {
            shinyjs::info(paste("Validate the agregation",":",
                                conditionMessage(e), sep=" "))
        }, finally = {
            #cleanup-code 
        })
    
    })





output$topNOption <- renderUI({
    input$aggregationMethod
    if(is.null(input$aggregationMethod )) {return(NULL)}
    
    if(input$aggregationMethod == gAgregateMethod[["sum on top n"]])
        numericInput("nTopn", "nTopn",value = NULL, min = 0)
    
})




#-----------------------------------------------
output$ObserverAggregationDone <- renderUI({
    rv$temp.aggregate
    input$perform.aggregation
    if (is.null(rv$temp.aggregate)) {return(NULL)}
    isolate({
        if (input$perform.aggregation == 0) 
        {return(NULL)  }
        else if (input$aggregationMethod != "none"){
            h3(paste("Aggregation done with the ", 
                     input$aggregationMethod, 
                     " method.", 
                     sep=""))
        }
        
    })
})





observeEvent(input$proteinId,{
    
    rv$current.obj
    if (is.null( input$proteinId) || (input$proteinId == "None"))
    {return(NULL)}
    
    
    if (rv$current.obj@experimentData@other$typeOfData == typeProtein) {
        return(NULL)}
    
    result = tryCatch(
        {
            matSharedPeptides <- BuildAdjacencyMatrix(rv$current.obj, 
                                                      input$proteinId,
                                                      FALSE)
            matUniquePeptides <- BuildAdjacencyMatrix(rv$current.obj, 
                                                      input$proteinId,
                                                      TRUE)
            
            rv$matAdj <- list(matWithSharedPeptides=matSharedPeptides,
                              matWithUniquePeptides=matUniquePeptides)
            
           # if (input$showCommandLog){
                writeToCommandLogFile(
            paste("matSharedPeptides <- BuildAdjacencyMatrix(current.obj,\"",
                      input$proteinId,"\",FALSE)", sep="")
            )
            writeToCommandLogFile(
            paste("matUniquePeptides <- BuildAdjacencyMatrix(current.obj,\"",
                      input$proteinId,"\",TRUE)", sep="")
            )
            
            writeToCommandLogFile(
            "mat <- list(matWithSharedPeptides=matSharedPeptides,
            matWithUniquePeptides=matUniquePeptides)"
    )
            #}
            
        }
    #, warning = function(w) {
    #    shinyjs::info(conditionMessage(w))
    #}
    , error = function(e) {
        shinyjs::info(paste("Build adjacency matrix:",
                            conditionMessage(e), 
                            sep=" "))
    }, finally = {
        #cleanup-code 
    })
    
})



#-----------------------------------------------
output$aggregationPlot <- renderPlot({
    input$proteinId
    rv$matAdj
    rv$current.obj
    if (is.null( input$proteinId) || (input$proteinId == "None")
        || is.null(rv$matAdj))
    {return(NULL)}
    if (is.null( rv$current.obj)){return(NULL)}
    
    
    if (input$checkSharedPeptides) {
        GraphPepProt(rv$matAdj$matWithSharedPeptides)
        }
    else {
        GraphPepProt(rv$matAdj$matWithUniquePeptides)
        }
    
})





output$aggregationStats <- renderUI ({
    input$proteinId
    rv$current.obj
    rv$matAdj
    if (is.null( input$proteinId) || (input$proteinId == "None") || is.null(rv$matAdj))
      {return(NULL)}
    if (is.null( rv$current.obj)){return(NULL)}
    
    res <- getProteinsStats(rv$matAdj$matWithUniquePeptides, 
                            rv$matAdj$matWithSharedPeptides)
    
    
    rv$AggregProtStats$nb <- c(nrow(rv$matAdj$matWithSharedPeptides),
                               nrow(rv$matAdj$matWithUniquePeptides),
                               nrow(rv$matAdj$matWithSharedPeptides)-nrow(rv$matAdj$matWithUniquePeptides),
                               ncol(rv$matAdj$matWithSharedPeptides),
                               length(res$protOnlyUniquePep),
                               length(res$protOnlySharedPep),
                               length(res$protMixPep))
    
    text <- paste("<ul style=\"list-style-type:disc;\">
                  <li>",
                  rv$AggregProtStats$name[1],": ", 
                  rv$AggregProtStats$nb[1],
                  "</li>
                  
                  <li>",
                  rv$AggregProtStats$name[2],": ", 
                  rv$AggregProtStats$nb[2],
                  "</li>
                  
                  
                  <li>",
                  rv$AggregProtStats$name[3],": ",
                  rv$AggregProtStats$nb[3],
                  "</li>
                  
                  <li>",
                  rv$AggregProtStats$name[4],":  ", rv$AggregProtStats$nb[4],
                  " </li>
                  <li>",
                  rv$AggregProtStats$name[5],": ", 
                  rv$AggregProtStats$nb[5], 
                  "</li>
                  
                  <li>",
                  rv$AggregProtStats$name[6],":  ", 
                  rv$AggregProtStats$nb[6], 
                  "</li>
                  
                  <li>",
                  rv$AggregProtStats$name[7],":  ", 
                  rv$AggregProtStats$nb[7], 
                  "</li>
                  
                  </ul>" , sep="")
    
    
    
    
    HTML(text)
})

output$aggregationPlotShared <- renderPlot({
    
    rv$matAdj
    
    if (is.null(rv$matAdj)) {return(NULL)}
    result = tryCatch(
        {
            GraphPepProt(rv$matAdj$matWithSharedPeptides)
        }
        , warning = function(w) {
            shinyjs::info(conditionMessage(w))
        }, error = function(e) {
            shinyjs::info(paste(match.call()[[1]],":",conditionMessage(e), 
                                sep=" "))
        }, finally = {
            #cleanup-code 
        })
    
    
})


output$aggregationPlotUnique <- renderPlot({
    rv$matAdj
    
    if (is.null(rv$matAdj)) {return(NULL)}
    result = tryCatch(
        {
            GraphPepProt(rv$matAdj$matWithUniquePeptides)
        }
        , warning = function(w) {
            shinyjs::info(conditionMessage(w))
        }, error = function(e) {
            shinyjs::info(paste(match.call()[[1]],":",conditionMessage(e), 
                                sep=" "))
        }, finally = {
            #cleanup-code 
        })
    
    
})



###------------ Perform aggregation--------------------
observeEvent(input$perform.aggregation,{
    #input$perform.aggregation
    #input$aggregationMethod
    if (is.null(input$perform.aggregation) 
        || (input$perform.aggregation == 0))
    {return(NULL)}
    
    isolate({
        
        result = tryCatch(
            {
                if (input$aggregationMethod != "none")
                {
                    rv$temp.aggregate <- RunAggregation()
                   # if (input$showCommandLog){
                      writeToCommandLogFile("temp.aggregate <- data")
                    #  }
                }
                
            }
            , warning = function(w) {
                shinyjs::info(conditionMessage(w))
            }, error = function(e) {
                shinyjs::info(paste(match.call()[[1]],":",conditionMessage(e), 
                                    sep=" "))
            }, finally = {
                #cleanup-code 
            })
        
        
        
        
    })
})








output$ChooseAggregationMethod <- renderUI({
    rv$current.obj
    if (is.null(rv$current.obj)) {return (NULL)}
    
    selectInput("aggregationMethod",
                "Aggregation methods",
                choices =  gAgregateMethod)
})


output$AggregationSideBar_Step1 <-  renderUI({
    rv$current.obj
    if (is.null(rv$current.obj) || 
        (rv$current.obj@experimentData@other$typeOfData == typeProtein))
    {return (NULL)}
    
    wellPanel(id = "sidebar_Aggregation",
              height = "100%",
              tagList(
                  h4("Aggregation options"),
                  uiOutput("warningAgregationMethod"),
                  uiOutput("chooseProteinId"),
                  checkboxInput("checkSharedPeptides",
                                "Include shared peptides",
                                value = FALSE),
                  uiOutput("ChooseAggregationMethod"),
                  uiOutput("topNOption"),
                  actionButton("perform.aggregation","Perform aggregation")
              )
    )
    
})




output$AggregationWellPanel_Step1 <- renderUI({
    rv$current.obj
    if (is.null(rv$current.obj))
    {return (NULL)}
    
    if (rv$current.obj@experimentData@other$typeOfData == typePeptide) {
        tagList(
            HTML("Please select first the id of protein in your dataset. 
                <br>Then, the stats will be showed and it will be possible to 
                perform the aggregation"),
            fluidRow(
                column(width=6, h4("Only specific peptides")),
                column(width=6, h4("All (specific & shared) peptides"))
                ),
            busyIndicator(WaitMsgPlot,wait = 0),
            fluidRow(
                column(width=6, plotOutput("aggregationPlotUnique")),
                column(width=6, plotOutput("aggregationPlotShared"))
                ),
            uiOutput("aggregationStats"),
            uiOutput("ObserverAggregationDone")
            )
    } else {
        h4("The dataset is a protein one: the aggregation cannot be performed.")
    }
})



output$displayNbPeptides <- renderUI({
    input$filterProtAfterAgregation
    if (is.null(input$filterProtAfterAgregation)){return (NULL) }
    
    if (input$filterProtAfterAgregation) {
        numericInput("nbPeptides", "Nb of peptides defining a protein", 
                     value = 0, min =0, step=1,
                     width = "250px")
    }
})



output$Aggregation_Step2 <- renderUI({
    
    rv$current.obj
    if (is.null(rv$current.obj)){return (NULL)}
    
    if (rv$current.obj@experimentData@other$typeOfData == typePeptide) {
        tagList(
            fluidRow(
                #column(width=3,
                #       checkboxInput("filterProtAfterAgregation",
                #                     "Filtering : remove the proteins that are 
                #                     defined by less than n peptides.",
                #                     value = FALSE)
                #),
                column(width=4,uiOutput("displayNbPeptides")
                )
                
            ),
            
            
            helpText("Select the columns of the meta-data (related to proteins)
                    that have to be recorded in the new protein dataset."),
            helpText("(e.g. the column which contains the protein ID if you wish 
                     to perform a GO analysis.)"),
            div(class="row"),
            div(class="span5", "",
                uiOutput("columnsForProteinDataset"),
                fluidRow(
                    column(width=3,
                           actionButton("valid.aggregation",
                                        "Save aggregation", 
                                        styleclass = "primary")
                    )
                ) 
                
            )
            )
    } else {
        h4("The peptide dataset has been aggregated into a protein dataset.")
    }
    
})





output$warningAgregationMethod <- renderUI({
    rv$current.obj
    if (is.null(rv$current.obj)) {return (NULL)}
    
    if (length(which(is.na(Biobase::exprs(rv$current.obj)))) > 0)
    {
        text <- "<font color=\"red\"> Warning ! <br> 
        Your dataset contains missing values.
        <br> For better results, you should impute  <br> them first"
        HTML(text)
    }
    
})


buildWritableVector <- function(v){
    t <- "c("
    for (i in v){
        t <- paste(t, "\"", as.character(i), "\"", sep="")
        if (i == last(v)) {t <- paste(t, ")", sep="")}
        else {t <- paste(t, ",", sep="")}
    }
    return(t)
}



######################################################### 
output$columnsForProteinDataset <- renderUI({
    rv$current.obj
    if (is.null(rv$current.obj)) {return(NULL)  }
    
    choices <- colnames(Biobase::fData(rv$current.obj))
    names(choices) <- colnames(Biobase::fData(rv$current.obj))
    selectizeInput("columnsForProteinDataset.box",
                   label = "",
                   choices = choices,
                   multiple = TRUE, width='200%')
    
})




######################################################### 

output$chooseProteinId <- renderUI({
    rv$current.obj
    if (is.null(rv$current.obj)) {return (NULL)}
    
    selectInput("proteinId", 
                "Choose the protein ID",
                choices = c("None",colnames(Biobase::fData(rv$current.obj))))
})

