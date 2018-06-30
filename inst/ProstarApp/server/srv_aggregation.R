



########################################################
RunAggregation <- reactive({
    req(rv$matAdj)
    
    n <- NULL
    if (input$aggregationMethod == gAgregateMethod[["sum on top n"]]) { 
        n <- as.numeric(input$nTopn)
        }

    if (input$checkSharedPeptides){
                data <- pepAgregate(rv$current.obj, 
                                    input$proteinId,
                                    input$aggregationMethod, 
                                    rv$matAdj$matWithSharedPeptides, 
                                    n)
                #if (input$showCommandLog){
                #     txt <- paste(
                #     "data <- pepAgregate(current.obj, '",
                #     input$proteinId, "', '",
                #     input$aggregationMethod, 
                #     "', mat$matWithSharedPeptides,",n,")",
                #     sep=""
                # )
                # writeToCommandLogFile(txt)
               # }
                
            }else{
                data <- pepAgregate(rv$current.obj, 
                                    input$proteinId,
                                    input$aggregationMethod, 
                                    rv$matAdj$matWithUniquePeptides
                                    , n)
                #if (input$showCommandLog){
                #     writeToCommandLogFile(
                #     paste(
                #         "data <- pepAgregate(current.obj, '",
                #         input$proteinId, "', '",
                #         input$aggregationMethod, 
                #         "', mat$matWithUniquePeptides,",n,")",
                #         sep=""
                #     )
                # )
               # }
            }
            
            return(data)

})




##' -- Validate the aggregation ---------------------------------------
##' @author Samuel Wieczorek
observeEvent(input$valid.aggregation,{ 
    input$nbPeptides
    input$filterProtAfterAgregation
    input$aggregationMethod
    input$columnsForProteinDataset.box
    req(rv$matAdj)
    req(rv$temp.aggregate)
    
    isolate({
        m <- NULL
        if (input$checkSharedPeptides){ 
            m <- rv$matAdj$matWithSharedPeptides
        }else{
            m <-rv$matAdj$matWithUniquePeptides
        }

        updateSelectInput(session, "proteinId",selected = input$proteinId)
        updateSelectInput(session, "aggregationMethod",selected = input$aggregationMethod)
        updateSelectInput(session, "nTopn",selected = input$nTopn)
        updateCheckboxInput(session,"checkSharedPeptides",input$checkSharedPeptides)
                
        l.params <- list(withSharedPeptides = input$checkSharedPeptides,
                                 agregMethod = input$aggregationMethod,
                                 proteinId = input$proteinId,
                                 topN = input$nTopn
                    )
                
                #total <- 60
                #delta <- round(total / length(input$columnsForProteinDataset.box))
                #cpt <- 10
                for(c in input$columnsForProteinDataset.box){
                    newCol <- BuildColumnToProteinDataset(
                        Biobase::fData(rv$current.obj), m, c, rownames(Biobase::fData(rv$temp.aggregate)))
                    cnames <- colnames(Biobase::fData(rv$temp.aggregate))
                    Biobase::fData(rv$temp.aggregate) <- 
                        data.frame(Biobase::fData(rv$temp.aggregate), newCol)
                    colnames(Biobase::fData(rv$temp.aggregate)) <- c(cnames, c)
                    #cpt <- cpt + delta
                    #updatePB(session,inputId="pb_SaveAggregation",value=cpt,text_value=paste(cpt," %", sep=""), striped = TRUE, active=TRUE)
                }
                
                rv$current.obj <- rv$temp.aggregate
                rv$current.obj <- saveParameters(rv$current.obj, "Aggregation",l.params)
                rv$current.obj@experimentData@other$Prostar_Version <- 
                  installed.packages(lib.loc = Prostar.loc)["Prostar","Version"]
                rv$current.obj@experimentData@other$DAPAR_Version <- 
                  installed.packages(lib.loc = DAPAR.loc)["DAPAR","Version"]
                
                name <- paste ("Aggregated", " - ", rv$typeOfDataset, sep="")
                UpdateLog("Aggregation", l.params)
                #updatePB(session,inputId="pb_SaveAggregation",value=70,text_value="70 %", striped = TRUE, active=TRUE)
                
                
                loadObjectInMemoryFromConverter()
                
                
                #updatePB(session,inputId="pb_SaveAggregation",value=90,text_value="90 %", striped = TRUE, active=TRUE)
                #}
                
                updateNavbarPage (session, "navPage", selected = "Descriptive statistics")
                
                updateSelectInput(session, "datasets", 
                                 # paste("Dataset versions of",rv$current.obj.name, sep=" "),
                                  choices = names(rv$dataset),
                                  selected = name)

                rv$temp.aggregate <- NULL
                #updatePB(session,inputId="pb_SaveAggregation",value=100,text_value="100 %", striped = TRUE, active=TRUE)

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
        else if (input$aggregationMethod != "None"){
            h3(paste("Aggregation done with the ", 
                     input$aggregationMethod, 
                     " method.", 
                     sep=""))
        }
        
    })
})





observeEvent(input$proteinId,{
    
    rv$current.obj
    
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
            
        }
    , error = function(e) {
        shinyjs::info(paste("Build adjacency matrix:",
                            conditionMessage(e), 
                            sep=" "))
    }, finally = {
        #cleanup-code 
    })
    
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
  req(rv$matAdj)
  GraphPepProt(rv$matAdj$matWithSharedPeptides)
})


output$aggregationPlotUnique <- renderPlot({
    req(rv$matAdj)
  GraphPepProt(rv$matAdj$matWithUniquePeptides)
    
})



###------------ Perform aggregation--------------------
observeEvent(input$perform.aggregation,{
    
    isolate({
  if (input$aggregationMethod != "None")
                {rv$temp.aggregate <- RunAggregation()
     }
     
    })
})









output$AggregationSideBar_Step1 <-  renderUI({
    
    wellPanel(id = "sidebar_Aggregation",
              height = "100%",
              tagList(
                  uiOutput("warningAgregationMethod"),
                  uiOutput("chooseProteinId"),
                  checkboxInput("checkSharedPeptides", "Include shared peptides", value = FALSE),
                  selectInput("aggregationMethod","Aggregation methods",choices =  gAgregateMethod),
                  uiOutput("topNOption"),
                  actionButton("perform.aggregation","Perform aggregation")
              )
    )
    
})




output$AggregationWellPanel_Step1 <- renderUI({
    req(rv$current.obj)
    
    tagList(
            HTML("Please select first the id of protein in your dataset. 
                <br>Then, the stats will be showed and it will be possible to 
                perform the aggregation"),
            # fluidRow(
            #     column(width=6, h4("Only specific peptides")),
            #     column(width=6, h4("All (specific & shared) peptides"))
            #     ),
            fluidRow(
                column(width=6, uiOutput("specificPeptideBarplot")),
                column(width=6, uiOutput("allPeptideBarplot"))
                ),
            uiOutput("aggregationStats"),
            uiOutput("ObserverAggregationDone")
            )
   
})



output$specificPeptideBarplot <- renderUI({
  req(rv$matAdj)
  tagList(
    h4("Only specific peptides"),
    plotOutput("aggregationPlotUnique") %>% withSpinner(type=spinnerType)
    )
})

output$allPeptideBarplot <- renderUI({
  req(rv$matAdj)
  tagList(
    h4("All (specific & shared) peptides"),
    plotOutput("aggregationPlotShared") %>% withSpinner(type=spinnerType)
    )
})


output$displayNbPeptides <- renderUI({
    req(input$filterProtAfterAgregation)
    
    if (input$filterProtAfterAgregation) {
        numericInput("nbPeptides", "Nb of peptides defining a protein", 
                     value = 0, min =0, step=1,
                     width = "250px")
    }
})



output$Aggregation_Step2 <- renderUI({
    
    req(rv$current.obj)
    
    if (rv$current.obj@experimentData@other$typeOfData == typePeptide) {
        tagList(
            fluidRow(
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

