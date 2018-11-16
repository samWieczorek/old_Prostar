require(imp4p)

callModule(moduleMVPlots,"mvImputationPlots_PeptideLevel", data=reactive(rv$current.obj), title=reactive("Missing values distribution"))


callModule(moduleDetQuantImpValues, "peptide_DetQuantValues_DT", 
           reactive({input$peptideLevel_detQuant_quantile}), 
           reactive({input$peptideLevel_detQuant_factor}))


##########
#####  UI for the PEPTIDE LEVEL Imputation process
##########
output$peptideLevelImputationPanel <- renderUI({
  #req(rv$current.obj)
  isolate({
    nbEmptyLines <- getNumberOfEmptyLines(Biobase::exprs(rv$current.obj))
  
  if (nbEmptyLines > 0) {
    tags$p("Your dataset contains empty lines (fully filled with missing values). In order to use
           the imputation tool, you must delete them by using the filter tool.")
    
  }
  else { 
    tabPanel("Miss. values imputation",
             id = "tabPanelImputation",
             value = "imputation",
             sidebarCustom(),
             splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
                         wellPanel(id = "sidebar_imputation",
                                   height = "100%"
                                   ,br(),
                                   selectInput("peptideLevel_missing.value.algorithm",
                                               "Algorithm",
                                               choices = imputationAlgorithms, 
                                               selected = rv$widgets$peptideImput$pepLevel_algorithm,
                                               width='150px'),
                                   uiOutput("basicAlgoUI"),
                                   uiOutput("detQuantOptsUI"),
                                   uiOutput("KNNOptsUI"),
                                   uiOutput("imp4pOptsUI"),
                                   uiOutput("imp4pOpts2UI"),
                                   actionButton("peptideLevel_perform.imputation.button", "Perform imputation", class = actionBtnClass),
                                   actionButton("peptideLevel_ValidImputation", "Save imputation", class = actionBtnClass),
                                   br(), br(), br()
                                   #uiOutput("warningImputationMethod"),
                                   
                                   ## progress bar
                                   #br(),
                                   #br(),
                                   #uiOutput(outputId = "progressOne")
                         ),
                         tagList(
                           uiOutput("peptideLevel_showImputationPanel"),
                           uiOutput("peptideLevel_detQuant_impValues"),
                           busyIndicator(WaitMsgPlot,wait = 0),
                           moduleMVPlotsUI("mvImputationPlots_PeptideLevel")
                         )      
                         
             )
    )
  }
  })
})



output$basicAlgoUI <- renderUI({
  if (input$peptideLevel_missing.value.algorithm != "BasicMethods"){return(NULL)}
  
  selectInput("peptideLevel_missing.value.basic.algorithm", 
              "Methods", width='150px',
              choices = basicMethodsImputationAlgos,
              selected = rv$widgets$peptideImput$pepLevel_basicAlgorithm)

})


output$detQuantOptsUI <- renderUI({
  req(input$peptideLevel_missing.value.basic.algorithm)
  req(input$peptideLevel_missing.value.algorithm)
  if ((input$peptideLevel_missing.value.basic.algorithm != "detQuantile") || 
      (input$peptideLevel_missing.value.algorithm != "BasicMethods")){return(NULL)}
  
  tagList(
    numericInput("peptideLevel_detQuant_quantile", "Quantile", 
                      value = rv$widgets$peptideImput$pepLevel_detQuantile
                      , step=1, min=0, max=100,
                 width='100px'),
  numericInput("peptideLevel_detQuant_factor", "Factor", 
                      value = rv$widgets$peptideImput$pepLevel_detQuant_factor,
                      step=1, min=0, max=10,
               width='100px')
         )
  
})


output$KNNOptsUI <- renderUI({
  req(input$peptideLevel_missing.value.basic.algorithm)
  req(input$peptideLevel_missing.value.algorithm)
  if ((input$peptideLevel_missing.value.basic.algorithm != "KNN") || 
      (input$peptideLevel_missing.value.algorithm != "BasicMethods")){return(NULL)}
  
  isolate({
  numericInput("KNN_n", "Number of neighbors", 
               value = rv$widgets$peptideImput$pepLevel_KNN_n, 
               step=1, min=0, 
               max=max(rv$widgets$peptideImput$KNN_n,nrow(rv$current.obj)),
               width='100px')
  })
})


output$imp4pOptsUI <- renderUI({
  if (input$peptideLevel_missing.value.algorithm != "imp4p"){return(NULL)}
  
  updateSelectInput(session,"peptideLevel_missing.value.basic.algorithm", selected="None")
  tagList(
    numericInput("peptideLevel_imp4p_nbiter", "Number of iterations", 
                      value = rv$widgets$peptideImput$pepLevel_imp4p_nbiter,
                      step=1, min=1, width='100px'),
  
  checkboxInput("peptideLevel_imp4p_withLapala", "Impute MEC also", 
                       value = rv$widgets$peptideImput$pepLevel_imp4p_withLapala )
  )
})


output$imp4pOpts2UI <- renderUI({
  if (!isTRUE(input$peptideLevel_imp4p_withLapala)){return(NULL)}
  
  
  tagList(
    
    numericInput("peptideLevel_imp4p_qmin", "Upper lapala bound", 
                 value = rv$widgets$peptideImput$pepLevel_imp4p_qmin,
                 step=0.1, min=0, max=100,
                 width='100px'),
    radioButtons("peptideLevel_imp4pLAPALA_distrib", "Distribution type", 
                   choices = G_imp4PDistributionType_Choices,
                   selected = rv$widgets$peptideImput$pepLevel_imp4pLAPALA_distrib) 

  )
})



output$peptideLevel_detQuant_impValues <- renderUI({
  req(input$peptideLevel_missing.value.basic.algorithm)
  req(input$peptideLevel_missing.value.algorithm)
  if ((input$peptideLevel_missing.value.basic.algorithm != "detQuantile") || 
      (input$peptideLevel_missing.value.algorithm != "BasicMethods")){return(NULL)}
  
  
  moduleDetQuantImpValuesUI("peptide_DetQuantValues_DT")

})

output$peptideLevel_TAB_detQuant_impValues <- renderDataTable({
  values <- getQuantile4Imp(Biobase::exprs(rv$current.obj), 
                            input$peptideLevel_detQuant_quantile/100, 
                            input$peptideLevel_detQuant_factor)
  DT::datatable(round(as.data.frame(t(values$shiftedImpVal)), digits=rv$settings_nDigits), 
                options = list(initComplete = initComplete(),
                               dom = 't',
                               bLengthChange = FALSE))
})



# 
#------------------------------------------
##' Missing values imputation - reactivity behavior
##' @author Samuel Wieczorek
observeEvent(input$peptideLevel_perform.imputation.button,{
    
    nbMVBefore <- length(which(is.na(Biobase::exprs(rv$current.obj))==TRUE))
    algo <- input$peptideLevel_missing.value.algorithm
    if (algo == "None"){
      rv$current.obj <- rv$dataset[[input$datasets]]
    } else {
      withProgress(message = '',detail = '', value = 0, {
        incProgress(0.5, detail = 'Imputation in progress')
        
      if (algo == "imp4p")
      {
        if (input$peptideLevel_imp4p_withLapala) {
          rv$current.obj <- wrapper.dapar.impute.mi(rv$dataset[[input$datasets]],
                                                    #eps = input$imp4p_eps,
                                                    nb.iter = input$peptideLevel_imp4p_nbiter,
                                                    lapala = input$peptideLevel_imp4p_withLapala,
                                                    q.min = input$peptideLevel_imp4p_qmin / 100,
                                                    distribution = as.character(input$peptideLevel_imp4pLAPALA_distrib))
         
          
        } else {
          rv$current.obj <- wrapper.dapar.impute.mi(rv$dataset[[input$datasets]],
                                                    #eps = input$imp4p_eps,
                                                    nb.iter = input$peptideLevel_imp4p_nbiter,
                                                    lapala = input$peptideLevel_imp4p_withLapala)
          
        }
        
        
      } else if (algo == "BasicMethods"){
        algoBasic <- input$peptideLevel_missing.value.basic.algorithm
        switch(algoBasic,
          KNN={  
              busyIndicator(WaitMsgCalc,wait = 0)
              rv$current.obj <- wrapper.impute.KNN(rv$dataset[[input$datasets]],K=input$KNN_n)
              },
          MLE={
                busyIndicator(WaitMsgCalc,wait = 0)
                rv$current.obj <- wrapper.impute.mle(rv$dataset[[input$datasets]])},
        detQuantile=
              {
              rv$current.obj <- wrapper.impute.detQuant(rv$dataset[[input$datasets]],
                                                    qval = (input$peptideLevel_detQuant_quantile/100),
                                                    factor = input$peptideLevel_detQuant_factor)
              }
        )
      }
        incProgress(1, detail = 'Finalize imputation')
        
      })
    }
    
    
    nbMVAfter <- length(which(is.na(Biobase::exprs(rv$current.obj))==TRUE))
    rv$nbMVimputed <- nbMVAfter - nbMVBefore
    
    
})






##' -- Validate the imputation ---------------------------------------
##' @author Samuel Wieczorek
observeEvent(input$peptideLevel_ValidImputation,{ 
  
  isolate({
    
    
    l.params <- build_ParamsList_PepImputation()
    
    name <- paste0("Imputed", ".", rv$typeOfDataset)
    rv$current.obj <- saveParameters(rv$current.obj, name,"peptideImputation",l.params)
    
    rv$dataset[[name]] <- rv$current.obj
    
    updateSelectInput(session, "datasets",choices = names(rv$dataset), selected = name)
  })
})





output$peptideLevel_warningImputationMethod <- renderText({
  req(input$peptideLevel_missing.value.algorithm)
  req(input$peptideLevel_imp4p_withLapala)
  
  
  if (input$peptideLevel_imp4p_withLapala == FALSE){return(NULL)}
  
  var <- ((input$peptideLevel_missing.value.algorithm == "imp4p") && (isTRUE(input$peptideLevel_imp4p_withLapala))) ||
    (input$peptideLevel_missing.value.basic.algorithm ==  "BasicMethods")
  
  if (var){
    t <- "<br> 
    <font color=\"red\"><strong>Warning:</strong> Warning: Imputed MEC (Missing on the Entire Condition) 
    values must be very cautiously interpreted <br>[see the User manual, Section 6.3.1].</font color=\"red\">"
    HTML(t)}
  
})



output$peptideLevel_showImputationPanel <- renderUI({
  #req(rv$current.obj)
  
  nbEmptyLines <- getNumberOfEmptyLines(exprs(rv$current.obj))
  if (nbEmptyLines == 0)
  {
    tagList(
      htmlOutput("peptideLevel_helpForImputation"),
      htmlOutput("peptideLevel_warningImputationMethod")
    )
    
  }
  else{
    text <- "<br> <br> <font color=\"red\">
    Warning ! Your dataset contains empty lines so that the imputation cannot be proceed.
    <br> <br> Please filter your data first."
    HTML(text)
  }
})

###################




output$peptideLevel_helpForImputation <- renderText({
  req(input$peptideLevel_missing.value.algorithm)
  input$peptideLevel_missing.value.basic.algorithm
  
  
  if ((input$peptideLevel_missing.value.algorithm == "None")) {return(NULL)}
  if ((input$peptideLevel_missing.value.algorithm == "BasicMethods") && is.null(input$peptideLevel_missing.value.basic.algorithm == "None")) {return(NULL)}
  
  name <- NULL
  
  helpTextImputation <- list("imp4p" = "<strong>imp4p [Ref. 7]</strong> is a proteomic-specific multiple imputation 
                             method that operates on peptide-level datasets and which proposes <br>
                             to impute each missing value according to its nature (left-censored 
                             or random). <br> To tune the number of iterations, let us keep in mind that, the more iterations, 
                             the more accurate the results, yet the more time-consuming the computation.",
                             "dummy censored" = "Dummy censored: each missing value is supposed to be a censored value and 
                             is replaced by the XXX quantile <br> of the corresponding sample 
                             abundance distribution",
                             "KNN" = "<strong>K-nearest neighbors</strong>, see [Other ref. 2].",
                             "MLE" = "Imputation with the maximum likelihood estimate of the expected intensity (see the norm R package).")
  
  
  if (input$peptideLevel_missing.value.algorithm == "BasicMethods") {
    name <- input$peptideLevel_missing.value.basic.algorithm}
  else {name <- input$peptideLevel_missing.value.algorithm}
  
  if (!is.null(name)) {
    HTML(helpTextImputation[[name]])
    
  }
})




# 
# output$peptideLevel_progressOne <- renderUI({
#   input$peptideLevel_missing.value.algorithm
#   rv$current.obj
#   if (is.null(input$peptideLevel_missing.value.algorithm)){return(NULL)}
#   if (!grepl( "imp4p",input$peptideLevel_missing.value.algorithm)) {return(NULL)}
#   if (is.null(rv$current.obj)) { return(NULL)}
#   
#   tagList(
#     h5("This may take a while,"),
#     h5("please be patient ..."),
#     progressBar2("peptideLevel_pb1",value=0, size="sm", color="aqua", striped=TRUE, active=TRUE, label=TRUE)
#   )
# })
