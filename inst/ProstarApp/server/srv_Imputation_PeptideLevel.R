require(imp4p)

callModule(moduleMVPlots,"mvImputationPlots_PeptideLevel", data=reactive(rv$current.obj))


callModule(moduleDetQuantImpValues, "peptide_DetQuantValues_DT", 
           reactive({input$peptideLevel_detQuant_quantile}), 
           reactive({input$peptideLevel_detQuant_factor}))


##########
#####  UI for the PEPTIDE LEVEL Imputation process
##########
output$peptideLevelImputationPanel <- renderUI({
  req(rv$current.obj)
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
                                               choices = imputationAlgorithms, width='200px'),
                                   hidden(selectInput("peptideLevel_missing.value.basic.algorithm", 
                                                      "Algorithm",
                                                      choices = basicMethodsImputationAlgos)
                                   ),
                                  hidden(numericInput("peptideLevel_detQuant_quantile", "Quantile", value = 2.5, step=1, min=0, max=100)),
                                  hidden(numericInput("peptideLevel_detQuant_factor", "Factor", value = 1, step=1, min=0, max=10)),

                                  hidden(numericInput("peptideLevel_imp4p_nbiter", "Number of iterations", value = 10, step=1, min=1, width='200px')),
                                  hidden(checkboxInput("peptideLevel_imp4p_withLapala", "with MEC", value = FALSE)),
                                  hidden(numericInput("peptideLevel_imp4p_qmin", "Upper lapala bound", value = 2.5, step=0.1, min=0, max=100)),
                                  hidden(radioButtons("peptideLevel_imp4pLAPALA_distrib", "Distribution type", choices = G_imp4PDistributionType_Choices)), 
                                  
                                  actionButton("peptideLevel_perform.imputation.button", "Perform imputation"),
                                   actionButton("peptideLevel_ValidImputation", "Save imputation",styleclass = "primary"),
                                   br(), br(), br()
                                   #uiOutput("warningImputationMethod"),
                                   
                                   ## progress bar
                                   #br(),
                                   #br(),
                                   #uiOutput(outputId = "progressOne")
                         ),
                         tagList(
                           uiOutput("peptideLevel_showImputationPanel"),
                           hidden(uiOutput("peptideLevel_detQuant_impValues")),
                           busyIndicator(WaitMsgPlot,wait = 0),
                           moduleMVPlotsUI("mvImputationPlots_PeptideLevel")
                         )      
                         
             )
    )
  }
})



observeEvent(input$peptideLevel_missing.value.algorithm,
             shinyjs::toggle('peptideLevel_missing.value.basic.algorithm', 
                             condition=input$peptideLevel_missing.value.algorithm == "BasicMethods"))




observeEvent(  c(input$peptideLevel_missing.value.algorithm, 
                 input$peptideLevel_missing.value.basic.algorithm) ,{
  algo <- input$peptideLevel_missing.value.algorithm
  cond <- (algo=='BasicMethods') && (input$peptideLevel_missing.value.basic.algorithm == "detQuantile")
  shinyjs::toggle('peptideLevel_detQuant_quantile', condition=cond)
  shinyjs::toggle('peptideLevel_detQuant_factor', condition=cond)
   
})

observeEvent(input$peptideLevel_missing.value.algorithm,{
  input$peptideLevel_imp4p_withLapala
  algo <- input$peptideLevel_missing.value.algorithm
  
  shinyjs::toggle('peptideLevel_imp4p_nbiter', condition=algo=='imp4p')
  shinyjs::toggle('peptideLevel_imp4p_withLapala', condition=algo=='imp4p')
})

observeEvent(c(input$peptideLevel_missing.value.algorithm,
               input$peptideLevel_imp4p_withLapala),{
  
  condImp4pLAPALA <- (input$peptideLevel_missing.value.algorithm == 'imp4p') &&
                      isTRUE(input$peptideLevel_imp4p_withLapala)
  shinyjs::toggle('peptideLevel_imp4p_qmin', condition=  condImp4pLAPALA)
  shinyjs::toggle('peptideLevel_imp4pLAPALA_distrib', condition= condImp4pLAPALA)
  
})


observeEvent(input$peptideLevel_missing.value.basic.algorithm,{
  shinyjs::toggle('peptideLevel_detQuant_impValues', 
                  condition=input$peptideLevel_missing.value.basic.algorithm== 'detQuantile')
})

output$peptideLevel_detQuant_impValues <- renderUI({
  tagList(
    h5("The missing values will be imputed by the following values :"),
   # dataTableOutput("peptideLevel_TAB_detQuant_impValues")
    moduleDetQuantImpValuesUI("peptide_DetQuantValues_DT")
  )
  
})

output$peptideLevel_TAB_detQuant_impValues <- renderDataTable({
    values <- getQuantile4Imp(Biobase::exprs(rv$current.obj), 
                              input$peptideLevel_detQuant_quantile/100, 
                              input$peptideLevel_detQuant_factor)
    DT::datatable(round(as.data.frame(t(values$shiftedImpVal)), digits=input$settings_nDigits), 
                  options = list(initComplete = initComplete(),
                                 dom = 't',
                                 bLengthChange = FALSE))
})



# 
#------------------------------------------
##' Missing values imputation - reactivity behavior
##' @author Samuel Wieczorek
observeEvent(input$peptideLevel_perform.imputation.button,{
  isolate({
       
        nbMVBefore <- length(which(is.na(Biobase::exprs(rv$current.obj))==TRUE))
        algo <- input$peptideLevel_missing.value.algorithm
        if (algo == "None"){
          rv$current.obj <- rv$dataset[[input$datasets]]
        } else {
          
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
            
            updateSelectInput(session, "peptideLevel_missing.value.algorithm", "peptideLevel_imp4p_nbiter", 
                              selected = input$peptideLevel_imp4p_nbiter)
            
          } else if (algo == "BasicMethods"){
            algoBasic <- input$peptideLevel_missing.value.basic.algorithm
            if (algoBasic %in% c("KNN", "MLE")) 
            {
              
              busyIndicator(WaitMsgCalc,wait = 0)
              rv$current.obj <- wrapper.mvImputation(rv$dataset[[input$datasets]],
                                                     input$peptideLevel_missing.value.basic.algorithm)

            } 
            else if (algoBasic ==  "detQuantile")
            {
              
              rv$current.obj <- wrapper.impute.detQuant(rv$dataset[[input$datasets]],
                                                        qval = (input$peptideLevel_detQuant_quantile/100),
                                                        factor = input$peptideLevel_detQuant_factor)

              
            }
          }
        }
        
        
        nbMVAfter <- length(which(is.na(Biobase::exprs(rv$current.obj))==TRUE))
        rv$nbMVimputed <- nbMVAfter - nbMVBefore
        
        updateSelectInput(session,"peptideLevel_missing.value.algorithm",selected = input$peptideLevel_missing.value.algorithm)
        updateSelectInput(session,"peptideLevel_missing.value.basic.algorithm",selected = input$peptideLevel_missing.value.basic.algorithm)
        updateSelectInput(session,"peptideLevel_detQuant_quantile",selected = input$peptideLevel_detQuant_quantile)
        updateSelectInput(session,"peptideLevel_detQuant_factor",selected = input$peptideLevel_detQuant_factor)
        updateSelectInput(session,"peptideLevel_imp4p_withLapala", selected = input$peptideLevel_imp4p_withLapala)
        updateSelectInput(session, "peptideLevel_imp4pLAPALA_distrib", selected = input$peptideLevel_imp4pLAPALA_distrib)
        updateSelectInput(session, "peptideLevel_imp4p_qmin", selected = input$peptideLevel_imp4p_qmin)

  })
})



build_ParamsList_PepImputation <- reactive({
  
  ll <- list( pepLevel_algorithm = input$peptideLevel_missing.value.algorithm,
              pepLevel_basicAlgorithm = input$peptideLevel_missing.value.basic.algorithm,
              pepLevel_detQuantile = input$peptideLevel_detQuant_quantile,
              pepLevel_detQuant_factor = input$peptideLevel_detQuant_factor,
              pepLevel_imp4p_nbiter = input$peptideLevel_imp4p_nbiter,
              pepLevel_imp4p_withLapala = input$peptideLevel_imp4p_withLapala,
              pepLevel_imp4p_qmin = input$peptideLevel_imp4p_qmin,
              pepLevel_imp4pLAPALA_distrib = input$peptideLevel_imp4pLAPALA_distrib)
  ll
})



##' -- Validate the imputation ---------------------------------------
##' @author Samuel Wieczorek
observeEvent(input$peptideLevel_ValidImputation,{ 
  
  isolate({

        l.params <- build_ParamsList_PepImputation()
        rv$current.obj <- saveParameters(rv$current.obj, GetCurrentDatasetName(),"peptideImputation",l.params)
        
        name <- paste0("Imputed", ".", rv$typeOfDataset)
        
        rv$dataset[[name]] <- rv$current.obj
        
        UpdateLog("Imputation", l.params)
        
        
        updateSelectInput(session,"peptideLevel_missing.value.algorithm", selected=input$peptideLevel_missing.value.algorithm)
        updateSelectInput(session,"peptideLevel_missing.value.basic.algorithm", selected=input$peptideLevel_missing.value.basic.algorithm)
        updateNumericInput(session,"peptideLevel_detQuant_quantile", value=input$peptideLevel_detQuant_quantile)
        updateNumericInput(session,"peptideLevel_detQuant_factor", value=input$peptideLevel_detQuant_factor)
        updateNumericInput(session,"peptideLevel_imp4p_nbiter", value=input$peptideLevel_imp4p_nbiter)
        updateCheckboxInput(session, "peptideLevel_imp4p_withLapala", value=input$peptideLevel_imp4p_withLapala)
        updateNumericInput(session,"peptideLevel_imp4p_qmin", value=input$peptideLevel_imp4p_qmin)
        updateRadioButtons(session,"peptideLevel_imp4pLAPALA_distrib", selected=input$peptideLevel_imp4pLAPALA_distrib)
        
        updateSelectInput(session, "datasets",choices = names(rv$dataset), selected = name)
  })
})





output$peptideLevel_warningImputationMethod <- renderText({
  input$peptideLevel_missing.value.algorithm
  input$peptideLevel_imp4p_withLapala
  
  if (is.null(input$peptideLevel_missing.value.algorithm)) {return (NULL)}
  
  if (is.null(input$peptideLevel_imp4p_withLapala) || (input$peptideLevel_imp4p_withLapala == FALSE)){return(NULL)}
  
  var <- ((input$peptideLevel_missing.value.algorithm == "imp4p") && (input$peptideLevel_imp4p_withLapala == TRUE)) ||
    (input$peptideLevel_missing.value.basic.algorithm ==  "dummy censored")
  
  if (var){
    t <- "<br> <strong>Lapala</strong> (from French \"là/pas-là\", meaning \"here/not-here\") refers 
    to analytes (peptides or proteins) <br>that are entirely missing in some 
    conditions while they are (partially or totally) <br>visible in others. There 
    specific accounting in a conservative way is a real issue as the imputation <br>
    cannot rely on any observed value in a given condition.
    <br> The parameter \"Upper LAPALA bound\" defines the maximum imputed 
    value as a centile of the observed <br>
    distribution (a tuning between 0% and 10% is advised). <br>
    <font color=\"red\"><strong>Warning:</strong> Imputed lapala values must be very cautiously interpreted.</font color=\"red\">"
    HTML(t)}
  
})


# observe({
#     rv$current.obj
#     if (is.null(rv$current.obj)) {return(NULL)}
#     
#     nbEmptyLines <- getNumberOfEmptyLines(Biobase::exprs(rv$current.obj))
#     if (nbEmptyLines > 0) {
#         shinyjs::disable("peptideLevel_perform.imputation.button")
#         shinyjs::disable("peptideLevel_ValidImputation")
#     } else {
#         shinyjs::enable("peptideLevel_perform.imputation.button")
#         shinyjs::enable("peptideLevel_ValidImputation")
#     }
# })



output$peptideLevel_showImputationPanel <- renderUI({
  rv$current.obj
  
  if (is.null(rv$current.obj)) {return (NULL)}
  
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
  input$peptideLevel_missing.value.algorithm
  input$peptideLevel_missing.value.basic.algorithm
  rv$typeOfDataset
  
  if (is.null(input$peptideLevel_missing.value.algorithm) || (input$peptideLevel_missing.value.algorithm == "None")) {return(NULL)}
  if ((input$peptideLevel_missing.value.algorithm == "BasicMethods") && is.null(input$peptideLevel_missing.value.basic.algorithm == "None")) {return(NULL)}
  
  name <- NULL
  
  helpTextImputation <- list("imp4p" = "<strong>imp4p [5]</strong> is a proteomic-specific multiple imputation 
                             method that operates on peptide-level datasets and which proposes <br>
                             to impute each missing value according to its nature (censored 
                             or random). <br> The more iterations, the more accurate the results, 
                             yet the more time-consuming.",
                             "dummy censored" = "Dummy censored: each missing value is supposed to be a censored value and 
                             is replaced by the XXX quantile <br> of the corresponding sample 
                             abundance distribution",
                             "KNN" = "<strong>K- nearest neighbors</strong>, see [7]",
                             "MLE" = "<strong>Maximum likelihood estimation</strong>, see [8]")
  
  
  if (input$peptideLevel_missing.value.algorithm == "BasicMethods") {
    name <- input$peptideLevel_missing.value.basic.algorithm}
  else {name <- input$peptideLevel_missing.value.algorithm}
  
  if (!is.null(name)) {
    HTML(helpTextImputation[[name]])
    
  }
})





output$peptideLevel_progressOne <- renderUI({
  input$peptideLevel_missing.value.algorithm
  rv$current.obj
  if (is.null(input$peptideLevel_missing.value.algorithm)){return(NULL)}
  if (!grepl( "imp4p",input$peptideLevel_missing.value.algorithm)) {return(NULL)}
  if (is.null(rv$current.obj)) { return(NULL)}
  
  tagList(
    h5("This may take a while,"),
    h5("please be patient ..."),
    progressBar2("peptideLevel_pb1",value=0, size="sm", color="aqua", striped=TRUE, active=TRUE, label=TRUE)
  )
})
