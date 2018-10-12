require(imp4p)

callModule(moduleMVPlots,"mvImputationPlots_PeptideLevel", data=reactive(rv$current.obj))


callModule(moduleDetQuantImpValues, "peptide_DetQuantValues_DT", 
           reactive({rv$widgets$peptideImput$pepLevel_detQuantile}), 
           reactive({rv$widgets$peptideImput$pepLevel_detQuant_factor}))


observeEvent(input$peptideLevel_missing.value.algorithm, {
rv$widgets$peptideImput$pepLevel_algorithm = input$peptideLevel_missing.value.algorithm
shinyjs::toggle('peptideLevel_missing.value.basic.algorithm', 
                condition=input$peptideLevel_missing.value.algorithm == "BasicMethods")

algo <- rv$widgets$peptideImput$peptideLevel_missing.value.algorithm

shinyjs::toggle('peptideLevel_imp4p_nbiter', condition=algo=='imp4p')
shinyjs::toggle('peptideLevel_imp4p_withLapala', condition=algo=='imp4p')

  })

observeEvent(input$peptideLevel_missing.value.basic.algorithm, {
  rv$widgets$peptideImput$pepLevel_basicAlgorithm = input$peptideLevel_missing.value.basic.algorithm
  
  shinyjs::toggle('peptideLevel_detQuant_impValues', 
                  condition=rv$widgets$peptideImput$pepLevel_basicAlgorithm== 'detQuantile')
  
  algo <- rv$widgets$peptideImput$pepLevel_algorithm
  cond <- (algo=='BasicMethods') && (rv$widgets$peptideImput$pepLevel_basicAlgorithm == "detQuantile")
  shinyjs::toggle('peptideLevel_detQuant_quantile', condition=cond)
  shinyjs::toggle('peptideLevel_detQuant_factor', condition=cond)
})

observeEvent(input$peptideLevel_detQuant_quantile, {
  rv$widgets$peptideImput$pepLevel_detQuantile = input$peptideLevel_detQuant_quantile
})

observeEvent(input$peptideLevel_detQuant_factor, {
  rv$widgets$peptideImput$pepLevel_detQuant_factor = input$peptideLevel_detQuant_factor
})


observeEvent(input$peptideLevel_imp4p_nbiter, {
  rv$widgets$peptideImput$pepLevel_imp4p_nbiter = input$peptideLevel_imp4p_nbiter
})


observeEvent(input$peptideLevel_imp4p_withLapala, {
  rv$widgets$peptideImput$pepLevel_imp4p_withLapala = input$peptideLevel_imp4p_withLapala
  condImp4pLAPALA <- (rv$widgets$peptideImput$pepLevel_algorithm == 'imp4p') &&
    isTRUE(rv$widgets$peptideImput$pepLevel_imp4p_withLapala)
  shinyjs::toggle('peptideLevel_imp4p_qmin', condition=  condImp4pLAPALA)
  shinyjs::toggle('peptideLevel_imp4pLAPALA_distrib', condition= condImp4pLAPALA)
  
})

observeEvent(input$peptideLevel_imp4p_qmin, {
  rv$widgets$peptideImput$pepLevel_imp4p_qmin = input$peptideLevel_imp4p_qmin
})


observeEvent(input$peptideLevel_imp4pLAPALA_distrib, {
  rv$widgets$peptideImput$pepLevel_imp4pLAPALA_distrib = input$peptideLevel_imp4pLAPALA_distrib
})



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
                                               choices = imputationAlgorithms, 
                                               selected = rv$widgets$peptideImput$pepLevel_algorithm,
                                               width='200px'),

                                   hidden(selectInput("peptideLevel_missing.value.basic.algorithm", 
                                                      "Methods",
                                                      choices = basicMethodsImputationAlgos)
                                   ),

                                  hidden(numericInput("peptideLevel_detQuant_quantile", "Quantile", 
                                                      value = rv$widgets$peptideImput$pepLevel_detQuantile,
                                                      step=1, min=0, max=100)),
                                  hidden(numericInput("peptideLevel_detQuant_factor", "Factor", 
                                                      value = rv$widgets$peptideImput$pepLevel_detQuant_factor, 
                                                      step=1, min=0, max=10)),

                                  hidden(numericInput("peptideLevel_imp4p_nbiter", "Number of iterations", 
                                                      value = rv$widgets$peptideImput$pepLevel_imp4p_nbiter, 
                                                      step=1, min=1, width='200px')),
                                  
                                  hidden(checkboxInput("peptideLevel_imp4p_withLapala", "Impute MEC also", 
                                                       value = rv$widgets$peptideImput$pepLevel_imp4p_withLapala)),
                                  
                                  hidden(numericInput("peptideLevel_imp4p_qmin", "Upper lapala bound", 
                                                      value = rv$widgets$peptideImput$pepLevel_imp4p_qmin, 
                                                      step=0.1, min=0, max=100)),

                                  
                                  hidden(radioButtons("peptideLevel_imp4pLAPALA_distrib", "Distribution type", choices = G_imp4PDistributionType_Choices)), 
                                  
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
                           hidden(uiOutput("peptideLevel_detQuant_impValues")),
                           busyIndicator(WaitMsgPlot,wait = 0),
                           moduleMVPlotsUI("mvImputationPlots_PeptideLevel")
                         )      
                         
             )
    )
  }
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
                              rv$widgets$peptideImput$pepLevel_detQuantile/100, 
                              rv$widgets$peptideImput$pepLevel_detQuant_factor)
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
  isolate({
       
        nbMVBefore <- length(which(is.na(Biobase::exprs(rv$current.obj))==TRUE))
        algo <- rv$widgets$peptideImput$pepLevel_algorithm
        if (algo == "None"){
          rv$current.obj <- rv$dataset[[input$datasets]]
        } else {
          
          if (algo == "imp4p")
          {
            if (rv$widgets$peptideImput$pepLevel_imp4p_withLapala) {
              
              rv$current.obj <- wrapper.dapar.impute.mi(rv$dataset[[input$datasets]],
                                                        #eps = input$imp4p_eps,
                                                        nb.iter = rv$widgets$peptideImput$pepLevel_imp4p_nbiter,
                                                        lapala = rv$widgets$peptideImput$pepLevel_imp4p_withLapala,
                                                        q.min = rv$widgets$peptideImput$pepLevel_imp4p_qmin / 100,
                                                        distribution = as.character(rv$widgets$peptideImput$pepLevel_imp4pLAPALA_distrib))
              
              
            } else {
              rv$current.obj <- wrapper.dapar.impute.mi(rv$dataset[[input$datasets]],
                                                        #eps = input$imp4p_eps,
                                                        nb.iter = rv$widgets$peptideImput$pepLevel_imp4p_nbiter,
                                                        lapala = rv$widgets$peptideImput$pepLevel_imp4p_withLapala)
        
            }
            
            updateSelectInput(session, "peptideLevel_imp4p_nbiter", 
                              selected = rv$widgets$peptideImput$pepLevel_imp4p_nbiter)
            
          } else if (algo == "BasicMethods"){
            algoBasic <- rv$widgets$peptideImput$pepLevel_basicAlgorithm
            if (algoBasic %in% c("KNN", "MLE")) 
            {
              
              busyIndicator(WaitMsgCalc,wait = 0)
              rv$current.obj <- wrapper.mvImputation(rv$dataset[[input$datasets]],
                                                     rv$widgets$peptideImput$pepLevel_basicAlgorithm)

            } 
            else if (algoBasic ==  "detQuantile")
            {
              
              rv$current.obj <- wrapper.impute.detQuant(rv$dataset[[input$datasets]],
                                                        qval = (rv$widgets$peptideImput$pepLevel_detQuantile/100),
                                                        factor = rv$widgets$peptideImput$pepLevel_detQuant_factor)

              
            }
          }
        }
        
        
        nbMVAfter <- length(which(is.na(Biobase::exprs(rv$current.obj))==TRUE))
        rv$nbMVimputed <- nbMVAfter - nbMVBefore
        
        updateSelectInput(session,"peptideLevel_missing.value.algorithm",selected = rv$widgets$peptideImput$pepLevel_algorithm)
        updateSelectInput(session,"peptideLevel_missing.value.basic.algorithm",selected = rv$widgets$peptideImput$pepLevel_basicAlgorithm)
        updateSelectInput(session,"peptideLevel_detQuant_quantile",selected = rv$widgets$peptideImput$pepLevel_detQuantile)
        updateSelectInput(session,"peptideLevel_detQuant_factor",selected = rv$widgets$peptideImput$pepLevel_detQuant_factor)
        updateSelectInput(session,"peptideLevel_imp4p_withLapala", selected = rv$widgets$peptideImput$pepLevel_imp4p_withLapala)
        updateSelectInput(session, "peptideLevel_imp4pLAPALA_distrib", selected = rv$widgets$peptideImput$pepLevel_imp4pLAPALA_distrib)
        updateSelectInput(session, "peptideLevel_imp4p_qmin", selected = rv$widgets$peptideImput$pepLevel_imp4p_qmin)

  })
})




##' -- Validate the imputation ---------------------------------------
##' @author Samuel Wieczorek
observeEvent(input$peptideLevel_ValidImputation,{ 
  
  isolate({

        l.params <- rv$widgets$peptideImput
        
        name <- paste0("Imputed", ".", rv$typeOfDataset)
        rv$current.obj <- saveParameters(rv$current.obj, name,"peptideImputation",l.params)
        
        rv$dataset[[name]] <- rv$current.obj
        
        
        
        updateSelectInput(session,"peptideLevel_missing.value.algorithm", selected=rv$widgets$peptideImput$pepLevel_algorithm)
        updateSelectInput(session,"peptideLevel_missing.value.basic.algorithm", selected=rv$widgets$peptideImput$pepLevel_basicAlgorithm)
        updateNumericInput(session,"peptideLevel_detQuant_quantile", value=rv$widgets$peptideImput$pepLevel_detQuantile)
        updateNumericInput(session,"peptideLevel_detQuant_factor", value=rv$widgets$peptideImput$pepLevel_detQuant_factor)
        updateNumericInput(session,"peptideLevel_imp4p_nbiter", value=rv$widgets$peptideImput$pepLevel_imp4p_nbiter)
        updateCheckboxInput(session, "peptideLevel_imp4p_withLapala", value=rv$widgets$peptideImput$pepLevel_imp4p_withLapala)
        updateNumericInput(session,"peptideLevel_imp4p_qmin", value=rv$widgets$peptideImput$pepLevel_imp4p_qmin)
        updateRadioButtons(session,"peptideLevel_imp4pLAPALA_distrib", selected=rv$widgets$peptideImput$pepLevel_imp4pLAPALA_distrib)
        
        updateSelectInput(session, "datasets",choices = names(rv$dataset), selected = name)
  })
})





output$peptideLevel_warningImputationMethod <- renderText({
  rv$widgets$peptideImput$pepLevel_algorithm
  rv$widgets$peptideImput$pepLevel_imp4p_withLapala
  
  if (is.null(rv$widgets$peptideImput$pepLevel_algorithm)) {return (NULL)}
  
  if (is.null(rv$widgets$peptideImput$pepLevel_imp4p_withLapala) || (rv$widgets$peptideImput$pepLevel_imp4p_withLapala == FALSE)){return(NULL)}
  
  var <- ((rv$widgets$peptideImput$pepLevel_algorithm== "imp4p") && (rv$widgets$peptideImput$pepLevel_imp4p_withLapala == TRUE)) ||
    (rv$widgets$peptideImput$pepLevel_basicAlgorithm ==  "dummy censored")
  
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
  rv$widgets$peptideImput$pepLevel_algorithm
  rv$widgets$peptideImput$pepLevel_basicAlgorithm
  rv$typeOfDataset
  
  if (is.null(rv$widgets$peptideImput$pepLevel_algorithm) || (rv$widgets$peptideImput$pepLevel_algorithm == "None")) {return(NULL)}
  if ((rv$widgets$peptideImput$pepLevel_algorithm == "BasicMethods") && is.null(rv$widgets$peptideImput$pepLevel_basicAlgorithm== "None")) {return(NULL)}
  
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
  
  
  if (rv$widgets$peptideImput$pepLevel_algorithm == "BasicMethods") {
    name <- rv$widgets$peptideImput$pepLevel_basicAlgorithm}
  else {name <- rv$widgets$peptideImput$pepLevel_algorithm}
  
  if (!is.null(name)) {
    HTML(helpTextImputation[[name]])
    
  }
})





output$peptideLevel_progressOne <- renderUI({
  req(rv$widgets$peptideImput$pepLevel_algorithm)
  req(rv$current.obj)
  if (!grepl( "imp4p",rv$widgets$peptideImput$pepLevel_algorithm)) {return(NULL)}
  
  tagList(
    h5("This may take a while,"),
    h5("please be patient ..."),
    progressBar2("peptideLevel_pb1",value=0, size="sm", color="aqua", striped=TRUE, active=TRUE, label=TRUE)
  )
})
