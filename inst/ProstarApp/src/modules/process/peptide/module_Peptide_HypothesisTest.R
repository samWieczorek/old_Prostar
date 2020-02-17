#source(file.path("./src", "modules/Misc/moduleNavigation.R"),  local = TRUE)$value
#source(file.path("./src", "modules/Misc/moduleNotaNumeric.R"), local = TRUE)$value



############# Definition of the module   #########################

moduleHypothesisTestUI <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('bars')),
    hr(),
    uiOutput(ns('screens'))
  )
}

moduleHypothesisTest <- function(input, output, session, dataIn, screen.id, settings=NULL){
  ns <- session$ns
  
  #def.progress.saveFiltering <- c("Build Parameters list", "Save Parameters list", "Compte adjacency matrix", "Compute connex composants", "Save new dataset")
  
  ###### definition of RV for navigation process
  rvNavProcess <- reactiveValues(
    Done = rep(FALSE,2),
    def = list(name = "HypothesisTest",
               stepsNames = c("HypothesisTest", "Save"),
               isMandatory = rep(TRUE,2),
               ll.UI = list(uiOutput(ns("screenHypoTest1")),
                            uiOutput(ns("screenHypoTest2"))
               ),
               rstFunc = reactive({resetModuleHypothesisTest()}))
  )
  
  
  ### appel du module de navigation
  observe({
    rv.hypothesisTest$nav2 <-  callModule(moduleNavigation2, "moduleProcess_HypothesisTest", 
               isDone = reactive({rvNavProcess$Done}), 
               pages = reactive({rvNavProcess$def}),
               rstFunc = resetModuleHypothesisTest,
               type = reactive({'bubble'}))
  })
  
  
  ### Definition of rv for the filtering module
  rv.hypothesisTest <- reactiveValues(
    ## temporary current data in module
    obj =  NULL,
    nav2 = NULL,
    ## return result of the module
    dataOut = NULL, 
    name = "processHypothesisTest",
    
    widgets = list(design = "None",
                                    method = "None",
                                    ttest_options = "Student",
                                    th_logFC = 0,
                                    listNomsComparaison = NULL),
    res_AllPairwiseComparisons = NULL
  )
  
  
  ################################################
  
  
  resetModuleHypothesisTest <- reactive({  
    ## update rv.filtering$widgets values (reactive values)
    #resetModuleProcess("PepImputation")
    ## update widgets in UI
    updateSelectInput(session,"anaDiff_Design", selected = rv.hypothesisTest$widgets$design)
    updateSelectInput(session,"diffAnaMethod", selected = rv.hypothesisTest$widgets$method)
    updateRadioButtons(session,"ttest_options", selected = rv.hypothesisTest$widgets$ttest_options)
    updateTextInput(session, "seuilLogFC", value= rv.hypothesisTest$widgets$th_logFC)
    
    rv.hypothesisTest$widgets$design <- "None"
    rv.hypothesisTest$widgets$method <- "None"
    rv.hypothesisTest$widgets$ttest_options <- "Student"
    rv.hypothesisTest$widgets$th_logFC <- 0
    rv.hypothesisTest$widgets$listNomsComparaison <- NULL
    
    rv.hypothesisTest$res_AllPairwiseComparisons
    
    
    rvNavProcess$Done = rep(FALSE, 2)
  })
  
  
  ### initialisation de la variable globale du process
  observe({
    dataIn()
    rv.hypothesisTest$obj <- dataIn()
  })
  
  output$bars <- renderUI({
    rv.hypothesisTest$nav2()$bars
  })
  
  
  output$screens <- renderUI({
    rv.hypothesisTest$nav2()$screens
  })
  
  
  ################# END of definitino part   #############################
  #######################################################################
  
  
  
  
  
  ############## Call different secondary modules ####################
  callModule(module_Not_a_numeric,"test_seuillogFC", reactive({input$seuilLogFC}))
  
  
  
  ###############################################################################
  observeEvent(input$seuilLogFC,{  rv.hypothesisTest$widgets$th_logFC<- as.numeric(input$seuilLogFC)})
  
  observeEvent(input$ttest_options,{rv.hypothesisTest$widgets$ttest_options <- input$ttest_options})
  
  observeEvent(input$diffAnaMethod,{
    
    toggle(id = "ttest_options",  condition = (input$diffAnaMethod == "ttests"))
  })
  
  ######### start of process functions ##########
  
  
  
  
  output$screenHypoTest1 <- renderUI({
    
    # req(rv.hypothesisTest$obj)
    isolate({
      NA.count<- length(which(is.na(Biobase::exprs(rv.hypothesisTest$obj))))
      if (NA.count > 0){
        tags$p("Your dataset contains missing values. Before using the differential analysis, you must filter/impute them")
      } else {
        tagList(
          
          tags$div(
            tags$div( style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                      selectInput(ns("anaDiff_Design"), "Contrast", 
                                  choices=c("None"="None", "One vs One"="OnevsOne", "One vs All"="OnevsAll"),
                                  selected=rv.hypothesisTest$widgets$design,
                                  width='150px')
            ),
            tags$div( style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                      selectInput(ns("diffAnaMethod"),"Statistical test",
                                  choices = c("None"="None",
                                              "Limma"="Limma", 
                                              "t-tests"="ttests"),
                                  selected=rv.hypothesisTest$widgets$method,
                                  width='150px')
            ),
            tags$div( style="display:inline-block; vertical-align: middle; padding-right: 20px;",
                      hidden( radioButtons(ns("ttest_options"), "t-tests options",choices=c("Student", "Welch"),
                                           selected=rv.hypothesisTest$widgets$ttest_options,
                                           width='150px'))
            ),
            tags$div( style="display:inline-block; vertical-align: middle; padding-right: 20px;",
                      textInput(ns("seuilLogFC"), "log(FC) threshold",  
                                value=rv.hypothesisTest$widgets$th_logFC,
                                width='150px'),
                      module_Not_a_numericUI(ns("test_seuillogFC"))
            )
          )
          ,
          tags$hr(),
          highchartOutput(ns("FoldChangePlot"), height="100%") %>% shinycssloaders::withSpinner(type=spinnerType)
        )
        
      }
    })
  })
  
  
  output$screenHypoTest2 <- renderUI({
    tagList(
      uiOutput(ns("btn_valid"))
    )
  })
  
  
  
  output$btn_valid <- renderUI({
    cond <- (input$diffAnaMethod != "None")&&(input$anaDiff_Design != "None")
    if (!cond){return(NULL)}
    actionButton(ns("ValidTest"),"Save significance test", class = actionBtnClass)
  })
  
  
  
  
  
  output$FoldChangePlot <- renderHighchart({
    #req(rv$res_AllPairwiseComparisons)
    settings()$paletteConditions
    
    data <- ComputeComparisons()
    #rv$tempplot$logFCDistr <- hc_logFC_DensityPlot(data$logFC,as.numeric(input$seuilLogFC))
    #rv$tempplot$logFCDistr
    hc_logFC_DensityPlot(data$logFC,as.numeric(input$seuilLogFC))
  })
  
  
  
  ########################################################
  
  ### calcul des comparaisons                         ####
  ########################################################
  ComputeComparisons <- reactive({
    req(input$diffAnaMethod)
    req(input$anaDiff_Design)
    input$ttest_options
    if ((input$diffAnaMethod=="None")|| (input$anaDiff_Design=="None")) {return (NULL)}
    if (length(which(is.na(Biobase::exprs(rv.hypothesisTest$obj)))) > 0) { return()}
    
    isolate({
      #if (is.null(rv.hypothesisTest$obj@experimentData@other$Params[["HypothesisTest"]])){
      switch(input$diffAnaMethod,
             Limma={
               rv.hypothesisTest$res_AllPairwiseComparisons <- limmaCompleteTest(Biobase::exprs(rv.hypothesisTest$obj), 
                                                                  Biobase::pData(rv.hypothesisTest$obj),
                                                                  input$anaDiff_Design) 
               
             },
             ttests={
               rv.hypothesisTest$res_AllPairwiseComparisons <- wrapper.t_test_Complete(rv.hypothesisTest$obj, 
                                                                        Contrast=input$anaDiff_Design,
                                                                        type=input$ttest_options)
             })
      rv.hypothesisTest$widgets$listNomsComparaison <- colnames(rv.hypothesisTest$res_AllPairwiseComparisons$logFC)
      
      
      rvNavProcess$Done[1] <- TRUE
      rv.hypothesisTest$res_AllPairwiseComparisons
    })
  })
  
  
  
  
  ########################################################################
  #
  #
  ########################################################################
  observeEvent(input$ValidTest,{ 
    # req(rv.hypothesisTest$obj)
    req(rv.hypothesisTest$res_AllPairwiseComparisons)
    
    if (length(which(is.na(Biobase::exprs(rv.hypothesisTest$obj)))) > 0) { return()}
    
    ### Save RAW data
    isolate({
      rv.hypothesisTest$obj <- diffAnaSave(obj = rv.hypothesisTest$obj,
                                           allComp = rv.hypothesisTest$res_AllPairwiseComparisons)
      
      
      name <- paste("HypothesisTest.", rv.hypothesisTest$obj@experimentData@other$typeOfDataset, sep="")
      rv.hypothesisTest$obj <- saveParameters(rv.hypothesisTest$obj, name,"HypothesisTest", build_ParamsList_HypothesisTest())
      
      rv.hypothesisTest$dataOut <- list(obj = rv.hypothesisTest$obj,
                                        res_AllPairwiseComparisons =rv.hypothesisTest$res_AllPairwiseComparisons)
      
      rvNavProcess$Done[2] <- TRUE
      
      #BuildNavbarPage()
    })
    
  })
  
  
  
  #------------------------------------------------------------------------
  build_ParamsList_HypothesisTest <- reactive({
    l.params <- list(design = input$anaDiff_Design,
                     method = input$diffAnaMethod,
                     ttest_options = input$ttest_options,
                     th_logFC = as.numeric(input$seuilLogFC),
                     AllPairwiseCompNames = list(logFC = colnames(rv.hypothesisTest$res_AllPairwiseComparisons$logFC), 
                                                 P_Value=colnames(rv.hypothesisTest$res_AllPairwiseComparisons$P_Value))
    )
    l.params
  })
  
  
  
  
  
  return(reactive({rv.hypothesisTest$dataOut}))
  
  }




