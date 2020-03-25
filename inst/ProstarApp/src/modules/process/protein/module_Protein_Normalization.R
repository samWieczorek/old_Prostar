#source(file.path("./src", "modules/Misc/moduleNavigation.R"),  local = TRUE)$value
source(file.path("./src", "modules/Plots/moduleIntensityPlots.R"), local = TRUE)$value
source(file.path("./src", "modules/Plots/moduleDensityPlot.R"), local = TRUE)$value
#source(file.path("./src", "modules/Misc/moduleNotaNumeric.R"), local = TRUE)$value


moduleProtNormalizationUI <- function(id){
  
  ns <- NS(id)
  tagList(
    uiOutput(ns('bars')),
    hr(),
    uiOutput(ns('screens'))
    
  )
}

moduleProtNormalization <- function(input, output, session, dataIn, screen.id, settings=NULL){
  ns <- session$ns
  
  ##### definition of RV for navigation process
  rvNavProcess <- reactiveValues(
    Done = rep(FALSE,2),
    def = list(name = "Filtering",
               stepsNames = c("Normalization", "Save"),
               isMandatory = rep(FALSE,2),
               ll.UI = list( screenStep1 = uiOutput(ns("screenNormalization1")),
                             screenStep2 = uiOutput(ns("screenNormalization2"))
               ),
               rstFunc = reactive({resetModuleNormalization()}))
  )
  

  
  ### appel du module de navigation
  observe({
    rv.normalization$nav2 <- callModule(moduleNavigation2, "moduleProcess_Normalization", 
               isDone = reactive({rvNavProcess$Done}), 
               pages = reactive({rvNavProcess$def}),
               rstFunc = resetModuleNormalization,
               type = reactive({'bubble'}))
  })
  
  
  ###################################
  ### Definition of rv for the filtering module
  ##############################################
  rv.normalization <- reactiveValues(
    ## temporary current data in module
    obj =  NULL,
    nav2 = NULL,
    ## return result of the module
    dataOut = NULL, 
    name = "processNormalization",
    
    widgets = list(method = "None",
                   type = "None",
                   varReduction = FALSE,
                   quantile = 0.15,
                   spanLOESS = 0.7,
                   normalizationFamily = NULL,
                   normalizationMethod = NULL 
                  )
  )
  
  
  resetModuleNormalization <- reactive({  
    ## update widgets values (reactive values)
    #resetModuleProcess("Normalization")
    
    rv.normalization$widgets$method <- "None"
    rv.normalization$widgets$type = "None"
    rv.normalization$widgets$varReduction = FALSE
    rv.normalization$widgets$quantile = 0.15
    rv.normalization$widgets$spanLOESS = 0.7
    rv.normalization$widgets$normalizationFamily = NULL
    rv.normalization$widgets$normalizationMethod = NULL 
    
    ## update widgets in UI
    #updateSelectInput(session, "normalization.method", selected = rv.normalization$widgets$method)
    #updateSelectInput(session, "normalization.type", selected = rv.normalization$widgets$type)
    #updateTextInput(session,"spanLOESS", value = rv.normalization$widgets$spanLOESS)
    #updateTextInput(session, "normalization.quantile", value = rv.normalization$widgets$quantile)
    #updateCheckboxInput(session, "normalization.variance.reduction", value = rv.normalization$widgets$varReduction)
    
    rvNavProcess$Done <-  rep(FALSE,2)
    
  })
  
  
  ### initialisation de la variable globale du process
  observe({
    dataIn()
    rv.normalization$obj <- dataIn()$obj
  })
  
  output$bars <- renderUI({
    rv.normalization$nav2()$bars
  })
  
  
  output$screens <- renderUI({
    rv.normalization$nav2()$screens
  })
  
  ################# END of customized skeleton part   #############################
  #######################################################################
  
  callModule(moduleIntensityPlots,"intensityPlots_Norm", dataIn = reactive({list(obj = rv.normalization$obj,
                                                                                 currentProcess = dataIn()$currentProcess)}))
  callModule(module_Not_a_numeric,"test_spanLOESS", reactive({input$spanLOESS}))
  
  callModule(modulePopover,"modulePopover_normQuanti", 
             data = reactive(list(title = HTML(paste0("<strong>Normalization quantile</strong>")), 
                                  content="lower limit/noise (quantile = 0.15), median (quantile = 0.5). Min value=0, max value=1")))
  
  normMethods <- list("None" = "None",
                      "Global quantile alignment" = "GlobalQuantileAlignment",
                      "Sum by columns" = "SumByColumns",
                      "Quantile Centering" = "QuantileCentering",
                      "Mean Centering" = "MeanCentering",
                      "LOESS" = "LOESS",
                      "vsn" = "vsn"
  )
  
  
  
  
  
  ############ SCREEN NORMALIZATION  #########
  output$screenNormalization1 <- renderUI({
    isolate({
      tagList(
        div(
          div(
            style="display:inline-block; vertical-align: middle; padding-right: 20px;",
            selectInput(ns("normalization.method"),"Normalization method", 
                        choices = normMethods, 
                        selected = rv.normalization$widgets$method,
                        width='200px')
          ),
          div(
            style="display:inline-block; vertical-align: middle; padding-right: 20px;",
            hidden(selectInput(ns("normalization.type"), "Normalization type",  
                               choices = c("overall", "within conditions"), 
                               selected = rv.normalization$widgets$type,
                               width='150px'))
          ),
          div(
            style="display:inline-block; vertical-align: middle; padding-right: 20px;",
            hidden(textInput(ns("spanLOESS"), "Span",value = rv.normalization$widgets$spanLOESS, width='100px')),
            module_Not_a_numericUI(ns("test_spanLOESS")),
            uiOutput(ns("choose_normalizationQuantile")),
            uiOutput(ns("choose_normalizationScaling"))
          ),
          div(
            style="display:inline-block; vertical-align: middle; padding-right: 20px;",
            hidden(actionButton(ns("perform.normalization"), "Perform normalization", class = actionBtnClass, width="170px"))
          )
        ),
        uiOutput(ns("helpForNormalizationMethods")),
        tags$hr(),
        fluidRow(
          column(width=8, moduleIntensityPlotsUI(ns("intensityPlots_Norm"))),
          column(width=4,plotOutput(ns("viewComparisonNorm_DS")) %>% shinycssloaders::withSpinner(type=spinnerType))
        )
      )
    })
    
  })
  
  
  
  
  output$screenNormalization2 <- renderUI({
    
    tagList(
      actionButton(ns("valid.normalization"),"Save normalization", class = actionBtnClass, width="170px")
    )
    
  })
  
  
  
  output$helpForNormalizationMethods <- renderUI({
    req(input$normalization.method)
    if (input$normalization.method == "None") {return(NULL)}
    
    
    switch(input$normalization.method,
           GlobalQuantileAlignment= txt <- "This method proposes a normalization of important
           magnitude that should be cautiously used. It proposes to align the quantiles of all 
           the replicates as described in [Other ref. 1]; practically it amounts to replace 
           abundances by order statistics.",
           QuantileCentering = txt <- "These methods propose to shift the sample distributions 
           (either all of them at once, or within each condition at a time) to align a specific 
           quantile: the median (under the assumption that up-regulations and down-regulations 
           are equally frequent), the 15% quantile (under the assumption that the signal/noise ratio is 
           roughly the same in all the samples), or any other user's choice.",
           MeanCentering = txt <- "These methods propose to shift the sample distributions (either all 
           of them at once, or within each condition at a time) to align their means. It is also possible 
           to force unit variance (or not).",
           SumByColumns = txt <- "These methods propose normalizations of important magnitude that should be cautiously used.
           It operates on the original scale (not the log2 one) and propose to normalize each abundance by the 
           total abundance of the sample (so as to focus on the analyte proportions among each sample).",
           LOESS = txt <- "This method proposes to apply a cyclic LOESS [Other ref. 4, 5] normalization to the data 
           (either all of them at once, or on each condition independently). It relates to  a 
           combination of multiple regression models. The user can tune the regression span (an higher span smooths
           the fit more, while a lower span captures more trends).",
           vsn = txt <- "This method proposes to apply the Variance Stabilization Normalization [Other ref. 6] to the 
           data (either all of them at once, or on each condition independently). No specific parameters required."
  )
    
    tags$p(txt)
  })
  
  
  callModule(module_Not_a_numeric,"test_normQuant", reactive({input$normalization.quantile}))
  
  output$choose_normalizationQuantile <- renderUI({
    req(input$normalization.method)
    if (input$normalization.method != "QuantileCentering") { return (NULL)}
    
    tagList(
      modulePopoverUI("modulePopover_normQuanti"),
      textInput(ns("normalization.quantile"), NULL,
                value = rv.normalization$widgets$quantile,width='150px'),
      module_Not_a_numericUI(ns("test_normQuant"))
    )
    
  })
  
  
  
  output$choose_normalizationScaling <- renderUI({
    req(input$normalization.method)
    
    if (input$normalization.method == "MeanCentering"){
      # check if the normalisation has already been performed
      
      checkboxInput(ns("normalization.variance.reduction"), "Include variance reduction",  
                    value = rv.normalization$widgets$varReduction)
    }
    
  })
  
  
  observeEvent(input$normalization.method,{
    #req(input$normalization.method)
    if (input$normalization.method == "None"){
      rv.normalization$obj <- dataIn()$obj
    }
    
    shinyjs::toggle("perform.normalization", condition=input$normalization.method != "None")
    shinyjs::toggle("spanLOESS", condition=input$normalization.method == "LOESS")
    
    shinyjs::toggle("normalization.type", 
                    condition=( input$normalization.method %in% c("QuantileCentering", "MeanCentering", "SumByColumns", "LOESS", "vsn")))
  })
  
  
  ##' Reactive behavior : Normalization of data
  ##' @author Samuel Wieczorek
  observeEvent(input$perform.normalization,{
    
    isolate({
      
      switch(input$normalization.method, 
             G_noneStr = rv.normalization$obj <- dataIn()$obj,
             GlobalQuantileAlignment = {
               rv.normalization$obj <- wrapper.normalizeD(dataIn()$obj, input$normalization.method)
             },
             QuantileCentering = {
               quant <-NA
               if (!is.null(input$normalization.quantile))
               {
                 quant <- as.numeric(input$normalization.quantile)
                 }
               
               rv.normalization$obj <- wrapper.normalizeD(dataIn()$obj, 
                                                    input$normalization.method, 
                                                    input$normalization.type, 
                                                    quantile = quant)
               
             } ,  
             MeanCentering = {
               rv.normalization$obj <- wrapper.normalizeD(dataIn()$obj, 
                                                    input$normalization.method, 
                                                    input$normalization.type, 
                                                    scaling=input$normalization.variance.reduction)
             }, 
             SumByColumns = {
               rv.normalization$obj <- wrapper.normalizeD(dataIn()$obj, 
                                                    input$normalization.method, 
                                                    input$normalization.type)
               
             },
             LOESS = { rv.normalization$obj <- wrapper.normalizeD(dataIn()$obj, 
                                                            input$normalization.method, 
                                                            input$normalization.type,
                                                            span=as.numeric(input$spanLOESS))
             },
             vsn = {
               rv.normalization$obj <- wrapper.normalizeD(dataIn()$obj, 
                                                    input$normalization.method, 
                                                    input$normalization.type)
             }
      )
    })
    rvNavProcess$Done[1] <- TRUE
    shinyjs::toggle("valid.normalization", condition=input$perform.normalization >= 1)
  })
  
  
  ##' -- Validate and save the normalization ---------------------------------------
  ##' @author Samuel Wieczorek
  observeEvent(input$valid.normalization,{ 
    req(input$perform.normalization)
    
    isolate({
      #if (input$normalization.method != G_noneStr) {
        typeOfDataset <- rv.normalization$obj@experimentData@other$typeOfData
        name <- paste0("Normalized", ".", typeOfDataset)
        rv.normalization$obj <- saveParameters(rv.normalization$obj,name,"Normalization",build_ParamsList_Normalization())
        
        rv.normalization$dataOut <- rv.normalization$obj
       #}
      
      rvNavProcess$Done[2] <- TRUE
      
    } )
  })
  
  
  ###########################################################################
  ###########################################################################
  ###########################################################################
  
  
  
  ##########################
  output$ChooseLegendForNormTabPanel <- renderUI({
    req(rv.normalization$obj)
    .names <- colnames(Biobase::pData(rv.normalization$obj))[-1]
    checkboxGroupInput(ns("legendXAxisNormTabPanel"),
                       label = "Data to show in legend",
                       choices = .names,
                       selected = .names[1])
  })
  
  
  
  
  
  
  
  viewComparisonNorm <- reactive({
    settings()$paletteConditions
    req(rv.normalization$obj)
    
    leg <- NULL
    grp <- NULL
    
    labelsNorm <- NULL
    labelsToShowNorm <- NULL
    gToColorNorm <- NULL
    if (is.null(input$lab2Show)) { 
      labelsToShowNorm <- c(1:nrow(Biobase::pData(rv.normalization$obj)))
    }
    else { labelsToShowNorm <- input$lab2Show}
    
    if (is.null(settings()$whichGroup2Color)){
      gToColorNorm <- "Condition"
    }else{gToColorNorm <- settings()$whichGroup2Color}
    
    
    if (is.null(settings()$whichGroup2Color)  || (settings()$whichGroup2Color == "Condition")){
      labelsNorm <- Biobase::pData(rv.normalization$obj)[,"Condition"]
    }else {
      labelsNorm <- apply(Biobase::pData(rv.normalization$obj), 1, function(x){paste0(x, collapse='_')})
      names(labelsNorm)<- NULL
      labelsNorm <- setNames(as.list(c(1:length(labs))),labs)
    }
    
    
    #dname <- paste0("Normalized.", rv.normalization$obj@experimentData@other$typeOfData)
    #if (input$datasets == dname){
      obj1 <- dataIn()$obj
      obj2 <- rv.normalization$obj
    #}
    #else {
    #  obj1 <-rv$dataset[[input$datasets]]
    #  obj2 <- rv.normalization$obj
      
    #}
    
    wrapper.compareNormalizationD(obj1, obj2,
                                  labelsNorm,
                                  as.numeric(labelsToShowNorm),
                                  palette = settings()$paletteConditions)
    
  })
  
  #######################
  output$viewComparisonNorm_DS<- renderPlot({
    
    viewComparisonNorm()
  })
  
  
  
  
  build_ParamsList_Normalization <- reactive({
    l.params <- list(method = input$normalization.method,
                     type = input$normalization.type,
                     varReduction = input$normalization.variance.reduction,
                     quantile = input$normalization.quantile,
                     spanLOESS = input$spanLOESS)
    l.params
  })
  
  
  return(reactive({rv.normalization$dataOut}))
  
}