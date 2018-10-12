###########################################################################
###########################################################################
###########################################################################
##            NORMALIZATION FUNCTIONS                                    ##
###########################################################################
###########################################################################


callModule(moduleDensityplot,"densityPlot_Norm")
callModule(moduleBoxplot,"boxPlot_Norm")

callModule(modulePopover,"modulePopover_normQuanti", 
           data = reactive(list(title = HTML(paste0("<strong>Normalization quantile</strong>")), 
                                content="lower limit/noise (quantile = 0.15), median (quantile = 0.5)")))




observeEvent(input$normalization.method,{
  rv$widgets$normalization$method <- input$normalization.method
})

observeEvent(input$normalization.type,{
  rv$widgets$normalization$type <- input$normalization.type
})


observeEvent(input$normalization.quantile,{
  rv$widgets$normalization$quantile <- input$normalization.quantile
})

observeEvent(input$normalization.variance.reduction,{
  rv$widgets$normalization$varReduction <- input$normalization.variance.reduction
})

observeEvent(input$spanLOESS,{
  rv$widgets$normalization$spanLOESS <- input$spanLOESS
})



output$helpForNormalizationMethods <- renderUI({
    req(rv$widgets$normalization$method)
    if (rv$widgets$normalization$method == "None") {return(NULL)}
    

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



output$choose_normalizationQuantile <- renderUI({
    req(rv$widgets$normalization$method)
    if (rv$widgets$normalization$method != "QuantileCentering") { return (NULL)}
   
    tagList(
      modulePopoverUI("modulePopover_normQuanti"),
        numericInput("normalization.quantile", "",
                     min=0, max = 1 , value = rv$widgets$normalization$quantile,step = 0.1, width='150px')
    )
    
})



output$choose_normalizationScaling <- renderUI({
    req(rv$widgets$normalization$method)

    if (rv$widgets$normalization$method == "MeanCentering"){
        # check if the normalisation has already been performed
        
        checkboxInput("normalization.variance.reduction", "Include variance reduction",  
                      value = rv$widgets$normalization$varReduction)
    }
    
})


observeEvent(rv$widgets$normalization$method,{
  #req(input$normalization.method)
  if (rv$widgets$normalization$method == "None"){
    rv$current.obj <- rv$dataset[[input$datasets]]
  }
  
  shinyjs::toggle("perform.normalization", condition=rv$widgets$normalization$method != "None")
  shinyjs::toggle("spanLOESS", condition=rv$widgets$normalization$method == "LOESS")
  
  shinyjs::toggle("normalization.type", 
                  condition=( rv$widgets$normalization$method %in% c("QuantileCentering", "MeanCentering", "SumByColumns", "LOESS", "vsn")))
})


##' Reactive behavior : Normalization of data
##' @author Samuel Wieczorek
observeEvent(input$perform.normalization,{
    
    isolate({

        switch(rv$widgets$normalization$method, 
          G_noneStr = rv$current.obj <- rv$dataset[[input$datasets]],
          GlobalQuantileAlignment = {
              rv$current.obj <- wrapper.normalizeD(rv$dataset[[input$datasets]], rv$widgets$normalization$method)
              updateSelectInput(session, "normalization.method", selected = rv$widgets$normalization$method)
              },
          QuantileCentering = {
                         quant <-NA
                        if (!is.null(rv$widgets$normalization$quantile))
                        {quant <- as.numeric(rv$widgets$normalization$quantile)}

                        rv$current.obj <- wrapper.normalizeD(rv$dataset[[input$datasets]], 
                                                             rv$widgets$normalization$method, 
                                                             rv$widgets$normalization$type, 
                                                              quantile = quant)
                        updateSelectInput(session, "normalization.method", selected = rv$widgets$normalization$method)
                        updateSelectInput(session, "normalization.type", selected = rv$widgets$normalization$type)
                        if (!is.null(rv$widgets$normalization$quantile)){
                          updateNumericInput(session, "normalization.quantile", value = rv$widgets$normalization$quantile)}
                        
                    } ,  
         MeanCentering = {
                        rv$current.obj <- wrapper.normalizeD(rv$dataset[[input$datasets]], 
                                                             rv$widgets$normalization$method, 
                                                             rv$widgets$normalization$type, 
                                                              scaling=rv$widgets$normalization$varReduction)
                        updateSelectInput(session, "normalization.method", selected = rv$widgets$normalization$method)
                        updateSelectInput(session, "normalization.type", selected = rv$widgets$normalization$type)
                        updateCheckboxInput(session,"normalization.variance.reduction", value=rv$widgets$normalization$varReduction)
                    }, 
         SumByColumns = {
                        rv$current.obj <- wrapper.normalizeD(rv$dataset[[input$datasets]], 
                                                             rv$widgets$normalization$method, 
                                                             rv$widgets$normalization$type)
                        updateSelectInput(session, "normalization.method", selected = rv$widgets$normalization$method)
                        updateSelectInput(session, "normalization.type", selected = rv$widgets$normalization$type)
                        
                    },
         LOESS = { rv$current.obj <- wrapper.normalizeD(rv$dataset[[input$datasets]], 
                                                        rv$widgets$normalization$method, 
                                                        rv$widgets$normalization$type,
                                                  span=rv$widgets$normalization$spanLOESS)
                    updateSelectInput(session, "normalization.method", selected = rv$widgets$normalization$method)
                    updateSelectInput(session, "normalization.type", selected = rv$widgets$normalization$type)
                    updateNumericInput(session, "spanLOESS", value = rv$widgets$normalization$spanLOESS)
                },
         vsn = {
            rv$current.obj <- wrapper.normalizeD(rv$dataset[[input$datasets]], 
                                                 rv$widgets$normalization$method, 
                                                 rv$widgets$normalization$type)
            updateSelectInput(session, "normalization.method", selected = rv$widgets$normalization$method)
            updateSelectInput(session, "normalization.type", selected = rv$widgets$normalization$type)
            }
        )
    })
  
  shinyjs::toggle("valid.normalization", condition=input$perform.normalization >= 1)
})


##' -- Validate and save the normalization ---------------------------------------
##' @author Samuel Wieczorek
observeEvent(input$valid.normalization,{ 
    req(input$perform.normalization)
    
    isolate({
        # result = tryCatch(
        #     {
                if (rv$widgets$normalization$method != G_noneStr) {
                    
                  
                  l.params <- rv$widgets$normalization
                  
                  
                  rv$typeOfDataset <-rv$current.obj@experimentData@other$typeOfData
                  
                    name <- paste0("Normalized", ".", rv$typeOfDataset)
                    rv$current.obj <- saveParameters(rv$current.obj,name,"Normalization",l.params)
                    rv$dataset[[name]] <- rv$current.obj
                    
                    updateSelectInput(session, "datasets", 
                                      #paste("Dataset versions of",rv$current.obj.name, sep=" "),
                                      choices = names(rv$dataset),
                                      selected = name)
                    
                    updateSelectInput(session, "normalization.method", selected = rv$widgets$normalization$method)
                    updateSelectInput(session, "normalization.type", selected = rv$widgets$normalization$type)
                    updateCheckboxInput(session,"normalization.variance.reduction", value=rv$widgets$normalization$varReduction)
                    updateNumericInput(session, "normalization.quantile", value = rv$widgets$normalization$quantile)
                    updateNumericInput(session, "spanLOESS", value = rv$widgets$normalization$spanLOESS)
                }

    } )
})


###########################################################################
###########################################################################
###########################################################################



##########################
output$ChooseLegendForNormTabPanel <- renderUI({
    rv$current.obj
    if (is.null(rv$current.obj)){return(NULL)}
    .names <- colnames(Biobase::pData(rv$current.obj))[-1]
    checkboxGroupInput("legendXAxisNormTabPanel",
                       label = "Data to show in legend",
                       choices = .names,
                       selected = .names[1])
})




#######################

viewComparisonNorm2 <- reactive({
  rv$PlotParams$paletteConditions
    leg <- NULL
    grp <- NULL
    
    labelsNorm <- NULL
    labelsToShowNorm <- NULL
    gToColorNorm <- NULL
    
        labelsToShowNorm <- c(1:nrow(Biobase::pData(rv$current.obj)))
    
    
    
    if (is.null(rv$whichGroup2Color) 
        || (rv$whichGroup2Color == "Condition")){
        labelsNorm <- Biobase::pData(rv$current.obj)[,"Condition"]
    }else {
        labelsNorm <- paste(Biobase::pData(rv$current.obj)[,"Condition"],
                            Biobase::pData(rv$current.obj)[,"Bio.Rep"],
                            Biobase::pData(rv$current.obj)[,"Tech.Rep"],
                            Biobase::pData(rv$current.obj)[,"Analyt.Rep"],
                            sep= "_")
    }
    

           if (input$datasets == paste0("Normalized.", rv$typeOfDataset)){
               obj1 <- rv$dataset[[(which(names(rv$dataset)==dname) - 1)]]
               obj2 <- rv$dataset[[input$datasets]]
           }
           else {
               obj1 <-rv$dataset[[input$datasets]]
               obj2 <- rv$current.obj
               
           }
    
            wrapper.compareNormalizationD(obj1, obj2,
                                  labelsNorm,
                                  as.numeric(labelsToShowNorm),
                                  palette = rv$PlotParams$paletteConditions)
           
})



viewComparisonNorm <- reactive({
  rv$PlotParams$paletteConditions
    req(rv$current.obj)
    
     leg <- NULL
    grp <- NULL
    
    labelsNorm <- NULL
    labelsToShowNorm <- NULL
    gToColorNorm <- NULL
    if (is.null(input$lab2Show)) { 
        labelsToShowNorm <- c(1:nrow(Biobase::pData(rv$current.obj)))
    }
    else { labelsToShowNorm <- input$lab2Show}
    
    if (is.null(rv$whichGroup2Color)){
        gToColorNorm <- "Condition"
    }else{gToColorNorm <- rv$whichGroup2Color}
    
    
    if (is.null(rv$whichGroup2Color) 
        || (rv$whichGroup2Color == "Condition")){
        labelsNorm <- Biobase::pData(rv$current.obj)[,"Condition"]
    }else {
        labelsNorm <- apply(pData(rv$current.obj), 1, function(x){paste0(x, collapse='_')})
        names(labelsNorm)<- NULL
        labelsNorm <- setNames(as.list(c(1:length(labs))),labs)
    }
    

            dname <- paste0("Normalized.", rv$typeOfDataset)
                if (input$datasets == dname){
                obj1 <- rv$dataset[[(which(names(rv$dataset)==dname) - 1)]]
                obj2 <- rv$dataset[[input$datasets]]
            }
            else {
                obj1 <-rv$dataset[[input$datasets]]
                obj2 <- rv$current.obj
                
            }
            
            wrapper.compareNormalizationD(obj1, obj2,
                                          labelsNorm,
                                          as.numeric(labelsToShowNorm),
                                          palette = rv$PlotParams$paletteConditions)
  
})

#######################
output$viewComparisonNorm_DS<- renderPlot({
    
    viewComparisonNorm()
})

