###########################################################################
###########################################################################
###########################################################################
##            NORMALIZATION FUNCTIONS                                    ##
###########################################################################
###########################################################################


callModule(moduleDensityplot,"densityPlot_Norm",reactive({input$lab2Show_DS}),reactive({ input$whichGroup2Color_DS}))
callModule(moduleBoxplot,"boxPlot_Norm", reactive({input$legendXAxis_DS}))



output$helpForNormalizationMethods <- renderUI({
    input$normalization.method
    input$normalization.type
    rv$typeOfDataset
    if (is.null(input$normalization.method) || (input$normalization.method == "None")) {return(NULL)}
    
     helpNormalization <- matrix(rep("", 12),nrow = 3, 
                                dimnames=list(c("Global quantile Alignment", "Quantile Centering", "Mean Centering"),
                                              c("sum by columns", "Alignment on all quantiles", "overall", "within conditions")))
    
    
    helpNormalization["Global quantile alignment"] <- "These methods propose 
    normalizations of important magnitude that should be cautiously used.<br>
    it proposes to align the quantiles of all the 
    replicates as described in [6]; <br> practically it amounts to replace 
    abundances by order statistics."




    helpNormalization["Sum by columns"] <- "These methods propose 
    normalizations of important magnitude that should be cautiously used.<br>
    It operates on the original scale (not the log2 one) and propose to 
    normalize each abundance <br> by the total abundance of the sample (so as to focus 
    on the analyte proportions among each sample)."

    
    helpNormalization["Quantile Centering"] <- "These methods propose 
    to shift the sample distributions (either all of them at once, or within 
    each condition at a time) to align <br> a specific quantile: the median (under 
    the assumption that up-regulations and down-regulations are equally frequent), <br>
    the 15% quantile <br> (under the assumption that the signal/noise ratio is 
    roughly the same in all the samples), or any other user's choice."
    
    
    helpNormalization["Mean Centering"] <- "These methods propose to shift the 
    sample distributions (either all of them at once, or within each condition 
    at a time) to align their means. <br> It is also possible to force unit variance 
    (or not)."
    
    
    
    HTML(helpNormalization[input$normalization.method])
})




output$choose_normalizationQuantile <- renderUI({
   req(rv$current.obj)
   req(input$normalization.method)
    if (input$normalization.method != "Quantile Centering") { return (NULL)}
    
    #    if (input$normalization.method == "Quantile Centering"){
    # check if the normalisation has already been performed
    quantileChoices <- list("0.15 (lower limit / noise)"="0.15", "0.5 (median)" = "0.5", "Other"="Other")
    
    radioButtons("normalization.quantile", "Normalization quantile",  choices = quantileChoices, selected=0.15)
    
    
})


output$choose_normalizationQuantileOther <- renderUI({
    req(input$normalization.quantile)
    input$normalization.method
    if (input$normalization.method != "Quantile Centering") { return (NULL)}
   
    if (input$normalization.quantile == "Other"){
        numericInput("normalization.quantileOther", "Normalization quantile other",
                     min=0, max = 1 , value = 0.15,
                     step = 0.1)
        
    }
    
})


output$choose_normalizationScaling <- renderUI({
    req(rv$current.obj)
    req(input$normalization.method)

    if (input$normalization.method %in% c("Mean Centering")){
        # check if the normalisation has already been performed
        
        checkboxInput("normalization.variance.reduction", "Include variance reduction",  value = FALSE)
    }
    
})

output$choose_normalizationType <- renderUI({
    req(rv$current.obj)
    req(input$normalization.method)
    
    if (input$normalization.method == "None") { return (NULL)}
    
    if (input$normalization.method %in% c("Quantile Centering", "Mean Centering", "Sum by columns")){
        
        # check if the normalisation has already been performed
        type <- c("overall", "within conditions")
        selectInput("normalization.type", "Normalization type",  choices = type)

    } 

    
})



output$choose_Normalization_Test <- renderUI({
    rv$current.obj
    if (is.null(rv$current.obj)) { return (NULL)}
    
    # check if the normalisation has already been performed
    method <- normMethods
    
    selectInput("normalization.method","Normalization method", method)
})


##' Reactive behavior : Normalization of data
##' @author Samuel Wieczorek
observeEvent(input$perform.normalization,{
    # input$normalization.method
    # input$normalization.type
    # input$normalization.quantile
    # 
    
    isolate({

                if (input$normalization.method == G_noneStr){
                    rv$current.obj <- rv$dataset[[input$datasets]]
                } else {
                    
                    if (input$normalization.method == "Global quantile alignment"){
                        rv$current.obj <- wrapper.normalizeD(rv$dataset[[input$datasets]], 
                                                              input$normalization.method)
                        updateSelectInput(session, "normalization.method", selected = input$normalization.method)

                    }
                    else if (input$normalization.method =="Quantile Centering"){
                        
                        
                        quant <-NA
                        #print(input$normalization.quantile)
                        if (!is.null(input$normalization.quantile) && (input$normalization.quantile != "Other"))
                        {quant <- as.numeric(input$normalization.quantile)}
                        else {quant <- as.numeric(input$normalization.quantileOther)}
                        rv$current.obj <- wrapper.normalizeD(rv$dataset[[input$datasets]], 
                                                              input$normalization.method, 
                                                              input$normalization.type, 
                                                              quantile = quant)
                        updateSelectInput(session, "normalization.method", selected = input$normalization.method)
                        updateSelectInput(session, "normalization.type", selected = input$normalization.type)
                        if (!is.null(input$normalization.quantile)){
                            updateRadioButtons(session, "normalization.quantile", selected = input$normalization.quantile)}
                        
                        if (!is.null(input$normalization.quantileOther)){
                            updateNumericInput(session, "normalization.quantileOther", value = input$normalization.quantileOther)}

                        
                    }   
                    else if (input$normalization.method =="Mean Centering"){
                        rv$current.obj <- wrapper.normalizeD(rv$dataset[[input$datasets]], 
                                                              input$normalization.method, 
                                                              input$normalization.type, 
                                                              scaling=input$normalization.variance.reduction)
                        updateSelectInput(session, "normalization.method", selected = input$normalization.method)
                        updateSelectInput(session, "normalization.type", selected = input$normalization.type)
                        updateCheckboxInput(session,"normalization.variance.reduction", value=input$normalization.variance.reduction)
                    } 
                    else if (input$normalization.method =="Sum by columns"){
                        rv$current.obj <- wrapper.normalizeD(rv$dataset[[input$datasets]], 
                                                              input$normalization.method, 
                                                              input$normalization.type)
                        updateSelectInput(session, "normalization.method", selected = input$normalization.method)
                        updateSelectInput(session, "normalization.type", selected = input$normalization.type)
                        
                    }

                }

        
    })
})


##' -- Validate and save the normalization ---------------------------------------
##' @author Samuel Wieczorek
observeEvent(input$valid.normalization,{ 
    
    req(input$normalization.method)
    #if (is.null(input$valid.normalization) || (input$valid.normalization == 0)) 
    #{return(NULL)}
    
    isolate({
        # result = tryCatch(
        #     {
                if (input$normalization.method != G_noneStr) {
                    
                  
                  l.params <- list(method = input$normalization.method,
                                   type = input$normalization.type,
                                   varReduction = input$normalization.variance.reduction,
                                   quantile = input$normalization.quantile,
                                   otherQuantile = input$normalization.quantileOther)
                  
                  rv$current.obj <- saveParameters(rv$current.obj,"Norm",l.params)
                  
                  rv$typeOfDataset <-rv$current.obj@experimentData@other$typeOfData
                    name <- paste ("Normalized", " - ", rv$typeOfDataset, sep="")
                    rv$dataset[[name]] <- rv$current.obj
                    
                    UpdateLog("Normalization", l.params)

                    updateSelectInput(session, "datasets", 
                                      #paste("Dataset versions of",rv$current.obj.name, sep=" "),
                                      choices = names(rv$dataset),
                                      selected = name)
                    
                    updateSelectInput(session, "normalization.method", selected = input$normalization.method)
                    updateSelectInput(session, "normalization.type", selected = input$normalization.type)
                    updateCheckboxInput(session,"normalization.variance.reduction", value=input$normalization.variance.reduction)
                    updateNumericInput(session, "normalization.quantileOther", value = input$normalization.quantileOther)
                    updateRadioButtons(session, "normalization.quantile", selected = input$normalization.quantile)
            
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

    leg <- NULL
    grp <- NULL
    
    labelsNorm <- NULL
    labelsToShowNorm <- NULL
    gToColorNorm <- NULL
    if (is.null(input$lab2Show)) { 
        labelsToShowNorm <- c(1:nrow(Biobase::pData(rv$current.obj)))
    }
    else { labelsToShowNorm <- input$lab2Show}
    
    if (is.null(input$whichGroup2Color)){
        gToColorNorm <- "Condition"
    }else{gToColorNorm <- input$whichGroup2Color}
    
    
    if (is.null(input$whichGroup2Color) 
        || (input$whichGroup2Color == "Condition")){
        labelsNorm <- Biobase::pData(rv$current.obj)[,"Condition"]
    }else {
        labelsNorm <- paste(Biobase::pData(rv$current.obj)[,"Condition"],
                            Biobase::pData(rv$current.obj)[,"Bio.Rep"],
                            Biobase::pData(rv$current.obj)[,"Tech.Rep"],
                            Biobase::pData(rv$current.obj)[,"Analyt.Rep"],
                            sep= "_")
    }
    

           if (input$datasets == paste("Normalized", rv$typeOfDataset, sep=" - ")){
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
                                  gToColorNorm)
           
})



viewComparisonNorm <- reactive({
    
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
    
    if (is.null(input$whichGroup2Color)){
        gToColorNorm <- "Condition"
    }else{gToColorNorm <- input$whichGroup2Color}
    
    
    if (is.null(input$whichGroup2Color) 
        || (input$whichGroup2Color == "Condition")){
        labelsNorm <- Biobase::pData(rv$current.obj)[,"Condition"]
    }else {
        labelsNorm <- apply(pData(rv$current.obj), 1, function(x){paste0(x, collapse='_')})
        names(labelsNorm)<- NULL
        labelsNorm <- setNames(as.list(c(1:length(labs))),labs)
    }
    

            dname <- paste("Normalized", rv$typeOfDataset, sep=" - ")
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
                                          gToColorNorm)
  
})

#######################
output$viewComparisonNorm_DS<- renderPlot({
    
    viewComparisonNorm()
})

