###########################################################################
###########################################################################
###########################################################################
##            NORMALIZATION FUNCTIONS                                    ##
###########################################################################
###########################################################################


callModule(moduleDensityplot,"densityPlot_Norm")


rv.norm <- reactiveValues(
  trackFromBoxplot = NULL,
  selectProt = NULL
)
rv.norm$selectProt <- callModule(moduleTrackProt, "ProtSelection")
rv.norm$trackFromBoxplot <- callModule(moduleBoxplot,"boxPlot_Norm", trackList = reactive({if (isTRUE(input$SynctForNorm)) {input$ProtForNorm} else NULL}))

callModule(module_Not_a_numeric,"test_spanLOESS", reactive({input$spanLOESS}))

callModule(modulePopover,"modulePopover_normQuanti", 
           data = reactive(list(title = HTML(paste0("<strong>Normalization quantile</strong>")), 
                                content="lower limit/noise (quantile = 0.15), median (quantile = 0.5). Min value=0, max value=1")))


callModule(moduleProcess, "moduleProcess_Normalization", 
           isDone = reactive({rvModProcess$moduleNormalizationDone}), 
           pages = reactive({rvModProcess$moduleNormalization}),
           rstFunc = resetModuleNormalization)






resetModuleNormalization <- reactive({  
  ## update widgets values (reactive values)
  resetModuleProcess("Normalization")
  
  
  ## update widgets in UI
  updateSelectInput(session, "normalization.method", selected = rv$widgets$normalization$method)
  updateSelectInput(session, "normalization.type", selected = rv$widgets$normalization$type)
  updateTextInput(session,"spanLOESS", value = rv$widgets$normalization$spanLOESS)
  updateTextInput(session, "normalization.quantile", value = rv$widgets$normalization$quantile)
  updateCheckboxInput(session, "normalization.variance.reduction", value = rv$widgets$normalization$varReduction)
  
  rvModProcess$moduleNormalizationDone =  rep(FALSE,2)
  
  ##update dataset to put the previous one
  rv$current.obj <- rv$dataset[[last(names(rv$dataset))]] 
  
})





############ SCREEN NORMALIZATION  #########
output$screenNormalization1 <- renderUI({
   
  isolate({
    req(rv$current.obj)
    ll <- Biobase::fData(rv$current.obj)[,rv$current.obj@experimentData@other$proteinId]
    
    
    tagList(
      div(
        
        div(
          style="display:inline-block; vertical-align: middle; padding-right: 20px;",
          selectInput("normalization.method","Normalization method", 
                      choices = normMethods, 
                      selected = rv$widgets$normalization$method,
                      width='200px')
        ),
        div(
          style="display:inline-block; vertical-align: middle; padding-right: 20px;",
          hidden(selectInput("normalization.type", "Normalization type",  
                             choices = c("overall", "within conditions"), 
                             selected = rv$widgets$normalization$type,
                             width='150px'))
        ),
        div(
          style="display:inline-block; vertical-align: middle; padding-right: 20px;",
          hidden(textInput("spanLOESS", "Span",value = rv$widgets$normalization$spanLOESS, width='100px')),
          module_Not_a_numericUI("test_spanLOESS"),
          uiOutput("choose_normalizationQuantile"),
          uiOutput("choose_normalizationScaling")
        ),
        div(
          style="display:inline-block; vertical-align: middle; padding-right: 20px;",
           hidden(div(id='DivProtSelection',moduleTrackProtUI('ProtSelection')))
          # hidden(selectInput("ProtForNorm", "Protein for normalization", choices=ll, multiple = TRUE, width='400px'))
        ),
        div(
          style="display:inline-block; vertical-align: middle; padding-right: 20px;",
          hidden(actionButton("perform.normalization", "Perform normalization", class = actionBtnClass, width="170px"))
        )
        
      ),
      uiOutput("helpForNormalizationMethods"),
      tags$hr(),
      
     tagList(
       hidden(checkboxInput("SynctForNorm", "Synchronise with selection above", value=FALSE)),
       moduleBoxplotUI("boxPlot_Norm")),
     div(
    div(
      style="display:inline-block; vertical-align: middle; padding-right: 20px;",
      moduleDensityplotUI("densityPlot_Norm")
    ),
    div(
      style="display:inline-block; vertical-align: middle; padding-right: 20px;",
      imageOutput("viewComparisonNorm_DS"))
  )
    
  )
  
  
  })
  
})




output$screenNormalization2 <- renderUI({
  
  tagList(
    actionButton("valid.normalization","Save normalization", class = actionBtnClass, width="170px")
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
    textInput("normalization.quantile", NULL,
              value = rv$widgets$normalization$quantile,width='150px'),
    module_Not_a_numericUI("test_normQuant")
  )
  
})



output$choose_normalizationScaling <- renderUI({
  req(input$normalization.method)
  
  if (input$normalization.method == "MeanCentering"){
    # check if the normalisation has already been performed
    
    checkboxInput("normalization.variance.reduction", "Include variance reduction",  
                  value = rv$widgets$normalization$varReduction)
  }
  
})



observeEvent(input$normalization.method,{
  #req(input$normalization.method)
  if (input$normalization.method == "None"){
    rv$current.obj <- rv$dataset[[input$datasets]]
  }
  
  shinyjs::toggle("perform.normalization", condition=input$normalization.method != "None")
  shinyjs::toggle("spanLOESS", condition=input$normalization.method == "LOESS")
  
  shinyjs::toggle("normalization.type", 
                  condition=( input$normalization.method %in% c("QuantileCentering", "MeanCentering", "SumByColumns", "LOESS", "vsn")))

    shinyjs::toggle('DivProtSelection', condition= input$normalization.method %in% c("QuantileCentering", "MeanCentering", "SumByColumns"))
   shinyjs::toggle('SynctForNorm', condition= input$normalization.method %in% c("QuantileCentering", "MeanCentering", "SumByColumns"))
  #print(rv.norm$trackFromBoxplot())
   print(rv.norm$selectProt())
  
  })


observeEvent(rv.norm$selectProt(),{
  print("resume des selections")
  print(GetIndicesOfSelectedProteins())
})

GetIndicesOfSelectedProteins <- reactive({
  req(rv.norm$selectProt()$type)

  
  ind <- NULL
  ll <- Biobase::fData(rv$current.obj)[,rv$current.obj@experimentData@other$proteinId]
  tt <- rv.norm$selectProt()$type
  print(tt)
  switch(tt,
         ProteinList = {ind <- match(rv.norm$selectProt()$list, ll)},
         Random = {ind <- sample(1:length(ll), rv.norm$selectProt()$rand, replace=FALSE)},
         Column = {ind <- which(rv.norm$selectProt()$col == 1)}
         )
  print("indices = ")
  print(ind)
  ind
})




##' Reactive behavior : Normalization of data
##' @author Samuel Wieczorek
observeEvent(input$perform.normalization,{
   
  isolate({
     ll <- Biobase::fData(rv$current.obj)[,rv$current.obj@experimentData@other$proteinId]
    
    switch(input$normalization.method, 
           G_noneStr = rv$current.obj <- rv$dataset[[input$datasets]],
           GlobalQuantileAlignment = {
             rv$current.obj <- wrapper.normalizeD(rv$dataset[[input$datasets]], input$normalization.method)
           },
           QuantileCentering = {
             quant <-NA
             if (!is.null(input$normalization.quantile))
             {quant <- as.numeric(input$normalization.quantile)}
             
             rv$current.obj <- wrapper.normalizeD(rv$dataset[[input$datasets]], 
                                                  input$normalization.method, 
                                                  input$normalization.type, 
                                                  quantile = quant,
                                                  subset.norm=match(input$ProtForNorm, ll))
             
           } ,  
           MeanCentering = {
             rv$current.obj <- wrapper.normalizeD(rv$dataset[[input$datasets]], 
                                                  input$normalization.method, 
                                                  input$normalization.type, 
                                                  scaling=input$normalization.variance.reduction,
                                                  subset.norm=match(input$ProtForNorm, ll))
           }, 
           SumByColumns = {
             rv$current.obj <- wrapper.normalizeD(rv$dataset[[input$datasets]], 
                                                  input$normalization.method, 
                                                  input$normalization.type,
                                                  subset.norm=match(input$ProtForNorm, ll))
             
           },
           LOESS = { rv$current.obj <- wrapper.normalizeD(rv$dataset[[input$datasets]], 
                                                          input$normalization.method, 
                                                          input$normalization.type,
                                                          span=as.numeric(input$spanLOESS))
           },
           vsn = {
             rv$current.obj <- wrapper.normalizeD(rv$dataset[[input$datasets]], 
                                                  input$normalization.method, 
                                                  input$normalization.type)
           }
    )
  })
  rvModProcess$moduleNormalizationDone[1] <- TRUE
  shinyjs::toggle("valid.normalization", condition=input$perform.normalization >= 1)
})


##' -- Validate and save the normalization ---------------------------------------
##' @author Samuel Wieczorek
observeEvent(input$valid.normalization,{ 
  req(input$perform.normalization)
  
  isolate({
    if (input$normalization.method != G_noneStr) {
      rv$typeOfDataset <-rv$current.obj@experimentData@other$typeOfData
      name <- paste0("Normalized", ".", rv$typeOfDataset)
      rv$current.obj <- saveParameters(rv$current.obj,name,"Normalization",build_ParamsList_Normalization())
      rv$dataset[[name]] <- rv$current.obj
      
      rvModProcess$moduleNormalizationDone[2] <- TRUE
      
      updateSelectInput(session, "datasets", 
                        #paste("Dataset versions of",rv$current.obj.name, sep=" "),
                        choices = names(rv$dataset),
                        selected = name)
      
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

# viewComparisonNorm2 <- reactive({
#   rv$PlotParams$paletteConditions
#   leg <- NULL
#   grp <- NULL
#   
#   labelsNorm <- NULL
#   labelsToShowNorm <- NULL
#   gToColorNorm <- NULL
#   
#   labelsToShowNorm <- c(1:nrow(Biobase::pData(rv$current.obj)))
#   
#   
#   
#   if (is.null(rv$whichGroup2Color) 
#       || (rv$whichGroup2Color == "Condition")){
#     labelsNorm <- Biobase::pData(rv$current.obj)[,"Condition"]
#   }else {
#     labelsNorm <- paste(Biobase::pData(rv$current.obj)[,"Condition"],
#                         Biobase::pData(rv$current.obj)[,"Bio.Rep"],
#                         Biobase::pData(rv$current.obj)[,"Tech.Rep"],
#                         Biobase::pData(rv$current.obj)[,"Analyt.Rep"],
#                         sep= "_")
#   }
#   
#   
#   if (input$datasets == paste0("Normalized.", rv$typeOfDataset)){
#     obj1 <- rv$dataset[[(which(names(rv$dataset)==dname) - 1)]]
#     obj2 <- rv$dataset[[input$datasets]]
#   }
#   else {
#     obj1 <-rv$dataset[[input$datasets]]
#     obj2 <- rv$current.obj
#     
#   }
#   
#   wrapper.compareNormalizationD(obj1, obj2,
#                                 labelsNorm,
#                                 as.numeric(labelsToShowNorm),
#                                 palette = rv$PlotParams$paletteConditions)
#   
# })

################
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
  
  if (is.null(rv.norm$trackFromBoxplot())) {
    wrapper.compareNormalizationD(obj1, obj2,
                                labelsNorm,
                                as.numeric(labelsToShowNorm),
                                palette = rv$PlotParams$paletteConditions) }
  else {
    
    ll <- Biobase::fData(rv$current.obj)[,rv$current.obj@experimentData@other$proteinId]
    
    wrapper.compareNormalizationDSubset(obj1, obj2,
                                        labelsNorm,
                                        as.numeric(labelsToShowNorm),
                                        idsForLegend = ll,
                                        palette = NULL,
                                        subset.view= match(rv.norm$trackFromBoxplot(), ll)
                                )
  }
  
})

#######################
output$viewComparisonNorm_DS<- renderImage({
  
  #req(rv$PCA_axes)
  # req(rv$res.pca)
  
  outfile <- tempfile(fileext='.png')
  # Generate a png
  png(outfile)
  viewComparisonNorm()

dev.off()

# Return a list
list(src = outfile,
     alt = "This is alternate text")
}, deleteFile = FALSE)

