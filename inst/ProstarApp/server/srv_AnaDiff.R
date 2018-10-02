callModule(moduleVolcanoplot,"volcano_Step1", reactive({input$selectComparison}),reactive({input$tooltipInfo}))
callModule(moduleVolcanoplot,"volcano_Step2",reactive({input$selectComparison}),reactive({input$tooltipInfo}))
callModule(moduleStaticDataTable,"params_AnaDiff", table2show=reactive({rv$params.anaDiff}), withBtns = FALSE)
#callModule(moduleStaticDataTable,"anaDiff_selectedItems", table2show=reactive({GetSelectedItems()}), withBtns = TRUE)


output$anaDiffPanel <- renderUI({
  #req(rv$current.obj)
  NA.count<- length(which(is.na(Biobase::exprs(rv$current.obj))))
  dataset.name <- last(names(rv$dataset))
  if (NA.count > 0){
    tags$p("Your dataset contains missing values. Before using the differential analysis, you must filter/impute them")
  } else if (is.null(rv$current.obj@experimentData@other$Params[[dataset.name]]['HypothesisTest'])) {
    tags$p("The statistical test has not been performed so the differential analysis cannot be done.")
    } else {
  tabsetPanel(
    id = "xxx",
    tabPanel("2 - Pairwise comparison",value = "DiffAnalysis_PairewiseComparison",
             uiOutput("diffAna_pairwiseComp")),
    tabPanel("3 - p-value calibration", value = "DiffAnalysis_Calibrate",
             uiOutput("diffAna_pvalCalib")),
    tabPanel("4 - FDR",value = "DiffAnalysis_viewFDR",
             uiOutput("diffAna_fdrCompute")),
    tabPanel("5 - Summary",value = "DiffAnalysis_ValidateAndSave",
             uiOutput("diffAna_Summary"))
  ) # end tabsetPanel
}
})



output$diffAna_pairwiseComp <- renderUI({
  req(rv$current.obj)
  
  sidebarCustom()
           splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
                       wellPanel(
                         id = "sidebar_DiffAna2",
                         height = "100%"
                         ,uiOutput("newComparisonUI")
                         ,uiOutput("diffAnalysis_PairwiseComp_SB")
                         ,actionButton("AnaDiff_perform.filtering.MV", "Perform", class = actionBtnClass),
                         uiOutput("tooltipInfo")
                       ),
                       tagList(
                         moduleVolcanoplotUI("volcano_Step1") %>% withSpinner(type=spinnerType)
                       )
           )
})


output$diffAna_pvalCalib <- renderUI({
  req(input$selectComparison)
  
   tagList(
               tags$div(
                 tags$div( style="display:inline-block; vertical-align: middle; padding-right: 40px;",
                           selectInput("calibrationMethod","Calibration method",choices = calibMethod_Choices, width='200px')
                 ),
                 tags$div( style="display:inline-block; vertical-align: middle;",
                           hidden(numericInput( "numericValCalibration","Proportion of TRUE null hypohtesis", 
                                                value = 0, min=0, max=1, step=0.05, width='200px'))
                 )
               ),
               tags$hr(),
               fluidRow(
                 column(width=6,htmlOutput("errMsgCalibrationPlotAll"),
                        plotOutput("calibrationPlotAll") %>% withSpinner(type=spinnerType)),
                 column(width=6,uiOutput("errMsgCalibrationPlot"),
                        plotOutput("calibrationPlot") %>% withSpinner(type=spinnerType)
                 )
               )
             )
  })    

  
output$diffAna_fdrCompute <- renderUI({
  req(input$selectComparison)
     
 tagList(
        numericInput("seuilPVal",  "Define the -log10(p_value) threshold",
                           min = 0,value = 0,step=0.1, width='100px'),
        checkboxInput("showpvalTable","Show p-value table", value=FALSE),
        htmlOutput("showFDR"),
        moduleVolcanoplotUI("volcano_Step2") %>% withSpinner(type=spinnerType),
        #hidden(div(id = 'toto',
        #.           moduleStaticDataTableUI("anaDiff_selectedItems")
        #)
        downloadButton('downloadSelectedItems', 'Download as Excel file'),
        hidden(DTOutput("anaDiff_selectedItems"))
        )

})     



output$anaDiff_selectedItems <- renderDT({
  
  DT::datatable(GetSelectedItems(),
                escape = FALSE,
                rownames=TRUE,
                extensions = 'Buttons',
                options = list(initComplete = initComplete(),
                               dom = 't',
                               server = TRUE,
                               columnDefs = list(list(width='200px',targets= "_all")),
                               ordering = FALSE)
  )
})


output$downloadSelectedItems <- downloadHandler(
  #input$chooseDatasetToExportToMSnset,
  filename = function() { 
    #input$nameExport
    #if (input$fileformatExport == gFileFormatExport$excel) {
    #  paste(input$nameExport,gFileExtension$excel,  sep="")}
    paste0('diffanalysis_', input$datasets,'.xlsx')
  },
  content = function(file) {
    wb <- openxlsx::createWorkbook() # Create wb in R
    openxlsx::addWorksheet(wb,sheetName="Output") #create sheet
    #Creates a Data Table in Excel if you want, otherwhise only use write Data
    openxlsx::writeData(wb,1, GetSelectedItems(), colNames = TRUE)
    openxlsx::mergeCells(wb,sheet = "Output", cols=1:5, rows=1)
    #writeData(wb,1, "Include text also based on reactive function and in merged cells" )
    openxlsx::saveWorkbook(wb, file = file, overwrite = TRUE)
  },
  contentType= "excel/xlsx")




observeEvent(input$showpvalTable, {
  print("show : anaDiff_selectedItems")
  shinyjs::toggle(id = "anaDiff_selectedItems", condition=isTRUE(input$showpvalTable))})

    
    
output$diffAna_Summary <- renderUI({     

 tagList(
   moduleStaticDataTableUI("params_AnaDiff")
   
         )
})
      







output$warningNA <- renderUI({
    rv$current.obj
    if (is.null(rv$current.obj)) {return ()}
    
    NA.count <- length(which(is.na(Biobase::exprs((rv$current.obj)))))
    
    if(NA.count    >    0){
        
        text <- "<br> <br> <font color=\"red\">
        Warning ! Your dataset contains empty lines so that the 
        imputation cannot be proceed.
        <br> <br> Please filter your data first."
        HTML(text)
    }
})



callModule(modulePopover,"modulePopover_volcanoTooltip", 
           data = reactive(list(title = HTML(paste0("<strong><font size=\"4\">Tooltip</font></strong>")), 
                                content="Infos to be displayed in the tooltip of volcanoplot")))

callModule(modulePopover,"modulePopover_pushPVal", data = reactive(list(title=HTML(paste0("<strong><font size=\"4\">P-Value push</font></strong>")),
                                                                        content= "This functionality is useful in case of multiple pairwise omparisons (more than 2 conditions): At the filtering step, a given analyte X (either peptide or protein) may have been kept because it contains very few missing values in a given condition (say Cond. A), even though it contains (too) many of them in all other conditions (say Cond B and C only contains “MEC” type missing values). Thanks to the imputation step, these missing values are no longer an issue for the differential analysis, at least from the computational viewpoint. However, statistically speaking, when performing B vs C, the test will rely on too many imputed missing values to derive a meaningful p-value: It may be wiser to consider analyte X as non-differentially abundant, regardless the test result (and thus, to push its p-value to 1). This is just the role of the “P-value push” parameter. It makes it possible to introduce a new filtering step that only applies to each pairwise comparison, and which assigns a p-value of 1 to analytes that, for the considered comparison are assumed meaningless due to too many missing values (before imputation).")))



output$newComparisonUI <- renderUI({
  req(rv$current.obj)
  if ("Significant" %in% colnames(Biobase::fData(rv$current.obj))){
    actionButton("newComparison", "New comparison", class = actionBtnClass)
  }
  
})


observeEvent(input$newComparison, {
    
    updateSelectInput(session,"selectComparison", selected="None")
    updateCheckboxInput(session,"swapVolcano", value=FALSE )
    updateRadioButtons(session, "AnaDiff_ChooseFilters", selected=gFilterNone)
    
    updateSelectInput(session,"calibrationMethod", selected="pounds")
    updateNumericInput(session, "seuilPVal", value=0)
})




observeEvent(input$AnaDiff_ChooseFilters,{
  if (input$AnaDiff_ChooseFilters==gFilterNone) {updateSelectInput(session, "AnaDiff_seuilNA", selected = 0)}
  
} 
             )

output$AnaDiff_seuilNADelete <- renderUI({ 
    input$AnaDiff_ChooseFilters
    req(rv$current.obj)
    if (input$AnaDiff_ChooseFilters==gFilterNone) {return(NULL)   }
    
    choix <- getListNbValuesInLines(rv$current.obj, type=input$AnaDiff_ChooseFilters)
   
    selectInput("AnaDiff_seuilNA", 
                "Keep lines with at least x intensity values", 
                choices = choix)
    
})


observeEvent(input$swapVolcano,{
    req(rv$resAnaDiff)
    rv$resAnaDiff$logFC <- - rv$resAnaDiff$logFC
})


#####
####  SELECT AND LOAD ONE PARIWISE COMPARISON
####
observeEvent(input$selectComparison,{
 # req(rv$res_AllPairwiseComparisons)
  
if (input$selectComparison== ""){
  rv$resAnaDiff <- NULL
} else {
    #if (is.null(rv$current.obj@experimentData@other$Params[["anaDiff"]])) {  ### There is no previous analysis
        index <- which(paste(input$selectComparison, "_logFC", sep="") == colnames(rv$res_AllPairwiseComparisons$logFC))
        rv$resAnaDiff <- list(logFC = (rv$res_AllPairwiseComparisons$logFC)[,index],
                          P_Value = (rv$res_AllPairwiseComparisons$P_Value)[,index],
                          condition1 = strsplit(input$selectComparison, "_vs_")[[1]][1],
                          condition2 = strsplit(input$selectComparison, "_vs_")[[1]][2]
                        )
    
}
})





output$diffAnalysis_PairwiseComp_SB <- renderUI({
    req(rv$res_AllPairwiseComparisons)
    
    .choices <- unlist(strsplit(colnames(rv$res_AllPairwiseComparisons$logFC), "_logFC"))
    
    tagList(
        selectInput("selectComparison","Select comparison",choices = c("None"="",.choices)),
        checkboxInput("swapVolcano", "Swap volcanoplot", value = FALSE),
        br(),
        modulePopoverUI("modulePopover_pushPVal"),
        radioButtons("AnaDiff_ChooseFilters","", choices = gFiltersListAnaDiff),
        uiOutput("AnaDiff_seuilNADelete")
    ) })



GetBackToCurrentResAnaDiff <- reactive({
  rv$res_AllPairwiseComparisons
  req(input$selectComparison)
  req(rv$res_AllPairwiseComparisons)
  
  index <- which(paste(input$selectComparison, "_logFC", sep="") == colnames(rv$res_AllPairwiseComparisons$logFC))
  rv$resAnaDiff <- list(logFC = (rv$res_AllPairwiseComparisons$logFC)[,index],
                        P_Value = (rv$res_AllPairwiseComparisons$P_Value)[,index],
                        condition1 = strsplit(input$selectComparison, "_vs_")[[1]][1],
                        condition2 = strsplit(input$selectComparison, "_vs_")[[1]][2]
  )
})


########################################################
## Perform missing values filtering
########################################################
observeEvent(input$AnaDiff_perform.filtering.MV,{
  #input$selectComparison
   
if (input$AnaDiff_ChooseFilters == gFilterNone){
  GetBackToCurrentResAnaDiff()
} else {
  condition1 = strsplit(input$selectComparison, "_vs_")[[1]][1]
  condition2 = strsplit(input$selectComparison, "_vs_")[[1]][2]
  ind <- c( which(pData(rv$current.obj)$Condition==condition1), 
            which(pData(rv$current.obj)$Condition==condition2))
  datasetToAnalyze <- rv$dataset[[input$datasets]][,ind]
  datasetToAnalyze@experimentData@other$OriginOfValues <-
    rv$dataset[[input$datasets]]@experimentData@other$OriginOfValues[ind]
  
  keepThat <- mvFilterGetIndices(datasetToAnalyze,
                                 input$AnaDiff_ChooseFilters,
                                 as.integer(input$AnaDiff_seuilNA))
        if (!is.null(keepThat))
            {
            rv$resAnaDiff$P_Value[-keepThat] <- 1
            rv$resAnaDiff
            
            updateSelectInput(session, "AnaDiff_ChooseFilters", selected = input$AnaDiff_ChooseFilters)
            updateSelectInput(session, "AnaDiff_seuilNA", selected = input$AnaDiff_seuilNA)
                    
        }
    }
})






output$tooltipInfo <- renderUI({
  #req(c(rv$current.obj,input$selectComparison))
  
  tagList(
    hr(),
    modulePopoverUI("modulePopover_volcanoTooltip"),
    selectInput("tooltipInfo",
                label = "",
                choices = colnames(fData(rv$current.obj)),
                multiple = TRUE, selectize=FALSE,width='500px', size=5)
  )
  
})



# output$diffAnalysis_Calibration_SB <- renderUI({
#     req(rv$current.obj)
#     
#     #if (is.null(calibMethod)){ calibMethod <- "Benjamini-Hochberg"}
#     
#     tagList(
#         selectInput("calibrationMethod", 
#                     "Calibration method",
#                     choices = calibMethod_Choices),
#         uiOutput("numericalValForCalibrationPlot"))
# })


observeEvent(input$calibrationMethod,{
  shinyjs::toggle("numericValCalibration", condition=input$calibrationMethod == "numeric value")
})




# output$diffAnalysis_FDR_SB <- renderUI({
#     req(rv$current.obj)
#     
#     
#         numericInput("seuilPVal", 
#                      "Define the -log10(p_value) threshold",
#                      min = 0,value = 0,step=0.1)
#      })



Get_FDR <- reactive({
  req(rv$current.obj)
  req(rv$seuilPVal)
  req(rv$seuilLogFC)
  input$numericValCalibration
  input$calibrationMethod
  req(rv$resAnaDiff)
  
  m <- NULL
  if (input$calibrationMethod == "Benjamini-Hochberg") { m <- 1}
  else if (input$calibrationMethod == "numeric value") {
    m <- as.numeric(input$numericValCalibration)} 
  else {m <- input$calibrationMethod }
  
  rv$params.anaDiff[which(rv$params.anaDiff$param=='FDR'),]$value <- diffAnaComputeFDR(rv$resAnaDiff[["logFC"]], 
                              rv$resAnaDiff[["P_Value"]],
                              rv$seuilPVal, 
                              rv$seuilLogFC, 
                              m)
  as.numeric(rv$params.anaDiff[which(rv$params.anaDiff$param=='FDR'),]$value)
})

output$showFDR <- renderUI({
  req(rv$current.obj)
  req(rv$seuilPVal)
  req(rv$seuilLogFC)
  input$numericValCalibration
  input$calibrationMethod
  req(rv$resAnaDiff)
  #input$selectComparison
  
  #if (is.null(input$selectComparison) || (input$selectComparison == "None")) {return()}
  #if (is.na(rv$seuilLogFC) || is.na(rv$seuilPVal) ){return()}
  
  
  # m <- NULL
  # if (input$calibrationMethod == "Benjamini-Hochberg") { m <- 1}
  # else if (input$calibrationMethod == "numeric value") {
  #   m <- as.numeric(input$numericValCalibration)} 
  # else {m <- input$calibrationMethod }
  # 
  # rv$fdr <- diffAnaComputeFDR(rv$resAnaDiff[["logFC"]], 
  #                             rv$resAnaDiff[["P_Value"]],
  #                             rv$seuilPVal, 
  #                             rv$seuilLogFC, 
  #                             m)
  tagList(
    if (!is.infinite(Get_FDR())){
      tags$p(style="font-size: 20;","FDR = ", round(100*Get_FDR(), digits=2)," % (p-value = ",
             signif(10^(- (input$seuilPVal)), digits=3), ")")
    }
  )
  
  
})



histPValue <- reactive({
    req(rv$current.obj)
    
    if (is.null(rv$seuilPVal) ||
        is.null(rv$seuilLogFC) ||
        is.null(input$diffAnaMethod)
    ) {return()}
    
    t <- NULL
    # Si on a deja des pVal, alors, ne pas recalculer avec ComputeWithLimma
   # if (isContainedIn(c("logFC","P_Value"),names(Biobase::fData(rv$current.obj)) ) ){
  #      t <- Biobase::fData(rv$current.obj)[,"P_Value"]
  #  } else{
        data <- RunDiffAna()
        if (is.null(data)) {return ()}
        t <- data$P_Value
  #  }
    
    
    hist(sort(1-t), breaks=80, col=grey)
    
    
})

output$histPValue <- renderPlot({
    histPValue()
})



output$numericalValForCalibrationPlot <- renderUI({
    req(input$calibrationMethod)
    
    if (input$calibrationMethod == "numeric value"){
        numericInput( "numericValCalibration","Proportion of TRUE null hypohtesis", 
                      value = 0, min=0, max=1, step=0.05)
    }
})


output$calibrationResults <- renderUI({
    req(rv$calibrationRes)
    rv$seuilLogFC
    input$diffAnaMethod
    rv$current.obj
    
    
    txt <- paste("Non-DA protein proportion = ", 
                 round(100*rv$calibrationRes$pi0, digits = 2),"%<br>",
                 "DA protein concentration = ", 
                 round(100*rv$calibrationRes$h1.concentration, digits = 2),
                 "%<br>",
                 "Uniformity underestimation = ", 
                 rv$calibrationRes$unif.under,"<br><br><hr>", sep="")
    HTML(txt)
    
})




calibrationPlot <- reactive({
    rv$seuilPVal
    rv$seuilLogFC
    input$diffAnaMethod
    rv$resAnaDiff
    req(rv$current.obj)
    
    if (is.null(rv$seuilLogFC) || is.na(rv$seuilLogFC) ||
        (length(rv$resAnaDiff$logFC) == 0)) { return()}
    if (length(which(is.na(Biobase::exprs(rv$current.obj)))) > 0) {
        return()}
    cond <- c(rv$resAnaDiff$condition1, rv$resAnaDiff$condition2)
    # ________
    
    if (is.null(input$calibrationMethod)  ) {return()}
    
    t <- NULL
    method <- NULL
    t <- rv$resAnaDiff$P_Value
    t <- t[which(abs(rv$resAnaDiff$logFC) >= rv$seuilLogFC)]
    toDelete <- which(t==1)
    if (length(toDelete) > 0){
	t <- t[-toDelete]
     }
    
    
    
    l <- NULL
    ll <- NULL
    result = tryCatch(
        {
            
            if ((input$calibrationMethod == "numeric value") 
                && !is.null(input$numericValCalibration)) {
                
                ll <-catchToList(
                    wrapperCalibrationPlot(t, 
                                           as.numeric(input$numericValCalibration)))
                rv$errMsgCalibrationPlot <- ll$warnings[grep( "Warning:", ll$warnings)]
            }
            else if (input$calibrationMethod == "Benjamini-Hochberg") {
                
                ll <-catchToList(wrapperCalibrationPlot(t, 1))
                rv$errMsgCalibrationPlot <- ll$warnings[grep( "Warning:", ll$warnings)]
            }else { 
                ll <-catchToList(wrapperCalibrationPlot(t, input$calibrationMethod))
                rv$errMsgCalibrationPlot <- ll$warnings[grep( "Warning:", ll$warnings)]
            }
            
        }
        , warning = function(w) {
            shinyjs::info(paste("Calibration plot",":",
                                conditionMessage(w), sep=" "))
        }, error = function(e) {
            shinyjs::info(paste("Calibration plot",":",
                                conditionMessage(e), sep=" "))
        }, finally = {
            #cleanup-code 
        })
    
    
})

output$calibrationPlot <- renderPlot({
    calibrationPlot()
})



output$errMsgCalibrationPlot <- renderUI({
    req(rv$errMsgCalibrationPlot)
    rv$seuilLogFC
    req(rv$current.obj)
    
    txt <- NULL
    
    for (i in 1:length(rv$errMsgCalibrationPlot)) {
        txt <- paste(txt, "errMsgCalibrationPlot: ",rv$errMsgCalibrationPlot[i], "<br>", sep="")
    }
    
    div(HTML(txt), style="color:red")
    
})


output$errMsgCalibrationPlotAll <- renderUI({
    rv$errMsgCalibrationPlotAll
    rv$seuilLogFC
    rv$current.obj
    if (is.null(rv$current.obj) ) {return()}
    if (is.null(rv$errMsgCalibrationPlotAll) ) {return()}
    
    txt <- NULL
    for (i in 1:length(rv$errMsgCalibrationPlotAll)) {
        txt <- paste(txt, "errMsgCalibrationPlotAll:", rv$errMsgCalibrationPlotAll[i], "<br>", sep="")
    }
    
    div(HTML(txt), style="color:red")
})



calibrationPlotAll <- reactive({
    rv$seuilPVal
    rv$seuilLogFC
    input$diffAnaMethod
    rv$resAnaDiff
    rv$current.obj
    if (is.null(rv$current.obj) ) {return()}
    
    if ( is.null(rv$seuilLogFC) || is.na(rv$seuilLogFC) ||
        (length(rv$resAnaDiff$logFC) == 0)) { return()}
    if (length(which(is.na(Biobase::exprs(rv$current.obj)))) > 0) {
        return()}
    cond <- c(rv$resAnaDiff$condition1, rv$resAnaDiff$condition2)
    # ________
    
    if (is.null(input$calibrationMethod)  ) {return()}
    
    t <- NULL
    method <- NULL
    t <- rv$resAnaDiff$P_Value
    t <- t[which(abs(rv$resAnaDiff$logFC) >= rv$seuilLogFC)]
    toDelete <- which(t==1)
    if (length(toDelete) > 0){
        t <- t[-toDelete]
     }
    
    l <- NULL
    result = tryCatch(
        {
            l <-catchToList(wrapperCalibrationPlot(t, "ALL")  )
            rv$errMsgCalibrationPlotAll <- l$warnings[grep( "Warning:", 
                                                            l$warnings)]
        }
        , warning = function(w) {
            shinyjs::info(paste("Calibration Plot All methods",":",
                                conditionMessage(w), sep=" "))
        }, error = function(e) {
            shinyjs::info(paste("Calibration Plot All methods",":",
                                conditionMessage(e), sep=" "))
        }, finally = {
            #cleanup-code 
        })
    
})



#--------------------------------------------------
output$calibrationPlotAll <- renderPlot({
    calibrationPlotAll()
})


# 
# observe({
#   rv$res_AllPairwiseComparisons
#   rv$seuilPVal 
#   rv$seuilLogFC
#   input$selectComparison
#   input$anaDiff_Design
#   input$diffAnaMethod
#   input$ttest_options
#   
#   #shinyjs::disable("ValidDiffAna")
#   
#   shinyjs::enable("ValidDiffAna")
# })




output$equivPVal <- renderUI ({
  req(rv$seuilPVal)
  #req(rv$current.obj)
  req(input$selectComparison)
  
  
  # if (input$selectComparison=="None"){return()}
  
  #if (length(which(is.na(Biobase::exprs(rv$current.obj)))) > 0) { return()}
  
  tags$p(paste0("(p-value = ",signif(10^(- (input$seuilPVal)), digits=3), ")"))
})


output$equivLog10 <- renderText ({
  req(rv$seuilPVal)
  
  tags$p(paste0("-log10 (p-value) = ",signif(- log10(rv$seuilPVal/100), digits=1)))
})



observeEvent(input$seuilPVal,{ rv$seuilPVal <- as.numeric(input$seuilPVal)})




GetSelectedItems <- reactive({
  #req(rv$seuilPVal)
  #req(input$selectComparison)
  req(rv$resAnaDiff$logFC)
  req(rv$resAnaDiff$P_Value )
  upItems1 <- which(-log10(rv$resAnaDiff$P_Value) >= rv$seuilPVal)
  upItems2 <- which(abs(rv$resAnaDiff$logFC) >= rv$seuilLogFC)
  selectedItems <- intersect(upItems1, upItems2)
  
  t <- data.frame(id = rownames(Biobase::exprs(rv$current.obj))[selectedItems],
                  logFC = round(rv$resAnaDiff$logFC[selectedItems], digits=rv$settings_nDigits),
                  P_Value = round(rv$resAnaDiff$P_Value[selectedItems], digits=rv$settings_nDigits))
  tmp <- as.data.frame(Biobase::fData(rv$current.obj)[selectedItems,input$tooltipInfo])
  t <- cbind(t, tmp)
  
  t
})



isContainedIn <- function(strA, strB){
    return (all(strA %in% strB))
}





observeEvent(req(input$selectComparison), {
  if (input$selectComparison != "None")
     rv$params.anaDiff[which(rv$params.anaDiff$param=='Condition1'),]$value <- strsplit(input$selectComparison, "_vs_")[[1]][1]
})


observeEvent(req(input$selectComparison), {
  if (input$selectComparison != "None")
      rv$params.anaDiff[which(rv$params.anaDiff$param=='Condition2'),]$value <- strsplit(input$selectComparison, "_vs_")[[1]][2]
})

observeEvent(req(input$selectComparison), {rv$params.anaDiff[which(rv$params.anaDiff$param=='Comparison'),]$value <- as.character(input$selectComparison)})
observeEvent(req(input$swapVolcano), {rv$params.anaDiff[which(rv$params.anaDiff$param=='swapVolcano'),]$value <- as.character(input$swapVolcano)})
 observeEvent(req(input$AnaDiff_ChooseFilters), {rv$params.anaDiff[which(rv$params.anaDiff$param=='filterType'),]$value <- as.character(input$AnaDiff_ChooseFilters)})
observeEvent(req(input$AnaDiff_seuilNA), {rv$params.anaDiff[which(rv$params.anaDiff$param=='filter_th_NA'),]$value <- as.character(input$AnaDiff_seuilNA)})
observeEvent(req(input$seuilPVal), {rv$params.anaDiff[which(rv$params.anaDiff$param=='th_pval'),]$value <- as.character(input$seuilPVal)})
observeEvent(req(input$calibrationMethod), {rv$params.anaDiff[which(rv$params.anaDiff$param=='calibMethod'),]$value  <- input$calibrationMethod})
observeEvent(req(input$numericValCalibration), {rv$params.anaDiff[which(rv$params.anaDiff$param=='numValCalibMethod'),]$value  <- as.character(input$numericValCalibration)})


