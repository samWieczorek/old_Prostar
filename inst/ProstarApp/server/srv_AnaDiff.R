callModule(moduleVolcanoplot,"volcano_Step1", reactive({input$selectComparison}),reactive({input$tooltipInfo}))
callModule(moduleVolcanoplot,"volcano_Step2",reactive({as.character(input$selectComparison)}),reactive({input$tooltipInfo}))
callModule(moduleStaticDataTable,"params_AnaDiff", table2show=reactive({convertAnaDiff2DF()}), dom='t')
#callModule(moduleStaticDataTable,"anaDiff_selectedItems", table2show=reactive({GetSelectedItems()}))
callModule(module_Not_a_numeric,"test_seuilPVal", reactive({input$seuilPVal}))


observeEvent(input$seuilPVal,{  rv$widgets$anaDiff$th_pval <- as.numeric(Get_seuilPVal())})
convertAnaDiff2DF <- reactive({
  
  df <- cbind(names(rv$widgets$anaDiff),
              as.data.frame(unlist(rv$widgets$anaDiff)))
  names(df) <- c("Parameter", "Value")
  rownames(df) <- NULL
  df
})

observeEvent(input$selectComparison, {
 # updateSelectInput(session,"selectComparison", selected="None")
  #updateCheckboxInput(session,"swapVolcano", value=FALSE )
 # updateRadioButtons(session, "AnaDiff_ChooseFilters", selected=gFilterNone)
  
  #updateSelectInput(session,"calibrationMethod", selected="pounds")
 # updateNumericInput(session, "seuilPVal", value=as.numeric(input$seuilPVal))
  
  
  if (as.character(input$selectComparison)== "None"){
    rv$resAnaDiff <- NULL
  } else {
    #if (is.null(rv$current.obj@experimentData@other$Params[["anaDiff"]])) {  ### There is no previous analysis
    index <- which(paste(as.character(input$selectComparison), "_logFC", sep="") == colnames(rv$res_AllPairwiseComparisons$logFC))
    rv$resAnaDiff <- list(logFC = (rv$res_AllPairwiseComparisons$logFC)[,index],
                          P_Value = (rv$res_AllPairwiseComparisons$P_Value)[,index],
                          condition1 = strsplit(as.character(input$selectComparison), "_vs_")[[1]][1],
                          condition2 = strsplit(as.character(input$selectComparison), "_vs_")[[1]][2]
    )
    
  }
})


observeEvent(req(input$swapVolcano), {
  if(!is.null(rv$resAnaDiff))
    rv$resAnaDiff$logFC <- - rv$resAnaDiff$logFC

})


observeEvent(req(input$AnaDiff_ChooseFilters), {
  if (input$AnaDiff_ChooseFilters==gFilterNone) {
    updateSelectInput(session, "AnaDiff_seuilNA", selected = 0)}})


observeEvent(req(input$calibrationMethod), {
  shinyjs::toggle("numericValCalibration", condition=input$calibrationMethod == "numeric value")
  })


output$anaDiffPanel <- renderUI({
  req(rv$current.obj)
  isolate({
    
  NA.count<- length(which(is.na(Biobase::exprs(rv$current.obj))))
  dataset.name <- last(names(rv$dataset))
  prev.dataset.name <- paste0('prev.HypothesisTest.',rv$current.obj@experimentData@other$typeOfData)
  if (NA.count > 0){
    tags$p("Your dataset contains missing values. Before using the differential analysis, you must filter/impute them")
  } else if (rv$current.obj@experimentData@other$Params[[dataset.name]][['HypothesisTest']]$design=="None" &&
             rv$current.obj@experimentData@other$Params[[prev.dataset.name]][['HypothesisTest']]$design=="None") {
    tags$p("The statistical test has not been performed so the differential analysis cannot be done.")
    } else {
  tabsetPanel(
    id = "xxx",
    tabPanel("1 - Pairwise comparison",value = "DiffAnalysis_PairewiseComparison",
             uiOutput("diffAna_pairwiseComp")),
    tabPanel("2 - p-value calibration", value = "DiffAnalysis_Calibrate",
             uiOutput("diffAna_pvalCalib")),
    tabPanel("3 - FDR",value = "DiffAnalysis_viewFDR",
             uiOutput("diffAna_fdrCompute")),
    tabPanel("4 - Summary",value = "DiffAnalysis_ValidateAndSave",
             uiOutput("diffAna_Summary"))
  ) # end tabsetPanel
    }
  
  })
})

observeEvent(input$toggleSidebar, {shinyjs::toggle(id = "sidebar_diffAna_1")})

output$diffAna_pairwiseComp <- renderUI({
  #req(rv$current.obj)
  isolate({
    tagList(
   # actionLink("toggleSidebar", "Hide/show options"),
  sidebarCustom(),
           splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
      wellPanel(
               id = "sidebar_DiffAna2",
               height = "100%"
               #,uiOutput("newComparisonUI")
               ,uiOutput("diffAnalysis_PairwiseComp_SB")
               ,actionButton("AnaDiff_perform.filtering.MV", "Perform", class = actionBtnClass)
                ),
   
   moduleVolcanoplotUI("volcano_Step1") %>% withSpinner(type=spinnerType)

           #)
  )
  )
  })

})


output$diffAna_pvalCalib <- renderUI({
  req(as.character(input$selectComparison))
  
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
  if(as.character(input$selectComparison) == "None"){return(NULL)}
     
  isolate({
  sidebarCustom()
  splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
              wellPanel(
                id = "sidebar_DiffAna3", height = "100%",
                textInput("seuilPVal",  "Define the -log10(p_value) threshold",
                             value=rv$widgets$anaDiff$th_pval, width='100px'),
                module_Not_a_numericUI("test_seuilPVal"),
                uiOutput("tooltipInfo"),
                br(),
                checkboxInput("showpvalTable","Show p-value table", value=FALSE),
                radioButtons("downloadAnaDiff", "Download as Excel file", choices=c("All data"="All", "only DA"="onlyDA" )),
                downloadButton('downloadSelectedItems', 'Download', class=actionBtnClass)
                
              ),
             tagList(
               htmlOutput("showFDR"),
               moduleVolcanoplotUI("volcano_Step2") %>% withSpinner(type=spinnerType),
                #hidden(div(id = 'toto',
                #.           moduleStaticDataTableUI("anaDiff_selectedItems")
                #)
                hidden(DTOutput("anaDiff_selectedItems"))
              )
  )

  })
})     



output$anaDiff_selectedItems <- renderDT({
  
  DT::datatable(GetSelectedItems(),
                escape = FALSE,
                rownames=TRUE,
                options = list(initComplete = initComplete(),
                               dom = 'Bfrtip',
                               server = TRUE,
                               columnDefs = list(list(width='200px',targets= "_all")),
                               ordering = TRUE)
  ) %>%
    formatStyle(
      'isDifferential',
      target = 'row',
      backgroundColor = styleEqual(c(0, 1), c("white","orange"))
    )
})


output$downloadSelectedItems <- downloadHandler(
  #input$chooseDatasetToExportToMSnset,
  filename = paste0('diffanalysis_', input$datasets,'.xlsx'),
  content = function(file) {
    print(paste0("file to write=", file))
    DA_Style <- openxlsx::createStyle(fgFill = "orange")
    
    wb <- openxlsx::createWorkbook() # Create wb in R
    openxlsx::addWorksheet(wb,sheetName="DA result") #create sheet
    openxlsx::writeData(wb,sheet = 1, GetSelectedItems(), colNames = TRUE)
    ll.DA.row <- which(GetSelectedItems()[,'isDifferential']==1)
    ll.DA.col <- rep(which(colnames(GetSelectedItems()) == 'isDifferential'), length(ll.DA.row))
    
     openxlsx::addStyle(wb, sheet=1, cols=ll.DA.col,
                        rows = 1+ ll.DA.row, style = DA_Style)
    
     openxlsx::saveWorkbook(wb, file = file, overwrite = TRUE)
     
  })




observeEvent(input$showpvalTable, {
  print("show : anaDiff_selectedItems")
  shinyjs::toggle(id = "anaDiff_selectedItems", condition=isTRUE(input$showpvalTable))})

    
    
output$diffAna_Summary <- renderUI({     

  if (as.character(input$selectComparison) == "None"){return(NULL)}
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


output$AnaDiff_seuilNADelete <- renderUI({ 
  as.character(input$AnaDiff_ChooseFilters)
    req(rv$current.obj)
    if (as.character(input$AnaDiff_ChooseFilters)==gFilterNone) {return(NULL)   }
    
    choix <- getListNbValuesInLines(rv$current.obj, type=as.character(input$AnaDiff_ChooseFilters))
   
    selectInput("AnaDiff_seuilNA", 
                "Keep lines with at least x intensity values", 
                choices = choix,
                selected=rv$widgets$anaDiff$filter_th_NA)
    
})



#####
####  SELECT AND LOAD ONE PARIWISE COMPARISON
####





output$diffAnalysis_PairwiseComp_SB <- renderUI({
    #req(rv$res_AllPairwiseComparisons)
    
    .choices <- unlist(strsplit(colnames(rv$res_AllPairwiseComparisons$logFC), "_logFC"))
    
    tagList(
        selectInput("selectComparison","Select comparison",
                    choices = c("None"="",.choices),
                    selected = rv$widgets$anaDiff$Comparison,
                    width='auto'),
        checkboxInput("swapVolcano", "Swap volcanoplot", value = FALSE),
        br(),
        modulePopoverUI("modulePopover_pushPVal"),
        radioButtons("AnaDiff_ChooseFilters","", choices = gFiltersListAnaDiff),
        uiOutput("AnaDiff_seuilNADelete")
    ) })



GetBackToCurrentResAnaDiff <- reactive({
  req(rv$res_AllPairwiseComparisons)
  
  index <- which(paste(as.character(input$selectComparison), "_logFC", sep="") == colnames(rv$res_AllPairwiseComparisons$logFC))
  rv$resAnaDiff <- list(logFC = (rv$res_AllPairwiseComparisons$logFC)[,index],
                        P_Value = (rv$res_AllPairwiseComparisons$P_Value)[,index],
                        condition1 = strsplit(as.character(input$selectComparison), "_vs_")[[1]][1],
                        condition2 = strsplit(as.character(input$selectComparison), "_vs_")[[1]][2]
  )
})


########################################################
## Perform missing values filtering
########################################################
observeEvent(input$AnaDiff_perform.filtering.MV,{
  #input$selectComparison
   
if (as.character(input$AnaDiff_ChooseFilters) == gFilterNone){
  GetBackToCurrentResAnaDiff()
} else {
  condition1 = strsplit(as.character(input$selectComparison), "_vs_")[[1]][1]
  condition2 = strsplit(as.character(input$selectComparison), "_vs_")[[1]][2]
  ind <- c( which(pData(rv$current.obj)$Condition==condition1), 
            which(pData(rv$current.obj)$Condition==condition2))
  datasetToAnalyze <- rv$dataset[[input$datasets]][,ind]
  datasetToAnalyze@experimentData@other$OriginOfValues <-
    rv$dataset[[input$datasets]]@experimentData@other$OriginOfValues[ind]
  
  keepThat <- mvFilterGetIndices(datasetToAnalyze,
                                 as.character(input$AnaDiff_ChooseFilters),
                                 as.integer(input$AnaDiff_seuilNA))
        if (!is.null(keepThat))
            {
            rv$resAnaDiff$P_Value[-keepThat] <- 1
            rv$resAnaDiff
            
            #updateSelectInput(session, "AnaDiff_ChooseFilters", selected = as.character(input$AnaDiff_ChooseFilters))
            #updateSelectInput(session, "AnaDiff_seuilNA", selected = input$AnaDiff_seuilNA)
                    
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



not_a_numeric <- function(input) {
  if (is.na(as.numeric(input))) {
    "Please input a number"
  }  else {
    NULL
  }
}


Get_seuilPVal <- reactive({
   shiny::validate(
    need(!is.na(as.numeric(input$seuilPVal)), "")
  )
  as.numeric(input$seuilPVal)
})


Get_FDR <- reactive({
  req(rv$current.obj)
  input$numericValCalibration
  input$calibrationMethod
  req(rv$resAnaDiff)
  
  m <- NULL
  if (input$calibrationMethod == "Benjamini-Hochberg") { m <- 1}
  else if (input$calibrationMethod == "numeric value") {
    m <- as.numeric(input$numericValCalibration)} 
  else {m <- input$calibrationMethod }
  
  rv$widgets$anaDiff$FDR <- diffAnaComputeFDR(rv$resAnaDiff[["logFC"]], 
                              rv$resAnaDiff[["P_Value"]],
                              as.numeric(Get_seuilPVal()), 
                              rv$widgets$hypothesisTest$th_logFC, 
                              m)
  as.numeric(rv$widgets$anaDiff$FDR)
})

output$showFDR <- renderUI({
  req(rv$current.obj)
  
  tagList(
    if (!is.infinite(Get_FDR())){
      tags$p(style="font-size: 25px;","FDR = ", round(100*Get_FDR(), digits=2)," % (p-value = ",
             signif(10^(- (as.numeric(Get_seuilPVal()))), digits=3), ")")
    } else {
      tags$p(style="font-size: 25px;","FDR = NA") 
    }
  )
  
  
})



histPValue <- reactive({
    req(rv$current.obj)
    t <- NULL
     data <- RunDiffAna()
        if (is.null(data)) {return ()}
        t <- data$P_Value
  hist(sort(1-t), breaks=80, col=grey)
    
    
})

output$histPValue <- renderPlot({
    histPValue()
})



output$numericalValForCalibrationPlot <- renderUI({
    
    if (input$calibrationMethod == "numeric value"){
        numericInput( "numericValCalibration","Proportion of TRUE null hypohtesis", 
                      value = 0, min=0, max=1, step=0.05)
    }
})


output$calibrationResults <- renderUI({
    req(rv$calibrationRes)
  rv$widgets$hypothesisTest$th_logFC
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
 
  rv$widgets$hypothesisTest$th_logFC
    rv$resAnaDiff
    req(rv$current.obj)
    
    if (is.null(rv$widgets$hypothesisTest$th_logFC) || is.na(rv$widgets$hypothesisTest$th_logFC) ||
        (length(rv$resAnaDiff$logFC) == 0)) { return()}
    if (length(which(is.na(Biobase::exprs(rv$current.obj)))) > 0) {
        return()}
    cond <- c(rv$resAnaDiff$condition1, rv$resAnaDiff$condition2)
    # ________
    
    
    t <- NULL
    method <- NULL
    t <- rv$resAnaDiff$P_Value
    t <- t[which(abs(rv$resAnaDiff$logFC) >= rv$widgets$hypothesisTest$th_logFC)]
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
  rv$widgets$hypothesisTest$th_logFC
    req(rv$current.obj)
    
    txt <- NULL
    
    for (i in 1:length(rv$errMsgCalibrationPlot)) {
        txt <- paste(txt, "errMsgCalibrationPlot: ",rv$errMsgCalibrationPlot[i], "<br>", sep="")
    }
    
    div(HTML(txt), style="color:red")
    
})


output$errMsgCalibrationPlotAll <- renderUI({
    rv$errMsgCalibrationPlotAll
  rv$widgets$hypothesisTest$th_logFC
    req(rv$current.obj)
    if (is.null(rv$errMsgCalibrationPlotAll) ) {return()}
    
    txt <- NULL
    for (i in 1:length(rv$errMsgCalibrationPlotAll)) {
        txt <- paste(txt, "errMsgCalibrationPlotAll:", rv$errMsgCalibrationPlotAll[i], "<br>", sep="")
    }
    
    div(HTML(txt), style="color:red")
})



calibrationPlotAll <- reactive({
    
  rv$resAnaDiff
    req(rv$current.obj)
    
    if (is.na(rv$widgets$hypothesisTest$th_logFC) ||
        (length(rv$resAnaDiff$logFC) == 0)) { return()}
    if (length(which(is.na(Biobase::exprs(rv$current.obj)))) > 0) {
        return()}
    cond <- c(rv$resAnaDiff$condition1, rv$resAnaDiff$condition2)
    # ________
    
    t <- NULL
    method <- NULL
    t <- rv$resAnaDiff$P_Value
    t <- t[which(abs(rv$resAnaDiff$logFC) >= rv$widgets$hypothesisTest$th_logFC)]
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



output$equivPVal <- renderUI ({
  req(input$seuilPVal)
  
  tags$p(paste0("(p-value = ",signif(10^(- (as.numeric(Get_seuilPVal()))), digits=3), ")"))
})


output$equivLog10 <- renderText ({
  req(input$seuilPVal)
  
  tags$p(paste0("-log10 (p-value) = ",signif(- log10(as.numeric(Get_seuilPVal())/100), digits=1)))
})




GetSelectedItems <- reactive({
  req(rv$resAnaDiff$logFC)
  req(rv$resAnaDiff$P_Value )
  input$downloadAnaDiff
  
  t <- NULL
  upItems1 <- which(-log10(rv$resAnaDiff$P_Value) >=as.numeric(Get_seuilPVal()))
  upItems2 <- which(abs(rv$resAnaDiff$logFC) >= rv$widgets$hypothesisTest$th_logFC)
  
  if (input$downloadAnaDiff == "All"){
    selectedItems <- 1:nrow(rv$current.obj)
    significant <- rep(0, nrow(rv$current.obj))
    significant[intersect(upItems1, upItems2)] <- 1
  } else {
    selectedItems <- intersect(upItems1, upItems2)
    significant <- rep(1, length(selectedItems))
  }
  
  
  t <- data.frame(id = rownames(Biobase::exprs(rv$current.obj))[selectedItems],
                  logFC = round(rv$resAnaDiff$logFC[selectedItems], digits=rv$settings_nDigits),
                  P_Value = round(rv$resAnaDiff$P_Value[selectedItems], digits=rv$settings_nDigits),
                  isDifferential = significant)
  tmp <- as.data.frame(Biobase::fData(rv$current.obj)[selectedItems,input$tooltipInfo])
  names(tmp) <-input$tooltipInfo
  t <- cbind(t, tmp)
  
  t
})



isContainedIn <- function(strA, strB){
    return (all(strA %in% strB))
}






