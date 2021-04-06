callModule(moduleVolcanoplot,"volcano_Step1", 
           data = reactive({rv$resAnaDiff}),
           comp = reactive({as.character(rv$widgets$anaDiff$Comparison)}),
           tooltip = reactive({ rv$widgets$anaDiff$tooltipInfo}),
           isSwaped = reactive({rv$widgets$anaDiff$swapVolcano}))

callModule(moduleVolcanoplot,"volcano_Step2",
           data = reactive({rv$resAnaDiff}),
           comp = reactive({as.character(rv$widgets$anaDiff$Comparison)}),
           tooltip = reactive({rv$widgets$anaDiff$tooltipInfo}),
           isSwaped = reactive({rv$widgets$anaDiff$swapVolcano}))

callModule(moduleStaticDataTable,"params_AnaDiff", table2show=reactive({convertAnaDiff2DF()}), dom='t',
           filename='AnaDiffParams')
#callModule(moduleStaticDataTable,"anaDiff_selectedItems", table2show=reactive({GetSelectedItems()}))

callModule(module_Not_a_numeric,"test_seuilPVal", reactive({rv$widgets$anaDiff$th_pval}))


callModule(moduleProcess, "moduleProcess_AnaDiff", 
           isDone = reactive({rvModProcess$moduleAnaDiffDone}), 
           pages = reactive({rvModProcess$moduleAnaDiff}),
           rstFunc = resetModuleAnaDiff,
           forceReset = reactive({rvModProcess$moduleAnaDiffForceReset}))




######
resetModuleAnaDiff <- reactive({  
  if (rv$widgets$anaDiff$swapVolcano == TRUE){
    rv$resAnaDiff$logFC <- -rv$resAnaDiff$logFC
  }

  #browser()
  ## update widgets values (reactive values)
  resetModuleProcess("AnaDiff")
  
  rv$nbTotalAnaDiff = NULL
  rv$nbSelectedAnaDiff = NULL
  rv$nbSelectedTotal_Step3 = NULL
  rv$nbSelected_Step3 = NULL  
  rv$conditions <- list(cond1 = NULL, cond2 = NULL)
  rv$calibrationRes <- NULL
  rv$errMsgcalibrationPlot <- NULL
  rv$errMsgcalibrationPlotALL <- NULL
  rv$pi0 <- NULL
  
  rv$widgets$anaDiff$Comparison = "None"
  rv$widgets$anaDiff$Condition1 = ""
  rv$widgets$anaDiff$Condition2 = ""
  rv$widgets$anaDiff$swapVolcano = FALSE
   rv$widgets$anaDiff$val_vs_percent = "Value"
  rv$widgets$anaDiff$ChooseFilters = "None"
  rv$widgets$anaDiff$seuilNA_percent = 0
  rv$widgets$anaDiff$seuilNA = 0
  rv$widgets$anaDiff$filter_th_NA = 0
  rv$widgets$anaDiff$calibMethod = 'None'
  rv$widgets$anaDiff$numValCalibMethod = 0
  rv$widgets$anaDiff$th_pval = 0
  rv$widgets$anaDiff$FDR = 0
  rv$widgets$anaDiff$NbSelected = 0
  rv$widgets$anaDiff$nBinsHistpval = 80
  rv$widgets$anaDiff$downloadAnaDiff = "All"
  rv$widgets$anaDiff$tooltipInfo=NULL
  
  
  rv$widgets$anaDiff[sapply(rv$widgets$anaDiff, is.null)] <- NA
  rvModProcess$moduleAnaDiffDone = rep(FALSE, 4)
  
  rv_anaDiff$filename = NULL
  ##update dataset to put the previous one
  #rv$current.obj <- rv$dataset[[last(names(rv$dataset))]] 
  #rv$resAnaDiff <- NULL
  
})
#####

rv_anaDiff <- reactiveValues(
  filename = NULL
)




###
### ------------------- SCREEN 1 ------------------------------
###
output$screenAnaDiff1 <- renderUI({
  isolate({
    tagList(
      tags$div(
        tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px",
                  selectInput("selectComparison","Select a comparison",
                              choices = c("None"="None",GetPairwiseCompChoice()),
                              selected = rv$widgets$anaDiff$Comparison,
                              width='200px'),
                  #  checkboxInput("swapVolcano", "Swap logFC in volcanoplot", value = rv$widgets$anaDiff$swapVolcano),
                  hidden(radioButtons("swapVolcano", "Swap volcano", choices=c("Original dataset"=FALSE,
                                                                               "Swaped dataset"=TRUE),
                                      selected=rv$widgets$anaDiff$swapVolcano))
        ),
        tags$div( style="display:inline-block; vertical-align: top; padding-right: 0px",
                  hidden(div(id='trtr',
                             modulePopoverUI("modulePopover_pushPVal"),
                             radioButtons("AnaDiff_ChooseFilters", "", 
                                          choices = gFiltersListAnaDiff, 
                                          selected = rv$widgets$anaDiff$ChooseFilters,
                                          width='200px')
                  )),
                  #uiOutput("pushPValUI"),
                  div( style="display:inline-block; vertical-align: middle;  padding-right: 40px;",
                       uiOutput("AnaDiff_seuilNADelete")
                       ),
                  div( style="display:inline-block; vertical-align: middle;",
                       hidden(actionButton("AnaDiff_perform.filtering.MV", 
                                           "Push p-value", 
                                           class = actionBtnClass))
                       ),
                  
        )
      ),
      tags$hr(),
      tags$div(
        tags$div( style="display:inline-block; vertical-align: top; padding-right: 60px",
                  moduleVolcanoplotUI("volcano_Step1")),
        tags$div( style="display:inline-block; vertical-align: top;",
                  tagList(
                    
                    br(),
                    uiOutput("volcanoTooltip_UI"))
        )
      )
    )
    
  })
})


output$AnaDiff_seuilNADelete <- renderUI({ 
  req(rv$widgets$anaDiff$ChooseFilters)
  req(rv$widgets$anaDiff$Comparison != 'None')
  #if (rv$widgets$anaDiff$Comparison == "None"){return(NULL)}
  
  if (as.character(rv$widgets$anaDiff$ChooseFilters)==gFilterNone){
    return(NULL)   }
  
 # isolate({  
    tagList(
     div(
       div( style="display:inline-block; vertical-align: middle;",
            radioButtons('AnaDiff_val_vs_percent', '#/% of values to keep', 
                   choices = c('Value'='Value', 'Percentage'='Percentage'),
                   selected = rv$widgets$anaDiff$val_vs_percent
      )
      ),
      div( style="display:inline-block; vertical-align: middle;",
           uiOutput('AnaDiff_keepVal_ui'),
           uiOutput('AnaDiff_keepVal_percent_ui')
           )
      ),
      uiOutput('AnaDiff_keep_helptext')
    )
 # })
})

Get_Dataset_to_Analyze <- reactive({
  rv$widgets$anaDiff$Comparison
  rv$current.obj
  
  datasetToAnalyze <- NULL
  
  if (length(grep("all-", rv$widgets$anaDiff$Comparison)) == 1){
    condition1 = strsplit(as.character(rv$widgets$anaDiff$Comparison), "_vs_")[[1]][1]
    ind_virtual_cond2 <- which(pData(rv$current.obj)$Condition != condition1)
    datasetToAnalyze <- rv$current.obj
    Biobase::pData(datasetToAnalyze)$Condition[ind_virtual_cond2] <- 'virtual_cond_2'
  }
  else {
    condition1 = strsplit(as.character(rv$widgets$anaDiff$Comparison), "_vs_")[[1]][1]
    condition2 = strsplit(as.character(rv$widgets$anaDiff$Comparison), "_vs_")[[1]][2]
    ind <- c( which(pData(rv$current.obj)$Condition == condition1), 
              which(pData(rv$current.obj)$Condition == condition2))
    
    datasetToAnalyze <- rv$current.obj[,ind]
    datasetToAnalyze@experimentData@other$names_metacell <-
    rv$current.obj@experimentData@other$names_metacell[ind]
  }
  
  datasetToAnalyze
})

output$AnaDiff_keepVal_ui <- renderUI({
  req(rv$widgets$anaDiff$val_vs_percent)
  if (rv$widgets$anaDiff$val_vs_percent != 'Value') {return(NULL)}
  if (rv$widgets$anaDiff$ChooseFilters %in% c('None', 'Emptylines')) {return(NULL)}
  
    
  choix <- getListNbValuesInLines(Get_Dataset_to_Analyze(), 
                                  type = as.character(rv$widgets$anaDiff$ChooseFilters))
  
  tagList(
    modulePopoverUI("modulePopover_anaDiff_keepVal"),
    selectInput("AnaDiff_seuilNA", NULL,
                choices =  choix,
                selected = rv$widgets$anaDiff$seuilNA,
                width='150px')
  )
})

output$AnaDiff_keepVal_percent_ui <- renderUI({
  req(rv$widgets$anaDiff$val_vs_percent=='Percentage')
  #if (rv$widgets$anaDiff$val_vs_percent != 'Percentage') {return(NULL)}
  
  tagList(
    modulePopoverUI("modulePopover_anaDiff_keepVal_percent"),
    numericInput("AnaDiff_seuilNA_percent", NULL,
                 min = 0,
                 max = 100,
                 value = rv$widgets$anaDiff$seuilNA_percent,
                 width='150px')
  )
})



output$AnaDiff_keep_helptext <- renderUI({
  rv$widgets$anaDiff$ChooseFilters
  txt <- NULL
  switch(rv$widgets$anaDiff$ChooseFilters,
         None = txt <-"All lines will be kept",
         WholeMatrix = {
           if (rv$widgets$anaDiff$val_vs_percent == 'Value')
             txt <- paste0("Only the lines (across all conditions) which contain at least ",
                           rv$widgets$anaDiff$seuilNA, 
                           " non-missing value are kept.")
           else if (rv$widgets$anaDiff$val_vs_percent == 'Percentage')
             txt <- paste0("The lines (across all conditions) which contain at least ",
                           rv$widgets$anaDiff$seuilNA_percent, 
                           "% of non-missing value are kept.")
         },
         AtLeastOneCond = {
           if (rv$widgets$anaDiff$val_vs_percent == 'Value')
             txt <- paste0("The lines which contain at least ",
                           rv$widgets$anaDiff$seuilNA, 
                           " non-missing value in, at least one condition, are kept.")
           else if (rv$widgets$anaDiff$val_vs_percent == 'Percentage')
             txt <- paste0("The lines which contain at least ",
                           rv$widgets$anaDiff$seuilNA_percent, 
                           "% of non-missing value in, at least one condition, are kept.")
         },
         AllCond = {
           if (rv$widgets$anaDiff$val_vs_percent == 'Value')
             txt <- paste0("The lines which contain at least ",
                           rv$widgets$anaDiff$seuilNA, 
                           " non-missing value in each condition are kept.")
           else if (rv$widgets$anaDiff$val_vs_percent == 'Percentage')
             txt <- paste0("The lines which contain at least ",
                           rv$widgets$anaDiff$seuilNA_percent, 
                           "% of non-missing value in each condition are kept.")
         }
  )
  tagList(
    tags$p(txt)
  )
})



observeEvent(input$AnaDiff_val_vs_percent, {
  rv$widgets$anaDiff$val_vs_percent <- input$AnaDiff_val_vs_percent
  })

observeEvent(input$AnaDiff_seuilNA_percent, ignoreNULL = TRUE, ignoreInit = TRUE, {
  rv$widgets$anaDiff$seuilNA_percent <- input$AnaDiff_seuilNA_percent
})

observeEvent(input$AnaDiff_ChooseFilters, {
  rv$widgets$anaDiff$ChooseFilters <-input$AnaDiff_ChooseFilters
  shinyjs::toggle("AnaDiff_perform.filtering.MV", 
                  condition=rv$widgets$anaDiff$ChooseFilters != "None")
})

# observe({
#   rv$widgets$anaDiff$ChooseFilters
#   shinyjs::toggle("AnaDiff_perform.filtering.MV", 
#                   condition=rv$widgets$anaDiff$ChooseFilters != "None")
# })


observeEvent(input$AnaDiff_seuilNA, {
  rv$widgets$anaDiff$seuilNA <-input$AnaDiff_seuilNA
  if (rv$widgets$anaDiff$seuilNA==gFilterNone) {
    updateSelectInput(session, "AnaDiff_seuilNA", selected = rv$widgets$anaDiff$seuilNA)}
})



########################################################
## Perform missing values filtering
########################################################
observeEvent(input$AnaDiff_perform.filtering.MV,{
  rv$widgets$anaDiff$Comparison
  rv$widgets$anaDiff$ChooseFilters
  rv$widgets$anaDiff$seuilNA
  rv$widgets$anaDiff$seuilNA_percent
  rv$widgets$anaDiff$val_vs_percent
  
  
  
  #isolate({
  if (as.character(rv$widgets$anaDiff$ChooseFilters) == gFilterNone){
    GetBackToCurrentResAnaDiff()
  } else {
   th <- NULL
    if (rv$widgets$anaDiff$val_vs_percent == 'Percentage')
      th <- as.numeric(rv$widgets$anaDiff$seuilNA_percent)/100
    else
      th <-  as.integer(rv$widgets$anaDiff$seuilNA)
    
    browser()
    obj2analyse <- Get_Dataset_to_Analyze()
    #----------------------------------------
    indices <- DAPAR::GetIndices_MetacellFiltering(obj = obj2analyse,
                                                   level = obj2analyse@experimentData@other$typeOfData,
                                                   pattern = 'imputed',
                                                   type = rv$widgets$anaDiff$ChooseFilters,
                                                   percent = rv$widgets$anaDiff$val_vs_percent == 'Percentage',
                                                   op = '==',
                                                   th = th
                                                   )
    
    
    #--------------------------------
    if (!is.null(indices) && length(indices) < nrow(obj2analyse)){
      rv$resAnaDiff$P_Value[-indices] <- 1
    }
  }
  #})
})



observeEvent(req(input$swapVolcano),{
  req(rv$resAnaDiff$logFC)
  #isolate({
  rv$widgets$anaDiff$swapVolcano <- input$swapVolcano
  rv$resAnaDiff$logFC <- -rv$resAnaDiff$logFC
  #})
})

observeEvent(input$selectComparison,ignoreInit = TRUE,{ 
  rv$widgets$anaDiff$Comparison <- input$selectComparison
  UpdateCompList()
  
  req(rv$widgets$anaDiff$Comparison)
  cond1 = rv$widgets$anaDiff$Condition1
  cond2 = rv$widgets$anaDiff$Condition2
  
  if (isTRUE(rv$widgets$anaDiff$swapVolcano)) {
    rv_anaDiff$filename = paste0('anaDiff_', cond2,'_vs_', cond1, '.xlsx')
  } else {
    rv_anaDiff$filename = paste0('anaDiff_', cond1,'_vs_', cond2, '.xlsx')
  }
  
})


UpdateCompList <- reactive({
  rv$widgets$anaDiff$Comparison 
  isolate({
    if (rv$widgets$anaDiff$Comparison== "None"){
      rv$resAnaDiff <- NULL
      return(NULL)
    } else {
      index <- which(paste(as.character(rv$widgets$anaDiff$Comparison), "_logFC", sep="") == colnames(rv$res_AllPairwiseComparisons$logFC))
      print("On met a jour la liste rv$resAnaDiff")
      rv$resAnaDiff <- list(logFC = (rv$res_AllPairwiseComparisons$logFC)[,index],
                            P_Value = (rv$res_AllPairwiseComparisons$P_Value)[,index],
                            condition1 = strsplit(as.character(rv$widgets$anaDiff$Comparison), "_vs_")[[1]][1],
                            condition2 = strsplit(as.character(rv$widgets$anaDiff$Comparison), "_vs_")[[1]][2]
      )
      rv$widgets$anaDiff$Condition1 <- strsplit(as.character(rv$widgets$anaDiff$Comparison), "_vs_")[[1]][1]
      rv$widgets$anaDiff$Condition2 <- strsplit(as.character(rv$widgets$anaDiff$Comparison), "_vs_")[[1]][2]
      rvModProcess$moduleAnaDiffDone[1] <- TRUE
    }
  })
})


##--------------------------------------------------------
##---------------------------------------------------------


output$volcanoTooltip_UI <- renderUI({
  req(rv$widgets$anaDiff$Comparison)
  if (rv$widgets$anaDiff$Comparison == "None"){return(NULL)}
  
  isolate({
    tagList(
      modulePopoverUI("modulePopover_volcanoTooltip"),
      selectInput("tooltipInfo",
                  label = NULL,
                  choices = colnames(fData(rv$current.obj)),
                  selected = rv$widgets$anaDiff$tooltipInfo,
                  multiple = TRUE, selectize=FALSE,width='200px', size=5),
      actionButton("validTooltipInfo", "Valid tooltip choices", class = actionBtnClass)
    )
  })
})



GetPairwiseCompChoice <- reactive({
  print(str(rv$res_AllPairwiseComparisons))
  req(rv$res_AllPairwiseComparisons$logFC)
  ll <- unlist(strsplit(colnames(rv$res_AllPairwiseComparisons$logFC), "_logFC"))
  ll
})




observe({
  req(rv$widgets$anaDiff$Comparison)
  shinyjs::toggle('trtr', condition=rv$widgets$anaDiff$Comparison != "None")
  shinyjs::toggle('swapVolcano', condition=rv$widgets$anaDiff$Comparison != "None")
})


# output$pushPValUI <- renderUI({
#   req(rv$widgets$anaDiff$Comparison)
#   if (rv$widgets$anaDiff$Comparison == "None"){return(NULL)}
#   isolate({
#     tagList(
#       modulePopoverUI("modulePopover_pushPVal"),
#       radioButtons("AnaDiff_ChooseFilters",NULL, choices = gFiltersListAnaDiff, selected = rv$widgets$anaDiff$filterType)
#     )
#   })
# })

callModule(modulePopover,"modulePopover_volcanoTooltip", 
           data = reactive(list(title = "Tooltip", 
                                content="Infos to be displayed in the tooltip of volcanoplot")))

callModule(modulePopover,"modulePopover_pushPVal", 
           data = reactive(list(title="P-Value push",
           content= "This functionality is useful in case of multiple pairwise omparisons (more than 2 conditions): At the filtering step, a given analyte X (either peptide or protein) may have been kept because it contains very few missing values in a given condition (say Cond. A), even though it contains (too) many of them in all other conditions (say Cond B and C only contains “MEC” type missing values). Thanks to the imputation step, these missing values are no longer an issue for the differential analysis, at least from the computational viewpoint. However, statistically speaking, when performing B vs C, the test will rely on too many imputed missing values to derive a meaningful p-value: It may be wiser to consider analyte X as non-differentially abundant, regardless the test result (and thus, to push its p-value to 1). This is just the role of the “P-value push” parameter. It makes it possible to introduce a new filtering step that only applies to each pairwise comparison, and which assigns a p-value of 1 to analytes that, for the considered comparison are assumed meaningless due to too many missing values (before imputation).")))

callModule(modulePopover,"modulePopover_keepLines", 
           data = reactive(list(title="n values",
                                content= "Keep the lines which have at least n intensity values.")))


GetBackToCurrentResAnaDiff <- reactive({
  req(rv$res_AllPairwiseComparisons)
  
  index <- which(paste(as.character(rv$widgets$anaDiff$Comparison), "_logFC", sep="") == colnames(rv$res_AllPairwiseComparisons$logFC))
  rv$resAnaDiff <- list(logFC = (rv$res_AllPairwiseComparisons$logFC)[,index],
                        P_Value = (rv$res_AllPairwiseComparisons$P_Value)[,index],
                        condition1 = strsplit(as.character(rv$widgets$anaDiff$Comparison), "_vs_")[[1]][1],
                        condition2 = strsplit(as.character(rv$widgets$anaDiff$Comparison), "_vs_")[[1]][2]
  )
  rv$resAnaDiff
})





not_a_numeric <- function(input) {
  if (is.na(as.numeric(input))) {
    "Please input a number"
  }  else {
    NULL
  }
}

# 
# Get_seuilPVal <- reactive({
#    shiny::validate(
#     need(!is.na(rv$widgets$anaDiff$th_pval), "")
#   )
#   rv$widgets$anaDiff$th_pval
# })



###
### ------------------- SCREEN 2 ------------------------------
###



output$screenAnaDiff2 <- renderUI({
  req(as.character(rv$widgets$anaDiff$Comparison))
  
  tagList(
    tags$div(
      tags$div( style="display:inline-block; vertical-align: middle; padding-right: 40px;",
                selectInput("calibrationMethod","Calibration method",
                            choices = c('None' = 'None', calibMethod_Choices),
                            selected = rv$widgets$anaDiff$calibMethod, width='200px')
      ),
      tags$div( style="display:inline-block; vertical-align: middle;",
                uiOutput('numericalValForCalibrationPlot')
                #hidden(numericInput( "numericValCalibration","Proportion of TRUE null hypohtesis", 
                #                     value = rv$widgets$anaDiff$numValCalibMethod, 
                #                     min=0, max=1, step=0.05, width='200px')
                #      )
      ),
      tags$div( style="display:inline-block; vertical-align: middle;padding-right: 40px;",
                uiOutput('nBins_ui')),
      tags$div( style="display:inline-block; vertical-align: middle;",
                p(tags$strong(paste0("value of pi0: ", round(as.numeric(rv$pi0), digits=2))))
      )
      
    ),
    tags$hr(),
    
    fluidRow(
      column(width=6,fluidRow(style = "height:800px;",imageOutput("calibrationPlotAll", height='800px'))),
      column(width=6,fluidRow(style = "height:400px;",imageOutput("calibrationPlot", height='400px')),
             fluidRow(style = "height:400px;",highchartOutput("histPValue"))
      )
    )
  )
  
})    





observeEvent(input$nBinsHistpval,{ 
  rv$widgets$anaDiff$nBinsHistpval <- as.numeric(input$nBinsHistpval)
  })


observeEvent(input$calibrationMethod,{  
  rv$widgets$anaDiff$calibMethod <- input$calibrationMethod
  shinyjs::toggle("numericValCalibration", condition=rv$widgets$anaDiff$calibMethod == "numeric value")
})


observeEvent(input$numericValCalibration,{  
  rv$widgets$anaDiff$numValCalibMethod <- input$numericValCalibration
})


output$nBins_ui <- renderUI({
  req(rv$resAnaDiff)
  req(rv$pi0)
  req(rv$widgets$anaDiff$nBinsHistpval)
  
  selectInput("nBinsHistpval", "n bins of p-value histogram", 
              choices = c(1,seq(from = 0, to = 100, by = 10)[-1]),
              selected=rv$widgets$anaDiff$nBinsHistpval, width='80px')
})


histPValue <- reactive({
  # browser()
  req(rv$resAnaDiff)
  req(rv$pi0)
  req(rv$widgets$anaDiff$nBinsHistpval)
  
  rv$widgets$hypothesisTest$th_logFC
  
  if (is.null(rv$widgets$hypothesisTest$th_logFC) || is.na(rv$widgets$hypothesisTest$th_logFC) ||
      (length(rv$resAnaDiff$logFC) == 0)) { return()}
  if (length(which(is.na(Biobase::exprs(rv$current.obj)))) > 0) {return()}
  
  # isolate({
  t <- NULL
  method <- NULL
  t <- rv$resAnaDiff$P_Value
  t <- t[which(abs(rv$resAnaDiff$logFC) >= rv$widgets$hypothesisTest$th_logFC)]
  toDelete <- which(t==1)
  if (length(toDelete) > 0){	t <- t[-toDelete] }
  histPValue_HC(t,
                bins=as.numeric(rv$widgets$anaDiff$nBinsHistpval), 
                pi0=rv$pi0)
  # })
})

output$histPValue <- renderHighchart({
  histPValue()
})



output$numericalValForCalibrationPlot <- renderUI({
  rv$widgets$anaDiff$calibMethod
  if (rv$widgets$anaDiff$calibMethod == "numeric value"){
    numericInput( "numericValCalibration","Proportion of TRUE null hypohtesis",
                  value = rv$widgets$anaDiff$numValCalibMethod,
                  min=0, max=1, step=0.05)
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
  req(rv$widgets$anaDiff$calibMethod != 'None')
  rv$widgets$hypothesisTest$th_logFC
  rv$resAnaDiff
  req(rv$current.obj)
  
  if (length(rv$resAnaDiff$logFC) == 0) { return()}
  
  if (length(which(is.na(Biobase::exprs(rv$current.obj)))) > 0) {
    return()
  }
  cond <- c(rv$resAnaDiff$condition1, rv$resAnaDiff$condition2)
  
  
  t <- NULL
  method <- NULL
  t <- rv$resAnaDiff$P_Value
  t <- t[which(abs(rv$resAnaDiff$logFC) >= rv$widgets$hypothesisTest$th_logFC)]
  toDelete <- which(t==1)
  if (length(toDelete) > 0){	t <- t[-toDelete] }
  
  l <- NULL
  ll <- NULL
  result = tryCatch(
    {
      
      if ((rv$widgets$anaDiff$calibMethod == "numeric value") 
          && !is.null(rv$widgets$anaDiff$numValCalibMethod)) {
        
        ll <-catchToList(
          wrapperCalibrationPlot(t, 
                                 rv$widgets$anaDiff$numValCalibMethod))
        rv$errMsgCalibrationPlot <- ll$warnings[grep( "Warning:", ll$warnings)]
      }
      else if (rv$widgets$anaDiff$calibMethod == "Benjamini-Hochberg") {
        
        ll <-catchToList(wrapperCalibrationPlot(t, 1))
        rv$errMsgCalibrationPlot <- ll$warnings[grep( "Warning:", ll$warnings)]
      }else { 
        ll <-catchToList(wrapperCalibrationPlot(t, rv$widgets$anaDiff$calibMethod))
        rv$errMsgCalibrationPlot <- ll$warnings[grep( "Warning:", ll$warnings)]
      }
      rv$pi0 <- ll$value$pi0
      # browser()
      rvModProcess$moduleAnaDiffDone[2] <- !is.null(rv$pi0)
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
  # browser()
  
})

output$calibrationPlot <- renderImage({
  
  outfile <- tempfile(fileext='.png')
  
  # Generate a png
  png(outfile, width=600, height=500)
  calibrationPlot()
  dev.off()
  
  # Return a list
  list(src = outfile,
       alt = "This is alternate text")
}, deleteFile = TRUE)




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
      rv$errMsgCalibrationPlotAll <- l$warnings[grep( "Warning:",l$warnings)]
      rvModProcess$moduleAnaDiffDone[2] <- !is.null(rv$pi0)
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
output$calibrationPlotAll <- renderImage({
  
  outfile <- tempfile(fileext='.png')
  
  # Generate a png
  png(outfile, width=600, height=500)
  calibrationPlotAll()
  dev.off()
  
  # Return a list
  list(src = outfile,
       alt = "This is alternate text")
}, deleteFile = TRUE)



###
### ------------------- SCREEN 3 ------------------------------
###

output$screenAnaDiff3 <- renderUI({
  print("in output$screenAnaDiff3")
  
  if(as.character(rv$widgets$anaDiff$Comparison) == "None"){return(NULL)}
  
  isolate({
    tagList(
      tags$div(
        tags$div( style="display:inline-block; vertical-align: center; padding-right: 2px;",
                  modulePopoverUI("modulePopover_pValThreshold"),
                  textInput("seuilPVal",  NULL,
                            value=rv$widgets$anaDiff$th_pval, width='100px')),
        actionButton("valid_seuilPVal", 'Validate value', class = actionBtnClass),
        tags$div( style="display:inline-block; vertical-align: top;",
                  module_Not_a_numericUI("test_seuilPVal"))
        
        
      ),
      tags$hr(),
      tagList(
        tags$div(
          tags$div( style="display:inline-block; vertical-align: top;",
                    htmlOutput("showFDR"),
                    withProgress(message = '',detail = '', value = 1, {
                      moduleVolcanoplotUI("volcano_Step2")
                    })
          ),
          
          tags$div( style="display:inline-block; vertical-align: top;",
                    uiOutput("tooltipInfo"),
                    checkboxInput("showpvalTable","Show p-value table", value=FALSE),
                    radioButtons("downloadAnaDiff", "Download as Excel file", 
                                 choices=c("All data"="All", "only DA"="onlyDA" ),
                                 selected = rv$widgets$anaDiff$downloadAnaDiff),
                    downloadButton('downloadSelectedItems', 'Download', class=actionBtnClass))
        ),
        hidden(DTOutput("anaDiff_selectedItems"))
      )
    )
    
  })
})     



###
### ------------------- SCREEN 4 ------------------------------
###



output$screenAnaDiff4 <- renderUI({     
  print("in output$screenAnaDiff4")
  if (as.character(rv$widgets$anaDiff$Comparison) == "None"){return(NULL)}
  tagList(
    moduleStaticDataTableUI("params_AnaDiff")
    
  )
})


convertAnaDiff2DF <- reactive({
  req(rv$widgets$anaDiff)
  rv$widgets$anaDiff[sapply(rv$widgets$anaDiff, is.null)] <- NA
  df <- as.data.frame(tibble::enframe(rv$widgets$anaDiff))
  names(df) <- c("Parameter", "Value")
  rownames(df) <- NULL
  df
})



output$diffAna_Summary <- renderUI({     
  
  if (as.character(rv$widgets$anaDiff$Comparison) == "None"){return(NULL)}
  
  tagList(
    moduleStaticDataTableUI("params_AnaDiff")
  )
  
})


##################################################################################
###### Set code for widgets managment
##################################################################################







observeEvent(input$valid_seuilPVal,{ 
  req(input$seuilPVal)
  tmp <- gsub(",", ".", input$seuilPVal, fixed=TRUE)
  
  rv$widgets$anaDiff$th_pval <- as.numeric(tmp)
})



observeEvent(input$showpvalTable, {
  print("show : anaDiff_selectedItems")
  shinyjs::toggle(id = "anaDiff_selectedItems", condition=isTRUE(input$showpvalTable))
})


observeEvent(input$validTooltipInfo,{  rv$widgets$anaDiff$tooltipInfo <- input$tooltipInfo})

observeEvent(input$downloadAnaDiff,{  rv$widgets$anaDiff$downloadAnaDiff <- input$downloadAnaDiff})

# 
# output$anaDiffPanel <- renderUI({
#   req(rv$current.obj)
#     NA.count<- length(which(is.na(Biobase::exprs(rv$current.obj))))
#   dataset.name <- last(names(rv$dataset))
#   prev.dataset.name <- paste0('prev.HypothesisTest.',rv$current.obj@experimentData@other$typeOfData)
#   if (NA.count > 0){
#     tags$p("Your dataset contains missing values. Before using the differential analysis, you must filter/impute them")
#   } else if (rv$current.obj@experimentData@other$Params[[dataset.name]][['HypothesisTest']]$design=="None" &&
#              rv$current.obj@experimentData@other$Params[[prev.dataset.name]][['HypothesisTest']]$design=="None") {
#     tags$p("The statistical test has not been performed so the differential analysis cannot be done.")
#     } else {
#       moduleProcessUI("moduleProcess_AnaDiff")
#     }
#   
# })





# 
# DatasetIsSwaped <- reactive({
#   req(input$swapVolcano)
#   isSwaped <- (input$swapVolcano %%2)==1
#   isSwaped
#   
# })


callModule(modulePopover,"modulePopover_pValThreshold", 
           data = reactive(list(title = "p-val cutoff", 
                                content="Define the -log10(p_value) threshold")))




output$anaDiff_selectedItems <- renderDT({
  
  DT::datatable(GetSelectedItems(),
                extensions = 'Buttons',
                escape = FALSE,
                rownames=FALSE,
                options = list(
                  buttons = list(
                    list(
                      extend = 'csv',
                      filename = rv_anaDiff$filename
                    ),
                    list(
                      extend = 'pdf',
                      filename = rv_anaDiff$filename
                    ),'print'),
                  initComplete = initComplete(),
                  dom = 'Bfrtip',
                  server = TRUE,
                  columnDefs = list(list(width='200px',targets= "_all")),
                  ordering = TRUE)
  ) %>%
    formatStyle(
      paste0('isDifferential (', as.character(rv$widgets$anaDiff$Comparison), ')'),
      target = 'row',
      backgroundColor = styleEqual(c(0, 1), c("white",orangeProstar))
    )
})



output$downloadSelectedItems <- downloadHandler(
  filename = reactive({rv_anaDiff$filename}),
  
  content = function(file) {
    DA_Style <- openxlsx::createStyle(fgFill = orangeProstar)
    hs1 <- openxlsx::createStyle(fgFill = "#DCE6F1", halign = "CENTER", textDecoration = "italic",
                                 border = "Bottom")
    wb <- openxlsx::createWorkbook() # Create wb in R
    openxlsx::addWorksheet(wb,sheetName="DA result") #create sheet
    openxlsx::writeData(wb,sheet = 1, as.character(rv$widgets$anaDiff$Comparison), colNames = TRUE,headerStyle = hs1)
    openxlsx::writeData(wb,sheet = 1, startRow = 3,GetSelectedItems(), colNames = TRUE)
    ll.DA.row <- which(GetSelectedItems()[,paste0('isDifferential (',as.character(rv$widgets$anaDiff$Comparison), ')')]==1)
    ll.DA.col <- rep(which(colnames(GetSelectedItems()) == paste0('isDifferential (',as.character(rv$widgets$anaDiff$Comparison),')')), length(ll.DA.row))
    
    openxlsx::addStyle(wb, sheet=1, cols=ll.DA.col,
                       rows = 3 + ll.DA.row, style = DA_Style)
    
    tempFile <- tempfile(fileext = ".xlsx")
    openxlsx::saveWorkbook(wb, file = tempFile, overwrite = TRUE)
    
    file.rename(tempFile, file)
    
  })











# output$anaDiff_selectedItems <- renderDT({
#   
#   DT::datatable(GetSelectedItems(),
#                 escape = FALSE,
#                 rownames=TRUE,
#                 options = list(initComplete = initComplete(),
#                                dom = 'Bfrtip',
#                                server = TRUE,
#                                columnDefs = list(list(width='200px',targets= "_all")),
#                                ordering = TRUE)
#   ) %>%
#     formatStyle(
#       'isDifferential',
#       target = 'row',
#       backgroundColor = styleEqual(c(0, 1), c("white",orangeProstar))
#     )
# })



# output$downloadSelectedItems <- downloadHandler(
#   #input$chooseDatasetToExportToMSnset,
#   filename = paste0('diffanalysis_', input$datasets,'.xlsx'),
#   content = function(file) {
#     print(paste0("file to write=", file))
#     DA_Style <- openxlsx::createStyle(fgFill = orangeProstar)
#     hs1 <- createStyle(fgFill = "#DCE6F1", halign = "CENTER", textDecoration = "italic",
#                        border = "Bottom")
#     wb <- openxlsx::createWorkbook() # Create wb in R
#     openxlsx::addWorksheet(wb,sheetName="DA result") #create sheet
#     openxlsx::writeData(wb,sheet = 1, as.character(rv$widgets$anaDiff$Comparison), colNames = TRUE,headerStyle = hs1)
#     openxlsx::writeData(wb,sheet = 1, startRow = 3,GetSelectedItems(), colNames = TRUE)
#     ll.DA.row <- which(GetSelectedItems()[,'isDifferential']==1)
#     ll.DA.col <- rep(which(colnames(GetSelectedItems()) == 'isDifferential'), length(ll.DA.row))
#     openxlsx::addStyle(wb, sheet=1, cols=ll.DA.col, rows = 1+ ll.DA.row, style = DA_Style)
#     openxlsx::saveWorkbook(wb, file = file, overwrite = TRUE)
#   })
# 













#####
####  SELECT AND LOAD ONE PARIWISE COMPARISON
####







Get_FDR <- reactive({
  req(rv$current.obj)
  rv$widgets$anaDiff$numValCalibMethod
  rv$widgets$anaDiff$calibMethod
  req(rv$resAnaDiff)
  
  m <- NULL
  if (rv$widgets$anaDiff$calibMethod == "Benjamini-Hochberg") { m <- 1}
  else if (rv$widgets$anaDiff$calibMethod == "numeric value") {
    m <- as.numeric(rv$widgets$anaDiff$numValCalibMethod)} 
  else {m <- rv$widgets$anaDiff$calibMethod }
  
  rv$widgets$anaDiff$FDR <- diffAnaComputeFDR(rv$resAnaDiff[["logFC"]], 
                                              rv$resAnaDiff[["P_Value"]],
                                              rv$widgets$anaDiff$th_pval, 
                                              rv$widgets$hypothesisTest$th_logFC, 
                                              m)
  rvModProcess$moduleAnaDiffDone[3] <- TRUE
  as.numeric(rv$widgets$anaDiff$FDR)
})


output$showFDR <- renderUI({
  req(rv$current.obj)
  nb <- length(which(GetSelectedItems()[paste0('isDifferential (',as.character(rv$widgets$anaDiff$Comparison),')')]==1))
  th <- Get_FDR() * nb
  print(th)
  
  
  tagList(
    if (!is.infinite(Get_FDR())){
      tags$p(style="font-size: 25px;","FDR = ", round(100*Get_FDR(), digits=2)," % (p-value = ",
             signif(10^(- (rv$widgets$anaDiff$th_pval)), digits=3), ")")
    } else {
      tags$p(style="font-size: 25px;","FDR = NA") 
    },
    
    if (th < 1){
      tags$p(style="color: red",paste0("Warning: With such a dataset size (", nb ," selected discoveries), an FDR of ",round(100*Get_FDR(), digits=2), "% should be cautiously interpreted as strictly less than one discovery (", 
                                       round(th, digits=2), ") 
             is expected to be false")
      )
    } 
  )
  
  
})





output$equivPVal <- renderUI ({
  req(rv$widgets$anaDiff$th_pval)
  
  tags$p(paste0("(p-value = ",signif(10^(- (rv$widgets$anaDiff$th_pval)), digits=3), ")"))
})


output$equivLog10 <- renderText ({
  req(rv$widgets$anaDiff$th_pval)
  
  tags$p(paste0("-log10 (p-value) = ",signif(- log10(rv$widgets$anaDiff$th_pval/100), digits=1)))
})




GetSelectedItems <- reactive({
  req(rv$resAnaDiff)
  rv$widgets$anaDiff$downloadAnaDiff
  
  
  t <- NULL
  upItems1 <- which(-log10(rv$resAnaDiff$P_Value) >=rv$widgets$anaDiff$th_pval)
  upItems2 <- which(abs(rv$resAnaDiff$logFC) >= rv$widgets$hypothesisTest$th_logFC)
  
  if ( rv$widgets$anaDiff$downloadAnaDiff == "All"){
    selectedItems <- 1:nrow(rv$current.obj)
    significant <- rep(0, nrow(rv$current.obj))
    significant[intersect(upItems1, upItems2)] <- 1
  } else {
    selectedItems <- intersect(upItems1, upItems2)
    significant <- rep(1, length(selectedItems))
  }
  
  t <- data.frame(id = rownames(Biobase::exprs(rv$current.obj))[selectedItems],
                  logFC = round(rv$resAnaDiff$logFC[selectedItems], digits=rv$settings_nDigits),
                  P_Value = rv$resAnaDiff$P_Value[selectedItems],
                  isDifferential = significant)
  tmp <- as.data.frame(Biobase::fData(rv$current.obj)[selectedItems, rv$widgets$anaDiff$tooltipInfo])
  names(tmp) <- rv$widgets$anaDiff$tooltipInfo
  t <- cbind(t, tmp)
  colnames(t)[2:4] <- paste0(colnames(t)[2:4], " (", as.character(rv$widgets$anaDiff$Comparison),')')
  t
})



isContainedIn <- function(strA, strB){
  return (all(strA %in% strB))
}

