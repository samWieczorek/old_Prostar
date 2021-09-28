

callModule(moduleProcess, "moduleProcess_HypothesisTest", 
           isDone = reactive({rvModProcess$moduleHypothesisTestDone}), 
           pages = reactive({rvModProcess$moduleHypothesisTest}),
           rstFunc = resetModuleHypothesisTest,
           forceReset = reactive({rvModProcess$moduleHypothesisTestForceReset })  )


rv.ht <- reactiveValues(
  listComp = NULL,
  keep = NULL,
  swap = NULL,
  n = NULL,
  comp.names = NULL
)

resetModuleHypothesisTest <- reactive({  
  ## update widgets values (reactive values)
  resetModuleProcess("HypothesisTest")
    
  rv$widgets$hypothesisTest$design <- "None"
  rv$widgets$hypothesisTest$method <- "None"
  rv$widgets$hypothesisTest$ttest_options <- "Student"
  rv$widgets$hypothesisTest$th_logFC <- 0
  rv$widgets$hypothesisTest$listNomsComparaison <- NULL
  
  rv$res_AllPairwiseComparisons <- NULL
  rv$tempplot$logFCDistr <- NULL
  rvModProcess$moduleHypothesisTestDone = rep(FALSE, 2)
  
  # Get back to previous dataset
  # if (length(grep("HypothesisTest.", names(rv$dataset))) > 0){
  #     i <- grep("HypothesisTest.", names(rv$dataset))
  #     rv$dataset <- rv$dataset[1:(i-1)]
  #     updateSelectInput(session, 
  #                       'datasets', 
  #                       choices = names(rv$dataset),
  #                       selected = names(rv$dataset)[length(names(rv$dataset))]
  #     )
  #     
  #   }
  #   
  #   rv$current.obj <- rv$dataset[[length(names(rv$dataset))]] 
  rv$current.obj <- rv$dataset[[input$datasets]]
})


callModule(module_Not_a_numeric,"test_seuillogFC", reactive({rv$widgets$hypothesisTest$th_logFC}))

# observeEvent(input$anaDiff_Design, ignoreInit=T,{  rv$widgets$hypothesisTest$design<- input$anaDiff_Design})
# observeEvent(input$diffAnaMethod,{rv$widgets$hypothesisTest$method <- input$diffAnaMethod})
# observeEvent(input$seuilLogFC,{  rv$widgets$hypothesisTest$th_logFC<- as.numeric(input$seuilLogFC)})
# observeEvent(input$ttest_options,{rv$widgets$hypothesisTest$ttest_options <- input$ttest_options})
# 

observeEvent(input$diffAnaMethod, {
  rv$widgets$hypothesisTest$method <- input$diffAnaMethod
})



observeEvent(input$PerformLogFCPlot, {
  rv$widgets$hypothesisTest$design<- input$anaDiff_Design

  rv$widgets$hypothesisTest$th_logFC<- as.numeric(input$seuilLogFC)
  rv$widgets$hypothesisTest$ttest_options <- input$ttest_options   
  
  rv$res_AllPairwiseComparisons <- ComputeComparisons()
  
  
  # rv$listComp <- DAPAR::limmaCompleteTest(Biobase::exprs(obj),
  #                                           Biobase::pData(obj),
  #                                           'OnevsAll')
  #   
    rv.ht$n <- ncol(rv$res_AllPairwiseComparisons$logFC)
    rv.ht$keep <- rep(TRUE, rv.ht$n)
    rv.ht$swap <- rep(FALSE, rv.ht$n)
    rv.ht$comp.names <- colnames(rv$res_AllPairwiseComparisons$logFC)
  
  
  
  }, priority = 1000)



output$inputGroup = renderUI({
  req(rv$res_AllPairwiseComparisons)
  
    input_list <- tagList(
      fluidRow(
        column(width = 2, h3(tags$strong('Condition 1'))),
        column(width = 2, h3(tags$strong('Condition 2'))),
        column(width = 2, div(id='keep_title', h3(tags$strong('Keep')))),
        column(width = 2, div(id='swap_title', h3(tags$strong('Swap'))))
      ),
      lapply(seq_len(rv.ht$n), function(i) {
        ll <- unlist(strsplit(rv.ht$comp.names[i], split = '_'))
        first.cond <- ll[1]
        second.cond <- gsub('[()]', '', ll[3])
        
        fluidRow(
          
          column(width = 2, p(id='pcond1', tags$strong(if (rv.ht$swap[i]) second.cond else first.cond))),
          column(width = 2, p(id='pcond2', tags$strong(if (rv.ht$swap[i]) first.cond else second.cond))),
          column(width = 2, isolate({
            checkboxInput(paste0('compkeep', i), '',
                                          value = rv.ht$keep[i],
                                          width = '80px')
            })
            ),
          column(width = 2, isolate({checkboxInput(paste0('compswap', i), '',
                                          value = rv.ht$swap[i],
                                          width = '80px')
            }))
          
        )
        
      })
    )

  input_list
})


GetShinyValue <- function(pattern){
  req(rv.ht$n)
  unlist(lapply(seq_len(rv.ht$n), function(x) input[[paste0(pattern,x)]]))
}


observeEvent(GetShinyValue('compkeep'), ignoreInit = TRUE, {
  rv.ht$keep <- GetShinyValue('compkeep')
  for (i in seq_len(rv.ht$n))
    shinyjs::toggleState(paste0("compswap", i), condition = rv.ht$keep[i])
  
})


observeEvent(GetShinyValue('compswap'),  ignoreInit = TRUE, {
  rv.ht$swap <- GetShinyValue('compswap')
  })



# First screen
output$screenHypoTest1 <- renderUI({
  
   rv$current.obj
  isolate({
    m <- match.metacell(DAPAR::GetMetacell(rv$current.obj), 
                        pattern="missing",
                        level = DAPAR::GetTypeofData(rv$current.obj)
    )
     NA.count<- length(which(m))

     
  if (NA.count > 0){
    tags$p("Your dataset contains missing values. Before using the differential analysis, you must filter/impute them.")
  } else {
    tagList(
      
      tags$div(
        tags$div( style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                  selectInput("anaDiff_Design", "Contrast", 
                              choices = c("None" = "None", "One vs One" = "OnevsOne", "One vs All" = "OnevsAll"),
                              selected = rv$widgets$hypothesisTest$design,
                              width='150px')
        ),
        tags$div( style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                  selectInput("diffAnaMethod","Statistical test",
                              choices = anaDiffMethod_Choices,
                              selected = rv$widgets$hypothesisTest$method,
                              width='150px')
        ),
        tags$div( style="display:inline-block; vertical-align: middle; padding-right: 20px;",
                  hidden( radioButtons("ttest_options", "t-tests options",choices=c("Student", "Welch"),
                                       selected=rv$widgets$hypothesisTest$ttest_options,
                                       width='150px'))
        ),
        tags$div( style="display:inline-block; vertical-align: middle; padding-right: 20px;",
                  textInput("seuilLogFC", "log(FC) threshold",  
                               value=rv$widgets$hypothesisTest$th_logFC,
                               width='150px'),
                  module_Not_a_numericUI("test_seuillogFC")
                  
        ),
        tags$div( style="display:inline-block; vertical-align: middle; padding-right: 20px;",
                  uiOutput("correspondingRatio")
                  
        ),
        tags$div( style="display:inline-block; vertical-align: middle; padding-right: 20px;",
                  actionButton("PerformLogFCPlot", "Perform log FC plot",class = actionBtnClass )
                  
        )
        
        )
      ,
      tags$hr(),
      highchartOutput("FoldChangePlot", height="100%"),
      uiOutput('inputGroup')
    )
    
  }
  })
})


# Definition of screen 2
output$screenHypoTest2 <- renderUI({
  tagList(
    uiOutput("btn_valid")
  )
})


# Test if a hypothesis test has already been done.
observe({
  if (length(grep("HypothesisTest.", names(rv$dataset))) > 0){
    rvModProcess$moduleHypothesisTestDone[1:2] <- TRUE
  }
})


output$correspondingRatio <- renderUI({
  
  ratio <- as.numeric(rv$widgets$hypothesisTest$th_logFC)
    
p("(FC = ", 2^(ratio), ")")
  
})


output$btn_valid <- renderUI({
  cond <- (rv$widgets$hypothesisTest$method != "None")&&(rv$widgets$hypothesisTest$design != "None")
  if (!cond){return(NULL)}
  
  actionButton("ValidTest","Save significance test", class = actionBtnClass)
})


observeEvent(rv$widgets$hypothesisTest$method,{
  
  toggle(id = "ttest_options",  condition = (rv$widgets$hypothesisTest$method == "ttests"))
})


# Highcharts plot
output$FoldChangePlot <- renderHighchart({
  
  name <- rv$current.obj@experimentData@other$Params$HypothesisTest.protein$HypothesisTest$AllPairwiseCompNames$logFC
  if (length(rv$res_AllPairwiseComparisons$logFC)==0 && length(as.data.frame(Biobase::fData(rv$current.obj)[,name])) ==0)
  return(NULL)
  
  withProgress(message = 'Computing plot...',detail = '', value = 0.5, {
  if (length(as.data.frame(Biobase::fData(rv$current.obj)[,name])) > 0)
    rv$tempplot$logFCDistr <- hc_logFC_DensityPlot(as.data.frame(Biobase::fData(rv$current.obj)[,name]),
                                                   rv$current.obj@experimentData@other$Params$HypothesisTest.protein$HypothesisTest$th_logFC)
  else if (length(rv$res_AllPairwiseComparisons$logFC) > 0)
    rv$tempplot$logFCDistr <- hc_logFC_DensityPlot(rv$res_AllPairwiseComparisons$logFC,
                                                   as.numeric(rv$widgets$hypothesisTest$th_logFC))
 # rv$tempplot$logFCDistr
  
  })
})



### Computation of comparisons selected in the variable 'rv$widgets$hypothesisTest$design' 
ComputeComparisons <- reactive({
  req(rv$widgets$hypothesisTest$method != 'None')
  req(rv$widgets$hypothesisTest$design != 'None')
  rv$widgets$hypothesisTest$ttest_options
  
  
  m <- match.metacell(DAPAR::GetMetacell(rv$current.obj), 
                      pattern="missing",
                      level = DAPAR::GetTypeofData(rv$current.obj)
  )
  if (length(which(m)) > 0)
    return()
  
  df <- NULL
#isolate({
  #if (is.null(rv$current.obj@experimentData@other$Params[["HypothesisTest"]])){
  withProgress(message = 'Computing comparisons ...', detail = '', value = 0.5, {
    
    switch(rv$widgets$hypothesisTest$method,
           Limma={
             df <- DAPAR::limmaCompleteTest(Biobase::exprs(rv$current.obj),
                                            Biobase::pData(rv$current.obj),
                                            rv$widgets$hypothesisTest$design) 
             
           },
           ttests={
             df <- DAPAR::compute_t_tests(rv$current.obj, 
                                          contrast = rv$widgets$hypothesisTest$design,
                                          type = rv$widgets$hypothesisTest$ttest_options)
           })
  rv$widgets$hypothesisTest$listNomsComparaison <- colnames(df$logFC)
    
  
  rvModProcess$moduleHypothesisTestDone[1] <- TRUE
  
  })
  
  #browser()
  df
})  %>% bindCache(rv$current.obj, 
                  rv$widgets$hypothesisTest$method, 
                  rv$widgets$hypothesisTest$design,
                  rv$widgets$hypothesisTest$ttest_options )
#})




########################################################################
#
#
########################################################################
observeEvent(input$ValidTest,{ 
  req(rv$res_AllPairwiseComparisons)
  

  rv$current.obj <- DAPAR::diffAnaSave(obj = rv$current.obj,
                                       allComp = rv$res_AllPairwiseComparisons
                                       )
  
  name <- paste("HypothesisTest.", rv$typeOfDataset, sep="")
  rv$current.obj <- saveParameters(rv$current.obj, 
                                   name,
                                   "HypothesisTest", 
                                   build_ParamsList_HypothesisTest()
                                   )
  BuildNavbarPage()
  rvModProcess$moduleHypothesisTestDone[2] <- TRUE
  UpdateDatasetWidget(rv$current.obj, name)

  
})