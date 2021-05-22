
# IF the dataset is changed par the user, the current process step is reset
#observeEvent(input$datasets,ignoreInit = TRUE,{ isolate({  ResetActivePage()}) })



observeEvent(rv$current.obj,{  
  print("## observeEvent(rv$current.obj ##")
  
  BuildNavbarPage()  
  })



observeEvent( req(input$datasets),ignoreInit = TRUE,{ 
  
 # isolate({
    
    if (rv$processSaved== TRUE) {
      print("---- changement de dataset par mise à jour de input$datasets !!!!-----")
      rv$processSaved <- FALSE
    } else {
      print("---- changement de dataset par le menu - Utilisateur !!!!-----")
      print("---- => On fait un reset de l'interface -----")
      rv$current.obj <- rv$dataset[[input$datasets]]
      if (!is.null( rv$current.obj)){
        rv$typeOfDataset <- rv$current.obj@experimentData@other$typeOfData
        
        ## remettre les logFC s'ils existent
          rv$res_AllPairwiseComparisons <- Get_AllComparisons(rv$current.obj)
        
      }
      ClearCurrentNavPage(input$navPage)
    }
    
# })
  
})


ClearCurrentNavPage <- function(page){
  switch(page,
         FilteringTab = {
           resetModuleProcess("Filtering")
           rvModProcess$moduleFilteringForceReset <-  1 + rvModProcess$moduleFilteringForceReset  
         },
         NormalizationTab = {
           resetModuleProcess("Normalization")
           rvModProcess$moduleNormalizationForceReset <-  1 + rvModProcess$moduleNormalizationForceReset  
         },
         imputationProteinLevelTabs = {
           resetModuleProcess("ProtImputation")
           rvModProcess$moduleProtImputationForceReset <-  1 + rvModProcess$moduleProtImputationForceReset  
         },
         imputationPeptideLevelTabs = {
           resetModuleProcess("PepImputation")
           rvModProcess$modulePepImputationForceReset <-  1 + rvModProcess$modulePepImputationForceReset  
         },
         testTab = {
           resetModuleProcess("HypothesisTest")
           rvModProcess$moduleHypothesisTestForceReset <-  1 + rvModProcess$moduleHypothesisTestForceReset  
         },
         AggregationTab = {
           resetModuleProcess("Aggregation")
           rvModProcess$moduleAggregationForceReset <-  1 + rvModProcess$moduleAggregationForceReset  
         },
         diffAnalysisTab = {
           resetModuleProcess("AnaDiff")
           rvModProcess$moduleAnaDiffForceReset <- 1 + rvModProcess$moduleAnaDiffForceReset
         },
         convertTab = {
           resetModuleProcess("Convert")
           rvModProcess$moduleConvertForceReset <- 1 + rvModProcess$moduleConvertForceReset
         },
         
         GoTab = {
           resetModuleProcess("GO")
           rvModProcess$moduleGOForceReset <- 1 + rvModProcess$moduleGOForceReset
         }
         
  )
}

## Change of page
observeEvent(input$navPage,{ 
  # print("---- changement de page !!!!-----")
  # print(paste0("La nouvelle page est :", input$navPage))
 ClearCurrentNavPage(input$navPage) 

  })





ClearNavbarPage <- reactive({
  
    if ("dataProcessPeptTab" %in% rv$UI_TabsList){
    removeTab(inputId = "navPage", target = "Data processing (peptide)")
    isolate({rv$UI_TabsList <- rv$UI_TabsList[-(which(rv$UI_TabsList == "dataProcessPeptTab"))] })
    }
  
  if ("dataProcessProtTab" %in% rv$UI_TabsList){
    removeTab(inputId = "navPage", target = "Data processing (protein)")
    isolate({rv$UI_TabsList <- rv$UI_TabsList[-(which(rv$UI_TabsList == "dataProcessProtTab"))] })
  }
  
  if ("DataMiningTab" %in% rv$UI_TabsList){
    removeTab(inputId = "navPage", target = "Data mining")
    isolate({rv$UI_TabsList <- rv$UI_TabsList[-(which(rv$UI_TabsList == "DataMiningTab"))] })
  }
  
  
})


################################################################################################
BuildNavbarPage <- reactive({   
rv$current.obj
#   rv$typeOfDataset
   isolate({rv$UI_TabsList})
  

   ## if a dataset is in memory (ie rv$current.obj is not null
   ## remove menus to import new dataset
   removeTab(inputId = "navPage", target = "demoTab")
   removeTab(inputId = "navPage", target = "convertTab")
   removeTab(inputId = "navPage", target = "openMSnsetTab")
   

    if (!is.null(rv$typeOfDataset)){

      switch(rv$typeOfDataset,
           protein = {
             if ("dataProcessPeptTab" %in% rv$UI_TabsList){
               removeTab(inputId = "navPage", target = "Data processing (peptide)")
               isolate({rv$UI_TabsList <- rv$UI_TabsList[-(which(rv$UI_TabsList == "dataProcessPeptTab"))] })
             } 
             
               if (!("dataProcessProtTab" %in% rv$UI_TabsList))
             {
             insertTab(inputId = "navPage",
              navbarMenu("Data processing (protein)" 
                         ,source(file.path("ui", "ui_Filtering.R"),  local = TRUE)$value
                         ,source(file.path("ui", "ui_Normalization.R"),  local = TRUE)$value
                         ,source(file.path("ui", "ui_ImputationProteinLevel.R"), local = TRUE)$value
                         ,source(file.path("ui", "ui_HypothesisTest.R"),  local = TRUE)$value
                         ),
              target = "Data manager",
              position="after")
               isolate({rv$UI_TabsList <- c(rv$UI_TabsList, "dataProcessProtTab")      })
             }
           }
           
           
        ,peptide = {
          if ("dataProcessProtTab" %in% rv$UI_TabsList){
            removeTab(inputId = "navPage", target = "Data processing (protein)")
            isolate({rv$UI_TabsList <- rv$UI_TabsList[-(which(rv$UI_TabsList == "dataProcessProtTab"))] })
          }
          
          if (!("dataProcessPeptTab" %in% rv$UI_TabsList))
          {
            insertTab(inputId = "navPage",
                    navbarMenu("Data processing (peptide)",
                     source(file.path("ui", "ui_Filtering.R"),  local = TRUE)$value,
                     source(file.path("ui", "ui_Normalization.R"),  local = TRUE)$value,
                     source(file.path("ui", "ui_ImputationPeptideLevel.R"), local = TRUE)$value,
                     source(file.path("ui", "ui_Aggregation.R"),  local = TRUE)$value,
                     source(file.path("ui", "ui_HypothesisTest.R"),  local = TRUE)$value),
          target = "Data manager",
          position="after"
          )
            isolate({rv$UI_TabsList <- c(rv$UI_TabsList, "dataProcessPeptTab")      })
          }
    }
    )
    }
   
   
   if (("DataMiningTab" %in% rv$UI_TabsList)){
     removeTab(inputId = "navPage", target = "Data mining")
   }
   
   dataset.name <- last(names(rv$dataset))
   prev.dataset.name <- paste0('prev.HypothesisTest.',rv$current.obj@experimentData@other$typeOfData)

   if ((is.null(rv$current.obj@experimentData@other$Params[[dataset.name]][['HypothesisTest']]$design) 
        && is.null(rv$current.obj@experimentData@other$Params[[prev.dataset.name]][['HypothesisTest']]$design) ) || 
       (rv$current.obj@experimentData@other$Params[[dataset.name]][['HypothesisTest']]$design=="None" &&
        rv$current.obj@experimentData@other$Params[[prev.dataset.name]][['HypothesisTest']]$design=="None")) {
     
     if (rv$typeOfDataset == "peptide"){
       insertTab(inputId = "navPage",
                 navbarMenu("Data mining" 
                            ,source(file.path("ui", "ui_DescriptiveStatistics.R"),  local = TRUE)$value
                            ,moduleCCUI('CC_Multi_Any')
                            ,source(file.path("ui", "ui_GO_Enrich.R"),  local = TRUE)$value
                 ),
                 target = "Help",
                 position="before")
     } else {
       insertTab(inputId = "navPage",
                 navbarMenu("Data mining" 
                            ,source(file.path("ui", "ui_DescriptiveStatistics.R"),  local = TRUE)$value
                            ,source(file.path("ui", "ui_GO_Enrich.R"),  local = TRUE)$value
                 ),
                 target = "Help",
                 position="before")
     }
     isolate({rv$UI_TabsList <- c(rv$UI_TabsList, "DataMiningTab")})
     
   } else {
     if (rv$typeOfDataset == "peptide"){
       insertTab(inputId = "navPage",
                 navbarMenu("Data mining" 
                            ,source(file.path("ui", "ui_DescriptiveStatistics.R"),  local = TRUE)$value
                            ,moduleCCUI('CC_Multi_Any')
                            ,source(file.path("ui", "ui_GO_Enrich.R"),  local = TRUE)$value
                            ,source(file.path("ui", "ui_AnaDiff.R"), local = TRUE)$value
                 ),
                 target = "Help",
                 position="before")
     } else {
       insertTab(inputId = "navPage",
                 navbarMenu("Data mining" 
                            ,source(file.path("ui", "ui_DescriptiveStatistics.R"),  local = TRUE)$value
                            ,source(file.path("ui", "ui_GO_Enrich.R"),  local = TRUE)$value
                            ,source(file.path("ui", "ui_AnaDiff.R"), local = TRUE)$value
                 ),
                 target = "Help",
                 position="before")
     }
     isolate({rv$UI_TabsList <- c(rv$UI_TabsList, "DataMiningTab")})
   }
   
   
   
   
   
   #}
})




