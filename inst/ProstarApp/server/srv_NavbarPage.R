
observeEvent(rv$current.obj,{  BuildNavbarPage()})


ClearNavbarPage <- reactive({
  
  
  # if ("UpdateDesign" %in% rv$UI_TabsList){
  #   removeTab(inputId = "navPage",target="updateDesignTab")
  #   isolate({rv$UI_TabsList <- c(rv$UI_TabsList, "UpdateDesign")})
  # }
  
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



BuildNavbarPage <- reactive({   
rv$current.obj
#   rv$typeOfDataset
   isolate({rv$UI_TabsList})
   

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




