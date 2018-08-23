


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
 
#   rv$typeOfDataset
   isolate({rv$UI_TabsList})
   

    if (!is.null(rv$typeOfDataset)){

      switch(rv$typeOfDataset,
           protein = {

             if (!("dataProcessProtTab" %in% rv$UI_TabsList))
             {
             insertTab(inputId = "navPage",
              navbarMenu("Data processing (protein)" 
                         ,source(file.path("ui", "ui_Filtering.R"),  local = TRUE)$value
                         ,source(file.path("ui", "ui_Normalization.R"),  local = TRUE)$value
                         ,source(file.path("ui", "ui_ImputationProteinLevel.R"), local = TRUE)$value
                         ,source(file.path("ui", "ui_Aggregation.R"),  local = TRUE)$value
                         ,source(file.path("ui", "ui_HypothesisTest.R"),  local = TRUE)$value
                         ),
              target = "Dataset manager",
              position="after")
               isolate({rv$UI_TabsList <- c(rv$UI_TabsList, "dataProcessProtTab")      })
             }
           }
           
           
        ,peptide = {
          if (!("dataProcessPeptTab" %in% rv$UI_TabsList))
          {
            insertTab(inputId = "navPage",
                    navbarMenu("Data processing (peptide)",
                     source(file.path("ui", "ui_Filtering.R"),  local = TRUE)$value,
                     source(file.path("ui", "ui_Normalization.R"),  local = TRUE)$value,
                     source(file.path("ui", "ui_ImputationPeptideLevel.R"), local = TRUE)$value,
                     source(file.path("ui", "ui_Aggregation.R"),  local = TRUE)$value,
                     source(file.path("ui", "ui_HypothesisTest.R"),  local = TRUE)$value),
          target = "Help",
          position="before"
          )
            isolate({rv$UI_TabsList <- c(rv$UI_TabsList, "dataProcessPeptTab")      })
          }
    }
    )
    }
     
     
     
     if (!("DataMiningTab" %in% rv$UI_TabsList))
     {
       insertTab(inputId = "navPage",
                 navbarMenu("Data mining" 
                            ,source(file.path("ui", "ui_DescriptiveStatistics.R"),  local = TRUE)$value
                            ,source(file.path("ui", "ui_GO_Enrich.R"),  local = TRUE)$value
                            ,source(file.path("ui", "ui_AnaDiff.R"), local = TRUE)$value
                 ),
                 target = "Help",
                 position="before")
       isolate({rv$UI_TabsList <- c(rv$UI_TabsList, "DataMiningTab")})
     }

  
 })




