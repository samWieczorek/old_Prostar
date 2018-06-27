


ClearNavbarPage <- reactive({
  
  
  if ("UpdateDesign" %in% rv$UI_TabsList){
    removeTab(inputId = "navPage",target="updateDesignTab")
    isolate({rv$UI_TabsList <- c(rv$UI_TabsList, "UpdateDesign")})
  }
  
  if ("ProteinLevelPanel" %in% rv$UI_TabsList){
    removeTab(inputId = "navPage", target = "Data processing")
    isolate({rv$UI_TabsList <- rv$UI_TabsList[-(which(rv$UI_TabsList == "ProteinLevelPanel"))] })
    }
  
  if ("PeptideLevelPanel" %in% rv$UI_TabsList){
    removeTab(inputId = "navPage", target = "Data processing")
    isolate({rv$UI_TabsList <- rv$UI_TabsList[-(which(rv$UI_TabsList == "PeptideLevelPanel"))] })
  }
  
  if ("DescriptiveStatisticsTab" %in% rv$UI_TabsList){
    removeTab(inputId = "navPage", target = "DescriptiveStatisticsTab")
    isolate({rv$UI_TabsList <- rv$UI_TabsList[-(which(rv$UI_TabsList == "DescriptiveStatisticsTab"))] })
  }
  
  
})



BuildNavbarPage <- reactive({   
 
#   rv$typeOfDataset
   isolate({rv$UI_TabsList})
   
 if (NeedsUpdate()){
   print(paste0("needs Update =", NeedsUpdate()))
   if (!("UpdateDesign" %in% rv$UI_TabsList))
   {
     appendTab(inputId = "navPage",source(file.path("ui", "ui_UpdateDesign.R"),  local = TRUE)$value)
     isolate({rv$UI_TabsList <- c(rv$UI_TabsList, "UpdateDesign")      })
   }
   
   } else {
      
      if (!("DescriptiveStatisticsTab" %in% rv$UI_TabsList))
        {
        insertTab(inputId = "navPage",
               source(file.path("ui", "ui_DescriptiveStatistics.R"),  local = TRUE)$value,
               target = "Dataset manager",
               position="after")
               isolate({rv$UI_TabsList <- c(rv$UI_TabsList, "DescriptiveStatisticsTab")})
        }
   
    if (!is.null(rv$typeOfDataset)){

      switch(rv$typeOfDataset,
           protein = {

             if (!("ProteinLevelPanel" %in% rv$UI_TabsList))
             {
             insertTab(inputId = "navPage",
              navbarMenu("Data processing" 
                         ,source(file.path("ui", "ui_Filtering.R"),  local = TRUE)$value
                         ,source(file.path("ui", "ui_Normalization.R"),  local = TRUE)$value
                         ,source(file.path("ui", "ui_ImputationProteinLevel.R"), local = TRUE)$value
                         ,source(file.path("ui", "ui_AnaDiff.R"),  local = TRUE)$value
                         ,source(file.path("ui", "ui_GO_Enrich.R"),  local = TRUE)$value
                         ),
              target = "Help",
              position="before")
               isolate({rv$UI_TabsList <- c(rv$UI_TabsList, "ProteinLevelPanel")      })
             }
           }
           
           
        ,peptide = {
          if (!("PeptideLevelPanel" %in% rv$UI_TabsList))
          {
            insertTab(inputId = "navPage",
                    navbarMenu("Data processing",
                     source(file.path("ui", "ui_Filtering.R"),  local = TRUE)$value,
                     source(file.path("ui", "ui_Normalization.R"),  local = TRUE)$value,
                     source(file.path("ui", "ui_ImputationPeptideLevel.R"), local = TRUE)$value,
                     source(file.path("ui", "ui_Aggregation.R"),  local = TRUE)$value,
                     source(file.path("ui", "ui_AnaDiff.R"),  local = TRUE)$value),
          target = "Help",
          position="before"
          )
            isolate({rv$UI_TabsList <- c(rv$UI_TabsList, "PeptideLevelPanel")      })
          }
    }
    )
    }

   }


  
 })




