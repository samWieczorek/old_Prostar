# output$navbarPageUI <- renderUI({
#   rv$current.obj
#   print("###### navbarPageUI  ######")
#  
#  if (is.null(rv$current.obj))
#    {
#    navbar <- navbarPage(theme = shinytheme("cerulean")
#             
#     ,id = "navPage"
#     ,""
#     ,navbarMenu("Prostar",
#                source(file.path("ui", "ui_Home.R"),  local = TRUE)$value,
#                source(file.path("ui", "ui_Settings.R"),  local = TRUE)$value
#     )
#     ,navbarMenu("Dataset manager",
#                 source(file.path("ui", "ui_OpenMSnSetFile.R"),  local = TRUE)$value,
#                 source(file.path("ui", "ui_ConvertData.R"),  local = TRUE)$value,
#                 source(file.path("ui", "ui_DemoMode.R"),  local = TRUE)$value,
#                 source(file.path("ui", "ui_Export.R"),  local = TRUE)$value,
#                 source(file.path("ui", "ui_LogSession.R"),  local = TRUE)$value
#                 )
#     ,navbarMenu("Help",
#                source(file.path("ui", "ui_UsefulLinks.R"),  local = TRUE)$value,
#                source(file.path("ui", "ui_ReleaseNotes.R"),  local = TRUE)$value,
#                source(file.path("ui", "ui_FAQ.R"),  local = TRUE)$value,
#                source(file.path("ui", "ui_CheckForUpdates.R"),  local = TRUE)$value
#     )
#     )
#  } else {appendTab(inputId = "navPage",
#            source(file.path("ui", "ui_DescriptiveStatistics.R"),  local = TRUE)$value
#  )
# }
#  
# })
#   
#   





 observe({   
 
#   rv$typeOfDataset
   isolate({rv$UI_TabsList})
   
 if (NeedsUpdate()){
   appendTab(inputId = "navPage",
             source.file("ui_UpdateDesign.R")
   )
             
   }
    else {
      print(" UI DescriptiveStatisticsTab ")
      print(rv$UI_TabsList)
      if (!("DescriptiveStatisticsTab" %in% rv$UI_TabsList))
     {
        insertTab(inputId = "navPage",
                  source.file("ui_DescriptiveStatistics.R"),
               target = "Dataset manager",
               position="after")
               isolate({rv$UI_TabsList <- c(rv$UI_TabsList, "DescriptiveStatisticsTab")      })
     
    }
    }
    if (!is.null(rv$typeOfDataset)){


      
      
    switch(rv$typeOfDataset,
           protein = {

             if (!("ProteinLevelPanel" %in% rv$UI_TabsList))
             {
             insertTab(inputId = "navPage",
              navbarMenu("Data processing"
                         ,source.file("ui_Filtering.R")
                         ,source.file("ui_Normalization.R")
                         ,source.file("ui_ImputationProteinLevel.R")
                         #,source.file("ui_Aggregation.R")
                         #,source.file("ui_AnaDiff.R")
                         #,source.file("ui_GO_Enrich.R")
                         ),
              target = "Help",
              position="before")
               isolate({rv$UI_TabsList <- c(rv$UI_TabsList, "ProteinLevelPanel")      })
             }
           }
           
           
    #     ,peptide = {
    #       if (!("PeptideLevelPanel" %in% rv$UI_TabsList))
    #       {
    #         insertTab(inputId = "navPage",
    #                 navbarMenu("Data processing",
    #                  source.file("ui_Filtering.R"),
    #                  ,source.file("ui_Normalization.R"),
    #                  ,source.file("ui_ImputationPeptideLevel.R"),
    #                  ,source.file("ui_Aggregation.R"),
    #                  ,source.file("ui_AnaDiff.R")
    #                  ,source.file("ui_GO_Enrich.R"),
    #       target = "Help", 
    #       position="before"
    #       )
    #         isolate({rv$UI_TabsList <- c(rv$UI_TabsList, "PeptideLevelPanel")      })
    #       }
    # }
    )


   }


  
 })

