source(file.path("server", "mod_filtering_example.R"),  local = TRUE)$value
source(file.path("server", "mod_query_metacell.R"),  local = TRUE)$value

tabPanel("Differential analysis",
         value = "diffAnalysisTab",
        # uiOutput("anaDiffPanel")
         moduleProcessUI("moduleProcess_AnaDiff")
          
)
