source(file.path("server", "mod_filtering_example.R"),  local = TRUE)$value
source(file.path("server", "mod_query_metacell.R"),  local = TRUE)$value

tabPanel("Filter data",
         value = "FilteringTab",
         moduleProcessUI("moduleProcess_Filtering")
)

