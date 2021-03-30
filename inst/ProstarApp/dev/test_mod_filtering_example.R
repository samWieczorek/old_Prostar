library(shiny)
library(DT)
library(MSnbase)
library(DAPAR)
library(shinyBS)
library(shinyjqui)

#source(file.path('../inst/ProstarApp/server', 'mod_plots_metacell_histo.R'), local=TRUE)$value


utils::data(Exp1_R25_prot, package='DAPARdata')
obj <- Exp1_R25_prot

ui <- fluidPage(
  mod_filtering_example_ui('example')
)



server <- function(input, output, session) {
  
  rv <- reactiveValues(
    widgets = list(
      filtering = list(
        MetacellTag = "missing",
        MetacellFilters = "AllCond",
        KeepRemove = 'delete',
        metacell_value_th = 1,
        choose_metacell_percent_th = 0,
        metacell_value_percent = 0,
        val_vs_percent = 'Value',
        metacellFilter_operator = '>'
        )
      )
  )
  
  mod_filtering_example_server(id = 'example', 
                               params = reactive({rv$widgets$filtering}),
                               txt = reactive({'query'})
                               )
}


shinyApp(ui, server)