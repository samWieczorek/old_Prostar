library(shiny)
library(highcharter)
library(MSnbase)



#source(file.path('../inst/ProstarApp/server', 'mod_plots_mv_histo.R'), local=TRUE)$value




ui <- fluidPage(
  mod_plots_mv_histo_ui('plots_boxplots')
)



server <- function(input, output, session) {
  
  utils::data(Exp1_R25_prot, package='DAPARdata')
  
  obj <- Exp1_R25_prot
  
callModule(mod_plots_mv_histo_server,'plots_boxplots', 
                         data = reactive({obj}),
                         palette=reactive({NULL})
  )
  
  
}


shinyApp(ui, server)