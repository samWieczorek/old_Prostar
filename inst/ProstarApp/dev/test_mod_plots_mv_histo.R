library(shiny)
library(highcharter)
library(MSnbase)
library(DAPAR)


#source(file.path('../inst/ProstarApp/server', 'mod_plots_metacell_histo.R'), local=TRUE)$value


utils::data(Exp1_R25_prot, package='DAPARdata')
obj <- Exp1_R25_prot

ui <- fluidPage(
  tagList(
    selectInput('pattern', 'Pattern', 
                choices = DAPAR::metacell.def(obj@experimentData@other$typeOfData)$node
                ),
    mod_plotsMetacellHistos_ui('plots_boxplots')
  )
)



server <- function(input, output, session) {
   
mod_plotsMetacellHistos_server(id = 'plots_boxplots', 
                               obj = reactive({obj}),
                               pattern = reactive({input$pattern}),
                               pal=reactive({NULL})
                               )
}


shinyApp(ui, server)