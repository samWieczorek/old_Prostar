library(shiny)
library(MultiAssayExperiment)
library(tibble)


options(shiny.fullstacktrace = TRUE)


#source(file.path('../../../R', 'mod_infos_dataset.R'), local=TRUE)$value
#source(file.path('../../../R', 'mod_format_DT.R'), local=TRUE)$value
source(file.path('../../../R', 'global.R'), local=TRUE)$value
source(file.path('../../../R', 'mod_open_demoDataset.R'), local=TRUE)$value

actionBtnClass <- "btn-primary"

ui <- fluidPage(
  tagList(
    mod_open_demoDataset_ui('rl'),
    hr(),
    mod_infos_dataset_ui("infos")
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
  
  rv.core <- reactiveValues(
    demoData = NULL,
    dataIn = NULL
  )
  
  mod_infos_dataset_server('infos', 
                           obj = reactive({rv.core$demoData()})
  )
  
  rv.core$demoData <- mod_open_demoDataset_server("rl")
  
  observeEvent(input$load_dataset_btn, {
    print(names(rv.core$demoData()))
    rv.core$dataIn <- rv.core$demoData()
  })
  
  
}


shinyApp(ui, server)
