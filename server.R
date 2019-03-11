library(shinyBS)

#' Title
#'
#' @param input 
#' @param output 
#' @param session 
#'
#' @return
#' @export
#'
#' @examples
server <- function(input, output, session){
  
  source(file.path(".", "modulesUI.R"),  local = TRUE)$value
  source(file.path(".", "moduleA.R"),  local = TRUE)$value
  source(file.path(".", "moduleB.R"),  local = TRUE)$value
  source(file.path(".", "moduleC.R"),  local = TRUE)$value
  source(file.path(".", "moduleD.R"),  local = TRUE)$value
  source(file.path(".", "modulePlots.R"),  local = TRUE)$value
  
  source(file.path(".", "moduleDataManager.R"),  local = TRUE)$value
  
  
  source(file.path(".", "modulePipelinePepUI.R"),  local = TRUE)$value
  source(file.path(".", "modulePipelinePep.R"),  local = TRUE)$value
  source(file.path(".", "modulePipelineProtUI.R"),  local = TRUE)$value
  source(file.path(".", "modulePipelineProt.R"),  local = TRUE)$value
  source(file.path(".", "modulePipelineP2pUI.R"),  local = TRUE)$value
  source(file.path(".", "modulePipelineP2p.R"),  local = TRUE)$value
  
 
  rvtmp <- reactiveValues(
    init.obj = 10)
  rv <- reactiveValues(
    page = NULL,
    indice = NULL,
    current.obj = list(original=1,
                       A_processed = NULL,
                       B_processed = NULL,
                       C_processed = NULL
                  ),
    dataset = NULL,
    
    pipeline = callModule(module = modulePipelinePep, 'test', 
                                    dataIn = reactive({rv$current.obj}), 
                                    navPage=reactive({input$navPage}),
                                    indice = reactive({GetIndex()}))

  )
    
  
  
 callModule(module = modulePlots, 'showPlots', dataIn=reactive({rv$current.obj}))
 
 obj <- callModule(module = moduleDataManager, 'datamanager')
 
 
  observeEvent(input$selectPipeline,{
    print('EVENT ON : observeEvent(input$selectPipeline')
    
    switch(input$selectPipeline,
           Peptide= insertTab(inputId = "navPage",modulePipelinePepUI('test'), target="Data manager", position="after"),
           Protein = insertTab(inputId = "navPage",modulePipelineProtUI('testProt'), target="Data manager", position="after"),
           P2p = insertTab(inputId = "navPage",modulePipelineP2pUI('testP2p'), target="Data manager", position="after")
    )
  })
  
  observeEvent(rv$pipeline(), {
    rv$dataset
    print('### EVENT ON : rv$pipeline()')
    rv$dataset <- rv$pipeline()$dataset
    rv$indice <- rv$pipeline()$indice
    updateSelectInput(session, "currentDataset", choices = names(rv$dataset[!sapply(rv$dataset,is.null)]),
                      selected = names(rv$dataset)[rv$indice])
    
    GetIndex()
    })
  
  

  output$summary <- renderUI({
   print(str(rv$pipeline()))
    tagList(
      h3('General summary'),
      p(paste0('rv$indice =',rvtmp$indice)),
      p(paste0('rv$current.obj= ',rv$current.obj)),
      p(paste0('rv$pipeline() = ',rv$pipeline()))
    )
  
  })
  

  GetIndex <- eventReactive(input$currentDataset,{
    print("IN GetIndex <- eventReactive(input$currentDataset")
    print(input$currentDataset)
    print(str(rv$dataset))
    id <- which(names(rv$dataset[!sapply(rv$dataset,is.null)])==input$currentDataset)
    print(paste0("New value of id : ", id))
    id
  })
  
  output$chooseDataset <- renderUI({

    req(rv$current.obj)
    div(
      div(
        style="display:inline-block; vertical-align: middle;",
        p("Current dataset")
      ),
      div(
        style="display:inline-block; vertical-align: middle;",
        selectInput('currentDataset', '',
                    choices =  c("None"="None",names(rv$dataset[!sapply(rv$dataset,is.null)])),
                    width='150px')
      )
    )
  })


   }
