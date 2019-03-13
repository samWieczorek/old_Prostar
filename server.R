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
  
  source(file.path(".", "modules/modulesUI.R"),  local = TRUE)$value
  source(file.path(".", "modules/moduleA.R"),  local = TRUE)$value
  source(file.path(".", "modules/moduleB.R"),  local = TRUE)$value
  source(file.path(".", "modules/moduleC.R"),  local = TRUE)$value
  source(file.path(".", "modules/moduleD.R"),  local = TRUE)$value
  source(file.path(".", "modules/modulePlotsUI.R"),  local = TRUE)$value
  source(file.path(".", "modules/modulePlots.R"),  local = TRUE)$value
  source(file.path(".", "modules/moduleDataManager.R"),  local = TRUE)$value
  source(file.path(".", "modules/moduleExport.R"),  local = TRUE)$value
  source(file.path(".", "modules/moduleExportUI.R"),  local = TRUE)$value
  
  
  source(file.path(".", "modules/modulePipelinePepUI.R"),  local = TRUE)$value
  source(file.path(".", "modules/modulePipelinePep.R"),  local = TRUE)$value
  source(file.path(".", "modules/modulePipelineProtUI.R"),  local = TRUE)$value
  source(file.path(".", "modules/modulePipelineProt.R"),  local = TRUE)$value
  source(file.path(".", "modules/modulePipelineP2pUI.R"),  local = TRUE)$value
  source(file.path(".", "modules/modulePipelineP2p.R"),  local = TRUE)$value
  
 
  rv <- reactiveValues(
    
    # name of the current dataset in the widget chooseDataset
   
    # current working data from current pipeline
    current.pipeline.data = NULL,
    
    
    #current indice (rank of current dataset in pipeline)
    indice = 1,
    init.obj = NULL,
    
    #model for the structure of dataset for peptide pipeline
    
     pipeline.pep = callModule(module = modulePipelinePep, 'test', 
                                     initData = reactive({rv$init.obj}), 
                                     navPage =reactive({input$navPage}),
                                     indice = reactive({rv$indice}))
    
  )
    
 obj <- callModule(module = moduleDataManager, 'datamanager')
  
 callModule(module = modulePlots, 'showPlots', dataIn=reactive({rv$current.pipeline.data}), llPlots=reactive({1:6}))
 

 observeEvent(req(obj()$initialData),{
    print('EVENT ON : obj()')
    print(paste0("Obj() = ", obj()$initialData))
    print(paste0("pipeline = ", obj()$pipeline))
    rv$init.obj <- list(original=obj()$initialData,
                        A_processed = NULL,
                        B_processed = NULL,
                        C_processed = NULL
                        )
    rv$indice <- 1
    
    switch(obj()$pipeline,
           Peptide= {
             insertTab(inputId = "navPage",modulePipelinePepUI('test'), target="Data manager", position="after")
           },
           Protein = {
             insertTab(inputId = "navPage",modulePipelineProtUI('testprot'), target="Data manager", position="after")
           },
           P2p = {
             insertTab(inputId = "navPage",modulePipelineP2pUI('testp2p'), target="Data manager", position="after")
           }
    )

  })
  
  
  observeEvent(input$currentDataset,{
    print("observeEvent(input$currentDataset,")

    print(str(rv$current.pipeline.data))
    rv$indice <- which(names(rv$current.pipeline.data[!sapply(rv$current.pipeline.data,is.null)])==input$currentDataset)
    print(paste0("New value of indice : ", rv$indice))
    #rv$indice
  })


  
 
 observe({
   rv$indice
   print(paste0("Watch rv$indice = ", rv$indice))
 })
 
 
  observeEvent(rv$pipeline.pep(),{
    
    print('### EVENT ON : rv$pipeline.pep()')
    rv$current.pipeline.data <- rv$pipeline.pep()$data
    rv$current.pipeline.indice <- rv$pipeline.pep()$indice
    print(paste0("current.pipeline.indice :",rv$current.pipeline.indice))
    print(rv$current.pipeline.data)
    updateSelectInput(session, "currentDataset", 
                      choices = names(rv$current.pipeline.data[!sapply(rv$current.pipeline.data,is.null)]),
                      selected = names(rv$current.pipeline.data)[rv$current.pipeline.indice])
    
    #GetIndex()
    })
  
 
GetNonNullNames <- reactive({
  rv$current.pipeline.data
  n <-names(rv$current.pipeline.data[!sapply(rv$current.pipeline.data,is.null)])
  n
})
  
  output$chooseDataset <- renderUI({

    req(rv$current.pipeline.data)
    tagList(
      #,
      #div(
      #   div(
      #   style="display:inline-block; vertical-align: center;",
      #   p("Current dataset")
      # ),
      div(
        
        style="display:inline-block; vertical-align: center; margin:0px",
        selectInput('currentDataset', '',
                    choices =  c("None"="None",GetNonNullNames()),
                    width='150px')
      #)
      )
    )
  })


   }
