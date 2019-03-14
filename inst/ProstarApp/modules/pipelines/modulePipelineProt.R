source(file.path(".", "modules/pipelines/protein/moduleD.R"), local = TRUE)$value
source(file.path(".", "modules/pipelines/protein/moduleE.R"), local = TRUE)$value
source(file.path(".", "modules/pipelines/protein/moduleF.R"), local = TRUE)$value
source(file.path(".", "modules/pipelines/protein/moduleG.R"), local = TRUE)$value



modulePipelineProtUI <- function(id){
  ns <- NS(id)
  
  navbarMenu("Pipeline protein" ,
             tabPanel("ProcessD",moduleDUI(ns('prot_processD'))),
             tabPanel("ProcessE",moduleEUI(ns('prot_processE'))),
             tabPanel("ProcessF",moduleFUI(ns('prot_processF'))),
             tabPanel("ProcessG",moduleGUI(ns('prot_processG')))
  )
  
}



modulePipelineProt <- function(input, output, session, initData, navPage, indice){
  ns <- session$ns
  
  
  rv <- reactiveValues(
    current.obj = NULL,
    indice = NULL,
    
    dataset = list(original=NULL,
                   D_processed = NULL,
                   E_processed = NULL,
                   F_processed = NULL,
                   G_processed = NULL),
    
    process = list(
      processNull = NULL,
      ProcessD = callModule(module=moduleD, 'prot_processD', 
                            dataIn=reactive({rv$current.obj}),
                            screen.id = reactive({GetScreenId()})),
      
      ProcessE  = callModule(module=moduleE, 'prot_processE', 
                             dataIn=reactive({rv$current.obj}),
                             screen.id = reactive({GetScreenId()})),
      
      ProcessF = callModule(module=moduleF, 'prot_processF', 
                           dataIn=reactive({rv$current.obj}),
                           screen.id = reactive({GetScreenId()})),
      
      ProcessG = callModule(module=moduleG, 'prot_processG', 
                           dataIn=reactive({rv$current.obj}),
                           screen.id = reactive({GetScreenId()}))
    )
  )
  
  
  ###### Beginning of the skeleton of the module  #############################
  
  
  ## Initialisation of the module
  observeEvent(initData(), {
    print("In module pipeline protein, observeEvent(initData()")
    print(initData())
    
    rv$dataset <- initData()
    rv$current.obj <- initData()[[indice()]]
    
    })
  
  
  
  observeEvent(req(indice()),{
    
    print(paste0("Change of current dataset in pipeline :", indice()))
    rv$indice <- indice()
    rv$current.obj <- rv$dataset[[rv$indice]]
    
  })
  
  
  GetScreenId <- reactive({
    navPage()
    req(rv$current.obj)
    
    screen <- NULL
    m <-  which(names(rv$process)==navPage())
    n <-  which(unlist(lapply(rv$dataset, function(x) length(which(x==rv$current.obj))))==1)
    
    if (m >= n) { screen <- 'Initial screen'}
    else {screen <- 'Final screen'}
    print(paste0("in GetScreenId(), n = ", n, ", m = ", m, ". screen = ", screen))
    screen
  })
  
  
  
  DeleteDatasetsAfter <- function(txt){
    indice <- which(names(rv$process) == txt)
    if (indice < length(names(rv$process))) {
      for (i in (indice+1):length(names(rv$process))){
        rv$dataset[i] <- list(NULL)
      }
    }
  }
  
  ###### End of the keleton of the module  #############################
  
  
  
  observeEvent(rv$process$ProcessD(), {
    print('### EVENT ON : rv$dataset$D_processed <- rv$obj')
    rv$current.obj <- rv$process$ProcessD()
    rv$indice <- 2
    rv$dataset$D_processed <- rv$process$ProcessD()
    DeleteDatasetsAfter('ProcessD')
    #rv$dataset <- DeleteDatasetsAfterTest('ProcessD', rv$process, rv$dataset)
    
    printStatus()
  })
  
  observeEvent(rv$process$ProcessE(), {
    print('### EVENT ON : rv$dataset$E_processed <- rv$obj')
    rv$current.obj <- rv$process$ProcessE()
    rv$indice <- 3
    rv$dataset$E_processed <- rv$process$ProcessE()
    #DeleteDatasetsAfter('ProcessE')
    rv$dataset <- DeleteDatasetsAfterTest('ProcessE', rv$process, rv$dataset)
    
    printStatus()
  })
  
  
  
  observeEvent(rv$process$ProcessF(), {
    print('### EVENT ON : rv$dataset$F_processed <- rv$obj')
    rv$current.obj <- rv$process$ProcessF()
    rv$indice <- 4
    rv$dataset$F_processed <- rv$process$ProcessF()
    #DeleteDatasetsAfter('ProcessF')
    rv$dataset <- DeleteDatasetsAfterTest('ProcessF', rv$process, rv$dataset)
    
    printStatus()
  })
  
  
  observeEvent(rv$process$ProcessG(), {
    print('### EVENT ON : rv$dataset$G_processed <- rv$obj')
    rv$current.obj <- rv$process$ProcessG()
    rv$indice <- 4
    rv$dataset$G_processed <- rv$process$ProcessG()
    #DeleteDatasetsAfter('ProcessG')
    rv$dataset <- DeleteDatasetsAfterTest('ProcessG', rv$process, rv$dataset)
    
    printStatus()
  })
  
  printStatus <- function(){
    print("PrintStatus of module ")
    print(paste0('initData() = ',initData()))
    print(paste0('rv$indice= ',rv$indice))
    print(paste0('rv$current.obj= ',rv$current.obj))
    print(paste0('rv$datasetl= ',rv$dataset))
  }
  
  
  
  return(reactive({list(name='protein',
                        indice=rv$indice,
                        dataset=rv$dataset)}))
  #return(reactive({rv$dataset}))
}