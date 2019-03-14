source(file.path(".", "modules/pipelines/p2p/moduleH.R"), local = TRUE)$value
source(file.path(".", "modules/pipelines/p2p/moduleI.R"), local = TRUE)$value

modulePipelineP2pUI <- function(id){
  ns <- NS(id)
  
  navbarMenu("Pipeline p2p" ,
             tabPanel("ProcessH",moduleHUI('p2p_processH')),
             tabPanel("ProcessI",moduleIUI('p2p_processI'))
            )
  
}



modulePipelineP2p <- function(input, output, session, initData, navPage, indice){
  ns <- session$ns
  
  
  rv <- reactiveValues(
    current.obj = NULL,
    indice = NULL,
    
    dataset = list(original=NULL,
                   H_processed = NULL,
                   I_processed = NULL),
    
    process = list(
      processNull = NULL,
      ProcessH = callModule(module=moduleH, 'p2p_processH', 
                            dataIn=reactive({rv$current.obj}),
                            screen.id = reactive({GetScreenId()})),
      
      ProcessI  = callModule(module=moduleI, 'p2p_processI', 
                             dataIn=reactive({rv$current.obj}),
                             screen.id = reactive({GetScreenId()}))
    )
  )
  
  
  ###### Beginning of the skeleton of the module  #############################
  
  
  ## Initialisation of the module
  observeEvent(initData(), {
    rv$dataset <- initData()
    rv$current.obj <- initData()[[indice()]]
    
    print("In module pipeline p2p, observeEvent(initData()")
    print(initData())
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
  
  
  
  observeEvent(rv$process$ProcessH(), {
    print('### EVENT ON : rv$dataset$H_processed <- rv$obj')
    rv$current.obj <- rv$process$ProcessH()
    rv$indice <- 2
    rv$dataset$H_processed <- rv$process$ProcessH()
    DeleteDatasetsAfter('ProcessH')
    
    printStatus()
  })
  
  observeEvent(rv$process$ProcessI(), {
    print('### EVENT ON : rv$dataset$I_processed <- rv$obj')
    rv$current.obj <- rv$process$ProcessI()
    rv$indice <- 3
    rv$dataset$I_processed <- rv$process$ProcessI()
    DeleteDatasetsAfter('ProcessI')
    
    printStatus()
  })
  
  
  
  printStatus <- function(){
    print("PrintStatus of module p2p")
    print(paste0('initData() = ',initData()))
    print(paste0('rv$indice= ',rv$indice))
    print(paste0('rv$current.obj= ',rv$current.obj))
    print(paste0('rv$dataset$ ',rv$dataset))
  }
  
  
  return(reactive({list(indice=rv$indice,dataset=rv$dataset)}))
  #return(reactive({rv$dataset}))
}