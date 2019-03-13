source(file.path(".", "modules/pipelines/peptide/moduleA.R"),  local = TRUE)$value
source(file.path(".", "modules/pipelines/peptide/moduleB.R"),  local = TRUE)$value
source(file.path(".", "modules/pipelines/peptide/moduleC.R"),  local = TRUE)$value


modulePipelinePepUI <- function(id){
  ns <- NS(id)
  
  navbarMenu("Pipeline peptide" ,
             tabPanel("ProcessA",moduleAUI(ns('pep_processA'))),
             tabPanel("ProcessB",moduleBUI(ns('pep_processB'))),
             tabPanel("ProcessC",moduleCUI(ns('pep_processC')))
  )
  
}



modulePipelinePep <- function(input, output, session, initData, navPage, indice){
  ns <- session$ns
  
  
  rv <- reactiveValues(
    current.obj = NULL,
    indice = NULL,
    
    dataset = list(original=NULL,
                   A_processed = NULL,
                   B_processed = NULL,
                   C_processed = NULL),
    
    process = list(
                  processNull = NULL,
                  ProcessA = callModule(module=moduleA, 'pep_processA', 
                            dataIn=reactive({rv$current.obj}),
                            screen.id = reactive({GetScreenId()})),
                  ProcessB  = callModule(module=moduleB, 'pep_processB', 
                             dataIn=reactive({rv$current.obj}),
                             screen.id = reactive({GetScreenId()})),
      
                  ProcessC =callModule(module=moduleC, 'pep_processC', 
                           dataIn=reactive({rv$current.obj}),
                           screen.id = reactive({GetScreenId()}))
                )
  )
  
  
  ## Initialisation of the module
  observeEvent(initData(), {
      rv$dataset <- initData()
      rv$current.obj <- initData()[[indice()]]
    
    print("In module pipeline, observeEvent(initData()")
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
  
  observeEvent(rv$process$ProcessA(), {
    print('### EVENT ON : rv$dataset$A_processed <- rv$obj')
    rv$current.obj <- rv$process$ProcessA()
    rv$indice <- 2
    rv$dataset$A_processed <- rv$process$ProcessA()
    DeleteDatasetsAfter('ProcessA')
    
    printStatus()
  })
  
  observeEvent(rv$process$ProcessB(), {
    print('### EVENT ON : rv$dataset$B_processed <- rv$obj')
    rv$current.obj <- rv$process$ProcessB()
    rv$indice <- 3
    rv$dataset$B_processed <- rv$process$ProcessB()
    DeleteDatasetsAfter('ProcessB')
    
    printStatus()
  })
  
  
  
  observeEvent(rv$process$ProcessC(), {
    print('### EVENT ON : rv$dataset$C_processed <- rv$obj')
    rv$current.obj <- rv$process$ProcessC()
    rv$indice <- 4
    rv$dataset$C_processed <- rv$process$ProcessC()
    DeleteDatasetsAfter('ProcessC')
    
    printStatus()
  })
  
  
  
  
  printStatus <- function(){
    print("PrintStatus of module peptide")
    print(paste0('initData() = ',initData()))
    print(paste0('rv$indice= ',rv$indice))
    print(paste0('rv$current.obj= ',rv$current.obj))
    print(paste0('rv$dataset$original= ',rv$dataset$original))
    print(paste0('rv$dataset$A_processed= ',rv$dataset$A_processed))
    print(paste0('rv$dataset$B_processed= ',rv$dataset$B_processed))
    print(paste0('rv$dataset$C_processed= ',rv$dataset$C_processed))
  }
 
  
  return(reactive({list(indice=rv$indice,dataset=rv$dataset)}))
  #return(reactive({rv$dataset}))
}