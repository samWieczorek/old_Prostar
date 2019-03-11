


modulePipelinePep <- function(input, output, session, dataIn, navPage){
  ns <- session$ns
  
  
  rv <- reactiveValues(
    current.obj = NULL,
    current.obj.name = NULL,
    dataset = list(
                    original = NULL,
                    A_processed = NULL,
                    B_processed = NULL,
                    C_processed = NULL
                  ),
    
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
  observeEvent(dataIn(),{
    rv$dataset$original <- dataIn()
    rv$current.obj <- dataIn()
    rv$current.obj.name <- 'Original'
    })
  
  GetScreenId <- reactive({
    navPage()
    rv$current.obj
    
    screen <- NULL
    
    m <-  which(names(rv$process)==navPage())
    n <-  which(unlist(lapply(rv$dataset$data, function(x) length(which(x==rv$current.obj))))==1)
    
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
    rv$dataset
    print('### EVENT ON : rv$process$ProcessA()')
    rv$current.obj <- rv$process$ProcessA()
    rv$current.obj.name <- 'A_processed'
    rv$dataset$A_processed <- rv$process$ProcessA()
    #rv$returnVal <- rv$current.obj
    DeleteDatasetsAfter('ProcessA')
    
    updateSelectInput(session,  'currentDataset', selected =  dplyr::last(names(unlist(rv$dataset))))
    printStatus()
    
  })
  
  observeEvent(rv$process$ProcessB(), {
    rv$dataset
    print('### EVENT ON : rv$process$ProcessB()')
    rv$current.obj <- rv$process$ProcessB()
    rv$current.obj.name <- 'B_processed'
    rv$dataset$B_processed <- rv$process$ProcessB()
    #rv$returnVal <- rv$current.obj
    DeleteDatasetsAfter('ProcessB')
    
    updateSelectInput(session,  'currentDataset', selected =  dplyr::last(names(unlist(rv$dataset))))
    printStatus()
  })
  
  
  
  observeEvent(rv$process$ProcessC(), {
    rv$dataset
    print('### EVENT ON : rv$process$ProcessC()')
    rv$current.obj <- rv$process$ProcessC()
    rv$current.obj.name <- 'C_processed'
    rv$dataset$C_processed <- rv$process$ProcessC()
    #rv$returnVal <- rv$current.obj
    DeleteDatasetsAfter('ProcessC')
    
    updateSelectInput(session,  'currentDataset', selected =  dplyr::last(names(unlist(rv$dataset))))
    printStatus()
  })
  
  
  
  
  printStatus <- function(){
    print("Summary of pipeline Peptide - rv$dataset:")
    print(paste0("Current obj name :", rv$current.obj.name))
    print(rv$dataset)
  }
 
  
  return(reactive({list(name=rv$current.obj.name,data=rv$dataset)}))
  #return(reactive({rv$current.obj}))
}