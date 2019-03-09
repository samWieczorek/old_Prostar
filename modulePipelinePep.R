


modulePipelinePep <- function(input, output, session, dataIn, navPage){
  ns <- session$ns
  
  
  rv <- reactiveValues(
    current.obj = NULL,
    returnVal = NULL,
    
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
    print("New dataIn() value")
    print(paste0("the current tab is : ",navPage()))
    })
  
  GetScreenId <- reactive({
    navPage()
    rv$current.obj
    
    screen <- NULL
    
    m <-  which(names(rv$process)==navPage())
    n <-  which(unlist(lapply(rv$dataset, function(x) length(which(x==rv$current.obj))))==1)
    
    if (m >= n) { screen <- 'Initial screen'}
    else {screen <- 'Final screen'}
    print(paste0("in GetScreenId(), n = ", n, ", m = ", m, ". screen = ", screen))
    screen
  })
  
  
  output$chooseDataset <- renderUI({
    
    req(rv$dataset)
    div(
      div(
        style="display:inline-block; vertical-align: middle;",
        p("Current dataset")
      ),
      div(
        style="display:inline-block; vertical-align: middle;",
        selectInput('currentDataset', '', 
                    choices =  names(unlist(rv$dataset)),
                    width='150px')
      )
    )
    
    
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
    print('### EVENT ON : rv$dataset$A_processed <- rv$obj')
    rv$current.obj <- rv$process$ProcessA()
    rv$dataset$A_processed <- rv$process$ProcessA()
    rv$returnVal <- rv$current.obj
    DeleteDatasetsAfter('ProcessA')
    
    updateSelectInput(session,  'currentDataset', selected =  dplyr::last(names(unlist(rv$dataset))))
    
    
  })
  
  observeEvent(rv$process$ProcessB(), {
    rv$dataset
    print('### EVENT ON : rv$dataset$B_processed <- rv$obj')
    rv$current.obj <- rv$process$ProcessB()
    rv$dataset$B_processed <- rv$process$ProcessB()
    rv$returnVal <- rv$current.obj
    DeleteDatasetsAfter('ProcessB')
    
    updateSelectInput(session,  'currentDataset', selected =  dplyr::last(names(unlist(rv$dataset))))
    
  })
  
  
  
  observeEvent(rv$process$ProcessC(), {
    rv$dataset
    print('### EVENT ON : rv$dataset$C_processed <- rv$obj')
    rv$current.obj <- rv$process$ProcessC()
    rv$dataset$C_processed <- rv$process$ProcessC()
    rv$returnVal <- rv$current.obj
    DeleteDatasetsAfter('ProcessC')
    
    updateSelectInput(session,  'currentDataset', selected =  dplyr::last(names(unlist(rv$dataset))))
    
  })
  
  
  
  
  output$summary <- renderUI({
    
    tagList(
      p(paste0('rv$dataset$original= ',rv$dataset$original)),
      p(paste0('rv$dataset$A_processed= ',rv$dataset$A_processed)),
      p(paste0('rv$dataset$B_processed= ',rv$dataset$B_processed)),
      p(paste0('rv$dataset$C_processed= ',rv$dataset$C_processed))
    )
    
  })
  
  
  
  return(reactive({rv$returnVal}))
}