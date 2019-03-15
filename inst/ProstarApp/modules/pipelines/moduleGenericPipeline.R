source(file.path(".", "modules/pipelines/peptide/moduleA.R"), local = TRUE)$value
source(file.path(".", "modules/pipelines/peptide/moduleB.R"), local = TRUE)$value
source(file.path(".", "modules/pipelines/peptide/moduleC.R"), local = TRUE)$value


moduleGenericPipelineUI <- function(id){
  ns <- NS(id)
  
  navbarMenu("Pipeline peptide" ,
             tabPanel("ProcessA",moduleAUI(ns('processA'))),
             tabPanel("ProcessB",moduleBUI(ns('processB'))),
             tabPanel("ProcessC",moduleCUI(ns('processC')))
             )
}



moduleGenericPipeline <- function(input, output, session, initData, navPage, indice, ll.process){
  ns <- session$ns
  
  rv <- reactiveValues(
    current.obj = NULL,
    indice = NULL,
    dataset = NULL
    )
  
 
  
  # observeEvent(req(ll.process()),{
  #   
  #   for (i in ll.process())
  #     {
  #     if (i != 'original') {
  #     source(file.path(".", paste0("modules/pipelines/",i,".R")), local = TRUE)$value
  #     }
  #   }
  # })
  
  
  
  WatchProcessA <- callModule(module=moduleA, 'processA', 
                         dataIn=reactive({rv$current.obj}),
                         screen.id = reactive({GetScreenId()}))
  
  observeEvent(WatchProcessA(),{
    print(paste0('observeEvent(ProcessA() : ', WatchProcessA()))
    rv$current.obj <- WatchProcessA()
    rv$indice <- which(ll.process() == 'processA')
    rv$dataset$processA <- WatchProcessA()
    #DeleteDatasetsAfter('ProcessA')
    # rv$dataset <- DeleteDatasetsAfterTest('ProcessA', rv$process, rv$dataset)
    printStatus()
  })
  
  
  WatchProcessB <- callModule(module=moduleB, 'processB', 
                         dataIn=reactive({rv$current.obj}),
                         screen.id = reactive({GetScreenId()}))
  
  observeEvent(WatchProcessB(),{
    print(paste0('observeEvent(ProcessB() : ', WatchProcessB()))
    rv$current.obj <- WatchProcessB()
    rv$indice <- which(ll.process() == 'processB')
    rv$dataset$processB <- WatchProcessB()
    #DeleteDatasetsAfter('ProcessB')
    # rv$dataset <- DeleteDatasetsAfterTest('ProcessB', rv$process, rv$dataset)
    printStatus()
  })
  
  WatchProcessC <- callModule(module=moduleC, 'processC', 
                         dataIn=reactive({rv$current.obj}),
                         screen.id = reactive({GetScreenId()}))
  
  observeEvent(WatchProcessC(),{
    print(paste0('observeEvent(ProcessA() : ', WatchProcessC()))
    rv$current.obj <- WatchProcessC()
    rv$indice <- which(ll.process() == 'processA')
    rv$dataset$processC <- WatchProcessC()
    #DeleteDatasetsAfter('ProcessC')
    # rv$dataset <- DeleteDatasetsAfterTest('ProcessC', rv$process, rv$dataset)
    printStatus()
  })
  
  
  ## Initialisation of the module
  observeEvent(initData(), {
    print("In module generic pipeline, observeEvent(initData()")
    print(initData())
    rv$dataset <- initData()
    rv$current.obj <- initData()[[indice()]]
    
    
    print(rv$dataset)
    print(rv$current.obj)
    
    printStatus()
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
    ## test if the navPage is one of a process one
    if (length(m) ==0 || length(n) ==0) {return(NULL)}

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

  
   
  
  
  printStatus <- function(){
    print("PrintStatus of module ")
    print(paste0('initData() = ',initData()))
    print(paste0('rv$indice= ',rv$indice))
    print(paste0('rv$current.obj= ',rv$current.obj))
    print(paste0('rv$datasetl= ',rv$dataset))
    #print(paste0('ProcessX = ',ProcessX()))
  }
  
  
  
  
  return(reactive({list(name = 'peptide',
                        indice=rv$indice,
                        dataset=rv$dataset)}))
  
}