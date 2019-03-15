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



moduleGenericPipeline <- function(input, output, session, initData, navPage, indice){
  ns <- session$ns
  
  
 
  rv <- reactiveValues(
    current.obj = NULL,
    indice = NULL,
    dataset = list(original=NULL,
                   A_processed = NULL,
                   B_processed = NULL,
                   C_processed = NULL),
    ProcessX = callModule(module=moduleA, 'processA', 
                           dataIn=reactive({4}),
                           screen.id = reactive({'Final screen'}))
   )
  
 
  
  
  
  
  
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
  
  
  
  # observeEvent(req(indice()),{
  #   
  #   print(paste0("Change of current dataset in pipeline :", indice()))
  #   rv$indice <- indice()
  #   rv$current.obj <- rv$dataset[[rv$indice]]
  #   
  # })
  
  
  # GetScreenId <- reactive({
  #   navPage()
  #   req(rv$current.obj)
  #   
  #   screen <- NULL
  #   m <-  which(names(rv$process)==navPage())
  #   n <-  which(unlist(lapply(rv$dataset, function(x) length(which(x==rv$current.obj))))==1)
  #   ## test if the navPage is one of a process one
  #   if (length(m) ==0 || length(n) ==0) {return(NULL)}
  #   
  #   if (m >= n) { screen <- 'Initial screen'}
  #   else {screen <- 'Final screen'}
  #   print(paste0("in GetScreenId(), n = ", n, ", m = ", m, ". screen = ", screen))
  #   screen
  # })
  
  
  
  # DeleteDatasetsAfter <- function(txt){
  #   indice <- which(names(rv$process) == txt)
  #   if (indice < length(names(rv$process))) {
  #     for (i in (indice+1):length(names(rv$process))){
  #       rv$dataset[i] <- list(NULL)
  #     }
  #   }
  # }
  # 
  
  # observeEvent(rv$ProcessX,{
  #   print(paste0('toto', ProcessX()))
  # })
  # observe({
  #   ProcessX()
  #   print('### EVENT ON : ProcessX()')
  #   print(ProcessX())
  #   #   rv$current.obj <- rv$process$ProcessA()
  #   # rv$indice <- 2
  #   # rv$dataset$A_processed <- rv$process$ProcessA
  #   # #DeleteDatasetsAfter('ProcessA')
  #   # rv$dataset <- DeleteDatasetsAfterTest('ProcessA', rv$process, rv$dataset)
  #   # 
  #   # printStatus()
  # })
  
  # observeEvent(rv$process$ProcessB(), {
  #   print('### EVENT ON : rv$dataset$B_processed <- rv$obj')
  #   rv$current.obj <- rv$process$ProcessB()
  #   rv$indice <- 3
  #   rv$dataset$B_processed <- rv$process$ProcessB()
  #   #DeleteDatasetsAfter('ProcessB')
  #   rv$dataset <- DeleteDatasetsAfterTest('ProcessB', rv$process, rv$dataset)
  #   
  #   printStatus()
  # })
  # 
  
  
  # observeEvent(rv$process$ProcessC(), {
  #   print('### EVENT ON : rv$dataset$C_processed <- rv$obj')
  #   rv$current.obj <- rv$process$ProcessC()
  #   rv$indice <- 4
  #   rv$dataset$C_processed <- rv$process$ProcessC()
  #   #DeleteDatasetsAfter('ProcessC')
  #   rv$dataset <- DeleteDatasetsAfterTest('ProcessC', rv$process, rv$dataset)
  #   
  #   printStatus()
  # })
  
  
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