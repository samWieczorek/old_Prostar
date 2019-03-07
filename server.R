

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
  
  
  source(file.path(".", "moduleA.R"),  local = TRUE)$value
  source(file.path(".", "moduleB.R"),  local = TRUE)$value
  source(file.path(".", "moduleC.R"),  local = TRUE)$value
  source(file.path(".", "modulePlots.R"),  local = TRUE)$value
  
  # GetProcNames <- reactive({
  #   names(as.vector(pipeline_pep))
  # })
  # 
  
  ## 
  pipeline.pep <- reactiveValues(
    dataset = list(
      original = 10,
      A_processed = NULL,
      B_processed = NULL,
      C_processed = NULL
      ),
  
  process = list(
      processNull = NULL,
      ProcessA = callModule(module=moduleA, 'processA', 
                          dataIn=reactive({rv$current.obj}),
                          screen.id = reactive({GetScreenId()})),
      ProcessB  = callModule(module=moduleB, 'processB', 
                            dataIn=reactive({rv$current.obj}),
                            screen.id = reactive({GetScreenId()})),
    
      ProcessC =callModule(module=moduleC, 'processC', 
                           dataIn=reactive({rv$current.obj}),
                           screen.id = reactive({GetScreenId()}))
    )
  )
  
  
  rv <- reactiveValues(
    obj = 10,
    current.obj = 10,
    returnVal = NULL
    )
  
  
  GetScreenId <- reactive({
    input$navPage
    rv$current.obj
    
    screen <- 'I'
    
    m <-  which(names(pipeline.pep$process)==input$navPage)
    n <-  which(unlist(lapply(pipeline.pep$dataset, function(x) length(which(x==rv$current.obj))))==1)
      
    if (m >= n) { screen <- 'I'}
    else {screen <- 'F'}
    print(paste0("in GetScreenId, n = ", n, ", m = ", m, ". screen = ", screen))
    screen
  })
  
  output$chooseDataset <- renderUI({

    req(pipeline.pep$dataset)

    selectInput('currentDataset', 'Choose current dataset', 
                choices =  names(unlist(pipeline.pep$dataset)),
                width='150px')
  })
  
  
  observeEvent(req(input$currentDataset), {
    rv$current.obj <- pipeline.pep$dataset[[input$currentDataset]]
  })

  # Delete <- reactive({
  #   pipeline.pep$dataset$A_processed <- NULL
  #   pipeline.pep$dataset$A_processed
  # })
  
  observeEvent(input$rst_process, {
    print("In reset process event")
    #pipeline.pep$dataset$A_processed <- Delete()
    #pipeline.pep$dataset$B_processed <- reactive({NULL})
    #pipeline.pep$dataset$C_processed <- NULL
  })
  
  
  observeEvent(rv$obj, {
    print('maj :  pipeline.pep$dataset$original <- rv$obj')
    #print(paste0("rv$returnVal() = ", rv$returnVal()))
    pipeline.pep$dataset$original <- rv$obj
    rv$current.obj <- pipeline.pep$dataset$original
  })
  
  
  
  DeleteDatasetsAfter <- function(txt){
    indice <- which(names(pipeline.pep$process) == txt)
    if (indice < length(names(pipeline.pep$process))) {
      for (i in (indice+1):length(names(pipeline.pep$process))){
        pipeline.pep$dataset[i] <- list(NULL)
      }
    }
  }
  
  observeEvent(pipeline.pep$process$ProcessA(), {
    pipeline.pep$dataset
    print('### EVENT ON : pipeline.pep$dataset$A_processed <- rv$obj')
    rv$current.obj <- pipeline.pep$process$ProcessA()
    pipeline.pep$dataset$A_processed <- pipeline.pep$process$ProcessA()
    
    DeleteDatasetsAfter('ProcessA')
    
    updateSelectInput(session,  'currentDataset', selected =  last(names(unlist(pipeline.pep$dataset))))
    
    
  })

  observeEvent(pipeline.pep$process$ProcessB(), {
    pipeline.pep$dataset
    print('### EVENT ON : pipeline.pep$dataset$B_processed <- rv$obj')
    rv$current.obj <- pipeline.pep$process$ProcessB()
    
    pipeline.pep$dataset$B_processed <- pipeline.pep$process$ProcessB()
    
    DeleteDatasetsAfter('ProcessB')
    
    updateSelectInput(session,  'currentDataset', selected =  last(names(unlist(pipeline.pep$dataset))))
    
  })
  
  

  observeEvent(pipeline.pep$process$ProcessC(), {
    pipeline.pep$dataset
    print('### EVENT ON : pipeline.pep$dataset$C_processed <- rv$obj')
    rv$current.obj <- pipeline.pep$process$ProcessC()
    
    pipeline.pep$dataset$C_processed <- pipeline.pep$process$ProcessC()
    
    DeleteDatasetsAfter('ProcessC')
    
    updateSelectInput(session,  'currentDataset', selected =  last(names(unlist(pipeline.pep$dataset))))
    
  })
 
  
  output$summary <- renderUI({
   
    tagList(
      p(paste0('rv$obj =',rv$obj)),
      p(paste0('rv$current.obj()= ',rv$current.obj)),
      p(paste0('rv$returnVal()$name = ',rv$returnVal)),
      #p(paste0('rv$returnVal()$res = ',rv$returnVal()$res))
      p(paste0('pipeline.pep$dataset$original= ',pipeline.pep$dataset$original)),
      p(paste0('pipeline.pep$dataset$A_processed= ',pipeline.pep$dataset$A_processed)),
      p(paste0('pipeline.pep$dataset$B_processed= ',pipeline.pep$dataset$B_processed)),
      p(paste0('pipeline.pep$dataset$C_processed= ',pipeline.pep$dataset$C_processed))
    )
  
  })
}
