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
  
  source(file.path(".", "modulePipelinePepUI.R"),  local = TRUE)$value
  source(file.path(".", "modulePipelinePep.R"),  local = TRUE)$value
  source(file.path(".", "modulePipelineProtUI.R"),  local = TRUE)$value
  source(file.path(".", "modulePipelineProt.R"),  local = TRUE)$value
  source(file.path(".", "modulePipelineP2pUI.R"),  local = TRUE)$value
  source(file.path(".", "modulePipelineP2p.R"),  local = TRUE)$value
  
 
  rvtmp <- reactiveValues(
    init.obj = 10,
    current.obj = 3,
    page = NULL
  )
  rv <- reactiveValues(
    
    dataset = list(
      original = NULL,
      pipeline.pep = NULL,
      pipeline.prot = NULL,
      pipeline.p2p = NULL
    ),
    
    pipeline = list(
      processNull = NULL,
      returnValPeptide = callModule(module = modulePipelinePep, 'test', 
                                    dataIn = reactive({rvtmp$current.obj}), 
                                    navPage=reactive({input$navPage})),
      returnValProtein  = NULL,
      returnValP2p = NULL
    )
  )
    
  
  
 callModule(module = modulePlots, 'showPlots', dataIn=reactive({rvtmp$current.obj}))
 observe({
   req(input$navPage)
   print(input$navPage)
   #rv$pipeline$returnValPeptide = callModule(module = modulePipelinePep, 'test', 
   #                            dataIn = reactive({rvtmp$current.obj}), 
   #                            navPage=reactive({input$navPage}))
 # returnValProtein  = callModule(module = modulePipelineProt, 'test', 
 #                                dataIn = reactive({rvtmp$current.obj}), 
 #                                navPage=reactive({input$navPage}))
 # 
 # returnValP2p = callModule(module = modulePipelineP2p, 'test', 
 #                           dataIn = reactive({rvtmp$current.obj}), 
 #                           navPage=reactive({input$navPage}))
 
 })
 
  # observe({
  #   req(rv$init.obj)
  #   rv$current.obj <- rv$init.obj
  #   })
  # 
  
  # observeEvent(req(input$currentDataset), {
  #   rv$current.obj <- pipeline.pep$dataset[[input$currentDataset]]
  # })

 
  observeEvent(rvtmp$init.obj,{
    print('maj :  pipeline.pep$dataset$original <- rv$init.obj')
    
    prependTab(inputId = "navPage",modulePipelinePepUI('test'))

     # insertTab(inputId = "navPage",modulePipelineProtUI('testProt'),
     #            target="Pipeline peptide",position="after")
     # 
     # insertTab(inputId = "navPage", modulePipelineProtUI('testP2p'),
     #           target="Pipeline protein",position="after")
    
  })
  
  
  observeEvent(rv$pipeline$returnValPeptide(), {
    rv$dataset
    print('### EVENT ON : rv$dataset$A_processed <- rv$obj')
    rvtmp$current.obj <- rv$pipeline$returnValPeptide()
    rv$dataset$pipeline.pep <- rv$pipeline$returnValPeptide()
    # updateSelectInput(session,  'currentDataset', selected =  dplyr::last(names(unlist(rv$dataset))))
   })
  
  # observeEvent(rv$pipeline$returnValProtein(), {
  #   rv$dataset
  #   print('### EVENT ON : rv$dataset$A_processed <- rv$obj')
  #   rvtmp$current.obj <- rv$pipeline$returnValProtein()
  #   rv$dataset$pipeline.prot <- rv$pipeline$returnValProtein()
  #   # updateSelectInput(session,  'currentDataset', selected =  dplyr::last(names(unlist(rv$dataset))))
  # })
  # 
  # observeEvent(rv$pipeline$returnValP2p(), {
  #   rv$dataset
  #   print('### EVENT ON : rv$dataset$A_processed <- rv$obj')
  #   rvtmp$current.obj <- rv$pipeline$returnValP2p()
  #   rv$dataset$pipeline.p2p <- rv$pipeline$returnValP2p()
  #   # updateSelectInput(session,  'currentDataset', selected =  dplyr::last(names(unlist(rv$dataset))))
  # })
  
  output$summary <- renderUI({
   
    tagList(
      h3('General summary'),
      p(paste0('rv$obj =',rvtmp$init.obj)),
      p(paste0('rv$current.obj()= ',rvtmp$current.obj)),
      p(paste0('rv$pipeline$returnValPipelinePeptide() = ',rv$pipeline$returnValPeptide()))
      #p(paste0('rv$returnValPipelineProtein() = ',rv$returnValPipelineProtein())),
     # p(paste0('rv$returnValPipelineP2p() = ',rv$returnValPipelineP2p()))
    )
  
  })
}
