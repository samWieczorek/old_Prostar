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
    current.obj = list(name='test', data=list(test=3)),
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
 
 
 
  observeEvent(input$selectPipeline,{
    print('EVENT ON : observeEvent(input$selectPipeline')
    
    switch(input$selectPipeline,
           Peptide= insertTab(inputId = "navPage",modulePipelinePepUI('test'), target="Home", position="after"),
           Protein = insertTab(inputId = "navPage",modulePipelineProtUI('testProt'), target="Home", position="after"),
           P2p = insertTab(inputId = "navPage",modulePipelineP2pUI('testP2p'), target="Home", position="after")
)
  })
  
  
  observeEvent(rv$pipeline$returnValPeptide(), {
    rv$dataset
    print('### EVENT ON : rv$pipeline$returnValPeptide()')
   # rvtmp$current.dataset <- rv$pipeline$returnValPeptide()
    rv$dataset$pipeline.pep <- rv$pipeline$returnValPeptide()
    print("new value from rv$pipeline$returnValPeptide()")
    print(rv$dataset$pipeline.pep)
    # updateSelectInput(session,  'currentDataset', selected =  dplyr::last(names(unlist(rv$dataset))))
   })
  
  #  observeEvent(rv$pipeline$returnValProtein(), {
  #    rv$dataset
  #    print('### EVENT ON : rv$pipeline$returnValProtein()')
  # #   rvtmp$current.obj <- rv$pipeline$returnValProtein()
  #    rv$dataset$pipeline.prot <- rv$pipeline$returnValProtein()
  #    print("new value from rv$pipeline$returnValProtein()")
  #    
  #    print(rv$pipeline$returnValProtein)
  # #   # updateSelectInput(session,  'currentDataset', selected =  dplyr::last(names(unlist(rv$dataset))))
  #  })
   
  #  observeEvent(rv$pipeline$returnValP2p(), {
  #    rv$dataset
  #    print('### EVENT ON : rv$pipeline$returnValP2p')
  # #   rvtmp$current.obj <- rv$pipeline$returnValP2p()
  #    rv$dataset$pipeline.p2p <- rv$pipeline$returnValP2p()
  #    print("new value from rv$pipeline$returnValP2p()")
  #    print(rv$pipeline$returnValP2p)
  # #   # updateSelectInput(session,  'currentDataset', selected =  dplyr::last(names(unlist(rv$dataset))))
  #  })
  
  
  
  output$chooseDataset <- renderUI({
    
    req(rvtmp$current.obj)
    div(
      div(
        style="display:inline-block; vertical-align: middle;",
        p("Current dataset")
      ),
      div(
        style="display:inline-block; vertical-align: middle;",
        selectInput('currentDataset', '', 
                    choices =  c("None"="None",names(unlist(rvtmp$current.obj$data))),
                    selected= rvtmp$current.obj$name,
                    width='150px')
      )
    )
    
    
  })
  
  
  
  
  output$summary <- renderUI({
   
    tagList(
      h3('General summary of prototype :'),
      p(paste0('rv$current.obj()= ',rvtmp$current.obj$name)),
      p(unlist(rvtmp$current.obj$data))
      )
  
  })
}
