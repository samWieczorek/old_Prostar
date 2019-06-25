source(file.path(".", "Classes/ClassPepPipeline.R"), local = TRUE)$value
source(file.path(".", "Classes/ClassProtPipeline.R"), local = TRUE)$value


source(file.path(".", "modules/Plots/moduleLegendColoredExprs.R"), local = TRUE)$value


source(file.path(".", "modules/process/peptide/moduleFiltering.R"), local = TRUE)$value
source(file.path(".", "modules/process/peptide/moduleNormalization.R"), local = TRUE)$value
source(file.path(".", "modules/process/peptide/modulePepImputation.R"), local = TRUE)$value
source(file.path(".", "modules/process/peptide/moduleHypothesisTest.R"), local = TRUE)$value

source(file.path(".", "modules/process/protein/moduleProtFiltering.R"), local = TRUE)$value
source(file.path(".", "modules/process/protein/moduleProtNormalization.R"), local = TRUE)$value
source(file.path(".", "modules/process/protein/moduleProtImputation.R"), local = TRUE)$value
source(file.path(".", "modules/process/protein/moduleProtHypothesisTest.R"), local = TRUE)$value

source(file.path(".", "modules/process/p2p/moduleH.R"), local = TRUE)$value
source(file.path(".", "modules/process/p2p/moduleI.R"), local = TRUE)$value

source(file.path(".", "modules/DataManager/moduleOpenDataset.R"), local = TRUE)$value
source(file.path(".", "modules/moduleDescriptiveStats.R"), local = TRUE)$value
source(file.path(".", "modules/Plots/moduleCC.R"),  local = TRUE)$value
source(file.path(".", "modules/moduleNavigation2.R"),  local = TRUE)$value



pipeline <- reactiveValues(
  # current working data from current pipeline
  type = NULL,
  current.indice = 1,
  ll.process = c('original'),
  
  # object returned by demode, openmode and convertmode
  #object that is used for modules in pipeline
  current.obj = NULL,
  tempplot = NULL,
  nav2 =NULL
  
)



###############################################################################

rvNav <- reactiveValues(
  Done = NULL,
  def = list(name = NULL,
             stepsNames = NULL,
             isMandatory = NULL,
             ll.UI = NULL
             ),
             rstFunc = reactive({resetNavPipeline()})
)

resetNavPipeline <- reactive({  
  
  rvNav$Done <- NULL
  rvNav$def <- list(name = NULL,
                    stepsNames = NULL,
                    isMandatory = NULL,
                    ll.UI = NULL
                  )
  
})



################################################################################

obj.openDataset <- callModule(module=moduleOpenDataset, 'moduleOpenDataset', selectedPanel = reactive({input$navPage}))




GetCurrentProcess <- reactive({
  req(pipeline$current.obj)
  pipeline$current.obj@ll.process[[pipeline$current.indice]]
})

GetCurrentMSnSet <- reactive({
  req(pipeline$current.obj)
  pipeline$current.obj@datasets[[pipeline$current.indice]]
  })

GetAdjacencyMatrix <- reactive({
  req(pipeline$current.obj)
  getAdjacencyMatrix(pipeline$current.obj)
  })

GetConnexComposant <- reactive({ 
  req(pipeline$current.obj)
  getConnexComp(pipeline$current.obj)
  })


observeEvent(GetCurrentMSnSet(),{
  callModule(module = modulePlots, 'showPlots', 
             dataIn=reactive({list(obj = GetCurrentMSnSet(),
                                   currentProcess = GetCurrentProcess())}), 
             llPlots=reactive({lstDescPlots}),
             settings = reactive({rv.prostar$settings}))
})



GetCurrentObjName <- reactive({
  req(pipeline$current.obj)
  pipeline$current.obj@name.dataset
  })



## Initialization of the pipeline
observeEvent(req(obj.openDataset()),{
  
  print(paste0("IN observeEvent(req(obj()$initialData : ", obj.openDataset()@pipeline))
  def <- name <- NULL
  type.pipeline <- obj.openDataset()@pipeline
  def <- pipeline.def[[type.pipeline]]
  for (i in def) {
        source(file.path("WatchProcess",paste0("watch_",type.pipeline, "_", i, '.R')),  local = TRUE)$value
       }
   
  
  # Load UI code for modules
  rvNav$Done = rep(FALSE,length(def))
  rvNav$def = list(name = type.pipeline,
                   stepsNames = def,
                   isMandatory = rep(TRUE,length(def)),
                   ll.UI = LoadModulesUI(def)
                   )
  
  pipeline$current.indice <- 1
  pipeline$current.obj <- obj.openDataset()
  
  pipeline$nav2 <- callModule(moduleNavigation2, "moduleGeneral",
                              isDone = reactive({rvNav$Done}),
                              pages = reactive({rvNav$def}),
                              rstFunc = resetNavPipeline,
                              type = reactive({'rectangle'})
                              )
  #BuildDataminingMenu("Data mining")
})





LoadModulesUI <- function(ll.modules){
  ll <- lapply(ll.modules, function(i) {
    UIfunc <- paste0(i, "UI")
    do.call(UIfunc, list(i))
    })
  
  return(ll)
}




####
### Construit le menu pour le datamining. Menu cosntruit en dur car pas nécessaire de la faire de 
## manière dynamique
###
BuildDataminingMenu <- function(name){
  
  
  callModule(moduleDescriptiveStats, "moduleDescrStats", 
                                  dataIn=reactive({list(obj = GetCurrentMSnSet(),
                                                        currentProcess = GetCurrentProcess())}))
  
  callModule(module = moduleCC, "CC_Multi_Any", 
             cc = reactive({pipeline$current.obj@ConnexComp$allPep}),
             matAdj = reactive({pipeline$current.obj@AdjacencyMat$matWithSharedPeptides}), 
             dataIn = reactive({GetCurrentMSnSet()})
             )
                                   
                                   
    tabs <- list(
    moduleDescriptiveStatsUI('moduleDescrStats'),
    moduleCCUI('CC_Multi_Any')
  )
  
  insertTab(inputId = "navPage",
            do.call(navbarMenu, c(name ,tabs)),
            target="Data manager",
            position="after")
}




GetScreenId <- reactive({
  input$navPage
  req(pipeline$current.obj)
  
  screen <- NULL
  m <-  which(names(pipeline$current.obj@datasets)==input$navPage)
  n <-  which(unlist(lapply(GetCurrentMSnSet(), function(x) length(which(x==pipeline$current.obj@datasets))))==1)
  ## test if the navPage is one of a process one
  if (length(m) ==0 || length(n) ==0) {return(NULL)}
  
  if (m >= n) { screen <- 'Initial screen'}
  else {screen <- 'Final screen'}
  print(paste0("in GetScreenId(), n = ", n, ", m = ", m, ". screen = ", screen))
  screen
})



DeleteDatasetsAfter <- function(txt){
  names <- names(pipeline$current.obj@datasets)
  indice <- which(names == txt)
  if (indice < length(names)) {
    for (i in (indice+1):length(names)){
      pipeline$current.obj@datasets[i] <- list(NULL)
    }
  }
}


output$header <- renderUI({
  req(obj.openDataset())
  req(pipeline$current.obj)
  req(pipeline$current.indice)
  
  
  tagList(
    div(
      div(
        style="display:inline-block; vertical-align: middle; margin:0px",
        pipeline$nav2()$bars
      ),
      div(
        style="display:inline-block; vertical-align: middle; margin:0px",
        p('Current dataset', style='color: white')
      ),
      div(
        style="display:inline-block; vertical-align: middle; margin:0px",
        selectInput('currentDataset', '',
                    choices = names(pipeline$current.obj@datasets[!sapply(pipeline$current.obj@datasets,is.null)]),
                    selected = names(pipeline$current.obj@datasets)[pipeline$current.indice],
                    width='200px')
      )
    )
  )
 
})
    
    

output$UI_dataAnalysis <- renderUI({
  obj.openDataset()
  
  if (!is.null(obj.openDataset()) && !is.null(pipeline$nav2())){
   pipeline$nav2()$screens
   }
  
})



output$menuItem_dataAnalysis <- renderMenu({
  
  if (!is.null(obj.openDataset()) && !is.null(pipeline$nav2())){
    menuItem("Data analysis", tabName = "dataAnalysis")
  }
})


output$btn_launch <- renderUI({
  obj.openDataset()
  
  if (!is.null(obj.openDataset())){

    updateTabItems(session, "sidebar_left", 'dataAnalysis')
    
    

  } else {
  #moduleOpenDatasetUI("moduleOpenDataset")
}
})


##################
# This function is used to generate source code files qui creent 
# les observateurs pour les modules de process
##################
# createWatchCode <- function(process){
#   #tempfile(fileext=".R")
#   filename <- 'watch.R'
#   con <- file(filename,open="wt")
#   
#   
#   for (p in process){
#   txt <- paste0("Watch",p," <- callModule(module=",p,",'", p,"',  
#                               dataIn=reactive({pipeline$current.obj}),
#                               screen.id = reactive({GetScreenId()}))")
#   writeLines(txt, con)
#   
#   txt <- paste0(
#     "observeEvent(Watch",p,"(),{
#     print(paste0('observeEvent(",p,"() : ', Watch",p,"()))
#     pipeline$current.obj <- Watch",p,"()
#     pipeline$current.indice <- 1 + which(pipeline$ll.process == '",p,"')
#     pipeline$current.dataset$",p," <- Watch",p,"()
#     DeleteDatasetsAfter('",p,"')
#     printStatus()
#   })")
#   
#   writeLines(txt, con)
#   
#   }
#   
#   close(con)
#   
#   return(filename)
#   
# }