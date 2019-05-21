source(file.path(".", "Classes/ClassPepPipeline.R"), local = TRUE)$value


source(file.path(".", "modules/Plots/moduleLegendColoredExprs.R"), local = TRUE)$value


source(file.path(".", "modules/process/peptide/moduleFiltering.R"), local = TRUE)$value
source(file.path(".", "modules/process/peptide/moduleNormalization.R"), local = TRUE)$value
source(file.path(".", "modules/process/peptide/modulePepImputation.R"), local = TRUE)$value
source(file.path(".", "modules/process/peptide/moduleHypothesisTest.R"), local = TRUE)$value

source(file.path(".", "modules/process/protein/moduleD.R"), local = TRUE)$value
source(file.path(".", "modules/process/protein/moduleE.R"), local = TRUE)$value
source(file.path(".", "modules/process/protein/moduleF.R"), local = TRUE)$value
source(file.path(".", "modules/process/protein/moduleG.R"), local = TRUE)$value

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



output$trtr <- renderUI({
  tags$p('tyty')
})

## Initialization of the pipeline
observeEvent(req(obj.openDataset()),{
  print(paste0("IN observeEvent(req(obj()$initialData : ", obj.openDataset()@pipeline))
  #print(str(obj.demomode()))
  def <- name <- NULL
  
  switch(obj.openDataset()@pipeline,
         
         peptide = {
           def <- peptide.def
           name <- "Peptide pipeline"
           
           for (i in peptide.def) {
             print(paste0('source file :', "watchPeptide", i, '.R'))
             source(file.path("WatchProcess",paste0("watchPeptide", i, '.R')),  local = TRUE)$value
           }
           
           },
         Protein = {
           #pipeline$current.obj@ll.process <- protein.def
           #LoadModulesUI(path2proteinModules, protein.def)
           BuildPipelineMenu("Pipeline protein", protein.def)
           
           #codeFile <- createWatchCode(protein.def)
           #source(file.path(".", codeFile),  local = TRUE)$value
           
           for (i in protein.def) {
             print(paste0('source file :', "watchProtein", i, '.R'))
             source(file.path("WatchProcess",paste0("watchProtein", i, '.R')),  local = TRUE)$value
           }
         },
         P2p = {
           #pipeline$current.obj@ll.process <- p2p.def
           #LoadModulesUI(path2p2pModules, p2p.def)
           BuildPipelineMenu("Pipeline p2p", p2p.def)
           
           #codeFile <- createWatchCode(p2p.def)
           #source(file.path(".", codeFile),  local = TRUE)$value
           
           for (i in p2p.def) {
             print(paste0('source file :', "watchP2p", i, '.R'))
             source(file.path("WatchProcess",paste0("watchP2p", i, '.R')),  local = TRUE)$value
           }
         }
  )
  
  # Load UI code for modules
  rvNav$Done = rep(FALSE,length(def))
  rvNav$def = list(name = name,
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
             type = reactive({'rectangle'}))
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
                    width='150px')
      )
    )
  )
 
})
    
    
output$btn_launch <- renderUI({
  obj.openDataset()
  
  if (!is.null(obj.openDataset())){
    #moduleNavigationUI("moduleGeneral")
    #tagList(
      pipeline$nav2()$screens
    #)
  } else {
  moduleOpenDatasetUI("moduleOpenDataset")
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