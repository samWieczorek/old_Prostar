
## CHargement des modules generaux
source(file.path("./src", "modules/Plots/moduleLegendColoredExprs.R"), local = TRUE)$value
source(file.path("./src", "modules/Menu_DataManager/moduleOpenDataset.R"), local = TRUE)$value
source(file.path("./src", "modules/moduleDescriptiveStats.R"), local = TRUE)$value
source(file.path("./src", "modules/Plots/moduleCC.R"),  local = TRUE)$value



## definition des variables globales liees a un pipeline
pipeline <- reactiveValues(
  # current working data from current pipeline
  type = NULL,
  
  ## indice du dataset courant dans la liste ll.process.
  current.indice = 1,
  
  ## liste qui contiendra les noms des différents datasets enregsitres au cours 
  ## de l'execution sdu module. Il est initialisé à Original car dans tous les cas,
  ## on démarre avec un dataset intitule original
  ll.process = c('original'),
  
  # object returned by demode, openmode and convertmode
  #object that is used for modules in pipeline. C'st une instance d'une classe Pipeline
  current.obj = NULL,
  
  tempplot = NULL
  
)



###############################################################################
## Declaration des variables gloables su pipeline qui vont gerer la navigation 
## entre les differents modules de traitement dans le pipeline courant
# ###############################################################################
# rvNav <- reactiveValues(
#   Done = NULL,
#   def = list(name = NULL,
#              stepsNames = NULL,
#              isMandatory = NULL,
#              ll.UI = NULL
#              ),
#              rstFunc = reactive({resetNavPipeline()})
# )
# 
# resetNavPipeline <- reactive({  
#   
#   rvNav$Done <- NULL
#   rvNav$def <- list(name = NULL,
#                     stepsNames = NULL,
#                     isMandatory = NULL,
#                     ll.UI = NULL
#                   )
#   
# })



################################################################################
#instanciation du premier dataset. 
obj.openDataset <- callModule(module=moduleOpenDataset, 'moduleOpenDataset', selectedPanel = reactive({input$navPage}))
#pipeline$current.obj <- callModule(module=moduleOpenDataset, 'moduleOpenDataset', selectedPanel = reactive({input$navPage}))




###############################################################################
##  Recupere le nom du process courant
GetCurrentProcess <- reactive({
  req(pipeline$current.obj)
  pipeline$current.obj@processes[[pipeline$current.indice]]
})


###############################################################################
## Recupere le dataset (MSnset) courant
GetCurrentMSnSet <- reactive({
  req(pipeline$current.obj)
  pipeline$current.obj@datasets[[pipeline$current.indice]]
  })


###############################################################################
## Recupere la matric d'adjacence du pipeline courant
GetAdjacencyMatrix <- reactive({
  req(pipeline$current.obj)
  getAdjacencyMatrix(pipeline$current.obj)
  })

###############################################################################
## Recupere les composantes connexes du pipeline courant
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



## Attente d'une valeur. Lorsqu'il y en a une, Initialization of the pipeline 
observeEvent(req(obj.openDataset()),{
  
  print(paste0("IN observeEvent(req(obj()$initialData : ", obj.openDataset()@pipeline))
  def <- name <- NULL
  type.pipeline <- obj.openDataset()@pipeline
  
  ## Get liste des process du pipeline
  def <- pipeline.def[[type.pipeline]]
  ## pour chaque process, on lance son observateur
  for (i in def) {
        source(file.path("src/WatchProcess",paste0("watch_",type.pipeline, "_", i, '.R')),  local = TRUE)$value
       }
   
  
  
  # BuildSidebarMenu()
  # # Load UI code for modules
  # rvNav$Done = rep(FALSE,length(def))
  # rvNav$def = list(name = type.pipeline,
  #                  stepsNames = def,
  #                  isMandatory = rep(TRUE,length(def)),
  #                  ll.UI = LoadModulesUI(def)
  #                  )
  # 
  # pipeline$current.indice <- 1
  # pipeline$current.obj <- obj.openDataset()
  # 
  # ## Lancement du module de navigation du pipeline pour suivre les différents process
  # ## de traitement liés au pipeline
  # pipeline$nav2 <- callModule(moduleNavigation2, "moduleGeneral",
  #                             isDone = reactive({rvNav$Done}),
  #                             pages = reactive({rvNav$def}),
  #                             rstFunc = resetNavPipeline,
  #                             type = reactive({'rectangle'})
  #                             )
  #BuildDataminingMenu("Data mining")
})





# LoadModulesUI <- function(ll.modules){
#   ll <- lapply(ll.modules, function(i) {
#     UIfunc <- paste0(i, "UI")
#     do.call(UIfunc, list(i))
#     })
#   
#   return(ll)
# }




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

# 
# output$header <- renderUI({
#   req(obj.openDataset())
#   req(pipeline$current.obj)
#   req(pipeline$current.indice)
#   
#   
#   tagList(
#     div(
#       div(
#         style="display:inline-block; vertical-align: middle; margin:0px",
#         pipeline$nav2()$bars
#       ),
#       div(
#         style="display:inline-block; vertical-align: middle; margin:0px",
#         p('Current dataset', style='color: white')
#       ),
#       div(
#         style="display:inline-block; vertical-align: middle; margin:0px",
#         selectInput('currentDataset', '',
#                     choices = names(pipeline$current.obj@datasets[!sapply(pipeline$current.obj@datasets,is.null)]),
#                     selected = names(pipeline$current.obj@datasets)[pipeline$current.indice],
#                     width='200px')
#       )
#     )
#   )
#  
# })
#     
    

output$UI_dataAnalysis <- renderUI({
  obj.openDataset()
  
  if (!is.null(obj.openDataset()) && !is.null(pipeline$nav2())){
   pipeline$nav2()$screens
   }
  
})



# output$menuItem_dataAnalysis <- renderMenu({
#   
#   if (!is.null(obj.openDataset()) && !is.null(pipeline$nav2())){
#     menuItem("Data analysis", tabName = "dataAnalysis")
#   }
# })


output$btn_launch <- renderUI({
  obj.openDataset()
  
  if (!is.null(obj.openDataset())){

    updateTabItems(session, "sidebar_left", 'dataAnalysis')
    
    

  } else {
  #moduleOpenDatasetUI("moduleOpenDataset")
}
})




## manual change of current dataset
observeEvent(input$currentDataset,{
  print('!!!!! Manual change of current dataset')
  n <- which(names(pipeline$current.obj@datasets)==input$currentDataset)
  if (length(n)==0){
    pipeline$current.indice <- 1
  } else {
    pipeline$current.indice <- n
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