
## CHargement des modules generaux
source(file.path("./src", "modules/Plots/moduleLegendColoredExprs.R"), local = TRUE)$value
source(file.path("./src", "modules/moduleDescriptiveStats.R"), local = TRUE)$value
source(file.path("./src", "modules/Plots/moduleCC.R"),  local = TRUE)$value

source(file.path("./src", "modules/Menu_DataManager/moduleConvertData.R"),  local = TRUE)$value
source(file.path("./src", "modules/Menu_DataManager/moduleOpenMSnSet.R"),  local = TRUE)$value
source(file.path("./src", "modules/Menu_DataManager/moduleOpenDemoDataset.R"),  local = TRUE)$value





# rv.opendataset <- reactiveValues(
#   
#   ## variables temporaires servant à stocker les datasets qui sont charges par l'utilisateur
#   ## Ces variables servent de tampon entre la sortie des modules appelés et la variable
#   ## obj qui va récupérer le seul dataset chargé
#   tmp.convert = NULL,
#   tmp.demo = NULL,
#   tmp.file = NULL
# )
# 
# ## Chargement en memoire des structure renvoyees 
# rv.opendataset$tmp.convert <- callModule(module=moduleConvertData, 'moduleProcess_Convert')
# rv.opendataset$tmp.demo <- callModule(module=moduleOpenDemoDataset, 'moduleOpenDemoDataset')
# rv.opendataset$tmp.file <- callModule(module=moduleOpenMSnSet, 'moduleOpenMSnSet')
# 

# ## Mise a jour de la variable obj avec le dataset charge
# observe({
#   req(rv.opendataset$tmp.file())
#   rv.opendataset <- rv.opendataset$tmp.file()
# })
# 
# observe({
#   req(rv.opendataset$tmp.convert())
#   rv.opendataset <- rv.opendataset$tmp.convert()
# })
# 
# 
# observe({
#   req(rv.opendataset$tmp.demo())
#   rv.opendataset <- rv.opendataset$tmp.demo()
# })

################################################################################
#instanciation du premier dataset. 
#obj.openDataset <- callModule(module=moduleOpenDataset, 'moduleOpenDataset', selectedPanel = reactive({input$navPage}))
#pipeline$current.obj <- callModule(module=moduleOpenDataset, 'moduleOpenDataset', selectedPanel = reactive({input$navPage}))


## Lorsqu'un dataset est charge, On opere un certain nombre de traitements preparatifs
## au cas ou des informations manqueraient
## Lorsque c'est fait, on rend disponible le bouton de chargement
# observeEvent(req(rv.opendataset$obj),{
#   print("PASS dedans")
#   
#   ## Cette partie sur les compo
#   if ( pipelineType(rv.opendataset$obj) == 'peptide') {
#     if (length(rv.opendataset$obj@AdjacencyMat)==0){
#       rv.opendataset$obj@AdjacencyMat <- ComputeAdjacencyMatrices(rv.opendataset$obj@datasets[[1]])
#       pipeline$current.obj@ConnexComp <- ComputeConnexComposants(pipeline$current.obj@AdjacencyMat)
#     }
#   }
#   shinyjs::enable('btn_launch')
# })


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



observeEvent(req(pipeline$loadData()),{ 
  print('------- DANS observeEvent(req(loadData()) --------------')
  show(pipeline$loadData())
  pipeline$obj <- pipeline$loadData()
  show(pipeline$obj)
})

################################################################################
#instanciation du premier dataset. 
# obj.openDataset <- callModule(module=moduleOpenDataset, 'moduleOpenDataset', selectedPanel = reactive({input$navPage}))
# #pipeline$current.obj <- callModule(module=moduleOpenDataset, 'moduleOpenDataset', selectedPanel = reactive({input$navPage}))

## Attente d'une valeur. Lorsqu'il y en a une, Initialization of the pipeline 
observe({
  print('------- DANS observe pipeline$obj --------------')
  req(pipeline$obj)
  
  print("pipeline$obj = ")
  pipeline$obj
  print("class(pipeline$obj) = ")
  print(class(pipeline$obj))

                          
  ## Sourcing the code for corresponding modules. le nom de l'item dans la liste
  ## doit correspondre au nom du fichier source prefixé par 'module' 
  # errors <- character()
  # for(p in processes(pipeline$obj)[-1]){
  #   path <- file.path("./src", paste0('modules/process/', pipelineType(pipeline$obj), "/",p, ".R"))
  #   if (!file.exists(path)) {
  #     msg <- paste0( path, ' was not found.')
  #     errors <- c(errors, msg)
  #   } else {
  #     source(path, local = TRUE)$value
  #   }
  # }
  # 
  
  #   
  #   print(paste0("IN observeEvent(req(obj()$initialData : ", pipeline$obj@pipeline))
  #   def <- name <- NULL
  #   type.pipeline <- pipeline$obj@pipeline
  #   
  #   ## Get liste des process du pipeline
  #   def <- pipeline.def[[type.pipeline]]
  #   ## pour chaque process, on lance son observateur
  #   for (i in def) {
  #         source(file.path("src/WatchProcess",paste0("watch_",type.pipeline, "_", i, '.R')),  local = TRUE)$value
  #        }
  #    
  
  
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




###############################################################################
##  Recupere le nom du process courant
# GetCurrentProcess <- reactive({
#   req(pipeline$obj)
#   pipeline$obj@processes[[pipeline$current.indice]]
# })


###############################################################################
## Recupere le dataset (MSnset) courant
# GetCurrentMSnSet <- reactive({
#   req(pipeline$obj)
#   pipeline$obj@datasets[[pipeline$current.indice]]
#   })



# observeEvent(GetCurrentMSnSet(),{
#   callModule(module = modulePlots, 'showPlots', 
#              dataIn=reactive({list(obj = GetCurrentMSnSet(),
#                                    currentProcess = GetCurrentProcess())}), 
#              llPlots=reactive({lstDescPlots}),
#              settings = reactive({rv.prostar$settings}))
# })








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
# BuildDataminingMenu <- function(name){
#   
#   
#   callModule(moduleDescriptiveStats, "moduleDescrStats", 
#                                   dataIn=reactive({list(obj = GetCurrentMSnSet(),
#                                                         currentProcess = GetCurrentProcess())}))
#   
#   callModule(module = moduleCC, "CC_Multi_Any", 
#              cc = reactive({pipeline$obj@ConnexComp$allPep}),
#              matAdj = reactive({pipeline$obj@AdjacencyMat$matWithSharedPeptides}), 
#              dataIn = reactive({GetCurrentMSnSet()})
#              )
#                                    
#                                    
#     tabs <- list(
#     moduleDescriptiveStatsUI('moduleDescrStats'),
#     moduleCCUI('CC_Multi_Any')
#   )
#   
#   insertTab(inputId = "navPage",
#             do.call(navbarMenu, c(name ,tabs)),
#             target="Data manager",
#             position="after")
# }




# GetScreenId <- reactive({
#   input$navPage
#   req(pipeline$obj)
#   
#   screen <- NULL
#   m <-  which(names(pipeline$obj@datasets)==input$navPage)
#   n <-  which(unlist(lapply(GetCurrentMSnSet(), function(x) length(which(x==pipeline$obj@datasets))))==1)
#   ## test if the navPage is one of a process one
#   if (length(m) ==0 || length(n) ==0) {return(NULL)}
#   
#   if (m >= n) { screen <- 'Initial screen'}
#   else {screen <- 'Final screen'}
#   print(paste0("in GetScreenId(), n = ", n, ", m = ", m, ". screen = ", screen))
#   screen
# })



# DeleteDatasetsAfter <- function(txt){
#   names <- names(pipeline$obj@datasets)
#   indice <- which(names == txt)
#   if (indice < length(names)) {
#     for (i in (indice+1):length(names)){
#       pipeline$obj@datasets[i] <- list(NULL)
#     }
#   }
# }

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
    

## manual change of current dataset
# observeEvent(input$currentDataset,{
#   print('!!!!! Manual change of current dataset')
#   n <- which(names(pipeline$obj@datasets)==input$currentDataset)
#   if (length(n)==0){
#     pipeline$current.indice <- 1
#   } else {
#     pipeline$current.indice <- n
#   }
# })




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