
## Lorsqu'un dataset est charge, on met à jour la variable de travail
## On ne peut pas travailler directment sur la variable retour d'un module
observeEvent(req(rv.core$loadData()),{ 
  rv.core$current.obj <- rv.core$loadData()
})

################################################################################
#instanciation du premier dataset. 
# obj.openDataset <- callModule(module=moduleOpenDataset, 'moduleOpenDataset', selectedPanel = reactive({input$navPage}))
# #rv.core$current.obj <- callModule(module=moduleOpenDataset, 'moduleOpenDataset', selectedPanel = reactive({input$navPage}))

## Attente d'une valeur. Lorsqu'il y en a une, Initialization of the pipeline 
observe({
  print('------- DANS observe rv.core$current.obj --------------')
  req(rv.core$current.obj)
  
  print("rv.core$current.obj = ")
  rv.core$current.obj
  print("class(rv.core$current.obj) = ")
  print(class(rv.core$current.obj))

                          
  ## Sourcing the code for corresponding modules. le nom de l'item dans la liste
  ## doit correspondre au nom du fichier source prefixé par 'module' 
  # errors <- character()
  # for(p in processes(rv.core$current.obj)[-1]){
  #   path <- file.path("./src", paste0('modules/process/', pipelineType(rv.core$current.obj), "/",p, ".R"))
  #   if (!file.exists(path)) {
  #     msg <- paste0( path, ' was not found.')
  #     errors <- c(errors, msg)
  #   } else {
  #     source(path, local = TRUE)$value
  #   }
  # }
  # 
  
  #   
  #   print(paste0("IN observeEvent(req(obj()$initialData : ", rv.core$current.obj@pipeline))
  #   def <- name <- NULL
  #   type.pipeline <- rv.core$current.obj@pipeline
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
  # rv.core$current.indice <- 1
  # rv.core$current.obj <- obj.openDataset()
  # 
  # ## Lancement du module de navigation du pipeline pour suivre les différents process
  # ## de traitement liés au pipeline
  # rv.core$nav2 <- callModule(moduleNavigation2, "moduleGeneral",
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
#   req(rv.core$current.obj)
#   rv.core$current.obj@processes[[rv.core$current.indice]]
# })


###############################################################################
## Recupere le dataset (MSnset) courant
# GetCurrentMSnSet <- reactive({
#   req(rv.core$current.obj)
#   rv.core$current.obj@datasets[[rv.core$current.indice]]
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
#              cc = reactive({rv.core$current.obj@ConnexComp$allPep}),
#              matAdj = reactive({rv.core$current.obj@AdjacencyMat$matWithSharedPeptides}), 
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
#   req(rv.core$current.obj)
#   
#   screen <- NULL
#   m <-  which(names(rv.core$current.obj@datasets)==input$navPage)
#   n <-  which(unlist(lapply(GetCurrentMSnSet(), function(x) length(which(x==rv.core$current.obj@datasets))))==1)
#   ## test if the navPage is one of a process one
#   if (length(m) ==0 || length(n) ==0) {return(NULL)}
#   
#   if (m >= n) { screen <- 'Initial screen'}
#   else {screen <- 'Final screen'}
#   print(paste0("in GetScreenId(), n = ", n, ", m = ", m, ". screen = ", screen))
#   screen
# })



# DeleteDatasetsAfter <- function(txt){
#   names <- names(rv.core$current.obj@datasets)
#   indice <- which(names == txt)
#   if (indice < length(names)) {
#     for (i in (indice+1):length(names)){
#       rv.core$current.obj@datasets[i] <- list(NULL)
#     }
#   }
# }

# 
# output$header <- renderUI({
#   req(obj.openDataset())
#   req(rv.core$current.obj)
#   req(rv.core$current.indice)
#   
#   
#   tagList(
#     div(
#       div(
#         style="display:inline-block; vertical-align: middle; margin:0px",
#         rv.core$nav2()$bars
#       ),
#       div(
#         style="display:inline-block; vertical-align: middle; margin:0px",
#         p('Current dataset', style='color: white')
#       ),
#       div(
#         style="display:inline-block; vertical-align: middle; margin:0px",
#         selectInput('currentDataset', '',
#                     choices = names(rv.core$current.obj@datasets[!sapply(rv.core$current.obj@datasets,is.null)]),
#                     selected = names(rv.core$current.obj@datasets)[rv.core$current.indice],
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
#   n <- which(names(rv.core$current.obj@datasets)==input$currentDataset)
#   if (length(n)==0){
#     rv.core$current.indice <- 1
#   } else {
#     rv.core$current.indice <- n
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
#                               dataIn=reactive({rv.core$current.obj}),
#                               screen.id = reactive({GetScreenId()}))")
#   writeLines(txt, con)
#   
#   txt <- paste0(
#     "observeEvent(Watch",p,"(),{
#     print(paste0('observeEvent(",p,"() : ', Watch",p,"()))
#     rv.core$current.obj <- Watch",p,"()
#     rv.core$current.indice <- 1 + which(rv.core$ll.process == '",p,"')
#     rv.core$current.dataset$",p," <- Watch",p,"()
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