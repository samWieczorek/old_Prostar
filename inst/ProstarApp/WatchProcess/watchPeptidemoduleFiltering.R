
########
#### Watch code#####

### Pour le module Filtering, comme on doit également mettre a jour la matrice d'adjacence et les composantes
### connexes, on les donne en parametre du module
####################
WatchmoduleFiltering <- callModule(module=moduleFiltering,'moduleFiltering',  
                                   dataIn=reactive({current.obj = GetCurrentMSnSet()}),
                                   screen.id = reactive({GetScreenId()}),
                                   settings = reactive({rv.prostar$settings()}))




# observeEvent(WatchmoduleFiltering(),{
#   print(paste0('observeEvent(moduleFiltering() : '))
#   pipeline$current.obj <- WatchmoduleFiltering()$current.obj
#   pipeline$AdjacencyMat <- WatchmoduleFiltering()$AdjacencyMat
#   pipeline$ConnexComp <- WatchmoduleFiltering()$ConnexComp
#   pipeline$current.indice <- 1 + which(pipeline$ll.process == 'moduleFiltering')
#   pipeline$current.dataset$moduleFiltering <- WatchmoduleFiltering()$current.obj
#   DeleteDatasetsAfter('moduleFiltering')
#   printStatus()
# })

######################################
#######################################

