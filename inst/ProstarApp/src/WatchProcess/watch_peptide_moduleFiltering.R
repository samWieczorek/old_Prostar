
########
#### Watch code#####

### Pour le module Filtering, comme on doit également mettre a jour la matrice d'adjacence et les composantes
### connexes, on les donne en parametre du module
####################
WatchmoduleFiltering <- callModule(module=moduleFiltering,'moduleFiltering',  
                                   dataIn=reactive({current.obj = GetCurrentMSnSet()}),
                                   screen.id = reactive({GetScreenId()}),
                                   settings = reactive({rv.prostar$settings()}))




observeEvent(req(WatchmoduleFiltering()),{
  
  rv.core$current.obj@datasets$moduleFiltering <- WatchmoduleFiltering()$obj
  rv.core$current.obj@AdjacencyMat <- WatchmoduleFiltering()$AdjacencyMat
  rv.core$current.obj@ConnexComp <- WatchmoduleFiltering()$ConnexComp
  
  ## doit avoir le meme nom que celui qui est indique dans la definition des modules pour le pipeline
  ## (fichier pipelineDefinition.R)
  rv.core$current.indice <- which(rv.core$current.obj@processes == 'moduleFiltering')
  DeleteDatasetsAfter('moduleFiltering')
  rvNav$Done[rv.core$current.indice-1] <- TRUE
  
})

######################################
#######################################

