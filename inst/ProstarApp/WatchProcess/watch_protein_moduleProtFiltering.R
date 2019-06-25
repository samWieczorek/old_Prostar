
########
#### Watch code#####

### Pour le module Filtering, comme on doit également mettre a jour la matrice d'adjacence et les composantes
### connexes, on les donne en parametre du module
####################
WatchmoduleProtFiltering <- callModule(module=moduleProtFiltering,'moduleProtFiltering',  
                                   dataIn=reactive({current.obj = GetCurrentMSnSet()}),
                                   screen.id = reactive({GetScreenId()}),
                                   settings = reactive({rv.prostar$settings()}))




observeEvent(req(WatchmoduleProtFiltering()),{
  
  pipeline$current.obj@datasets$moduleProtFiltering <- WatchmoduleProtFiltering()$obj
  pipeline$current.obj@AdjacencyMat <- WatchmoduleProtFiltering()$AdjacencyMat
  pipeline$current.obj@ConnexComp <- WatchmoduleProtFiltering()$ConnexComp
  
  ## doit avoir le meme nom que celui qui est indique dans la definition des modules pour le pipeline
  ## (fichier pipelineDefinition.R)
  pipeline$current.indice <- which(pipeline$current.obj@ll.process == 'moduleProtFiltering')
  DeleteDatasetsAfter('moduleProtFiltering')
  rvNav$Done[pipeline$current.indice-1] <- TRUE
  
})

######################################
#######################################

