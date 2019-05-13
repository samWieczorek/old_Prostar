
########
#### Watch code#####

### Pour le module Filtering, comme on doit également mettre a jour la matrice d'adjacence et les composantes
### connexes, on les donne en parametre du module
####################
WatchmoduleFiltering <- callModule(module=moduleFiltering,'moduleFiltering',  
                                   dataIn=reactive({current.obj = GetCurrentMSnSet()}),
                                   screen.id = reactive({GetScreenId()}),
                                   settings = reactive({rv.prostar$settings()}))




observeEvent(WatchmoduleFiltering(),{
  pipeline$current.obj@datasets$moduleFiltering <- WatchmoduleFiltering()$obj
  pipeline$current.obj@AdjacencyMat <- WatchmoduleFiltering()$AdjacencyMat
  pipeline$current.obj@ConnexComp <- WatchmoduleFiltering()$ConnexComp
  
  ## doit avoir le meme nom que celui qui est indique dans la definition des modules pour le pipeline
  ## (fichier pipelineDefinition.R)
  pipeline$current.indice <- 1 + which(pipeline$current.obj@l.process == 'moduleFiltering')
  DeleteDatasetsAfter('moduleFiltering')
})

######################################
#######################################

