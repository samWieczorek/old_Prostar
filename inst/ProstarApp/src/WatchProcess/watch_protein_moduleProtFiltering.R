
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
  
  rv.core$current.obj@datasets$moduleProtFiltering <- WatchmoduleProtFiltering()$obj
  
  ## doit avoir le meme nom que celui qui est indique dans la definition des modules pour le pipeline
  ## (fichier pipelineDefinition.R)
  rv.core$current.indice <- which(pipeline$current.obj@processes == 'moduleProtFiltering')
  DeleteDatasetsAfter('moduleProtFiltering')
  rvNav$Done[rv.core$current.indice-1] <- TRUE
  
})

######################################
#######################################

