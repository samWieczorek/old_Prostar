
########
#### Watch code#####

### Pour le module Filtering, comme on doit également mettre a jour la matrice d'adjacence et les composantes
### connexes, on les donne en parametre du module
####################
WatchmoduleProtImputation <- callModule(module=moduleProtImputation,'moduleProtImputation',  
                                       dataIn=reactive({GetCurrentMSnSet()}),
                                       screen.id = reactive({GetScreenId()}),
                                       settings = reactive({rv.prostar$settings()}))




observeEvent(req(WatchmoduleProtImputation()),{
  print(names(pipeline$current.obj@datasets))
  pipeline$current.obj@datasets$moduleProtImputation <- WatchmoduleProtImputation()
  print(str((pipeline$current.obj@datasets)))
  ## doit avoir le meme nom que celui qui est indique dans la definition des modules pour le pipeline
  ## (fichier pipelineDefinition.R)
  pipeline$current.indice <- which(pipeline$current.obj@processes == 'moduleProtImputation')
  DeleteDatasetsAfter('moduleProtImputation')
  rvNav$Done[pipeline$current.indice-1] <- TRUE
})

######################################
#######################################

