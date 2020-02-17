
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
  print(names(rv.core$current.obj@datasets))
  rv.core$current.obj@datasets$moduleProtImputation <- WatchmoduleProtImputation()
  print(str((rv.core$current.obj@datasets)))
  ## doit avoir le meme nom que celui qui est indique dans la definition des modules pour le pipeline
  ## (fichier pipelineDefinition.R)
  rv.core$current.indice <- which(rv.core$current.obj@processes == 'moduleProtImputation')
  DeleteDatasetsAfter('moduleProtImputation')
  rvNav$Done[rv.core$current.indice-1] <- TRUE
})

######################################
#######################################

