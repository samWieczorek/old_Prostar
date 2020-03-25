
########
#### Watch code#####

### Pour le module Filtering, comme on doit également mettre a jour la matrice d'adjacence et les composantes
### connexes, on les donne en parametre du module
####################
WatchmoduleProtNormalization <- callModule(module=moduleProtNormalization,'moduleProtNormalization',  
                                   dataIn=reactive({list(obj = GetCurrentMSnSet(),
                                                        currentProcess = GetCurrentProcess())}),
                                   screen.id = reactive({GetScreenId()}),
                                   settings = reactive({rv.prostar$settings()}))




observeEvent(req(WatchmoduleProtNormalization()),{
  print(names(rv.core$current.obj@datasets))
  rv.core$current.obj@datasets$moduleProtNormalization <- WatchmoduleProtNormalization()
  print(str((rv.core$current.obj@datasets)))
  ## doit avoir le meme nom que celui qui est indique dans la definition des modules pour le pipeline
  ## (fichier pipelineDefinition.R)
  rv.core$current.indice <- which(rv.core$current.obj@processes == 'moduleProtNormalization')
  DeleteDatasetsAfter('moduleProtNormalization')
  rvNav$Done[rv.core$current.indice-1] <- TRUE
})

######################################
#######################################

