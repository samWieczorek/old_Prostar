
########
#### Watch code#####

### Pour le module Filtering, comme on doit également mettre a jour la matrice d'adjacence et les composantes
### connexes, on les donne en parametre du module
####################
WatchmoduleHypothesisTest <- callModule(module=moduleHypothesisTest,'moduleHypothesisTest',  
                                   dataIn=reactive({current.obj = GetCurrentMSnSet()}),
                                   screen.id = reactive({GetScreenId()}),
                                   settings = reactive({rv.prostar$settings()}))




observeEvent(req(WatchmoduleHypothesisTest()),{
  print("######## observeEvent(req(WatchmoduleFiltering())  ########")
  print(str(WatchmoduleHypothesisTest()))
  pipeline$current.obj@datasets$moduleFiltering <- WatchmoduleHypothesisTest()$obj
  
  ## doit avoir le meme nom que celui qui est indique dans la definition des modules pour le pipeline
  ## (fichier pipelineDefinition.R)
  pipeline$current.indice <- which(pipeline$current.obj@ll.process == 'moduleHypothesisTest')
  print(paste0("pipeline$current.indice = ", pipeline$current.indice))
  DeleteDatasetsAfter('moduleHypothesisTest')
})

######################################
#######################################

