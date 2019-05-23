
########
#### Watch code#####


####################
WatchmoduleHypothesisTest <- callModule(module=moduleHypothesisTest,'moduleHypothesisTest',  
                                   dataIn=reactive({current.obj = GetCurrentMSnSet()}),
                                   screen.id = reactive({GetScreenId()}),
                                   settings = reactive({rv.prostar$settings()}))




observeEvent(req(WatchmoduleHypothesisTest()),{
  pipeline$current.obj@datasets$moduleHypothesisTest <- WatchmoduleHypothesisTest()$obj
  pipeline$current.obj@res_AllPairwiseComparisons <- WatchmoduleHypothesisTest()$res_AllPairwiseComparisons
  
  ## doit avoir le meme nom que celui qui est indique dans la definition des modules pour le pipeline
  ## (fichier pipelineDefinition.R)
  pipeline$current.indice <- which(pipeline$current.obj@ll.process == 'moduleHypothesisTest')
  
  DeleteDatasetsAfter('moduleHypothesisTest')
  rvNav$Done[pipeline$current.indice-1] <- TRUE
})

######################################
#######################################

