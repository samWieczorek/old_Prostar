
########
#### Watch code#####


####################
WatchmoduleProtHypothesisTest <- callModule(module=moduleProtHypothesisTest,'moduleProtHypothesisTest',  
                                   dataIn=reactive({current.obj = GetCurrentMSnSet()}),
                                   screen.id = reactive({GetScreenId()}),
                                   settings = reactive({rv.prostar$settings()}))




observeEvent(req(WatchmoduleProtHypothesisTest()),{
  pipeline$current.obj@datasets$moduleProtHypothesisTest <- WatchmoduleProtHypothesisTest()$obj
  pipeline$current.obj@res_AllPairwiseComparisons <- WatchmoduleProtHypothesisTest()$res_AllPairwiseComparisons
  
  ## doit avoir le meme nom que celui qui est indique dans la definition des modules pour le pipeline
  ## (fichier pipelineDefinition.R)
  pipeline$current.indice <- which(pipeline$current.obj@processes == 'moduleProtHypothesisTest')
  
  DeleteDatasetsAfter('moduleProtHypothesisTest')
  rvNav$Done[pipeline$current.indice-1] <- TRUE
})

######################################
#######################################

