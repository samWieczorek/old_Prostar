
########
#### Watch code#####


####################
WatchmoduleProtHypothesisTest <- callModule(module=moduleProtHypothesisTest,'moduleProtHypothesisTest',  
                                   dataIn=reactive({current.obj = GetCurrentMSnSet()}),
                                   screen.id = reactive({GetScreenId()}),
                                   settings = reactive({rv.prostar$settings()}))




observeEvent(req(WatchmoduleProtHypothesisTest()),{
  rv.core$current.obj@datasets$moduleProtHypothesisTest <- WatchmoduleProtHypothesisTest()$obj
  rv.core$current.obj@res_AllPairwiseComparisons <- WatchmoduleProtHypothesisTest()$res_AllPairwiseComparisons
  
  ## doit avoir le meme nom que celui qui est indique dans la definition des modules pour le pipeline
  ## (fichier pipelineDefinition.R)
  rv.core$current.indice <- which(rv.core$current.obj@processes == 'moduleProtHypothesisTest')
  
  DeleteDatasetsAfter('moduleProtHypothesisTest')
  rvNav$Done[rv.core$current.indice-1] <- TRUE
})

######################################
#######################################

