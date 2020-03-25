
########
#### Watch code#####


####################
WatchmoduleHypothesisTest <- callModule(module=moduleHypothesisTest,'moduleHypothesisTest',  
                                   dataIn=reactive({current.obj = GetCurrentMSnSet()}),
                                   screen.id = reactive({GetScreenId()}),
                                   settings = reactive({rv.prostar$settings()}))




observeEvent(req(WatchmoduleHypothesisTest()),{
  rv.core$current.obj@datasets$moduleHypothesisTest <- WatchmoduleHypothesisTest()$obj
  rv.core$current.obj@res_AllPairwiseComparisons <- WatchmoduleHypothesisTest()$res_AllPairwiseComparisons
  
  ## doit avoir le meme nom que celui qui est indique dans la definition des modules pour le pipeline
  ## (fichier pipelineDefinition.R)
  rv.core$current.indice <- which(rv.core$current.obj@processes == 'moduleHypothesisTest')
  
  DeleteDatasetsAfter('moduleHypothesisTest')
  rvNav$Done[rv.core$current.indice-1] <- TRUE
})

######################################
#######################################

