
########
#### Watch code#####


####################
WatchmoduleAgregation <- callModule(module=moduleAgregation,'moduleAgregation',  
                                        dataIn=reactive({current.obj = GetCurrentMSnSet()}),
                                        screen.id = reactive({GetScreenId()}),
                                        settings = reactive({rv.prostar$settings()}))




observeEvent(req(WatchmoduleAgregation()),{
  rv.core$current.obj@datasets$moduleAgregation <- WatchmoduleAgregation()
  
  ## doit avoir le meme nom que celui qui est indique dans la definition des modules pour le pipeline
  ## (fichier pipelineDefinition.R)
  rv.core$current.indice <- which(pipeline$current.obj@processes == 'moduleAgregation')
  
  DeleteDatasetsAfter('moduleAgregation')
  rvNav$Done[rv.core$current.indice-1] <- TRUE
})

######################################
#######################################

