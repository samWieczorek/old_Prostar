
# WatchProcessA <- callModule(module=moduleA, 'processA', 
#                             dataIn=reactive({pipeline$current.obj}),
#                             screen.id = reactive({GetScreenId()}))
# 
# observeEvent(WatchProcessA(),{
#   print(paste0('observeEvent(ProcessA() : ', WatchProcessA()))
#   pipeline$current.obj <- WatchProcessA()
#   pipeline$current.indice <- which(pipeline$ll.process == 'processA')
#   pipeline$current.dataset$processA <- WatchProcessA()
#   DeleteDatasetsAfter('processA')
#   printStatus()
# })



# 
# WatchProcessB <- callModule(module=moduleB, 'processB', 
#                             dataIn=reactive({pipeline$current.obj}),
#                             screen.id = reactive({GetScreenId()}))
# 
# observeEvent(WatchProcessB(),{
#   print(paste0('observeEvent(ProcessB() : ', WatchProcessB()))
#   pipeline$current.obj <- WatchProcessB()
#   pipeline$current.indice <- which(pipeline$ll.process == 'processB')
#   pipeline$current.dataset$processB <- WatchProcessB()
#   DeleteDatasetsAfter('processB')
#   printStatus()
# })
# 
# WatchProcessC <- callModule(module=moduleC, 'processC', 
#                             dataIn=reactive({pipeline$current.obj}),
#                             screen.id = reactive({GetScreenId()}))
# 
# observeEvent(WatchProcessC(),{
#   print(paste0('observeEvent(ProcessC() : ', WatchProcessC()))
#   pipeline$current.obj <- WatchProcessC()
#   pipeline$current.indice <- which(pipeline$ll.process == 'processC')
#   pipeline$current.dataset$processC <- WatchProcessC()
#   DeleteDatasetsAfter('processC')
#   printStatus()
# })
