ProcessA <- callModule(module=moduleA, 'processA', 
                       dataIn=reactive({rv$current.obj}),
                       screen.id = reactive({GetScreenId()}))

observeEvent(ProcessA(),{
  print(paste0('observeEvent(ProcessX() : ', ProcessA()))
  rv$current.obj <- ProcessA()
  rv$indice <- which(ll.process() == 'processA')
  rv$dataset$processA <- ProcessA()
  #DeleteDatasetsAfter('ProcessA')
  # rv$dataset <- DeleteDatasetsAfterTest('ProcessA', rv$process, rv$dataset)
  printStatus()
})