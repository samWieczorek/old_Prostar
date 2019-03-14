

printStatusTest <- function(){
  print("PrintStatus of module ")
  print(paste0('initData() = ',initData()))
  print(paste0('rv$indice= ',rv$indice))
  print(paste0('rv$current.obj= ',rv$current.obj))
  print(paste0('rv$datasetl= ',rv$dataset))
}



DeleteDatasetsAfterTest <- function(txt, processes, dataset){
  indice <- which(names(processes) == txt)
  if (indice < length(names(processes))) {
    for (i in (indice+1):length(names(processes))){
      dataset[i] <- list(NULL)
    }
  }
  
  return(dataset)
}