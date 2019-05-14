####### Common functions

ConfigureData <- function(obj){
  
  
  data <- getMSnSet(obj, 1)
  
  data <- DAPAR::addOriginOfValue(data)
  #data <- obj$datasets$original
  #if (is.null(data@experimentData@other$typeOfData)) {
  #  obj@experimentData@other$typeOfData <- ""
  #}
  
  obj <- setIndexNA(obj,which(is.na(data)))
  
  colnames(fData(data)) <- gsub(".", "_", colnames(fData(data)), fixed=TRUE)
  names(data@experimentData@other) <- gsub(".", "_", names(data@experimentData@other), fixed=TRUE)
  obj <- setMSnSet(obj, 1, data)
  
  
  #rv.opendataset$current.obj$ConnexComp <- ComputeConnexComposants()
  #obj <- setMSnSet(obj, 'original',data)
  
  
  return(obj)
}

