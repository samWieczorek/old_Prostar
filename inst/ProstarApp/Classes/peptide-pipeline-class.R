

## Cette classe définit un pipeline générique avec les structures de données adéquates
## elle n'est instanciée qu'une seule fois dans une session prostar
source(file.path(".", "Classes/PipelineTemplate-class.R"), local = TRUE)$value

.PipelinePeptide <- setClass("PipelinePeptide",
                             contains = "PipelineTemplate", 
                             representation = representation(),
                             prototype = prototype(),
)



#' @export
#' @importFrom MultiAssayExperiment MultiAssayExperiment
PipelinePeptide <- function(...)
{
  template <- PipelineTemplate(...)
  obj <- new("PipelinePeptide", template)
  
  obj
}







.ComputeAdjacencyMatrices <- function(obj){
  req(obj@experimentData@other$proteinId)
  if (obj@experimentData@other$typeOfData != 'peptide') {return (NULL)}
  
  pId <- obj@experimentData@other$proteinId
  print(pId)
  print(paste0("class of obj : ", class(obj)))
  
  matAdj <- NULL
  matSharedPeptides <- BuildAdjacencyMatrix(obj, pId, FALSE)
  matUniquePeptides <- BuildAdjacencyMatrix(obj, pId, TRUE)
  matAdj <- list(matWithSharedPeptides=matSharedPeptides, matWithUniquePeptides=matUniquePeptides)
  
  return(matAdj)
}

.ComputeConnexComposants <- function(X){
  req(X)
  CC <- NULL
  CC <- list(allPep = get.pep.prot.cc(as.matrix(X$matWithSharedPeptides)),
             onlyUniquePep = get.pep.prot.cc(as.matrix(X$matWithUniquePeptides)))
  
  CC
}
