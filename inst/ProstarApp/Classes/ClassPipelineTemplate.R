
#########################################################
#' @export
#' @import methods
#' @importClassesFrom MultiAssayExperiment MultiAssayExperiment
.PipelineTemplate <- setClass("PipelineTemplate",
                          slots= list(
                            indexNA = "numeric",
                            PairwiseComparisons = "list",
                            datasetName = "character",
                            pipelineType = "character",
                            processes = "character"
                          ),
                          contains=c("MultiAssayExperiment"),

)



#' @export
#' @importFrom MultiAssayExperiment MultiAssayExperiment
PipelineTemplate <- function(
  indexNA = numeric(),
  PairwiseComparisons = list(),
  datasetName = character(),
  pipelineType = character(),
  processes = character(),
  ...)
  {
  mae <- MultiAssayExperiment(...)
  obj <- new("PipelineTemplate",mae,
                    indexNA = indexNA,
                    PairwiseComparisons = PairwiseComparisons,
                    datasetName=datasetName, 
                    pipelineType=pipelineType,
                    processes=c('original',processes)
  )
  validObject(obj)
  
  
  ## Sourcing the code for corresponding modules. le nom de l'item dans la liste
  ## doit correspondre au nom du fichier source prefixé par 'module' 
  for(p in processes(obj)[-1]){
     path <- file.path(".", paste0('modules/process/', pipelineType(obj), "/module_",pipelineType(obj), "_",p, ".R"))
    source(path, local = TRUE)$value
     }
  
  obj
  }




## Create getter methods for 1D data structures
#' @export
setGeneric("indexNA", function(x, ...) standardGeneric("indexNA"))

#' @export
setGeneric("datasetName", function(x, ...) standardGeneric("datasetName"))


#' @export
setGeneric("pipelineType", function(x, ...) standardGeneric("pipelineType"))


#' @export
setGeneric("processes", function(x, ...) standardGeneric("processes"))

setGeneric("GetExperimentList", function(x, ...) standardGeneric("GetExperimentList"))


#' @export
setMethod("indexNA", "PipelineTemplate", function(x, withDimnames=TRUE) {
  out <- x@indexNA
  out
})

#' @export
setMethod("datasetName", "PipelineTemplate", function(x, withDimnames=TRUE) {
  out <- x@datasetName
  out
})

#' @export
setMethod("pipelineType", "PipelineTemplate", function(x, withDimnames=TRUE) {
  out <- x@pipelineType
  out
})

#' @export
setMethod("processes", "PipelineTemplate", function(x, withDimnames=TRUE) {
  out <- x@processes
  out
})



setMethod("GetExperimentList", "PipelineTemplate", function(x) {
  out <- x@ExperimentList
  out
})




##
## Create getter methods for 2D data structures
##
#' @export
setGeneric("PairwiseComparisons", function(x, ...) standardGeneric("PairwiseComparisons"))


#' @export
setMethod("PairwiseComparisons", "PipelineTemplate", function(x, withDimnames=TRUE) {
  out <- x@PairwiseComparisons
  out
})








# For MultiAssayExperiment slots
# The getter methods defined in MultiAssayExperiment can be directly used to retrieve 
# data from slots in the base class. These should generally not require any re-defining 
# for a derived class. However, if it is necessary, the methods should use callNextMethod 
# internally. This will call the method for the base MultiAssayExperiment class, the output
# of which can be modified as required.


#' #' @export
#' #' @importMethodsFrom MultiAssayExperiment rowData
#' setMethod("rowData", "PipelineTemplate", function(x, ...) {
#'   out <- callNextMethod()
#'   
#'   # Do something extra here.
#'   out$extra <- runif(nrow(out))
#'   
#'   # Returning the rowData object.
#'   out
#' })



# We use setValidity2 to define a validity function for PipelineTemplate. We use the previously 
# defined getter functions to retrieve the slot values rather than using @. This is generally 
# a good idea to keep the interface separate from the implementation
# (This protects against changes to the slot names, and simplifies development when the 
#   storage mode differs from the conceptual meaning of the data, e.g., for efficiency 
#   purposes.)
# We also set withDimnames=FALSE in our getter calls, as consistent naming is not necessary 
# for internal functions.



#' #' @importFrom BiocGenerics NCOL NROW
#' setValidity("PipelineTemplate", function(object) {
  # NR <- NROW(object)
  # NC <- NCOL(object)
  # msg <- NULL
  # 
  # # 1D
  # if (length(rowVec(object, withDimnames=FALSE)) != NR) {
  #   msg <- c(msg, "'rowVec' should have length equal to the number of rows")
  # }
  # if (length(colVec(object, withDimnames=FALSE)) != NC) {
  #   msg <- c(
  #     msg, "'colVec' should have length equal to the number of columns"
  #   )
  # }
  # 
  # # 2D
  # if (NROW(rowToRowMat(object, withDimnames=FALSE)) != NR) {
  #   msg <- c(
  #     msg, "'nrow(rowToRowMat)' should be equal to the number of rows"
  #   )
  # }
  # if (NCOL(colToColMat(object, withDimnames=FALSE)) != NC) {
  #   msg <- c(
  #     msg, "'ncol(colToColMat)' should be equal to the number of columns"
  #   )
  # }
  # if (NROW(rowToColMat(object, withDimnames=FALSE)) != NC) {
  #   msg <- c(
  #     msg, "'nrow(rowToColMat)' should be equal to the number of columns"
  #   )
  # }
  # if (NCOL(colToRowMat(object, withDimnames=FALSE)) != NR) {
  #   msg <- c(
  #     msg, "'ncol(colToRowMat)' should be equal to the number of rows"
  #   )
  # }
  # 
  # if (length(msg)) {
  #   msg
  # } else TRUE
#   TRUE
# })




# Creating a show method
# The default show method will only display information about the MultiAssayExperiment slots. 
# We can augment it to display some relevant aspects of the custom slots. This is done by 
# calling the base show method before printing additional fields as necessary.

#' @export
#' @importMethodsFrom MultiAssayExperiment show
setMethod("show", "PipelineTemplate", function(object) {
  cat(
    "The name of the pipeline is ", pipelineType(object), " \n",
    "The pipeline is composed of the following processes: ", processes(object), " \n",
    "The name of the dataset is ", datasetName(object), " \n",
     sep=""
  )
  cat('-----------------------------------------\n')
  callNextMethod()
  
})



# Creating setter methods
# 3.6.1 For 1D data structures
# We define some setter methods for the custom slots containing the 1D structures. 
# Again, this usually requires the creation of new generics.


#' @export
setGeneric("pipelineType<-", function(x, ..., value) standardGeneric("pipelineType<-"))


#' @export
setGeneric("datasetName<-", function(x, ..., value) standardGeneric("datasetName<-"))


#' @export
setGeneric("processes<-", function(x, ..., value) standardGeneric("processes<-"))


# We define the class-specific methods for these generics. Note that use of validObject 
# to ensure that the assigned input is still valid.


#' @export
setReplaceMethod("pipelineType", "PipelineTemplate", function(x, value) {
  x@pipelineType <- value
  validObject(x)
  x
})

#' @export
setReplaceMethod("processes", "PipelineTemplate", function(x, value) {
  x@processes <- value
  validObject(x)
  x
})

#' @export
setReplaceMethod("datasetName", "PipelineTemplate", function(x, value) {
  x@datasetName <- value
  validObject(x)
  x
})


# For 2D data structures
# We repeat this process for the 2D structures.


#' @export
setGeneric("PairwiseComparisons<-", function(x, ..., value) standardGeneric("PairwiseComparisons<-"))

# Again, we define class-specific methods for these generics.
#' @export
setReplaceMethod("PairwiseComparisons", "PipelineTemplate", function(x, value) {
  x@PairwiseComparisons <- value
  validObject(x)
  x
})




# 3.6.3 For MultiAssayExperiment slots
# Again, we can use the setter methods defined in MultiAssayExperiment 
# to modify slots in the base class. These should generally not require 
# any re-defining. However, if it is necessary, the methods should use 
# callNextMethod internally:
#   
  
#' #' @export
#' #' @importMethodsFrom MultiAssayExperiment "rowData<-"
#' setReplaceMethod("rowData", "ExampleClass", function(x, ..., value) {
#'   y <- callNextMethod() # returns a modified ExampleClass
#'   
#'   # Do something extra here.
#'   message("hi!\n")
#'   
#'   y
#' })


#' @export
setGeneric("rmDatasetByIndice", function(x, ind) standardGeneric("rmDatasetByIndice"))

#' @export
setMethod("rmDatasetByIndice", "PipelineTemplate", function(x, ind) {
  #mae <- callNextMethod()
  x@ExperimentList <- x@ExperimentList[-ind] 
  validObject(x)
  x
})


#' @export
setGeneric("rmDatasetByName", function(x, name) standardGeneric("rmDatasetByName"))

#' @export
setMethod("rmDatasetByName", "PipelineTemplate", function(x, name) {
  #mae <- callNextMethod()
  x@ExperimentList <- within(x@ExperimentList, rm(name)) 
  validObject(x)
  x
})

#' @export
setGeneric("addDataset<-", function(x, name, dataset) standardGeneric("addDataset<-"))
#' @export
setMethod("addDataset", "PipelineTemplate", function(x, name, dataset) {
  #mae <- callNextMethod()
  ds <- list()
  ds[[name]]<- dataset
  x <- c(x, ds)
  validObject(x)
  x
})




.checkProcessSourceCode <- function(object) {
  errors <- character()
  for(p in processes(object)[-1]){
    path <- file.path(".", paste0('modules/process/', pipelineType(object), "/module_",pipelineType(object), "_",p, ".R"))
    if (!file.exists(path)) {
      msg <- paste0( path, ' was not found.')
      errors <- c(errors, msg)
    }
  }
  return(errors)
}


.validPipelineTemplate <- function(object) {
  #if (length(experiments(object)) != 0L) {
  c(.checkProcessSourceCode(object) )
  # }
}

S4Vectors::setValidity2("PipelineTemplate", .validPipelineTemplate)
















