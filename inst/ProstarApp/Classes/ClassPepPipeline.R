source(file.path(".", "Classes/ClassGenericPipeline.R"), local = TRUE)$value

pepPipeline <- setClass("pepPipeline",
                        representation = representation(
                          AdjancencyMatrix = 'matrix',
                          ConnexComp = 'list'),
                        prototype = prototype(
                          AdjancencyMatrix = NULL,
                          ConnexComp = NULL),
                        # Make a function that can test to see if the data is consistent.
                        # This is not called if you have an initialize function defined!
                        validity=function(object)
                        {
                          #if(sum(object@velocity^2)>100.0) {
                          #  return("The velocity level is out of bounds.")
                          # }
                          return(TRUE)
                        },
                        contains = 'genericPipeline'
)



# create a method to initialize the datasets list with the name of the processes
setGeneric(name="setAdjacencyMatrix",
           def=function(theObject,X)
           {
             standardGeneric("setAdjacencyMatrix")
           }
)

setMethod(f="setAdjacencyMatrix",
          signature="pepPipeline",
          definition=function(theObject,X)
          {
            theObject@AdjacencyMatrix <- X
            validObject(theObject)
            return(theObject)
          }
)


setGeneric(name="getAdjacencyMatrix",
           def=function(theObject)
           {
             standardGeneric("getAdjacencyMatrix")
           }
)

setMethod(f="getAdjacencyMatrix",
          signature="pepPipeline",
          definition=function(theObject)
          {
            return(theObject@AdjacencyMatrix)
          }
)



setGeneric(name="setConnexComp",
           def=function(theObject,cc)
           {
             standardGeneric("setConnexComp")
           }
)

setMethod(f="setConnexComp",
          signature="pepPipeline",
          definition=function(theObject,cc)
          {
            theObject@ConnexComp <- cc
            validObject(theObject)
            return(theObject)
          }
)


setGeneric(name="getConnexComp",
           def=function(theObject)
           {
             standardGeneric("getConnexComp")
           }
)

setMethod(f="getConnexComp",
          signature="pepPipeline",
          definition=function(theObject)
          {
            return(theObject@ConnexComp)
          }
)