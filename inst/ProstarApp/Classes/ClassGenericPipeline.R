


genericPipeline <- setClass("genericPipeline",
         representation = representation(
           datasets="list",
           name.dataset = "character",
           pipeline = "character",
           ll.process = "character"),
         prototype = prototype(
           datasets = list(),
           name.dataset = NA_character_,
           pipeline = NA_character_,
           ll.process = c()),
         # Make a function that can test to see if the data is consistent.
         # This is not called if you have an initialize function defined!
         validity=function(object)
         {
           #if(sum(object@velocity^2)>100.0) {
           #  return("The velocity level is out of bounds.")
          # }
           return(TRUE)
         }
         )

# create a method to initialize the datasets list with the name of the processes
setGeneric(name="initialize",
           def=function(theObject,ll.process, obj, data.name, type)
           {
             standardGeneric("initialize")
           }
)

setMethod(f="initialize",
          signature="genericPipeline",
          definition=function(theObject,ll.process, obj, data.name, type)
          {
            theObject@datasets[ll.process] <- list(NULL)
            theObject@ll.process <- ll.process
            theObject@datasets[[1]] <- obj
            theObject@name.dataset <- data.name
            theObject@pipeline <- type
            validObject(theObject)
            return(theObject)
          }
)



setGeneric(name="setMSnSet",
           def=function(theObject,name, obj)
           {
             standardGeneric("setMSnSet")
           }
)

setMethod(f="setMSnSet",
          signature="genericPipeline",
          definition=function(theObject,name, obj)
          {
            theObject@datasets[name] <- obj
            validObject(theObject)
            return(theObject)
          }
)



# create a method to get the value of the location
setGeneric(name="getMSnSet",
           def=function(theObject, indice)
           {
             standardGeneric("getMSnSet")
           }
)

setMethod(f="getMSnSet",
          signature="genericPipeline",
          definition=function(theObject, indice)
          {
            if (is.null(theObject)){return()}
            return(theObject@datasets[[indice]])
          }
)



# create a method to assign the value of the location
setGeneric(name="setPipeline",
           def=function(theObject,txt)
           {
             standardGeneric("setPipeline")
           }
)

setMethod(f="setPipeline",
          signature="genericPipeline",
          definition=function(theObject,txt)
          {
            theObject@pipeline <- txt
            validObject(theObject)
            return(theObject)
          }
)

# create a method to get the value of the location
setGeneric(name="getPipeline",
           def=function(theObject)
           {
             standardGeneric("getPipeline")
           }
)

setMethod(f="getPipeline",
          signature="genericPipeline",
          definition=function(theObject)
          {
            return(theObject@pipeline)
          }
)




######
# create a method to assign the value of the location
setGeneric(name="setNameDataset",
           def=function(theObject,txt)
           {
             standardGeneric("setNameDataset")
           }
)

setMethod(f="setNameDataset",
          signature="genericPipeline",
          definition=function(theObject,txt)
          {
            theObject@name.dataset <- txt
            validObject(theObject)
            return(theObject)
          }
)

# create a method to get the value of the location
setGeneric(name="getNameDataset",
           def=function(theObject)
           {
             standardGeneric("getNameDataset")
           }
)

setMethod(f="getNameDataset",
          signature="genericPipeline",
          definition=function(theObject)
          {
            return(theObject@name.dataset)
          }
)





