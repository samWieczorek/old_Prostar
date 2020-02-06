
## Cette classe définit un pipeline générique avec les structures de données adéquates
## elle n'est instanciée qu'une seule fois dans une session prostar

genericPipeline <- setClass("genericPipeline",
          contains = "MultiAssayExperiment",                
         representation = representation(
           indexNA = "vector",
           res_AllPairwiseComparisons = "list",
           name.dataset = "character",
           pipeline = "character",
           processes = "character"),
         
         
         prototype = prototype(
           datasets = list(),
           indexNA = vector("list", 1),
           res_AllPairwiseComparisons = list(),
           name.dataset = NA_character_,
           pipeline = NA_character_,
           processes = c()),
         
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
           def=function(theObject,processes, obj, data.name, type)
           {
             standardGeneric("initialize")
           }
)

setMethod(f="initialize",
          signature="genericPipeline",
          definition=function(theObject,ll.processes, obj, data.name, type)
          {
            print("## Initialization of the class genericPipeline ##")
            #theObject@datasets[ll.processes] <- list(NULL)
            #theObject@datasets[[1]] <- obj
            theObject <- MultiAssayExperiment(obj,...)
            theObject@processes <- ll.processes
            theObject@name.dataset <- data.name
            theObject@pipeline <- type
           
            ## Sourcing the code for corresponding modules. le nom de l'item dans la liste
            ## doit correspondre au nom du fichier source prefixé par 'module' 
            for(p in ll.process[-1]){
              print(p)
              path <- file.path(".", paste0('modules/process/', type, "/",p, ".R"))
              print(paste0("looking for source code :", path))
              source(path, local = TRUE)$value
            }
             
            
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
            theObject@datasets[[name]] <- obj
            validObject(theObject)
            return(theObject)
          }
)

setGeneric(name="setMSnSet",
           def=function(theObject,indice, obj)
           {
             standardGeneric("setMSnSet")
           }
)

setMethod(f="setMSnSet",
          signature="genericPipeline",
          definition=function(theObject,indice, obj)
          {
            theObject@datasets[[indice]] <- obj
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




setGeneric(name="setIndexNA",
           def=function(theObject,ind)
           {
             standardGeneric("setIndexNA")
           }
)

setMethod(f="setIndexNA",
          signature="genericPipeline",
          definition=function(theObject,ind)
          {
            theObject@indexNA <- ind
            validObject(theObject)
            return(theObject)
          }
)

# create a method to get the value of the location
setGeneric(name="getIndexNA",
           def=function(theObject)
           {
             standardGeneric("getIndexNA")
           }
)

setMethod(f="getIndexNA",
          signature="genericPipeline",
          definition=function(theObject)
          {
            return(theObject@indexNA)
          }
)


