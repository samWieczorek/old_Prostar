source(file.path(".", "Classes/ClassGenericPipeline.R"), local = TRUE)$value

protPipeline <- setClass("protPipeline",
                        representation = representation(),
                        
                        prototype = prototype(),
                        
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
