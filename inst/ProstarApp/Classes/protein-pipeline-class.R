
## Cette classe définit un pipeline générique avec les structures de données adéquates
## elle n'est instanciée qu'une seule fois dans une session prostar
source(file.path(".", "Classes/pipeline-template-class.R"), local = TRUE)$value

.PipelineProtein <- setClass("PipelineProtein",
          contains = "PipelineTemplate", 
          representation = representation(),
          prototype = prototype(),
         )



#' @export
#' @importFrom MultiAssayExperiment MultiAssayExperiment
PipelineProtein <- function(...)
{
  template <- PipelineTemplate(...)
  obj <- new("PipelineProtein", template)
  
  obj
}
