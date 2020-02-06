
## Cette classe définit un pipeline générique avec les structures de données adéquates
## elle n'est instanciée qu'une seule fois dans une session prostar
source(file.path(".", "Classes/ClassGenericPipeline.R"), local = TRUE)$value

prot_pipeline <- setClass("prot_pipeline",
          contains = "PipelineTemplate", 
          representation = representation(),
          prototype = prototype(),
         )
