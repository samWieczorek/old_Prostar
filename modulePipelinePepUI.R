modulePipelinePepUI <- function(id){
  ns <- NS(id)
  
  navbarMenu("Pipeline peptide" ,
         tabPanel("ProcessA",moduleAUI(ns('pep_processA'))),
         tabPanel("ProcessB",moduleBUI(ns('pep_processB'))),
         tabPanel("ProcessC",moduleCUI(ns('pep_processC')))
       )

}
