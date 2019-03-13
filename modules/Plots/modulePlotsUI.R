######
modulePlotsUI <- function(id){
  ns <- NS(id)
  
  tagList(

    absolutePanel(
    id  = "#AbsolutePanel",
    class = "panel panel-default",
    style= "text-align: center; color: grey; border-width:0px",
    top = 150, right = 50, width = "70px",height = "450px",
    draggable = TRUE,fixed = FALSE,
    cursor = "default",
    tags$head(tags$style(".modal-dialog{ width:100%}")),
    tags$head(tags$style(".modal-body{ min-height:50%}")),
    actionButton(ns('plotBtn'), 'Plots', "data-toggle"='collapse', "data-target"=paste0('#',ns('plotDiv')), 
                 style='color: white;background-color: lightgrey'),
    tags$div(
      id = ns('plotDiv'),  
      class="collapse", 
      style='background-color: white',
      uiOutput(ns('vignettes'))
    )
    )
)
}