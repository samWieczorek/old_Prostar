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
    actionButton('plotBtn', 'Plots', "data-toggle"='collapse', "data-target"='#plotDiv', 
                 style='color: white;background-color: blue'),
    tags$div(
      id = 'plotDiv',  
      class="collapse", 
      style='background-color: white',
      uiOutput(ns('vignettes'))
    )
    )
)
}