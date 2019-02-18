
###### Module de navigation dans les interfaces pour les data Process   ######
module_Nav_ProcessUI <- function(id){
  ns <- NS(id)
  
  tagList(
    div(
      div( style="align: center;display:inline-block; vertical-align: top; padding: 7px",
           shinyjs::hidden(actionButton(ns("rstBtn"), "reset", class = PrevNextBtnClass,style='padding:4px; font-size:80%'))),
      div( style="align: center;display:inline-block; vertical-align: top; padding: 7px",
                     shinyjs::hidden(actionButton(ns("prevBtn"), "<<", class = PrevNextBtnClass,style='padding:4px; font-size:80%'))),
      div( style="align: center;display:inline-block; vertical-align: top;",
                     uiOutput(ns("checkPanel" ))),
      div( style="align: center;display:inline-block; vertical-align: top; padding: 7px",
                     shinyjs::hidden(actionButton(ns("nextBtn"), ">>", class = PrevNextBtnClass, style='padding:4px; font-size:80%')))

  ),
  hr(),
  uiOutput(ns("screens"))
 
  )
}
