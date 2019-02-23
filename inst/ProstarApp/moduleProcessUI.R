
###### Module de gestion des interfaces pour les data Process   ######
moduleProcessUI <- function(id){
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





moduleProcessFilteringUI <- function(id){
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
    #uiOutput(ns("screens"))
    div(id=ns('screen1'),uiOutput(ns("screenFiltering1"))),
    hidden(div(id=ns('screen2'),uiOutput(ns("screenFiltering2")))),
    hidden(div(id=ns('screen3'),uiOutput(ns("screenFiltering3")))),
    hidden(div(id=ns('screen4'),uiOutput(ns("screenFiltering4"))))
  )
}