mod_loading_page_ui <- function(id)
{
  ns <- NS(id)
  absolutePanel(
    id  = ns("AbsolutePanel"),
    class = "panel panel-default",
    style= "text-align: center; background-color: #25949A;",
    top = '30%',
    left = '25%',
    width = "50%",
    height = "150px",
    draggable = FALSE,
    fixed = TRUE,
    tagList(
      tags$h1(style='text-align: center; color: white', "Prostar is loading, please wait..."),
      br(),
      tags$div(class="progress",
               tags$div(class="indeterminate")
      )
    )
  )
  
}


mod_loading_page <- function(input, output, session){
  ns <- session$ns
  
  
  
}
