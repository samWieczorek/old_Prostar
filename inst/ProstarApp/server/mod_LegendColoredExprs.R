
#' importFrom shinyBS bsCollapse, bsCollapsePanel
#' 
mod_LegendColoredExprs_ui <- function(id){
  ns <- NS(id)
  
  
  bsCollapse(id = "collapseExample", 
             open = "",
             bsCollapsePanel(title = "Legend of colors",
                             uiOutput(ns('legend')),
                             style = ""
             )
  )
  
}



mod_LegendColoredExprs_server <- function(id){
  
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      colors <- list('missing POV' = "lightblue",
                     'missing MEC' = "orange",
                     'recovered' = "lightgrey",
                     'identified' = "white",
                     'combined' = "red")
                     
      output$legend <- renderUI({
        tagList(
          lapply(1:length(colors), function(x){
            tagList(
                 tags$div(class="color-box",
                       style = paste0("display:inline-block; 
                                      vertical-align: middle;
                                      width:20px; height:20px;
                                      border:1px solid #000; 
                                      background-color: ", 
                                      colors[[x]] , ";"),
                       ),
                 tags$p(style = paste0("display:inline-block; 
                                       vertical-align: middle;"),
                        names(colors)[x]),
                 br()
                 )
      })
    )
  })
    })
      
}
