
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



mod_LegendColoredExprs_server <- function(id, obj, hide.white = TRUE){
  
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
                      
      output$legend <- renderUI({
        mc <- metacell.def(GetTypeofData(obj()))
        
        tagList(
          lapply(1:nrow(mc), function(x){
            if (mc[x, 'color'] != 'white' || (mc[x, 'color'] == 'white' && !isTRUE(hide.white))) {
              tagList(
                 tags$div(class="color-box",
                       style = paste0("display:inline-block; 
                                      vertical-align: middle;
                                      width:20px; height:20px;
                                      border:1px solid #000; 
                                      background-color: ", 
                                      mc[x, 'color'] , ";"),
                       ),
                 tags$p(style = paste0("display:inline-block; 
                                       vertical-align: middle;"),
                        mc[x, 'node']),
                 br()
                 )
            }
      })
    )
  })
    })
      
}
