moduleLegendColoredExprsUI <- function(id,colorsTypeMV){
  ns <- NS(id)
  
  
    tagList(
      tags$p(tags$b("Legend of colors")),
      
      fluidRow(
        column(width=2, HTML(paste0("<div style=\"width:50px;height:20px;border:0px solid #000; background-color: ",
                                    rv.prostar$settings()$colorsTypeMV$POV,";\"></div>")) ),
        column(width=10, tags$p("Partially Observed Value"))
      ),
      
      fluidRow(
        column(width=2,HTML(paste0("<div style=\"width:50px;height:20px;border:0px solid #000; background-color: ",
                                   rv.prostar$settings()$colorsTypeMV$MEC,";\"></div>"))),
        column(width=10, tags$p("Missing in Entire Condition"))
      )
    )
}




moduleLegendColoredExprs <- function(input, output, session){}