moduleLegendColoredExprsUI <- function(id,colorsTypeMV){
  ns <- NS(id)
  
  tagList(
    tags$p(tags$b("Legend of colors")),
    
    fluidRow(
      column(width=2, 
             tags$div(class="input-color", checked=NA,
                      tags$input(type="text", value=""),
                      tags$div(class="color-box", style=paste0("background-color: ",colorsTypeMV$POV, ";"))
             )),
      column(width=10, tags$p("Partially Observed Value"))
    ),
    
    fluidRow(
      column(width=2, 
             tags$div(class="input-color", checked=NA,
                      tags$input(type="text", value=""),
                      tags$div(class="color-box", style=paste0("background-color: ",colorsTypeMV$MEC, ";"))
             )),
      column(width=10, tags$p("Missing in Entire Condition"))
    )
  )
}



#------------------------------------------------------------
moduleLegendColoredExprs <- function(input, output, session){}
