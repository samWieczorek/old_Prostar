output$plotintensitysmall <- renderImage({
  filename <- normalizePath(file.path('./images','desc_intdistrib.png'))
  list(src = filename,
       width = .width,
       height = .height)
}, deleteFile = FALSE)



output$plotintensitylarge <- renderPlot({
  tagList(
    tags$br(),tags$br(),
    tags$div(
      tags$div(style="display:inline-block; vertical-align: middle;",
               tags$p("Plot options")
      ),
      
      tags$div(style="display:inline-block; vertical-align: middle;",
               
               tags$div(
                 tags$div(style="display:inline-block; vertical-align: top;",
                          shinyWidgets::dropdownButton(
                            tags$div(
                              tags$div(style="display:inline-block; vertical-align: bottom;",
                                       selectInput("whichGroup2Color",
                                                   "Color lines",
                                                   choices=list("By condition" = "Condition",
                                                                "By replicate" = "Replicate"),
                                                   selected=GetWhichGroup2Color(), width='150px')
                              ),
                              tags$div(style="display:inline-block; vertical-align: bottom;",
                                       uiOutput("ChooseLegendForSamples")
                              )
                            ),
                            tooltip="Plots parameters",
                            style = "material-circle", icon = icon("gear"), status = optionsBtnClass
                          )))
               
      )),
    
    
    fluidRow(
      column(width=6,moduleDensityplotUI("densityPlot_DS")),
      column(width=6, moduleBoxplotUI("boxPlot_DS"))
    )
  )
  
})
