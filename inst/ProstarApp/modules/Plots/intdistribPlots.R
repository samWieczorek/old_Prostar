output$plotintensitysmall <- renderImage({
  filename <- normalizePath(file.path('./images','desc_intdistrib.png'))
  list(src = filename,
       width = .width,
       height = .height)
}, deleteFile = FALSE)


callModule(moduleBoxplot, ns("boxPlot"))
callModule(moduleDensityplot, ns("densityPlot"))


output$plotintensitylarge <- renderUI({
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
                                       selectInput(ns("whichGroup2Color"),
                                                   "Color lines",
                                                   choices=list("By condition" = "Condition",
                                                                "By replicate" = "Replicate"),
                                                   selected= Group2Color(), width='150px')
                              ),
                              tags$div(style="display:inline-block; vertical-align: bottom;",
                                       uiOutput(ns("ChooseLegendForSamples"))
                              )
                            ),
                            tooltip="Plots parameters",
                            style = "material-circle", icon = icon("gear"), status = optionsBtnClass
                          )))
               
      )),
    
    
    fluidRow(
      column(width=6,moduleDensityplotUI(ns("densityPlot"))),
      column(width=6, moduleBoxplotUI(ns("boxPlot")))
    )
  )
  
})




output$ChooseLegendForSamples <- renderUI({
  req(GetCurrentMSnSet())
  
  .names <- colnames(Biobase::pData(GetCurrentMSnSet()))
  checkboxGroupInput(ns("legendForSamples"),
                     label = "Choose data to show in legend",
                     choices = .names,
                     selected=.names[2])
})

observeEvent(input$legendForSamples, {
  rv$PlotParams$legendForSamples <- as.vector(apply(as.data.frame(Biobase::pData(GetCurrentMSnSet())[,input$legendForSamples]), 1, function(x) paste(x, collapse="_")))
})