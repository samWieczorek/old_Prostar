output$plotintensitysmall <- renderImage({
  filename <- normalizePath(file.path('./images','desc_intdistrib.png'))
  list(src = filename,
       width = .width,
       height = .height)
}, deleteFile = FALSE)


callModule(moduleBoxplot, "boxPlot")
callModule(moduleDensityplot, "densityPlot")


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
                                                   selected=GetWhichGroup2Color(), width='150px')
                              ),
                              tags$div(style="display:inline-block; vertical-align: bottom;",
                                       uiOutput(ns("ChooseLegendForSamples"))
                              )
                            ),
                            tooltip="Plots parameters",
                            style = "material-circle", icon = icon("gear"), status = optionsBtnClass
                          )))
               
      )),
    
    
    tagList(
      moduleDensityplotUI(ns("densityPlot"))
      moduleBoxplotUI(ns("boxPlot"))
    )
  )
  
})




output$ChooseLegendForSamples <- renderUI({
  req(rv$current.obj)
  
  .names <- colnames(Biobase::pData(rv$current.obj))
  
  
  checkboxGroupInput(ns("legendForSamples"),
                     label = "Choose data to show in legend",
                     choices = .names,
                     selected=.names[2])
})

observeEvent(input$legendForSamples, {
  rv$PlotParams$legendForSamples <- as.vector(apply(as.data.frame(Biobase::pData(rv$current.obj)[,input$legendForSamples]), 1, function(x) paste(x, collapse="_")))
})