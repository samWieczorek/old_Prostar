output$plotintensitysmall <- renderImage({
  filename <- normalizePath(file.path('./images','desc_intdistrib.png'))
  list(src = filename,
       width = .width,
       height = .height)
}, deleteFile = FALSE)


callModule(moduleBoxplot, "boxPlot", dataIn = reactive({dataIn()}))
callModule(moduleDensityplot, "densityPlot", dataIn = reactive({dataIn()}))


output$plotintensitylarge <- renderUI({
  
  print("IN output$plotintensitylarge <- renderUI")
  tagList(
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
      column(width=6, moduleDensityplotUI(ns("densityPlot"))),
      column(width=6, moduleBoxplotUI(ns("boxPlot")))
    )
  )
  
  
})




output$ChooseLegendForSamples <- renderUI({
  #req(data())
  print("IN output$ChooseLegendForSamples <- renderUI")
  .names <- colnames(Biobase::pData(dataIn()$obj()))
  checkboxGroupInput(ns("legendForSamples"),
                     label = "Choose data to show in legend",
                     choices = .names,
                     selected=.names[2])
})

observeEvent(input$legendForSamples, {
  rv.settings$legendForSamples <- as.vector(apply(as.data.frame(Biobase::pData(dataIn()$obj())[,input$legendForSamples]), 1, function(x) paste(x, collapse="_")))
})