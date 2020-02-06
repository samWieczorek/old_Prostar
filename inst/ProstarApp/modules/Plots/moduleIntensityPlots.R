source(file.path(".", "modules/Plots/moduleBoxplot.R"), local = TRUE)$value
source(file.path(".", "modules/Plots/moduleDensityPlot.R"), local = TRUE)$value

moduleIntensityPlotsUI<- function(id)
{
  ns <- NS(id)
  
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
    column(width=6, moduleDensityplotUI(ns("densityPlot_AbsPanel"))),
    column(width=6, moduleBoxplotUI(ns("boxPlot_AbsPanel")))
  )
)


}



moduleIntensityPlots <- function(input, output, session, dataIn){
  ns <- session$ns
  
  
  
  callModule(moduleBoxplot, "boxPlot_AbsPanel", dataIn = reactive({dataIn()}),
             params = reactive({NULL}),
             reset=reactive({FALSE}))
  callModule(moduleDensityplot, "densityPlot_AbsPanel", dataIn = reactive({dataIn()}))
  
  
  
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
    rv.prostar$settings()$legendForSamples <- as.vector(apply(as.data.frame(Biobase::pData(dataIn()$obj())[,input$legendForSamples]), 1, function(x) paste(x, collapse="_")))
  })
  
}