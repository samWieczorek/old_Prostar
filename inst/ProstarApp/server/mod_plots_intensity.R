# Module UI

#' @title   mod_plots_intensity_plots_ui and mod_plots_intensity_plots_server
#' 
#' @description  A shiny Module.
#'
#' @param id shiny id
#' 
#' @param input internal
#' 
#' @param output internal
#' 
#' @param session internal
#'
#' @rdname mod_plots_intensity_plots
#'
#' @keywords internal
#' 
#' @export 
#' 
#' @importFrom shiny NS tagList 
#' @import shinyjs
#' 
mod_plots_intensity_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    tags$div(
      tags$div(style="display:inline-block; vertical-align: middle;",
               highchartOutput(ns("BoxPlot")),
               shinyjs::hidden(imageOutput(ns("viewViolinPlot")))
      ),
      tags$div(style="display:inline-block; vertical-align: middle;",
               selectInput(ns("choosePlot"), "Choose plot", 
                           choices=setNames( c("violinplot", "boxplot"),c('violinplot', "boxplot")), 
                           width='100px'),
               uiOutput(ns('slave_tracking_ui'))
      )
    )  
  )
}

# Module Server

#' @rdname mod_plots_intensity_plots
#' 
#' @export
#' 
#' @keywords internal
#' 
#' @importFrom DAPAR violinPlotD boxPlotD_HC 
#' 
mod_plots_intensity_server <- function(input, output, session,
                                       dataIn,
                                       meta,
                                       keyId,
                                       conds,
                                       base_palette=NULL,
                                       params=NULL,
                                       reset=NULL,
                                       slave = FALSE){
  
  
  ns <- session$ns
  
  rv.modboxplot <- reactiveValues(
    indices = NULL,
    varTrack = NULL
  )
  
  
  
  rv.modboxplot$varTrack <- callModule(mod_plots_tracking_server, "slave_tracking",
                                       obj = reactive({dataIn()}),
                                       keyId=reactive({keyId()}),
                                       params=reactive({params()}),
                                       reset=reactive({reset()}),
                                       slave = reactive({slave()})
  )
  
  
  observe({
    params()
    print('params() = ')
    print(params())
  })
  
  observe({
    slave()
    print('slave() = ')
    print(slave())
  })
  
  
  output$slave_tracking_ui <- renderUI({
    slave()
    dataIn()
    if ((slave()==FALSE) && dataIn()@experimentData@other$typeOfData=='protein')
    {
      mod_plots_tracking_ui(ns('slave_tracking'))
    }
    else 
      return(NULL)
  })
  
  
  
  observeEvent(c(slave(),rv.modboxplot$varTrack()),ignoreInit = TRUE, ignoreNULL=FALSE, {
    if (slave() == TRUE){
      switch(params()$typeSelect,
             ProteinList = rv.modboxplot$indices <- params()$list.indices,
             Random = rv.modboxplot$indices <- params()$rand.indices,
             Column = rv.modboxplot$indices <- params()$col.indices,
             None = rv.modboxplot$indices <- NULL
      )
    } else {
      tmp <- if (is.null(rv.modboxplot$varTrack()$typeSelect)) 'None' 
      else rv.modboxplot$varTrack()$typeSelect
      switch(tmp,
             ProteinList = rv.modboxplot$indices <- rv.modboxplot$varTrack()$list.indices,
             Random =  rv.modboxplot$indices <- rv.modboxplot$varTrack()$rand.indices,
             Column =  rv.modboxplot$indices <- rv.modboxplot$varTrack()$col.indices,
             None = rv.modboxplot$indices <- NULL
      )
    }
  })
  
  
  
  
  
  observeEvent(input$choosePlot, {
    shinyjs::toggle('viewViolinPlot', condition=input$choosePlot=='violinplot')
    shinyjs::toggle('BoxPlot', condition=input$choosePlot=='boxplot')
  })
  
  
  
  output$BoxPlot <- renderHighchart({
    dataIn()
    rv.modboxplot$indices
    tmp <- NULL
    
    
    pattern <- paste0('test',".boxplot")
    withProgress(message = 'Making plot', value = 100, {
      tmp <- DAPAR::boxPlotD_HC(obj = dataIn(),
                                 conds = conds(),
                                 keyId = keyId(),
                                 palette = base_palette(),
                                 subset.view = rv.modboxplot$indices)
      #future(createPNGFromWidget(tmp,pattern))
    })
    tmp
  })
  
  
  output$viewViolinPlot<- renderImage({
    dataIn()
    rv.modboxplot$indices
    tmp <- NULL
    
    # A temp file to save the output. It will be deleted after renderImage
    # sends it, because deleteFile=TRUE.
    outfile <- tempfile(fileext='.png')
    # Generate a png
    withProgress(message = 'Making plot', value = 100, {
      # png(outfile, width = 640, height = 480, units = "px")
      png(outfile)
      pattern <- paste0('test',".violinplot")
      tmp <- DAPAR::violinPlotD(obj = dataIn(),
                                 keyId = keyId(),
                                 conds = conds(),
                                 palette = base_palette(),
                                 subset.view =  rv.modboxplot$indices)
      #future(createPNGFromWidget(tmp,pattern))
      dev.off()
    })
    tmp
    
    # Return a list
    list(src = outfile,
         alt = "This is alternate text")
  }, deleteFile = TRUE)
  
  
  return(reactive({rv.modboxplot$varTrack()}))
}

## To be copied in the UI
# mod_plots_intensity_plots_ui("plots_intensity_plots_ui_1")

## To be copied in the server
# callModule(mod_plots_intensity_plots_server, "plots_intensity_plots_ui_1")
