moduleBoxplotUI <- function(id) {
  ns <- NS(id)
  tagList(
    highchartOutput(ns("BoxPlot")),
    plotOutput(ns("viewViolinPlot")),
    selectInput(ns("choosePlot"), "Choose plot", choices=c( "violinplot"="violinplot","boxplot"="boxplot"), width='100px'),
    moduleTrackProtUI(ns('widgets'))
    )
}




#------------------------------------------------------------
moduleBoxplot <- function(input, output, session, dataIn, params, reset) {
  
  ns <- session$ns
  rv.modboxplot <- reactiveValues(
    var = NULL,
    ind = NULL,
    indices = NULL
  )
  source(file.path("./src", "modules/Plots/moduleTracking.R"), local = TRUE)$value
  
  rv.modboxplot$var <- callModule(moduleTrackProt, "widgets", 
                                  dataIn = reactive({dataIn()$obj}),
                                  params=reactive({params()}), 
                                  reset=reactive({reset()}))
  
  observeEvent(req(rv.modboxplot$var()),{
    print("In observe rv.modboxplot$var")
    print(rv.modboxplot$var())
    
    
    if (is.null(rv.modboxplot$var()$type)){return(NULL)}
    
    ll <- Biobase::fData(rv$current.obj)[,rv$current.obj@experimentData@other$proteinId]
    switch(rv.modboxplot$var()$type,
           #ProteinList = rv.modboxplot$ind <- rv.modboxplot$var()$list,
           #Random = rv.modboxplot$ind <- rv.modboxplot$var()$rand,
           # Column = rv.modboxplot$ind <- rv.modboxplot$var()$col,
           ProteinList = rv.modboxplot$indices <- rv.modboxplot$var()$list.indices,
           Random = rv.modboxplot$indices <- rv.modboxplot$var()$rand.indices,
           Column = rv.modboxplot$indices <- rv.modboxplot$var()$col.indices
    )
    #if (length(rv.modboxplot$ind)==0){rv.modboxplot$ind <- NULL}
    if (length(rv.modboxplot$indices)==0){rv.modboxplot$indices <- NULL}
  })
  
  
  observeEvent(input$choosePlot, {
    switch(input$choosePlot,
           boxplot={
             shinyjs::hide('viewViolinPlot')
             shinyjs::show('BoxPlot')
           },
           violinplot={
             shinyjs::hide('BoxPlot')
             shinyjs::show('viewViolinPlot')
           }
    )
  })
  
  
  
  output$BoxPlot <- renderHighchart({
    dataIn()
    rv$current.obj.name
    rv$PlotParams$paletteConditions
    rv$PlotParams$legendForSamples
    rv.modboxplot$indices
    tmp <- NULL
    isolate({
      ll <- Biobase::fData(rv$current.obj)[,rv$current.obj@experimentData@other$proteinId]
      
      pattern <- paste0(GetCurrentObjName(),".boxplot")
      tmp <- DAPAR::boxPlotD_HC(dataIn(), rv$PlotParams$legendForSamples, palette=rv$PlotParams$paletteConditions,
                                subset.view = rv.modboxplot$indices)
      #future(createPNGFromWidget(tmp,pattern))
      
      
    })
    tmp
  })
  
  
  output$viewViolinPlot<- renderImage({
    dataIn()
    rv$PlotParams$legendForSamples
    rv$PlotParams$paletteConditions
    rv.modboxplot$indices
    tmp <- NULL
    isolate({
      
      # A temp file to save the output. It will be deleted after renderImage
      # sends it, because deleteFile=TRUE.
      outfile <- tempfile(fileext='.png')
      print("IN violinPlot")
      print(rv.modboxplot$indices)
      print("END IN violinplot")
      # Generate a png
      # png(outfile, width = 640, height = 480, units = "px")
      png(outfile)
      pattern <- paste0(GetCurrentObjName(),".violinplot")
      tmp <- DAPAR::violinPlotD(dataIn(), legend = rv$PlotParams$legendForSamples, 
                                palette = rv$PlotParams$paletteConditions,
                                subset.view =  rv.modboxplot$indices)
      #future(createPNGFromWidget(tmp,pattern))
      dev.off()
    })
    tmp
    
    # Return a list
    list(src = outfile,
         alt = "This is alternate text")
  }, deleteFile = TRUE)
  
  
  return(reactive({rv.modboxplot$var()}))
}
