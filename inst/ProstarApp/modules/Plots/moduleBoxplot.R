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
moduleBoxplot <- function(input, output, session, dataIn, params) {
  ns <- session$ns
  rv.modboxplot <- reactiveValues(
    var = NULL,
    ind = NULL
  )
  
  rv.modboxplot$var <- callModule(moduleTrackProt, "widgets", params=reactive({params()}), reset=reactive({FALSE}))
  
  observeEvent(req(rv.modboxplot$var()),{
    if (is.null(rv.modboxplot$var()$type)){return(NULL)}
    print("In observe rv.modboxplot$var")
    print(rv.modboxplot$var())
    
    ll <- Biobase::fData(dataIn()$obj)[,dataIn()$obj@experimentData@other$proteinId]
    switch(rv.modboxplot$var()$type,
           ProteinList = {rv.modboxplot$ind <- match(rv.modboxplot$var()$list, ll)},
           Random = {rv.modboxplot$ind <- sample(1:length(ll), rv.modboxplot$var()$rand, replace=FALSE)},
           Column = {rv.modboxplot$ind <- which(rv.modboxplot$var()$col == 1)}
    )
    if (length(rv.modboxplot$ind)==0){rv.modboxplot$ind <- NULL}
    
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
    req(dataIn()$obj)
    #rv$PlotParams$paletteConditions
    rv.prostar$settings()$legendForSamples
    name <- dataIn()$name
    rv.modboxplot$ind
    
    tmp <- NULL
    isolate({
      
      pattern <- paste0(name,".boxplot")
      tmp <- DAPAR::boxPlotD_HC(dataIn()$obj, rv.prostar$settings()$legendForSamples, 
                                palette=rv.prostar$settings()$examplePalette,
                                subset.view = rv.modboxplot$ind)
      #future(createPNGFromWidget(tmp,pattern))
      
      
    })
    tmp
  })
  
  
  output$viewViolinPlot <- renderPlot({
    
    req(dataIn())
    rv.prostar$settings()$legendForSamples
    rv.prostar$settings()$examplePalette
    tmp <- NULL
    rv.modboxplot$ind
    isolate({
      pattern <- paste0(dataIn()$name,".violinplot")
      tmp <- DAPAR::violinPlotD(dataIn()$obj, 
                                rv.prostar$settings()$legendForSamples, 
                                palette=rv.prostar$settings()$examplePalette,
                                subset.view =  rv.modboxplot$ind)
      #future(createPNGFromWidget(tmp,pattern))
    })
    tmp
  }) 
  
  return(reactive({rv.modboxplot$var()}))
}
