#####################################
moduleTrackProtUI <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(ns("typeSelect"), "Type of selection", 
                choices=c("Protein list"="ProteinList", "Random"="Random", "Column"="Column"),
                width=('130px')),
    uiOutput(ns("listSelect_UI")),
    uiOutput(ns("randomSelect_UI")),
    uiOutput(ns("columnSelect_UI"))
  )
}



#-----------------------------------------------
moduleTrackProt <- function(input, output, session, params, reset){
  
  ns <- session$ns
  
  # rv.track <- reactiveValues(reset = FALSE)
  # 
  # observeEvent(reset(),{
  #   if(isTRUE(reset())) {
  #     
  #     print("toto")
  #   } else {
  #     print("NON")
  #   }
  #   rv.track$reset <- FALSE
  # })
  # 
  
  observe({
    params()
    updateSelectInput(session, "typeSelect", selected=params()$type)
    updateSelectInput(session, "listSelect", selected=params()$list)
    updateSelectInput(session, "randSelect", selected=params()$rand)
    updateSelectInput(session, "colSelect", selected=params()$col)
  })
  
  observeEvent(input$typeSelect, {
    shinyjs::toggle("listSelect", condition=input$typeSelect=="ProteinList")
    shinyjs::toggle("randSelect", condition=input$typeSelect=="Random")
    shinyjs::toggle("colSelect", condition=input$typeSelect=="Column")
  })
  
  output$listSelect_UI <- renderUI({
    isolate({
      ll <-  Biobase::fData(rv$current.obj)[,rv$current.obj@experimentData@other$proteinId]
      selectInput(ns("listSelect"), "Protein for normalization", choices=ll, multiple = TRUE, width='400px')
    })
  })
  
  
  output$randomSelect_UI <- renderUI({
    isolate({
      ll <-  Biobase::fData(rv$current.obj)[,rv$current.obj@experimentData@other$proteinId]
      hidden(textInput(ns("randSelect"), "Random", value="1", width=('120px')))
    })
  })
  
  output$columnSelect_UI <- renderUI({
    isolate({
      ll <-  colnames(Biobase::fData(rv$current.obj))
      hidden(selectInput(ns("colSelect"), "Column", choices=ll))
    })
  })
  
  return(reactive({list(type = input$typeSelect,
                        list = input$listSelect,
                        rand = as.numeric(input$randSelect),
                        col = input$colSelect)}))
}
