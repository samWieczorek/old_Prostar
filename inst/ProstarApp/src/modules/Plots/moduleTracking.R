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
moduleTrackProt <- function(input, output, session, dataIn, params, reset=FALSE){
  
  ns <- session$ns
  
  observe({
    reset()
    print("In track module =RESET observe")
    print(reset())
    if (reset() > 0) {
      updateSelectInput(session, "typeSelect", selected="None")
      updateSelectInput(session, "listSelect", NULL)
      updateSelectInput(session, "randSelect", selected="1")
      updateSelectInput(session, "colSelect", selected=NULL)
    }
  })
  
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
      ll <-  Biobase::fData(dataIn())[,dataIn()@experimentData@other$proteinId]
      selectInput(ns("listSelect"), "Protein for normalization", choices=ll, multiple = TRUE, width='400px')
    })
  })
  
  
  output$randomSelect_UI <- renderUI({
    isolate({
      ll <-  Biobase::fData(dataIn())[,dataIn()@experimentData@other$proteinId]
      hidden(textInput(ns("randSelect"), "Random", value="1", width=('120px')))
    })
  })
  
  output$columnSelect_UI <- renderUI({
    isolate({
      ll <-  colnames(Biobase::fData(dataIn()))
      hidden(selectInput(ns("colSelect"), "Column", choices=ll))
    })
  })
  
  
  BuildResult <- reactive({
    
    #isolate({
    ll <-  Biobase::fData(dataIn())[,dataIn()@experimentData@other$proteinId]
    
    
    res <- list(type= input$typeSelect,
                list = input$listSelect,
                rand = as.numeric(input$randSelect),
                col = input$colSelect,
                list.indices = if (length(input$listSelect)==0){NULL} else match(input$listSelect, ll),
                rand.indices = if (length(input$randSelect)==0){NULL} else sample(1:length(ll), as.numeric(input$randSelect), replace=FALSE),
                col.indices =  if (length(input$colSelect)==0){NULL} else which(input$colSelect == 1)
    )
    
    # })
    print("res")
    res
  })
  
  return(reactive({BuildResult()}))

}