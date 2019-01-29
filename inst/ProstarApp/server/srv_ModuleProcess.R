moduleProcess <- function(input, output, session, nbPage, currentPage, txt){
  
  ns <- session$ns
  current <- reactiveVal(1)
  ##--------------------------------------------------------------
  ## Gestion du slideshow
  ##--------------------------------------------------------------
  
  
  output$checkPanel <- renderUI({
    req(current())
    # input$datasets
    #rv$pageFiltering
    color <- rep("lightgrey",nbPage())
    
    ##Step 1
    if (current() >= 1){
      res <- rv$mvFiltering_Done
      ifelse(res, color[1] <- "green", color[1] <- orangeProstar)
    }
    
    ##Step 2: Choose data ID
    
    if (current() >= 2){
      res <- rv$stringBasedFiltering_Done
      ifelse(res, color[2] <- "green", color[2] <- orangeProstar)
      
    } 
    
    ## Step 3: Choose quantitative data
    if (current() == 3){
      res <- length(grep("Filtered", input$datasets))==1
      ifelse(res, color[3] <- "green", color[3] <- "red")
    }
    
    buildTable(txt(), color)
  })
  
  
  
  
  
  observe({
    toggleState(id = "prevBtn", condition = current() > 1)
    toggleState(id = "nextBtn", condition = current() < nbPage())
    hide(selector = ".page")
    # show(paste0("step", rv$pageFiltering))
  })
  
  navPage <- function(direction) {
    newValue <- current() + direction 
    current(newValue)  
   
    
    #updateSelectInput(session, "ChooseFilters", selected = ns("input$ChooseFilters"))
    #updateSelectInput(session, "seuilNA", selected = ns("input$seuilNA"))
  }
  
  observeEvent(input$prevBtn,{print("toto"); navPage(-1)})
  observeEvent(input$nextBtn,{print("titi"); navPage(1)})
  
  
  output$Done <- renderUI({
    #input$datasets
    if( length(grep("Filtered", input$datasets))==0) {return()}
    
    # shinyjs::hide('prevBtnFiltering')
    # shinyjs::hide('nextBtnFiltering')
    
    tags$p(style="font-size: 24;",tags$b("The filtering has been processed."))
    
    
  })
  
  
  
}


