moduleProcess <- function(input, output, session, params, final_msg, ll.UI){
  
  ns <- session$ns
  current <- reactiveVal(1)
  nbSteps <- length(params()$name)
  print(nbSteps)
  print(params())
  ##--------------------------------------------------------------
  ## Gestion des couleurs du slideshow
  ##--------------------------------------------------------------
  
  output$checkPanel <- renderUI({
    req(current())
    color <- rep("lightgrey",nbSteps)
    
    for (i in 1:nbSteps){
      ##Step 1
      if (current() >= i){
       res <- params()$isDone[i]
        ifelse(params()$isMandatory[i], col <- "red", col <- orangeProstar)
        ifelse(res, color[i] <- "green", color[i] <- col)
        }
      }

    buildTable(params()$name, color)
  })
  

  observe({
    toggleState(id = "prevBtn", condition = current() > 1)
    toggleState(id = "nextBtn", condition = current() < nbSteps)
    hide(selector = ".page")
  })
  
  ##--------------------------------------------------------------
  ## Navigation dans le slideshow
  ##--------------------------------------------------------------
  
  navPage <- function(direction) {
    newValue <- current() + direction 
    current(newValue)
    
   }
  
  observeEvent(input$prevBtn,{navPage(-1)})
  observeEvent(input$nextBtn,{navPage(1)})
  
  
  output$Done <- renderUI({
    if (params()$isDone[nbSteps] == TRUE) {
      tags$p(style="font-size: 24;",tags$b(final_msg()))
      # shinyjs::hide('prevBtnFiltering')
      # shinyjs::hide('nextBtnFiltering')
    }
   })
  
  
  output$screens <- renderUI({
      tagList(
        div(id = ns("titi1"), ll.UI()[[1]]),
        shinyjs::hidden(div(id = ns("titi2"), ll.UI()[[2]])),
        shinyjs::hidden(div(id = ns("titi3"), ll.UI()[[3]]))
      )
    
    
    
  })
  
  
  observe({
    shinyjs::toggle(id = "titi1", condition = current() == 1)
    shinyjs::toggle(id = "titi2", condition = current() == 2)
    shinyjs::toggle(id = "titi3", condition = current() == 3)
  })
  
  
  # observe({
  #   shinyjs::toggle(id = ll.UI()[[1]], condition = current() == 1)
  #   shinyjs::toggle(id = ll.UI()[[2]], condition = current() == 2)
  #   shinyjs::toggle(id = ll.UI()[[3]], condition = current() == 3)
  # })
  
}


