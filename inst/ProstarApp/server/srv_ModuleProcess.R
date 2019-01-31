moduleProcess <- function(input, output, session, params, final_msg, ll.UI){
  
  ns <- session$ns
  current <- reactiveVal(1)
  nbSteps <- length(params()$name)
  
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
    #hide(selector = ".page")
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
      ll <- NULL
        for (i in 1:nbSteps){
          if (i == 1) {
            ll[[i]] <- div(id = ns(paste0("screen",i)), ll.UI()[[i]])
            } else {
              ll[[i]] <- shinyjs::hidden(div(id = ns(paste0("screen",i)), ll.UI()[[i]]))
            }
        }
      
  tagList(ll)
  })
  
  
  observeEvent(current(),{
    for (i in 1:nbSteps){
      shinyjs::toggle(id = paste0("screen", i), condition = current() == i)
    }
   })
  
 }


