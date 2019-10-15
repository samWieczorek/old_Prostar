moduleProcess <- function(input, output, session, isDone, pages, rstFunc, forceReset){
  ns <- session$ns
  
  current <- reactiveVal(1)
  nbSteps <- length(pages()$stepsNames)
   
   ##--------------------------------------------------------------
  ## Gestion des couleurs du slideshow
  ##--------------------------------------------------------------
   
  output$checkPanel <- renderUI({
    current()
    color <- rep("lightgrey",nbSteps)
    colorForCursor <- rep("white",nbSteps)
    

      for (i in 1:nbSteps){
        status <- isDone()[i]
        col <- ifelse(pages()$isMandatory[i], "red", orangeProstar)
        ifelse(status, color[i] <- "green", color[i] <- col)
        }

    colorForCursor[current()] <- "black"
    buildTable(pages()$stepsNames, color,colorForCursor)

  })
  

   observeEvent(c(forceReset(),input$rstBtn),{
     current()
     if (forceReset()>0 || input$rstBtn > 0){
       print("ON FAIT LE RESET EFFECTIF")
       rstFunc()
       current(1)
     }
     })
   
  observe({
    current()
     toggle(id = "prevBtn", condition = (nbSteps >1))
    toggle(id = "nextBtn", condition = (nbSteps >1) )
    
    toggle(id = "rstBtn", condition = !(isDone()[nbSteps])) 
    
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
  
  observeEvent(input$prevBtn,ignoreInit = TRUE,{navPage(-1)})
  observeEvent(input$nextBtn,ignoreInit = TRUE,{navPage(1)})
  
 
  
  output$screens <- renderUI({
    isolate({
      ll <- NULL
       #isolate({
         
        for (i in 1:nbSteps){
          if (i == 1) {
            ll[[i]] <- div(id = ns(paste0("screen",i)), pages()$ll.UI[[i]])
            } else {
              ll[[i]] <- shinyjs::hidden(div(id = ns(paste0("screen",i)), pages()$ll.UI[[i]]))
            }
        }
      
   
  tagList(ll)
       })

  })
  
  
  observeEvent(current(),{
    
    for (i in 1:nbSteps){
      shinyjs::toggle(id = paste0("screen", i), condition = current() == i)
    }
   })
  
 }


