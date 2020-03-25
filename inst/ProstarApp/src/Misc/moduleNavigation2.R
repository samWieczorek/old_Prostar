
######################################################################################
##
###### Module de gestion des interfaces pour suivre les data Process  dans un module pipeline  ######
##
####################################################################################################
moduleNavigation2UI <- function(id){
  ns <- NS(id)
  
}







moduleNavigation2 <- function(input, output, session, isDone, pages, rstFunc, type){
  ns <- session$ns
  
  current <- reactiveValues(
    val = 1
  )
  nbSteps <- length(pages()$stepsNames)
  print(paste0('nbSteps = ',nbSteps))

  ##--------------------------------------------------------------
  ## Gestion des couleurs du slideshow
  ##--------------------------------------------------------------
  
  
  output$checkPanel <- renderUI({
    status <- isDone()
    #buildTable(pages()$stepsNames, color,colorForCursor, params())
  
    
    steps <- pages()$stepsNames
    status[which(status==1)] <- 'completed'
    status[current$val] <- 'active'
    status[which(status==0)] <- 'undone'
    
    if (type() == 'bubble') {
      txt <- "<ul class=\'progress-indicator\'>"
    } else if (type() == 'rectangle'){
      txt <- "<ul class=\'progress-indicator custom-complex\'>"
    }
    
    for( i in 1:length(steps) ) {
      txt <- paste0(txt, "<li class='",status[i], "'> <span class=\'bubble\'> </span>", steps[i], "</li>")
    }
    txt <- paste0(txt,"</ul>")
    
    return(HTML(txt))
  })
  

  
 
  
  observeEvent(input$rstBtn,{
    current$val <- 1
    rstFunc()
  })
  
  
  ##--------------------------------------------------------------
  ## Navigation dans le slideshow
  ##--------------------------------------------------------------
  
  navPage <- function(direction) {
 
    current$val <- current$val + direction
  }
  
  observeEvent(input$prevBtn,{navPage(-1)})
  observeEvent(input$nextBtn,{navPage(1)})
  
  
  
  bars <- reactive({
    tagList(
     
    div(
      div( style="align: center;display:inline-block; vertical-align: top; padding: 7px",
           disabled(actionButton(ns("rstBtn"), "reset", 
                                          class = PrevNextBtnClass,
                                          style='padding:4px; font-size:80%'))),
      div( style="align: center;display:inline-block; vertical-align: top; padding: 7px",
           disabled(actionButton(ns("prevBtn"), "<<", 
                                          class = PrevNextBtnClass,
                                          style='padding:4px; font-size:80%'))),
      div( style="align: center;display:inline-block; vertical-align: top;",
           uiOutput(ns("checkPanel" ))),
      
      
      div(style="align: center;display:inline-block; vertical-align: top; padding: 7px",
            actionButton(ns("nextBtn"), ">>", 
                       class = PrevNextBtnClass, 
                       style='padding:4px; font-size:80%')
            
      )
        )
  
    )
  })
  
  screens <- reactive({
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
  
 
  
  observe({
    current$val
    
    if (current$val < nbSteps) {
      enable('nextBtn')
    } else {disable('nextBtn')}
    
    if (current$val == 1) {
      disable('prevBtn')
    } else {enable('prevBtn')}
    
    hide(selector = ".page")
    
  })
  
  observeEvent(current$val,{
    
    for (i in 1:nbSteps){
      shinyjs::toggle(id = paste0("screen", i), condition = current$val == i)
    }
  })
  
  
  return(reactive(list(bars=bars(),
                  screens=screens())))
}


