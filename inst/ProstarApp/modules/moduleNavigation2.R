
###### Module de gestion des interfaces pour les data Process   ######
moduleNavigation2UI <- function(id){
  ns <- NS(id)
  
}







moduleNavigation2 <- function(input, output, session, isDone, pages, rstFunc, type){
  ns <- session$ns
  
  current <- reactiveVal(1)
  nbSteps <- length(pages()$stepsNames)
  
  ##--------------------------------------------------------------
  ## Gestion des couleurs du slideshow
  ##--------------------------------------------------------------
  
  
  output$checkPanel <- renderUI({
    status <- isDone()
    #buildTable(pages()$stepsNames, color,colorForCursor, params())
  
    
    steps <- pages()$stepsNames
    status[which(status==1)] <- 'completed'
    status[current()] <- 'active'
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
  

  
  # buildTable <- function(text, color, colorCurrentPos, params){
  #   paste0("     ", text, "     ")
  #   rows.color <- rows.text <-  rows.cursor <- list()
  #   rows.text <- list()
  #   for( i in 1:length( color ) ) {
  #     rows.color[[i]] <- lapply( color[i], function( x ) tags$th(  style=paste0("border-radius: 10px; background-color:", x,"; height: ",params$height,"px;" ), text[i] ))
  #     #rows.cursor[[i]] <-lapply( colorCurrentPos[i], function( x ) tags$td(  style=paste0("background-color:", x,"; height: 5px;" ) ))
  #   }
  #   
  #   style <- paste0("width: 100%; text-align: center;border-collapse: separate; border-spacing: 10px 0;text-align: center; padding-top: 5px; cellpadding: 10px;")
  #   html.table <-  tags$table(style = style,
  #                             tags$tr( style=" color: white; padding-left: 5px; padding-right: 5px;",
  #                                      rows.color )
  #                             #tags$tr( style="padding-top: 0px; padding-left: 5px; padding-right: 5px;",
  #                             #         rows.cursor )
  #   )
  #   
  #   print(html.table)
  #    return(html.table)
  #   
  # }
  # 
  # 
  
  observeEvent(input$rstBtn,{
    current(1)
    rstFunc()
  })
  
  observe({
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
  
  observeEvent(input$prevBtn,{navPage(-1)})
  observeEvent(input$nextBtn,{navPage(1)})
  
  
  bars <- reactive({
    div(
      div( style="align: center;display:inline-block; vertical-align: top; padding: 7px",
           shinyjs::hidden(actionButton(ns("rstBtn"), "reset", class = PrevNextBtnClass,style='padding:4px; font-size:80%'))),
      div( style="align: center;display:inline-block; vertical-align: top; padding: 7px",
           shinyjs::hidden(actionButton(ns("prevBtn"), "<<", class = PrevNextBtnClass,style='padding:4px; font-size:80%'))),
      div( style="align: center;display:inline-block; vertical-align: top;",
           uiOutput(ns("checkPanel" ))),
      div( style="align: center;display:inline-block; vertical-align: top; padding: 7px",
           shinyjs::hidden(actionButton(ns("nextBtn"), ">>", class = PrevNextBtnClass, style='padding:4px; font-size:80%')))
      
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
  
  
  observeEvent(current(),{
    
    for (i in 1:nbSteps){
      shinyjs::toggle(id = paste0("screen", i), condition = current() == i)
    }
  })
  
  
  return(reactive(list(bars=bars(),
                  screens=screens())))
}


