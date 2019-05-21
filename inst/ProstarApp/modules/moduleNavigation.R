
###### Module de gestion des interfaces pour les data Process   ######
moduleNavigationUI <- function(id){
  ns <- NS(id)
  
  tagList(
    div( style = 'width: 100%',
      div( style="align: center;display:inline-block; vertical-align: top; padding: 7px",
           shinyjs::hidden(actionButton(ns("rstBtn"), "reset", class = PrevNextBtnClass,style='padding:4px; font-size:80%'))),
      div( style="align: center;display:inline-block; vertical-align: top; padding: 7px",
           shinyjs::hidden(actionButton(ns("prevBtn"), "<<", class = PrevNextBtnClass,style='padding:4px; font-size:80%'))),
      div( style="align: center;display:inline-block; vertical-align: top;",
           uiOutput(ns("checkPanel" ))),
      div( style="align: center;display:inline-block; vertical-align: top; padding: 7px",
           shinyjs::hidden(actionButton(ns("nextBtn"), ">>", class = PrevNextBtnClass, style='padding:4px; font-size:80%')))
      
    ),
    hr(),
    uiOutput(ns("screens"))
    
  )
}







moduleNavigation <- function(input, output, session, isDone, pages, rstFunc, params){
  ns <- session$ns
  current <- reactiveVal(1)
  print(pages()$stepsNames)
  nbSteps <- length(pages()$stepsNames)
  
  ##--------------------------------------------------------------
  ## Gestion des couleurs du slideshow
  ##--------------------------------------------------------------
  
  
  output$checkPanel <- renderUI({
    params()
    
    rectColors <- params()$rectColors
    color <- rep(rectColors[1],nbSteps)
    colorForCursor <- rep("white",nbSteps)
    
    
    for (i in 1:nbSteps){
      status <- isDone()[i]
      col <- ifelse(pages()$isMandatory[i], rectColors[2], orangeProstar)
      ifelse(status, color[i] <- rectColors[3], color[i] <- col)
    }
    
    colorForCursor[current()] <- "black"
    print(color)
    #buildTable(pages()$stepsNames, color,colorForCursor, params())
    buildTimeline()
  })
  
  
  
  
  buildTimeline <- function() {
    
    
    txt <- "<ul class=\"progress-indicator\">
 <li class=\"completed\"> <span class=\"bubble\"></span> Original </li>
    <li class=\"active\"> <span class=\"bubble\"></span> Filtering </li>
    <li> <span class=\"bubble\"></span> Imputation </li>
    <li> <span class=\"bubble\"></span> Imputation </li>
    <li> <span class=\"bubble\"></span> Agregation </li>
    </ul>
    "
    
    return(HTML(txt))
  }
  
  
  
  buildTable <- function(text, color, colorCurrentPos, params){
    paste0("     ", text, "     ")
    rows.color <- rows.text <-  rows.cursor <- list()
    rows.text <- list()
    # for( i in 1:length( color ) ) {
    #   rows.color[[i]] <-lapply( color[i], function( x ) tags$th(  style=paste0("color: white; background-color:", x,"; height: ",params$height,"px;" ),text[i] ))
    #   rows.cursor[[i]] <-lapply( colorCurrentPos[i], function( x ) tags$th(  style=paste0("background-color:", x,"; height: 5px;" ) ))
    # }
    # style <- paste0("width: 100%; text-align: center;border: 1;border-collapse: separate;border-spacing: 10px;padding-top: 0px;")
    # html.table <-  tags$table(style = style,
    #                           tags$tr( rows.color ),
    #                           tags$tr( rows.cursor )
    # )
    
    
    for( i in 1:length( color ) ) {
      rows.color[[i]] <- lapply( color[i], function( x ) tags$th(  style=paste0(" background-color:", x,"; height: ",params$height,"px;" ), text[i] ))
     # rows.cursor[[i]] <-lapply( colorCurrentPos[i], function( x ) tags$td(  style=paste0("background-color:", x,"; height: 5px;" ) ))
    }
    
    style <- paste0("width: 100%; text-align: center;border-collapse: separate; border-spacing: 10px 0;text-align: center; padding-top: 5px; cellpadding: 10px;")
    html.table <-  tags$table(style = style,
                              tags$tr( style="color: white; padding-left: 5px; padding-right: 5px; ",
                                       rows.color )
                             # tags$tr( style="padding-top: 0px; padding-left: 5px; padding-right: 5px;",
                             #          rows.cursor )
    )
    
   
    
    return(html.table)
    
  }
  
  
  
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


