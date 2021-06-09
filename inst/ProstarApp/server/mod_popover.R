modulePopoverUI <- function(id){
  ns <- NS(id)
  uiOutput(ns("customPopover"))
}

modulePopover <- function(input, output, session, data){
  
  ns <- session$ns
  
  output$customPopover <- renderUI({
    req(data())
    
    div(
      div(
        # edit1
        style="display:inline-block; vertical-align: middle; padding-bottom: 5px;",
        HTML(paste0("<strong>", data()$title, "</strong>"))
      ),
      div(
        # edit2
        style="display:inline-block; vertical-align: middle;padding-bottom: 5px;",
        if (!is.null(data()$color) && ('white' == data()$color)) {
          tags$button(id=ns("q1"), tags$sup("[?]"), class="Prostar_tooltip_white")
        } else {
          tags$button(id=ns("q1"), tags$sup("[?]"), class="Prostar_tooltip")
        },
        shinyBS::bsPopover(id = ns("q1"), title = "",
                           content = data()$content,
                           placement = "right", 
                           trigger = "hover", 
                           options = list(container = "body")
        )
      )
    )
    
    
  })
}