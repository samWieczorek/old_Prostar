


moduleFilterStringbasedOptionsUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("FilterStringbasedOptions"))
}



moduleFilterStringbasedOptions <- function(input, output, session) {
  ns <- session$ns
  output$FilterStringbasedOptions <- renderUI({
    rv$current.obj
    if (is.null(rv$current.obj)){return()}
    
    tagList(
      h4("String based filtering options")
      ,hr()
      ,h4("Filter contaminants"),
      uiOutput(ns("id_Contaminants")),
      uiOutput(ns("choosePrefixContaminants")),
      br(),
      h4("Filter reverse"),
      uiOutput(ns("id_Reverse")),
      uiOutput(ns("choosePrefixReverse")),
      br(),
      #actionButton("resetFilterParamsButton","Reset parameters"),
      actionButton(ns("performFilteringContaminants"),
                   "Perform string-based filtering", class = actionBtnClass)
    )
    
    
    
    
    
    
  })
}

