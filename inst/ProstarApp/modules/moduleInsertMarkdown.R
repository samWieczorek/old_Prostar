moduleInsertMarkdownUI <- function(id){
  ns <- NS(id)
  uiOutput(ns("insertMD"))
}



moduleInsertMarkdown <- function(input, output, session,url){
  
  output$insertMD <- renderUI({
    tryCatch(
      {
        print(url)
        includeMarkdown(url)
      }
      , warning = function(w) {
        tags$p("URL not found<br>",conditionMessage(w))
      }, error = function(e) {
        shinyjs::info(paste("URL not found :", conditionMessage(e), sep=" "))
      }, finally = {
        #cleanup-code 
      })
    
  })
  
}