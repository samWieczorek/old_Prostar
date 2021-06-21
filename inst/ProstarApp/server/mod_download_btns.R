
mod_download_btns_ui <- function(id){
  
  ns <- NS(id)
  
  tagList(
    downloadButton(ns("download_as_Excel_btn"), "Excel", class = actionBtnClass),
    downloadButton(ns("download_as_csv_btn"), "csv", class = actionBtnClass)
  )
}





mod_download_btns_server <- function(id, df.data, name, colors, df.tags) {
  
  
  moduleServer(
    id,
    function(input, output, session) {

      
      # observeEvent(df.data(), {
      #   browser()
      # })
      
      output$download_as_csv_btn <- downloadHandler(
        filename = function() {
          paste(name(), "-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
          write.table(df.data(), file, sep = ";", row.names = FALSE)
        }
      )
      
      
      
      output$download_as_Excel_btn <- downloadHandler(
        filename = function() {
          paste(name(), "-", Sys.Date(), ".xlsx", sep="")
        },
        content = function(file) {
          fname <- paste("temp", Sys.Date(), ".xlsx", sep="")
          write.excel(df = df.data(), 
                      colors = colors(), 
                      tags = df.tags(),
                      filename = fname
                      )
          
          file.rename(fname, file)
        }
      )
      
      
    }
  )
}

