library(shiny)
library(DT)
library(MSnbase)




shinyOutput <- function(FUN,id,num,...) {
  inputs <- character(num)
  for (i in seq_len(num)) {
    inputs[i] <- as.character(FUN(paste0(id,i),label=NULL,...))
  }
  inputs
}


# function for dynamic inputs in DT
shinyInput <- function(FUN, id, num,...) {
  inputs <- character(num)
  for (i in seq_len(num)) {
    inputs[i] <- as.character(FUN(paste0(id,i),label=NULL,...))
  }
  inputs
}


# function to read DT inputs
shinyValue <- function(id, num) {
  unlist(lapply(seq_len(num),function(i) {
    value <- input[[paste0(id,i)]]
    if (is.null(value)) NA else value
  }))
}





ui <- fluidPage(
  tagList(
    actionButton('test', 'test'),
    DT::dataTableOutput('x1'),
  tags$script(HTML("Shiny.addCustomMessageHandler('unbind-DT', function(id) {
                                   Shiny.unbindAll($('#'+id).find('table').DataTable().table().node());
                                   })")),
  tags$script("
    Shiny.addCustomMessageHandler('input2', function(value) {
    Shiny.setInputValue('input2', value);
    });
  ")
  )
)



server <- function(input, output, session) {
  
  
  observeEvent(input$test, {
    print('toto')
    session$sendCustomMessage("input2", 'Species')
    Shiny.setInputValue("input2", 'Species');
  })
  
  quantiDataTable <- reactive({
    
    df <- NULL
    session$sendCustomMessage('unbind-DT', 'x1')
      
      df <- data.frame(test=c(selectInput("input1","1",
                                  choices = setNames(nm = c("None", colnames(iris)))),
                              selectInput("input2","2",
                                         choices = setNames(nm = c("None", colnames(iris))))
      )
                              
      )

    df
  })
  
  
  
  output$x1 <- renderDataTable(
    quantiDataTable(),
    escape=FALSE,
    rownames = FALSE,
    extensions = c('Scroller'),
    server=FALSE,
    selection='none', 
    class = 'compact',
    options=list(
      preDrawCallback=JS(
        'function() {
            Shiny.unbindAll(this.api().table().node());}'),
      drawCallback= JS(
        'function(settings) {
            Shiny.bindAll(this.api().table().node());}'),
      # rowCallback = JS("function(r,d) {$(r).attr('height', '10px')}"),
      dom = 'Bfrtip',
      autoWidth=TRUE,
      deferRender = TRUE,
      bLengthChange = FALSE,
      scrollX = 200,
      scrollY = 500,
      scroller = TRUE,
      ajax = list(url = dataTableAjax(session, quantiDataTable()))
      
    )
    
  )
}


shinyApp(ui, server)