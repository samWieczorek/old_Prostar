library(shiny)
library(shinyjs)



ui <- fluidPage(
  tagList(
    useShinyjs(),
    uiOutput('inputGroup'),
    mainPanel(textOutput("inputValues"))
  )
)


server <- shinyServer(function(session, input, output) {
  utils::data(Exp1_R25_prot, package='DAPARdata')
  obj <- Exp1_R25_prot
  n <- length(obj@experimentData@other$names_metacell)
  
  output$inputGroup = renderUI({
    
    input_list <- lapply(1:n, function(i) {
      inputName <- paste("colForOriginValue_", i, sep = "")
      div(
        div( style="align: center;display:inline-block; vertical-align: middle;padding-right: 10px;",
             p(tags$strong(inputName))
        ),
        div( style="align: center;display:inline-block; vertical-align: middle;",
             selectInput(inputName, '', choices = c('None', colnames(fData(obj))))
        )
      )
    })
    do.call(tagList, input_list)
  })
  
  # this is just a demo to display all the input values
  output$inputValues <- renderText({
    paste(lapply(1:n, function(i) {
      inputName <- paste("colForOriginValue_", i, sep = "")
      input[[inputName]]
    }))
  })
  
  
  observeEvent(input[['colForOriginValue_1']], ignoreInit = T, ignoreNULL = F, {
    print(input[['colForOriginValue_1']])
    
    lapply(2:n, function(i) {
      inputName <- paste("colForOriginValue_", i, sep = "")
      start <- which(colnames(fData(obj))==input[['colForOriginValue_1']])

      if (input[['colForOriginValue_1']] == 'None')
        .select <- 'None'
      else 
        .select <- colnames(fData(obj)[(i-1)+start])
      updateSelectInput(session, inputName, selected = .select) 
      
    })
  })
  
  # 
  #  observeEvent(lapply(names(input)[grep('colForOriginValue', names(input))], function(x) input[[x]]), ignoreInit = T, ignoreNULL = F, {
  #  
  # browser() 
  #   })
  #  
  
})

# Run the application
shinyApp(ui = ui, server = server)