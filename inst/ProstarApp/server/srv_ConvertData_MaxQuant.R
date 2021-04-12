output$maxquant_metacell <- renderUI({

  req(input$choose_software == 'MaxQuant')
  tagList(
    uiOutput("checkIdentificationTab"),
    shinyjs::hidden(
      div(id = 'warning_neg_values',
          p("Warning : Your original dataset may contain negative values",
            "so that they cannot be logged. Please check back the dataset or", 
            "the log option in the first tab."))
    )
  )
})








observe({

  shinyjs::toggle('warning_neg_values', condition = !is.null(input$choose_quantitative_columns) && length(which(rv$tab1[,input$choose_quantitative_columns] < 0)) > 0)
  shinyjs::toggle('selectIdent', condition = !is.null(rv$tab1))
  shinyjs::toggle('x1', condition = isTRUE(input$selectIdent))
})





output$checkIdentificationTab <- renderUI({
  req(input$selectIdent == TRUE)
  #if (!isTRUE(input$selectIdent)){return(NULL)}
  
  shinyValue("colForOriginValue_",length(input$choose_quantitative_columns))
  temp <- shinyValue("colForOriginValue_",length(input$choose_quantitative_columns))
  
  if ((length(which(temp == "None")) == length(temp)))
  {
    img <- "images/Ok.png"
    txt <- "Correct"
  }  else {
    if (length(which(temp == "None")) > 0)
    {
      img <- "images/Problem.png"
      txt <- "The identification method is not appropriately defined for each sample."
    } else {
      if(length(temp) != length(unique(temp))){
        img <- "images/Problem.png"
        txt <- "There are duplicates in identification columns."
      }else { 
        img <- "images/Ok.png"
        txt <- "Correct"
      }
    }
  }
  tags$div(
    tags$div(
      tags$div(style="display:inline-block;",tags$img(src = img, height=25)),
      tags$div(style="display:inline-block;",tags$p(txt))
    )
  )
  
})



# reactive dataset
quantiDataTable <- reactive({
  rv$tab1
  input$choose_quantitative_columns
  input$selectIdent
  
  if (is.null(input$choose_quantitative_columns) || is.null(rv$tab1)) return(NULL)
  
  df <- NULL
  session$sendCustomMessage('unbind-DT', 'x1')
  choices <- c("None", colnames(rv$tab1))
  names(choices) <- c("None",colnames(rv$tab1))
  
  if (isTRUE(input$selectIdent)) {
    
    df <- data.frame(as.data.frame(input$choose_quantitative_columns),
                     shinyInput(selectInput,
                                "colForOriginValue_",
                                nrow(as.data.frame(input$choose_quantitative_columns)),
                                choices = choices))
    colnames(df) <- c("Sample", "Identification method")
  } else {
    df <- data.frame(Sample = as.data.frame(input$choose_quantitative_columns))
    colnames(df) <- c("Sample")
  }
  df
})



output$x1 <- renderDataTable(
  quantiDataTable(),
  escape=FALSE,
  rownames = FALSE,
  extensions = c('Scroller', 'Buttons'),
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


observeEvent(shinyValue("colForOriginValue_",
                        nrow(as.data.frame(quantiDataTable()))),{})


checkIdentificationMethod_Ok <- reactive({
  #req(input$selectIdent)
  res <- TRUE
  tmp <- NULL
  if (isTRUE(input$selectIdent)) {
    tmp <- shinyValue("colForOriginValue_",nrow(quantiDataTable()))
    if ((length(grep("None", tmp)) > 0)  || (sum(is.na(tmp)) > 0)){ res <- FALSE }
  } 
  res
  
})
