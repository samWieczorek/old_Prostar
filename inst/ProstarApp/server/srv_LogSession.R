

output$InfoTextSourceCode <- renderUI({
  txt <- "Upgrading this  feature from Beta-version to full release requires a lot of work.
  If you use it, please let us know by email so that we can better evaluate its priority."
  helpText(txt)
})


output$code <- renderUI({
  rv$commandLog
  if (is.null(rv$commandLog)){return(NULL)}
  aceEditor("ui"
            , value = paste( rv$commandLog, collapse="\n")
            , mode = "r"
            , theme = "chrome"
            , height = "600px"
            , readOnly = TRUE
  )
  
  
})


output$logSession <- DT::renderDataTable(server=TRUE,{
  req(rv$text.log)
  
  dt <- DT::datatable(rv$text.log,
                      escape = FALSE,
                      extensions = c('Scroller', 'Buttons'),
                      rownames = FALSE,
                      options=list(initComplete = initComplete(),
                                   buttons = list('copy',
                                                  list(
                                                    extend = 'csv',
                                                    filename = 'logSession'
                                                  ),'print'),
                                   dom='Bfrtip',
                                   pageLength=DT_pagelength,
                                   deferRender = TRUE,
                                   bLengthChange = FALSE,
                                   scrollX = 200,
                                   scrollY = 600,
                                   scroller = TRUE,
                                   orderClasses = TRUE,
                                   autoWidth=FALSE,
                                   columnDefs = list(
                                     list(columns.width=c("60px","60px"),
                                          columnDefs.targets= c(list(0),list(1))))
                      ))
  dt
})


