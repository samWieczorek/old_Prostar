


output$choosedataToExportMSnset <- renderUI({
  req(rv$dataset)
  
  dnames <- unlist(lapply(names(rv$dataset), function(x){unlist(strsplit(x, " - "))[[1]]}))
  .choices <- names(rv$dataset)
  names(.choices) <- dnames
  radioButtons("chooseDatasetToExportToMSnset", 
               "Datasets to export",
               choices = c("None"="None",.choices))
  
})





output$exportOptions <- renderUI({
  req(input$chooseDatasetToExportToMSnset)
  if (input$chooseDatasetToExportToMSnset == "None"){return(NULL)}
  
  tagList(
    fluidRow(
      column(width=2,modulePopoverUI("modulePopover_exportFileFormat")),
      column(width=10,selectInput("fileformatExport", "",choices=  gFileFormatExport))
    ),
    
    br(),
    fluidRow(
      column(width=2,modulePopoverUI("modulePopover_exportMetaData")),
      column(width=10,uiOutput("chooseMetaDataExport",width = widthWellPanel))
    ),
    br(),
    fluidRow(
      column(width=2,modulePopoverUI("modulePopover_exportFilename")),
      column(width=10,uiOutput("chooseExportFilename"))
    ),
    
    br(),
    downloadButton('downloadMSnSet', 'Download')
  )
})







output$chooseMetaDataExport <- renderUI({
  rv$current.obj
  if (is.null(rv$current.obj)) {return(NULL)  }
  
  choices <- colnames(fData(rv$current.obj))
  names(choices) <- colnames(fData(rv$current.obj))
  selectizeInput("colsToExport",
                 label = "",
                 choices = choices,
                 multiple = TRUE, width='500px')
  
})




callModule(modulePopover,"modulePopover_exportMetaData", 
           data = reactive(list(title = HTML(paste0("<strong><font size=\"4\">Metadata</font></strong>")), 
                                content="Select the columns you want to keep as metadata. By default, if any column is specified, all metadata in your dataset will be exported.")))

callModule(modulePopover,"modulePopover_exportFileFormat", 
           data = reactive(list(title = HTML(paste0("<strong><font size=\"4\">File format</font></strong>")), 
                                content="File format")))



callModule(modulePopover,"modulePopover_exportFilename", 
           data = reactive(list(title = HTML(paste0("<strong><font size=\"4\">Filename</font></strong>")), 
                                content="Enter the name of the files to be created")))




output$chooseExportFilename <- renderUI({
  
  textInput("nameExport", 
            label = "",
            value = rv$current.obj.name)
})






output$downloadMSnSet <- downloadHandler(
  #input$chooseDatasetToExportToMSnset,
  filename = function() { 
    #input$nameExport
    if (input$fileformatExport == gFileFormatExport$excel) {
      paste(input$nameExport,gFileExtension$excel,  sep="")}
    else if (input$fileformatExport == gFileFormatExport$msnset)
    {
      paste(input$nameExport,gFileExtension$msnset,  sep="")}
    else if (input$fileformatExport == gFileFormatExport$zip)
    {
      paste(input$nameExport,gFileExtension$zip,  sep="")}
    
  },
  content = function(file) {
    dataToExport <- rv$dataset[[input$chooseDatasetToExportToMSnset]]
    if (length(input$colsToExport) == 1){
      Biobase::fData(dataToExport) <- 
        data.frame(fData(dataToExport)[,input$colsToExport])
      colnames( Biobase::fData(dataToExport)) <- input$colsToExport
      t <- buildWritableVector(input$colsToExport)
    }
    else if (length(input$colsToExport) > 1){
      Biobase::fData(dataToExport) <- 
        data.frame(fData(dataToExport)[,input$colsToExport])
      t <- buildWritableVector(input$colsToExport)
    }
    
    
    colnames(fData(dataToExport)) <- gsub(".", "_", 
                                          colnames(fData(dataToExport)), 
                                          fixed=TRUE)
    names(dataToExport@experimentData@other) <- gsub(".", "_", names(dataToExport@experimentData@other), fixed=TRUE)
    
    dataToExport@experimentData@other$Prostar_Version = installed.packages(lib.loc = Prostar.loc)["Prostar","Version"]
    dataToExport@experimentData@other$DAPAR_Version = installed.packages(lib.loc = DAPAR.loc)["DAPAR","Version"]
    
    if (input$fileformatExport == gFileFormatExport$excel) {
      fname <- paste(input$nameExport,gFileExtension$excel,  sep="")
      writeMSnsetToExcel(dataToExport, input$nameExport)
      file.copy(fname, file)
      file.remove(fname)
    }
    
    else if  (input$fileformatExport == gFileFormatExport$msnset) {
      fname <- paste(input$nameExport,gFileExtension$msnset,  sep="")
      saveRDS(dataToExport,file=fname)
      file.copy(fname, file)
      file.remove(fname)
    }
    
    else if  (input$fileformatExport == gFileFormatExport$zip) {
      fname <- paste(input$nameExport,gFileExtension$zip,  sep="")
      writeMSnsetToCSV(dataToExport,fname)
      file.copy(fname, file)
      file.remove(fname)
    }
  }
)





output$choosedataTobuildReport <- renderUI({
  rv$dataset
  if (is.null(rv$dataset)){return (NULL)}
  
  checkboxGroupInput("chooseDatasetToExport", 
                     "Datasets to export",
                     choices = names(rv$dataset),
                     selected = names(rv$dataset))
  
})



