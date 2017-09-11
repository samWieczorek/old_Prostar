

output$getUniprotIDCol <- renderUI({
    rv$current.obj
    if (is.null(rv$current.obj) ){return(NULL)}
    
selectInput("UniprotID", "Uniprot ID column",
            choices = colnames(Biobase::fData(rv$current.obj)))

})



output$chooseUniverseFile <- renderUI({
    input$universe
    if (input$universe == "Custom"){
        fileInput("UniverseFile", "Choose universe file")
    }
})


observeEvent(input$UniverseFile,{ 
    rv$universeData <- read.table(input$UniverseFile$datapath, header = FALSE, stringsAsFactors = FALSE)
})


observeEvent(input$UNIPROTID_File,{ 
    rv$uniprotID <- read.table(input$UNIPROTID_File$datapath, header = FALSE, stringsAsFactors = FALSE)$V1
})


##' Reactive behavior : Normalization of data
##' @author Samuel Wieczorek
observeEvent(input$perform.GO.button,{
    rv$current.obj
    input$perform.GO.button
    input$Organism
    input$Ontology
    input$PAdjustMethod
    input$pvalueCutoff
    input$qvalueCutoff
    input$GO_level
    rv$uniprotID
    if (is.null(input$perform.GO.button) ){return(NULL)}
    #if (input$perform.normalization == 0){return(NULL)}
    
    require(clusterProfiler)
    isolate({
        #result = tryCatch(
            #{
                idFrom <- "UNIPROT"
                idTo <- "ENTREZID"
                data <- NULL
                if (is.null(rv$uniprotID)){
                    data <- getUniprotID(input$UniprotID)
                } else { data <- rv$uniprotID}
                    
                print(head(data))
                    rv$groupGO_data <- group_GO(data,
                                                idFrom, 
                                                idTo, 
                                   orgdb = input$Organism, 
                                 ont=input$Ontology, 
                                 level=input$GO_level)
                    
                    rv$enrichGO_data <- enrich_GO(data,
                                    idFrom, 
                                    idTo, 
                                    orgdb = input$Organism, 
                                  ont = input$Ontology, 
                                  pAdj = input$PAdjustMethod, 
                                  pval = input$pvalueCutoff, 
                                  qval = input$qvalueCutoff, 
                                  universe = rv$universeData )
                     
            # }
            # , warning = function(w) {
            #     shinyjs::info(conditionMessage(w))
            # }, error = function(e) {
            #     shinyjs::info(paste("Perform GO enrichment",":",conditionMessage(e), sep=" "))
            # }, finally = {
            #     #cleanup-code 
            # })
        
        
    })
})




output$GOplotGroup <- renderPlot({

    rv$groupGO_data
    if (is.null(rv$groupGO_data)) {return(NULL)}
    
    barplot(rv$groupGO_data)
})


output$GObarplotEnrich <- renderPlot({
    rv$enrichGO_data
    if (is.null(rv$enrichGO_data)) {return(NULL)}
    
    barplot(rv$enrichGO_data)

})

output$GOdotplotEnrich <- renderPlot({
    rv$enrichGO_data
    if (is.null(rv$enrichGO_data)) {return(NULL)}
    
    dotplot(rv$enrichGO_data)
    
})

output$GOEnrichMap <- renderPlot({
    rv$enrichGO_data
    if (is.null(rv$enrichGO_data)) {return(NULL)}
    
    enrichMap(rv$enrichGO_data)
    
})


output$GODatatable <- renderDataTable({
    rv$enrichGO_data
    rv$groupGO_data
    if (is.null(rv$enrichGO_data) && is.null(rv$groupGO_data)) {return(NULL)}
    
    as.data.frame(rv$groupGO_data@result)
    
})


# 
# ##' -- Validate the normalization ---------------------------------------
# ##' @author Samuel Wieczorek
# observeEvent(input$valid.normalization,{ 
#     
#     input$normalization.method
#     if (is.null(input$valid.normalization) || (input$valid.normalization == 0)) 
#     {return(NULL)}
#     
#     isolate({
#         result = tryCatch(
#             {
#                 if (input$normalization.method != "None") {
#                     
#                     rv$typeOfDataset <-rv$current.obj@experimentData@other$typeOfData
#                     name <- paste ("Normalized", " - ", rv$typeOfDataset, sep="")
#                     rv$dataset[[name]] <- rv$current.obj
#                     
#                     
#                     #write command log file
#                     writeToCommandLogFile(
#                         paste("dataset[['",name,"']] <- current.obj", sep="")
#                     )
#                     
#                     updateSelectInput(session, "datasets", 
#                                       paste("Dataset versions of",rv$current.obj.name, sep=" "),
#                                       choices = names(rv$dataset),
#                                       selected = name)
#                     UpdateLog(paste("Normalization : data normalized with the method",
#                                     input$normalization.method, sep=" "), name)
#                     
#                     
#                     ## Add the necessary text to the Rmd file
#                     txt2Rmd <- readLines("Rmd_sources/normalization_Rmd.Rmd")
#                     filename <- paste(tempdir(), sessionID, 'report.Rmd',sep="/")
#                     write(txt2Rmd, file = filename,append = TRUE, sep = "\n")
#                     createPNG_Normalization()
#                     
#                 }
#             }
#             , warning = function(w) {
#                 shinyjs::info(conditionMessage(w))
#             }, error = function(e) {
#                 shinyjs::info(info("Validate the normalization",":",conditionMessage(e), sep=" "))
#             }, finally = {
#                 #cleanup-code 
#             })
#         
#     } )
# })
