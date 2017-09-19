
output$GOAnalysisMenu <- renderUI({
    rv$current.obj
    if (is.null(rv$current.obj)){return (NULL)}
    
    
    if (rv$current.obj@experimentData@other$typeOfData == "protein") {
        
        tabsetPanel(
            tabPanel("GO Setup",
                     sidebarCustom(),
                     splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
                                 wellPanel(id = "sidebar_Normalization"
                                           ,height = "100%"
                                           ,h4("General GO setup")
                                           , radioButtons("sourceOfProtID", "Source of protein ID",
                                                          choices = c("colInDataset" = "colInDataset",
                                                                      "extFile" = "extFile"))
                                           
                                           ,uiOutput("chooseSourceForProtID")
                                           ,selectInput("Organism", "Genome Wide Annotation", choices = GetListInstalledOrgdDB())
                                           #,uiOutput("selectOrganism")
                                           ,selectInput("Ontology", "Ontology",
                                                        choices = c("Molecular Function (MF)"="MF" , 
                                                                    "Biological Process (BP)" = "BP", 
                                                                    "Cellular Component (CC)" = "CC"))
                                 )
                                 ,tagList(
                                     uiOutput("warnDifferentSizeID"),
                                     uiOutput("infoIDProt_NA"),
                                     br(), br(),
                                     dataTableOutput("nonIdentifiedProteins", width = "80%")
                                     
                                 )
                     )
                     
                     
            ),
            tabPanel("GO Classification",
                     sidebarCustom(),
                     splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
                                 wellPanel(id = "sidebar_GO",
                                           height = "100%",
                                           numericInput("GO_level", "Level",min = 0, max = 10, step = 1, value = 2)
                                           
                                 ),
                                 tagList(
                                     busyIndicator("Calculation in progress",wait = 0),
                                     plotOutput("GOplotGroup",  width = "80%"),
                                     dataTableOutput("GODatatable")
                                     
                                 )
                                 
                                 
                     )
            ),
            tabPanel("GO Enrichment",
                     #id = "tabPanelEnrichGO",
                     sidebarCustom(),
                     splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
                                 wellPanel(id = "sidebar_GO1",
                                           height = "100%",
                                           radioButtons("universe", "Universe", choices = c("Entire organism" = "Entire organism",
                                                                                            "Entire dataset" = "Entire dataset",
                                                                                            "Custom" = "Custom")),
                                           uiOutput("chooseUniverseFile"),
                                           selectInput("PAdjustMethod", "P Adjust Method",choices = c("BH", "fdr", "none")),
                                           numericInput("pvalueCutoff", "p-Value cutoff", min = 0, max = 1, step = 0.01, value = 0.01),
                                           
                                           
                                           actionButton("perform.GO.button",
                                                        "Perform analysis"),
                                           busyIndicator("Calculation in progress",wait = 0),
                                           
                                           actionButton("ValidGO",
                                                        "Save analysis",
                                                        styleclass = "primary")
                                 ),
                                 tagList(
                                     busyIndicator("Calculation in progress",wait = 0),
                                     plotOutput("GObarplotEnrich", width = "80%"),
                                     plotOutput("GOdotplotEnrich", width = "80%")
                                     #plotOutput("GOEnrichMap")
                                     
                                     #dataTableOutput("GODatatableEn")
                                     
                                 )
                                 
                                 
                     )
            )
        )
    } else {
        h4("The dataset is a peptide one: the GO analysis cannot be performed.")
    }
})


GetListInstalledOrgdDB <- function(){
    l <- installed.packages()[,"Package"]
    l <- l[grep("org", l)]
   res <-  list_org_db[l,]$longName
    return(res)
}


GetDataIndexForAnalysis <- reactive({
    rv$current.obj
    if (is.null(rv$current.obj) ){return(NULL)}
    
    index <- NULL
    if ("Significant.Welch" %in% names(Biobase::fData(rv$current.obj) )){
        index <- which(Biobase::fData(rv$current.obj)$Significant.Welch == TRUE)
    } else if ("Significant.limma" %in% names(Biobase::fData(rv$current.obj) )){
        index <- which(Biobase::fData(rv$current.obj)$Significant.limma == TRUE)
    } else{
        index <- seq(1:nrow(rv$current.obj))
    }
    
    return(index)
})



output$chooseSourceForProtID <- renderUI({
    rv$current.obj
    input$sourceOfProtID
    if (is.null(rv$current.obj) ){return(NULL)}
    
    print(input$sourceOfProtID)
    if (input$sourceOfProtID == "colInDataset"){
        selectInput("UniprotIDCol", "Select column which contains protein ID (UNIPROT)",
                    choices = c("", colnames(Biobase::fData(rv$current.obj))))
    }
    else  if (input$sourceOfProtID == "extFile"){
        fileInput("UNIPROTID_File", "Select file for UNIPROT protein ID")

    }
})


# output$infoIDProt_NA <- renderUI({
#     rv$ProtIDList
#     rv$current.obj
#     if (is.null(rv$current.obj) ){return(NULL)}
#     if (is.null(rv$ProtIDList) ){return(NULL)}
#     
#     nbNA <- length(which(is.na(rv$ProtIDList)))
#     pourcentage= round(100*nbNA/length(rv$ProtIDList), digits=0)
#     h3(paste("Total of non-identified proteins :", nbNA, " (", pourcentage, "% of the dataset).",sep =""))
# })


# observeEvent(input$UniprotIDCol, {
#     if (input$UniprotIDCol == "") {return (NULL)}
#     
#     rv$ProtIDList <- NULL
#     index <- GetDataIndexForAnalysis()
#     rv$ProtIDList <- DAPAR::getUniprotID_FromVector(Biobase::fData(rv$current.obj)[index,input$UniprotIDCol])
#     lIndex <- which(!is.na(rv$ProtIDList))
#     if (length(lIndex) >0){
#         rv$ProtIDList <- rv$ProtIDList[]
#     }
#     
#     print(rv$ProtIDList)
#     
# })




# output$getUniprotIDCol <- renderUI({
#     rv$current.obj
#     if (is.null(rv$current.obj) ){return(NULL)}
#     
# selectInput("UniprotIDCol", "Uniprot ID column",
#             choices = colnames(Biobase::fData(rv$current.obj)))
# 
# })




output$chooseUniverseFile <- renderUI({
    input$universe
    if (input$universe == "Custom"){
        fileInput("UniverseFile", "Choose universe file")
    }
})


#observeEvent(input$UniverseFile,{ 
#    rv$universeData <- read.table(input$UniverseFile$datapath, header = FALSE, stringsAsFactors = FALSE)
#})


observeEvent(input$UniprotIDCol,{ 
    if(is.null(input$UniprotIDCol) || (input$UniprotIDCol == "")) {  rv$ProtIDList <- return (NULL)}
    else {
        rv$ProtIDList <- Biobase::fData(rv$current.obj)[,input$UniprotIDCol]
    }
})

observeEvent(input$UNIPROTID_File,{ 
    rv$ProtIDList <- read.table(input$UNIPROTID_File$datapath, header = FALSE, stringsAsFactors = FALSE)$V1
})


output$warnDifferentSizeID <- renderUI({
    rv$ProtIDList
    if (is.null(rv$ProtIDList)) { return (NULL)}
    print(length(rv$ProtIDList))
    print(nrow(rv$current.obj))
    if (length(rv$ProtIDList) != nrow(rv$current.obj)){
        #rv$ProtIDList <- NULL
        h4("Warning : the protein ID list has not the same number of entites as the dataset.")
        br()
        h4("Please select another list of ID")
    }
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
    rv$ProtIDList

    input$GO_level
    rv$uniprotID
    if (is.null(input$perform.GO.button) ){return(NULL)}
    if (is.null(rv$ProtIDList)){return(NULL)}
    
    require(clusterProfiler)
    isolate({
        #result = tryCatch(
            #{
                idFrom <- "UNIPROT"
                idTo <- "ENTREZID"
                data <- NULL
               
               index <- GetDataIndexForAnalysis()
               
                    rv$groupGO_data <- group_GO(rv$ProtIDList[index],
                                                idFrom, 
                                                idTo, 
                                   orgdb = input$Organism, 
                                 ont=input$Ontology, 
                                 level=input$GO_level)
                    
                    
                    
                    if (input$universe == "Entire dataset") {
                        rv$universeData  <- DAPAR::getUniprotID_FromVector(Biobase::fData(rv$current.obj)[,input$UniprotIDCol])
                    } else if (input$universe == "Entire organism") {
                        rv$universeData = DAPAR::univ_AnnotDbPkg(input$Organism)
                    } else {
                        rv$universeData <- read.table(input$UniverseFile$datapath, header = FALSE, stringsAsFactors = FALSE)
                    }
                    
                    
                    rv$enrichGO_data <- enrich_GO(rv$ProtIDList[index],
                                    idFrom, 
                                    idTo, 
                                    orgdb = input$Organism, 
                                  ont = input$Ontology, 
                                  pAdj = input$PAdjustMethod, 
                                  pval = input$pvalueCutoff, 
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



output$nonIdentifiedProteins <- renderDataTable({
    input$UniprotIDCol
    rv$ProtIDList
    rv$current.obj
    if (is.null(rv$current.obj) ){return(NULL)}
    if (is.null(rv$ProtIDList) ){return(NULL)}
    
        as.data.frame(fData(rv$current.obj)[which(is.na(rv$ProtIDList)),])
    
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
