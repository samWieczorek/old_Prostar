

observe({
    rv$current.obj
    if(is.null(rv$current.obj)) {return (NULL)}
    
    if (rv$current.obj@experimentData@other$typeOfData == "peptide")
    { 
        hideTab(inputId ="navPage", target = "GO_Analysis")
    } else {
        showTab(inputId ="navPage", target = "GO_Analysis")
        }
})


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
                                                          choices = G_sourceOfProtID_Choices)
                                           
                                           ,uiOutput("chooseSourceForProtID")
                                           ,selectInput("Organism", "Genome Wide Annotation", choices = GetListInstalledOrgdDB())
                                           #,uiOutput("selectOrganism")
                                           ,selectInput("Ontology", "Ontology",
                                                        choices = G_ontology_Choices)
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
                                     highchartOutput("GOplotGroup",  width = "80%"),
                                     dataTableOutput("GODatatable")
                                     
                                 )
                                 
                                 
                     )
            ),
            tabPanel("GO Enrichment",
                     id = "tabPanelEnrichGO",
                     sidebarCustom(),
                     splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
                                 wellPanel(id = "sidebar_GO1",
                                           height = "100%",
                                           radioButtons("universe", "Universe", choices = G_universe_Choices),
                                           uiOutput("chooseUniverseFile"),
                                           selectInput("PAdjustMethod", "P Adjust Method",choices = G_pAdjustMethod_Choices),
                                           numericInput("pvalueCutoff", "p-Value cutoff", min = 0, max = 1, step = 0.01, value = 0.01),
                                           
                                           
                                           actionButton("perform.GO.button",
                                                        "Perform analysis"),
                                           busyIndicator("Calculation in progress",wait = 0),
                                           
                                           actionButton("ValidGOAnalysis",
                                                        "Save analysis",
                                                        styleclass = "primary")
                                 ),
                                 tagList(
                                     busyIndicator("Calculation in progress",wait = 0),
                                     highchartOutput("GObarplotEnrich", width = "80%"),
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
   names(l) <- res
   
    return(l)
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
    input$universe
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
                        rv$universeData  <- rv$ProtIDList
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




GOplotGroup <- reactive({
    rv$groupGO_data
    if (is.null(rv$groupGO_data)) {return(NULL)}
    
    barplotGroupGO_HC(rv$groupGO_data)
    #barplot(rv$groupGO_data)
    
})

output$GOplotGroup <- renderHighchart({
    GOplotGroup()
    
})


GObarplotEnrich <- reactive({
    rv$enrichGO_data
    if (is.null(rv$enrichGO_data)) {return(NULL)}
    
    barplotEnrichGO_HC(rv$enrichGO_data)
    #barplot(rv$enrichGO_data)
})

output$GObarplotEnrich <- renderHighchart({
    GObarplotEnrich()

})

GOdotplotEnrich <- reactive({
    
    rv$enrichGO_data
    if (is.null(rv$enrichGO_data)) {return(NULL)}
    
    dotplot(rv$enrichGO_data)
    })

output$GOdotplotEnrich <- renderPlot({
    GOdotplotEnrich()
    
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
    
    data <- as.data.frame(fData(rv$current.obj)[which(is.na(rv$ProtIDList)),])
    if( nrow(data) != 0){
        data
    }
})


## Validation of the GO analysis
observeEvent(input$ValidGOAnalysis,{ 
    input$Organism
    input$Ontology
    input$PAdjustMethod
    input$pvalueCutoff
    rv$current.obj
    rv$enrichGO_data
    rv$groupGO_data 
    input$universe
    
    if (is.null(rv$current.obj)){ return()}
    
    
    if ((input$ValidGOAnalysis == 0) ||  is.null(input$ValidGOAnalysis) ) {
        return()}
   
    isolate({
        
        result = tryCatch(
            {
               
                    temp <- DAPAR::GOAnalysisSave(rv$dataset[[input$datasets]],
                                                rv$groupGO_data ,
                                                rv$enrichGO_data ,
                                                input$Organism,
                                                input$Ontology,
                                                input$GO_level,
                                                input$PAdjustMethod,
                                                input$pvalueCutoff,
                                                input$universe)
               
                name <- paste("GOAnalysis - ", rv$typeOfDataset, sep="")
                rv$dataset[[name]] <- temp
                rv$current.obj <- temp
                
                
                updateSelectInput(session, "datasets", 
                                  paste("Dataset versions of",
                                        rv$current.obj.name, sep=" "),
                                  choices = names(rv$dataset),
                                  selected = name)
                
                
                # 
                # ####write command Log file
                # writeToCommandLogFile(paste("cond1 <- '", input$condition1, "'", sep=""))
                # writeToCommandLogFile(paste("cond2 <- '", input$condition2, "'", sep=""))
                # writeToCommandLogFile(paste("method <- '", input$diffAnaMethod, "'", sep=""))
                # if (input$diffAnaMethod == "Limma"){
                #     writeToCommandLogFile("data <- wrapper.diffAnaLimma(current.obj, cond1, cond2)")
                # } else if (input$diffAnaMethod == "Welch"){
                #     writeToCommandLogFile( "data <- wrapper.diffAnaWelch(current.obj, cond1, cond2)")
                # }
                # 
                # 
                # writeToCommandLogFile(paste("threshold_pValue <- ", input$seuilPVal, sep=""))
                # writeToCommandLogFile(paste("threshold_logFC <- ", input$seuilLogFC,sep=""))
                # 
                # writeToCommandLogFile(paste("calibMethod <- \"", input$calibrationMethod, "\"", sep=""))
                # if (input$calibrationMethod == "Benjamini-Hochberg") { 
                #     writeToCommandLogFile("m <- 1") }
                # else if (input$calibrationMethod == "numeric value") 
                # { writeToCommandLogFile(paste(" m <- ",as.numeric(input$numericValCalibration), sep=""))}
                # else {writeToCommandLogFile("m <- calibMethod")}
                # 
                # writeToCommandLogFile("fdr <- diffAnaComputeFDR(data, threshold_pValue, threshold_logFC, m)")
                # 
                # 
                # writeToCommandLogFile(paste(" temp <- diffAnaSave(dataset[['",
                #                             input$datasets,"']],  data, method, cond1, cond2, threshold_pValue, threshold_logFC, fdr, calibMethod)", sep=""))
                # writeToCommandLogFile(paste(" name <- \"DiffAnalysis.", 
                #                             input$diffAnaMethod, " - ", rv$typeOfDataset,"\"", sep="" ))
                # writeToCommandLogFile("dataset[[name]] <- temp")
                # writeToCommandLogFile("current.obj <- temp")
                # 
                # 
                # 
                # cMethod <- NULL
                # if (input$calibrationMethod == "numeric value"){
                #     cMethod <- paste("The proportion of true null
                #                      hypotheses was set to", 
                #                      input$numericValCalibration, sep= " ")}
                # else {cMethod <-input$calibrationMethod }
                # 
                # text <- paste("Dataset of ", 
                #               rv$typeOfDataset,
                #               ": differential analysis with", 
                #               input$diffAnaMethod, 
                #               "Selection with the following threshold values :logFC =",
                #               rv$seuilLogFC,
                #               "The calibration was made with the method", cMethod,
                #               ", -log10(p-value) = ",
                #               rv$seuilPVal,
                #               "corresponding to a FDR = ", round(100*rv$fdr, digits=2),
                #               sep=" ")
                # UpdateLog(text,name)
                # 
                 updateTabsetPanel(session, "abc", selected = "tabPanelEnrichGO")
 
                
                
                
                ## Add the necessary text to the Rmd file
                txt2Rmd <- readLines("Rmd_sources/GOanalysis_Rmd.Rmd")
                filename <- paste(tempdir(), sessionID, 'report.Rmd',sep="/")
                write(txt2Rmd, file = filename,append = TRUE, sep = "\n")
                createPNG_GOAnalysis()
                
                
                
            }
            #, warning = function(w) {
            #    shinyjs::info(conditionMessage(w))
            #}
            , error = function(e) {
                shinyjs::info(paste("Valid GO Ana",":",
                                    conditionMessage(e), sep=" "))
            }, finally = {
                #cleanup-code 
            })
        
        
        
            }) 
    
})