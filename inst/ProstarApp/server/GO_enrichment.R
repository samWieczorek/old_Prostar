

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
                                           ,selectInput("idFrom", "Id From", choices = c("UNIPROT"))
                                           ,selectInput("Organism", "Genome Wide Annotation", choices = GetListInstalledOrgdDB())
                                           ,selectInput("Ontology", "Ontology",
                                                        choices = G_ontology_Choices)
                                           ,actionButton("mapProtein.GO.button",
                                                         "Map proteins IDs")
                                 )
                                 ,tagList(
                                     uiOutput("warnDifferentSizeID"),
                                     uiOutput("infoIDProt_NA"),
                                     br(), br(),
                                     uiOutput("GeneMappedRatio"),
                                     br(), br(),
                                     dataTableOutput("nonIdentifiedProteins", width = "80%")
                                     
                                 )
                     )
                     
                     
            ),
            tabPanel("GO Classification",
                      sidebarCustom(),
                      splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
                     #             wellPanel(id = "sidebar_GO",
                     #                       height = "100%",
                     #                       numericInput("GO_level", "Level",min = 0, max = 10, step = 1, value = 2)
                     #                       
                     actionButton("group.GO.perform.button","Perform GO grouping")
                                  ),
                                 tagList(
                                     busyIndicator("Calculation in progress",wait = 0),
                                     highchartOutput("GOplotGroup_level2",  width = "80%"),
                                     highchartOutput("GOplotGroup_level3",  width = "80%"),
                                     highchartOutput("GOplotGroup_level4",  width = "80%")
                                     #dataTableOutput("GODatatable")
                                     
                                 )
                                 
                                 
                    # )
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
                                           numericInput("pvalueCutoff", "FDR (Adjusted P-value cutoff)", min = 0, max = 1, step = 0.01, value = 0.01),
                                           
                                           actionButton("perform.GO.button",
                                                        "Perform enrichment analysis!"),
                                           busyIndicator("Calculation in progress",wait = 0)
                                 ),
                                 tagList(
                                     busyIndicator("Calculation in progress",wait = 0),
                                     highchartOutput("GObarplotEnrich", width = "80%"),
                                     highchartOutput("GOdotplotEnrich", width = "80%")
                                     #plotOutput("GOEnrichMap")
                                     
                                     #dataTableOutput("GODatatableEn")
                                     
                                 )
                     )
            ),
            tabPanel("Save GO analysis",
                     id = "tabPanelSaveGO",
                     sidebarCustom(),
                     splitLayout(cellWidths = c(widthLeftPanel, widthRightPanel),
                                 wellPanel(id = "sidebar_GO4",
                                           height = "100%",
                                           uiOutput("chooseGOtoSave"),
                                           actionButton("ValidGOAnalysis",
                                                        "Save analysis",
                                                        styleclass = "primary")
                                 ),
                                 tagList(
                                     busyIndicator("Calculation in progress",wait = 0)
                                     
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
    if (length(rv$ProtIDList) != nrow(rv$current.obj)){
        #rv$ProtIDList <- NULL
        h4("Warning : the protein ID list has not the same number of entites as the dataset.")
        br()
        h4("Please select another list of ID")
    }
})


observeEvent(input$mapProtein.GO.button,{
    input$UniprotIDCol
    input$Organism
    input$idFrom
    
    if(is.null(input$UniprotIDCol) || (input$UniprotIDCol == "")) {  
        rv$ProtIDList <- NULL
        return (NULL)}

    require(clusterProfiler)
    isolate({
        #rv$ratio <- NULL
    rv$gene <- NULL
    rv$ProtIDList <- Biobase::fData(rv$current.obj)[,input$UniprotIDCol]
    index <- GetDataIndexForAnalysis()

    tryCatch({
        rv$gene <- bitr(rv$ProtIDList[index], fromType=input$idFrom, toType="ENTREZID", OrgDb=input$Organism)
        rv$proteinsNotMapped <- which((rv$ProtIDList[index] %in% rv$gene[,input$idFrom]) == FALSE)
        rv$ratio <- 100*length(rv$proteinsNotMapped) / length(index)
    }, warning = function(w) {
        rv$gene <- bitr(rv$ProtIDList[index], fromType=input$idFrom, toType="ENTREZID", OrgDb=input$Organism)
        rv$proteinsNotMapped <- which((rv$ProtIDList[index] %in% rv$gene[,input$idFrom]) == FALSE)
        rv$ratio <- 100*length(rv$proteinsNotMapped) / length(index)
    
    }, error = function(e) {
       # shinyjs::info(paste("Perform GO enrichment",":",conditionMessage(e), sep=" "))
            rv$ratio <- 100
        }, finally = {    
            }
    )


    })
})


##' Reactive behavior : GO analysis of data
##' @author Samuel Wieczorek
observeEvent(input$perform.GO.button,{
     input$universe
    input$Organism
    input$Ontology
    input$PAdjustMethod
    input$pvalueCutoff
    rv$ProtIDList
    input$idFrom
    rv$ratio
     rv$uniprotID
    if (is.null(input$perform.GO.button) ){return(NULL)}
    if (is.null(rv$ProtIDList)){return(NULL)}
    if (is.null(rv$ratio) || rv$ratio == 100){return(NULL)}
    
    require(clusterProfiler)
    isolate({
        # result = tryCatch(
        #     {
                
                    if (input$universe == "Entire dataset") {
                        rv$universeData  <- rv$ProtIDList
                    } else if (input$universe == "Entire organism") {
                        rv$universeData = DAPAR::univ_AnnotDbPkg(input$Organism)
                    } else {
                        rv$universeData <- read.table(input$UniverseFile$datapath, header = FALSE, stringsAsFactors = FALSE)
                    }
                    
                    
                    rv$enrichGO_data <- enrich_GO(rv$ProtIDList[index],
                                    idFrom = input$idFrom, 
                                    idTo = "ENTREZID", 
                                    orgdb = input$Organism, 
                                    ont = input$Ontology, 
                                    pAdj = input$PAdjustMethod, 
                                    pval = input$pvalueCutoff, 
                                    universe = rv$universeData )
})
})




observeEvent(input$group.GO.perform.button, {
    input$Organism
    input$Ontology
    rv$ProtIDList
    rv$ratio
    rv$uniprotID
    if (is.null(rv$ProtIDList)){return(NULL)}
    if (is.null(rv$ratio) || rv$ratio == 100){return(NULL)}
    
    
    index <- GetDataIndexForAnalysis()
    rv$groupGO_data <- list()
    for (i in 1:3){
        rv$groupGO_data[[i]] <- group_GO(rv$ProtIDList[index],
                                input$idFrom, 
                                "ENTREZID", 
                                orgdb = input$Organism, 
                                ont=input$Ontology, 
                                level=i+1)
    }
    
    
})



GOplotGroup_level2 <- reactive({
    rv$groupGO_data
    if (is.null(rv$groupGO_data)){return(NULL)}
    barplotGroupGO_HC(rv$groupGO_data[[1]])
})


output$GOplotGroup_level2 <- renderHighchart({
    GOplotGroup_level2()
})


GOplotGroup_level3 <- reactive({
    rv$groupGO_data
    if (is.null(rv$groupGO_data)){return(NULL)}
    
    barplotGroupGO_HC(rv$groupGO_data[[2]])
})


output$GOplotGroup_level3 <- renderHighchart({
    GOplotGroup_level3()
})

GOplotGroup_level4 <- reactive({
    rv$groupGO_data
    if (is.null(rv$groupGO_data)){return(NULL)}
    
    barplotGroupGO_HC(rv$groupGO_data[[3]])
})


output$GOplotGroup_level4 <- renderHighchart({
    GOplotGroup_level4()
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
    
    #dotplot(rv$enrichGO_data)
    scatterplotEnrichGO_HC(rv$enrichGO_data)
    })

# output$GOdotplotEnrich <- renderHighchart({
#     GOdotplotEnrich()
#     
# })

# output$GOEnrichMap <- renderPlot({
#     rv$enrichGO_data
#     if (is.null(rv$enrichGO_data)) {return(NULL)}
#     
#     enrichMap(rv$enrichGO_data)
#     
# })


output$GODatatable <- renderDataTable({
    rv$enrichGO_data
    rv$groupGO_data
    if (is.null(rv$enrichGO_data) && is.null(rv$groupGO_data)) {return(NULL)}
    
    as.data.frame(rv$groupGO_data@result)
    
})


# GetProteinMappedRatio <- reactive({
#     
#     
#     print(ratio)
#     return(ratio)
# })


output$GeneMappedRatio <- renderUI({
    
    rv$ratio
    if (is.null(rv$ratio)) {return (NULL)}
    
    tagList(
        h5(paste(round(rv$ratio, digits=2), " % of the proteins have not been mapped.", sep="")),
        if (rv$ratio == 100){
            h3(paste("Advice a rediger", sep=""))
        }
        )
})



output$nonIdentifiedProteins <- renderDataTable({
    rv$ProtIDList
    rv$current.obj
    rv$gene
    input$idFrom
    if (is.null(rv$current.obj) ){return(NULL)}
    if (is.null(rv$ProtIDList) ){return(NULL)}
    if (is.null(rv$gene)) {return(NULL)}
    
    
    index <- GetDataIndexForAnalysis()
    rv$proteinsNotMapped <- which((rv$ProtIDList[index] %in% rv$gene[,input$idFrom]) == FALSE)
   data <- as.data.frame(fData(rv$current.obj)[index[rv$proteinsNotMapped],])
    
    
    if( nrow(data) != 0){
        data
    }
})



output$chooseGOtoSave <- renderUI({
    rv$groupGO_data
    rv$enrichGO_data
    if(is.null(rv$enrichGO_data) && is.null(rv$groupGO_data)){return(NULL)}
    
    .choices <- c()
    if(!is.null(rv$groupGO_data)){.choices <- c(.choices, "Classification")}
    if(!is.null(rv$enrichGO_data)){.choices <- c(.choices, "Enrichment")}
    if(!is.null(rv$enrichGO_data) && !is.null(rv$groupGO_data)){.choices <- c(.choices, "Both")}

    radioButtons("whichGO2Save", "Choose which GO analysis to save", choices = .choices)
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
    input$whichGO2Save
    
    if (is.null(rv$current.obj)){ return()}
    
    
    if ((input$ValidGOAnalysis == 0) ||  is.null(input$ValidGOAnalysis) ) {
        return()}
   
    isolate({
        
        result = tryCatch(
            {
               if (input$whichGO2Save == "Both"){
                    temp <- DAPAR::GOAnalysisSave(rv$dataset[[input$datasets]],
                                                rv$groupGO_data ,
                                                rv$enrichGO_data ,
                                                input$Organism,
                                                input$Ontology,
                                                input$GO_level,
                                                input$PAdjustMethod,
                                                input$pvalueCutoff,
                                                input$universe)
               }
                else if  (input$whichGO2Save == "Classification"){
                    temp <- DAPAR::GOAnalysisSave(rv$dataset[[input$datasets]],
                                                  rv$groupGO_data ,
                                                  NULL ,
                                                  input$Organism,
                                                  input$Ontology,
                                                  input$GO_level,
                                                  input$PAdjustMethod,
                                                  input$pvalueCutoff,
                                                  input$universe)
                }
                else if (input$whichGO2Save == "Enrichment"){
                    temp <- DAPAR::GOAnalysisSave(rv$dataset[[input$datasets]],
                                                  NULL ,
                                                  rv$enrichGO_data ,
                                                  input$Organism,
                                                  input$Ontology,
                                                  input$GO_level,
                                                  input$PAdjustMethod,
                                                  input$pvalueCutoff,
                                                  input$universe)
                }
                
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
