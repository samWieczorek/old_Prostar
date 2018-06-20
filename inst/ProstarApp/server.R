rm(list=ls())

options(shiny.maxRequestSize=300*1024^2) 


require(compiler)
enableJIT(3)
###library(DAPARdata)

source(file.path("ui", "uiConfigure.R"),  local = TRUE)$value

# initialize data with colnames
df <- data.frame(matrix(c("0","0"), 1, 2))
colnames(df) <- c("Input1", "Input2")



shinyServer(function(input, output, session) {
    cat(file=stderr())
    #Sys.setlocale("LC_ALL", 'en_GB.UTF-8')
    Sys.setlocale("LC_ALL", 'en_GB.UTF-8')
    Sys.setenv("R_ZIPCMD"= Sys.which("zip"))
    sessionID <- Sys.getpid()
    
    
   # unsuspendAll(session)
    

      hideTab(inputId ="navPage", target = "updateDesign")  
      
      
    serverAdmin <- FALSE
    if (isTRUE(serverAdmin)){
        hname <- System$getHostname()
        
        clientdataText <- observe({
            rv$IP_Client = session$clientData$url_hostname
        })
        
        #verbose <- TRUE
        sessionLogFile <- paste("www/sessionLogs_", gsub("\\.", "_", rv$IP_Client), "_", sessionID, ".txt",sep="")
        if (!interactive()) sink(sessionLogFile, type = "output")
    }
    # Simulate work being done for 1 second
    #Sys.sleep(1)

    # Hide the loading message when the rest of the server function has executed
    hide(id = "loading-content", anim = TRUE, animType = "fade")
    
    env <- environment()
    source(file.path("server", "srv_modulesSrv.R"),  local = TRUE)$value
    source(file.path("server", "srv_help.R"),  local = TRUE)$value
    source(file.path("server", "srv_openMSnset.R"),  local = TRUE)$value
    source(file.path("server", "srv_general.R"), local = TRUE)$value
    source(file.path("server", "srv_datasetManager.R"),  local = TRUE)$value
    source(file.path("server", "srv_descriptiveStats.R"),  local = TRUE)$value
    
    hideTab(inputId ="navPage", target = "updateDesignTab")
    # source(file.path("server", "srv_updateDesign.R"),  local = TRUE)$value
      source(file.path("server", "srv_buildDesign.R"),  local = TRUE)$value
      source(file.path("server", "srv_convertData.R"),  local = TRUE)$value
    #  source(file.path("server", "srv_saveGraphics.R"), local = TRUE)$value
    # source(file.path("server", "srv_filtering.R"),  local = TRUE)$value
      source(file.path("server", "srv_imputation_ProteinLevel.R"),  local = TRUE)$value
      source(file.path("server", "srv_imputation_PeptideLevel.R"),  local = TRUE)$value
    #  source(file.path("server", "srv_normalization.R"),  local = TRUE)$value
      source(file.path("server", "srv_anaDiff.R"),  local = TRUE)$value
    #  source(file.path("server", "srv_aggregation.R"),  local = TRUE)$value
    # source(file.path("server", "srv_GO_enrichment.R"),  local = TRUE)$value
    # 
    outputOptions(output, 'currentObjLoaded', suspendWhenHidden=FALSE)
    
    #activatePopover()
  
    
     observe({
        input$navPage
        print(input$navPage)
        
        switch(input$navPage,
               DescriptiveStatisticsTab = {
                 #cat("source de srv_descriptiveStats.R")
                 #source(file.path("server", "srv_descriptiveStats.R"),  local = TRUE)$value
                 },
               demoTab = 
                 source(file.path("server", "srv_demoMode.R"),  local = TRUE)$value,
               convertTab = {
                 #source(file.path("server", "srv_convertData.R"),  local = TRUE)$value
                 #source(file.path("server", "srv_buildDesign.R"),  local = TRUE)$value
                   },
               ExportTab = {
                 source(file.path("server", "srv_export.R"),  local = TRUE)$value
                 source(file.path("server", "srv_saveGraphics.R"), local = TRUE)$value
               },
                 
               FilterDataTab =
                 source(file.path("server", "srv_filtering.R"),  local = TRUE)$value,
               Normalization = 
                 source(file.path("server", "srv_normalization.R"),  local = TRUE)$value,
               imputationTabs = {
                 #source(file.path("server", "srv_imputation_ProteinLevel.R"),  local = TRUE)$value
                 #source(file.path("server", "srv_imputation_PeptideLevel.R"),  local = TRUE)$value
               },
               AggregationTab =
                 source(file.path("server", "srv_aggregation.R"),  local = TRUE)$value,
               #diffAnalysisTab = 
               #  source(file.path("server", "srv_anaDiff.R"),  local = TRUE)$value,
               GOAnalysisTab = 
                 source(file.path("server", "srv_GO_enrichment.R"),  local = TRUE)$value,
               
               updateDesignTab = 
                 source(file.path("server", "srv_updateDesign.R"),  local = TRUE)$value
               )

     })
    
})
