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
    Sys.setlocale("LC_ALL", 'en_GB.UTF-8')
    Sys.setenv("R_ZIPCMD"= Sys.which("zip"))
    sessionID <- Sys.getpid()
    
    
    
    
    
    
    
   # unsuspendAll(session)
       
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
    source(file.path("server", "srv_NavbarPage.R"), local = TRUE)$value
    source(file.path("server", "srv_ModulesSrv.R"), local = TRUE)$value
    source(file.path("server", "srv_General.R"), local = TRUE)$value
    
    #outputOptions(output, 'currentObjLoaded', suspendWhenHidden=FALSE)
    
    #activatePopover()
  
    
     observe({
        req(input$navPage)
        print(input$navPage)
        
        switch(input$navPage,
               DescriptiveStatisticsTab = source.file("srv_DescriptiveStats.R"),
               
               openMSnsetTab = source.file("srv_OpenMSnset.R"),
               
               SessionLogsTab = source.file("srv_LogSession.R"),
               
               demoTab =  source.file("srv_DemoMode.R"),
                                 
               convertTab = {
                 source.file("srv_ConvertData.R")
                 source.file("srv_BuildDesign.R")
                   },
               ExportTab = {
                 source.file("srv_Export.R")
                 source.file("srv_SaveGraphics.R")
               },
                 
               FilterDataTab = source.file("srv_Filtering.R"),
               
               Normalization = source.file("srv_Normalization.R"),
               
               imputationProteinLevelTabs =source.file("srv_Imputation_ProteinLevel.R"),
               
               imputationPeptideLevelTabs = source.file("srv_Imputation_PeptideLevel.R"),
               
               AggregationTab = source.file("srv_Aggregation.R"),
               
               diffAnalysisTab =  source.file("srv_AnaDiff.R"),
               
               GOAnalysisTab = source.file("srv_GO_enrichment.R"),
               
               updateDesignTab = source.file("srv_UpdateDesign.R"),
               
               faqTab = source.file("srv_FAQ.R"),
               
                checkForUpdatesTab = source.file("srv_CheckForUpdates.R"),
               
                usefulLinksTab = source.file("srv_UsefulLinks.R"),
               
               ReleaseNotesTab = source.file("srv_ReleaseNotes.R")
               
               
               )

     })
    
})
