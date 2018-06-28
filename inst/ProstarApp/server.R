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
    #cat(file=stderr())
    Sys.setlocale("LC_ALL", 'en_GB.UTF-8')
    Sys.setenv("R_ZIPCMD"= Sys.which("zip"))
    sessionID <- Sys.getpid()
    
    
    #Set up writing
    logfilename <-paste(tempdir(),"shiny.log", sep="/")
    print(logfilename)
      con <- file(logfilename)
    if(interactive()){
      sink(con, append=TRUE)
      sink(con, append=TRUE, type="message")
    }
    
    
    
    
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
    source(file.path("server", "srv_NavbarPage.R"),  local = TRUE)$value
    source(file.path("server", "srv_ModulesSrv.R"),  local = TRUE)$value
    source(file.path("server", "srv_General.R"), local = TRUE)$value
    
    #outputOptions(output, 'currentObjLoaded', suspendWhenHidden=FALSE)
    
    #activatePopover()
  
    
     observe({
        req(input$navPage)
        print(input$navPage)
        
        switch(input$navPage,
               DescriptiveStatisticsTab = source(file.path("server", "srv_DescriptiveStats.R"),  local = TRUE)$value,
               openMSnsetTab = source(file.path("server", "srv_OpenMSnset.R"),  local = TRUE)$value,
               SessionLogsTab = source(file.path("server", "srv_LogSession.R"),  local = TRUE)$value,
               
                 demoTab = 
                 source(file.path("server", "srv_DemoMode.R"),  local = TRUE)$value,
               convertTab = {
                 source(file.path("server", "srv_ConvertData.R"),  local = TRUE)$value
                 source(file.path("server", "srv_BuildDesign.R"),  local = TRUE)$value
                   },
               ExportTab = {
                 source(file.path("server", "srv_Export.R"),  local = TRUE)$value
                 source(file.path("server", "srv_SaveGraphics.R"), local = TRUE)$value
               },
                 
               FilterDataTab =
                 source(file.path("server", "srv_Filtering.R"),  local = TRUE)$value,
               
               Normalization = 
                 source(file.path("server", "srv_Normalization.R"),  local = TRUE)$value,
               
               imputationProteinLevelTabs = {
                 source(file.path("server", "srv_Imputation_ProteinLevel.R"),  local = TRUE)$value
                 },
               imputationPeptideLevelTabs = {
                  source(file.path("server", "srv_Imputation_PeptideLevel.R"),  local = TRUE)$value
               },
               AggregationTab =
                 source(file.path("server", "srv_Aggregation.R"),  local = TRUE)$value,
               
               diffAnalysisTab = 
                 {
                   source(file.path("server", "srv_AnaDiff.R"),  local = TRUE)$value
                   },
               
               GOAnalysisTab = 
                 source(file.path("server", "srv_GO_enrichment.R"),  local = TRUE)$value,
               
               updateDesignTab = 
                 source(file.path("server", "srv_UpdateDesign.R"),  local = TRUE)$value,
               
               faqTab = 
                 source(file.path("server", "srv_FAQ.R"),  local = TRUE)$value,
               checkForUpdatesTab = 
                 source(file.path("server", "srv_CheckForUpdates.R"),  local = TRUE)$value,
               usefulLinksTab = 
                 source(file.path("server", "srv_UsefulLinks.R"),  local = TRUE)$value,
               
               ReleaseNotesTab = 
                 source(file.path("server", "srv_ReleaseNotes.R"),  local = TRUE)$value,
               
               bugReportTab = source(file.path("server", "srv_BugReport.R"),  local = TRUE)$value
               )

     })
    
})
