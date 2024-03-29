rm(list=ls())

options(shiny.maxRequestSize=300*1024^2)
options(encoding = "UTF-8")

options(shiny.fullstacktrace=T)

require(compiler)
enableJIT(3)

source(file.path("ui", "ui_Configure.R"),  local = TRUE)$value

# initialize data with colnames
df <- data.frame(matrix(c("0","0"), 1, 2))
colnames(df) <- c("Input1", "Input2")

onStart = function() {
  cat("Doing application setup\n")
  
  onStop(function() {
    cat("Doing application cleanup\n")
    graphics.off()
    unlink(sessionID, recursive = TRUE)
    unlink(paste(tempdir(), sessionID, commandLogFile, sep="/"),recursive = TRUE)
    unlink(paste(tempdir(), sep="/"), recursive = TRUE)
    unlink(paste(tempdir(), "*", sep="/"), recursive = TRUE)
    unlink(paste(tempdir(), "*html", sep="/"))
    unlink(paste(tempdir(), "*log", sep="/"))
    unlink("www/*pdf")
  })
}




shinyServer(function(input, output, session) {
  #Sys.setlocale("LC_ALL","English")
  #Sys.setlocale("LC_ALL", 'en_GB.UTF-8')
  #Sys.setlocale("LC_ALL", 'fr_FR.UTF-8')
  #Sys.setenv(LANG = "fr")
  print(Sys.getlocale())
  
  
  
  Sys.setenv("R_ZIPCMD"= Sys.which("zip"))
  sessionID <- Sys.getpid()
  
  
  #Set up writing
  logfilename <- tempfile(fileext=".log")
  con <- file(logfilename,open="wt")
  if(!interactive()){
    sink(con, append=TRUE)
    sink(con, append=TRUE, type="message")
  }
  
  message(tempdir())
  
  message(tempdir())
  message(normalizePath(tempdir()))
  message(getwd())
  message("TEST=")
  message(Sys.getenv('TEST'))
  
  message(Sys.getenv('TMP'))
  message(Sys.getenv('TMPDIR'))
  message(Sys.getenv('TEMP'))
  
  # unsuspendAll(session)
  
  serverAdmin <- FALSE
  if (isTRUE(serverAdmin)){
    hname <- system$getHostname()
    
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
  
  env <- environment()
  source(file.path("server", "mod_staticDT.R"), local=TRUE)$value
  source(file.path("server", "mod_popover.R"), local = TRUE)$value
  source(file.path("server","mod_download_btns.R"), local=TRUE)$value
  source(file.path("modules/Plots", "mod_MSnSetExplorer.R"), local=TRUE)$value
  source(file.path("server", "mod_LegendColoredExprs.R"), local=TRUE)$value
  
  
  
  source(file.path("server", "srv_NavbarPage.R"),  local = TRUE)$value
  source(file.path("server", "srv_ModulesSrv.R"),  local = TRUE)$value
  source(file.path("server", "srv_ModuleProcess.R"),  local = TRUE)$value
  source(file.path("server", "srv_General.R"), local = TRUE)$value
  source(file.path("server", "srv_DefineRVmoduleProcess.R"), local = TRUE)$value
  source(file.path("server", "srv_Home.R"), local = TRUE)$value
  source(file.path("server", "srv_Settings.R"), local = TRUE)$value
  source(file.path("server", "srv_ParamsManager.R"), local = TRUE)$value
  
   #source(file.path(".", "modules/Plots/modulePlots.R"),  local = TRUE)$value
  source(file.path(".", "modules/Plots/moduleCC.R"),  local = TRUE)$value
  
  
  # observeEvent(rv$current.obj,{
  #   print("callModule showPlots")
  #   callModule(module = modulePlots, 'showPlots', 
  #              dataIn=reactive({rv$current.obj}), 
  #              llPlots=reactive({lstDescPlots}))
  # })
  
  # outputOptions(output, 'settings_nDigits', suspendWhenHidden=FALSE)
  
  #activatePopover()
  
  loadLibraries()
  
  observeEvent(input$distance,{rv$PlotParams$heatmap.distance <- input$distance})
  observeEvent(input$distance,{rv$PlotParams$heatmap.linkage <- input$linkage})
  
  
  
  observe({
    req(input$navPage)
    shinyjs::toggle('tete', condition=!(input$navPage %in% c('graphTab', 'bugReportTab', 'checkForUpdatesTab', 'faqTab')))
    switch(input$navPage,
           DescriptiveStatisticsTab = {
             source(file.path("server", "mod_plots_metacell_histo.R"),  local = TRUE)$value
             source(file.path("server", "srv_DescriptiveStats.R"),  local = TRUE)$value
             },
           openMSnsetTab = {
             source(file.path("server", "srv_OpenMSnset.R"),  local = TRUE)$value
           },
           #SessionLogsTab = source(file.path("server", "srv_LogSession.R"),  local = TRUE)$value,
           
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
           ReloadTab = {
             source(file.path("server", "srv_ReloadProstar.R"), local = TRUE)$value
           },
           
           FilteringTab =
            {
              source(file.path("server", "mod_plots_metacell_histo.R"), local = TRUE)$value
              source(file.path("server", "mod_filtering_example.R"),  local = TRUE)$value
              source(file.path("server", "srv_Filtering.R"), local = TRUE)$value
              source(file.path("server", "mod_filtering_example.R"),  local = TRUE)$value
              
            },
           
           NormalizationTab = 
             {
               source(file.path("server", "mod_plots_tracking.R"), local = TRUE)$value
               source(file.path("server", "mod_plots_intensity.R"), local = TRUE)$value
               source(file.path("server", "srv_Normalization.R"), local = TRUE)$value
               },
           
           imputationProteinLevelTabs = {
             source(file.path("server", "srv_Imputation_ProteinLevel.R"), local = TRUE)$value
           },
           imputationPeptideLevelTabs = {
             source(file.path("server", "srv_Imputation_PeptideLevel.R"), local = TRUE)$value
           },
           AggregationTab =
             source(file.path("server", "srv_Aggregation.R"), local = TRUE)$value,
           
           diffAnalysisTab = 
             {
               source(file.path("server", "srv_AnaDiff.R"), local = TRUE)$value
             },
           
           graphTab = 
             {
               mod_cc_server("CC_Multi_Any", 
                             obj = reactive({rv$current.obj}),
                             cc = reactive({GetCC(rv$current.obj)$allPep})
                             )
               
             },
           
           GoTab = 
             source(file.path("server", "srv_GO_enrichment.R"), local = TRUE)$value,
           
           # updateDesignTab = 
           #   source(file.path("server", "srv_UpdateDesign.R"),  local = TRUE)$value,
           # 
           faqTab = 
             source(file.path("server", "srv_FAQ.R"), local = TRUE)$value,
           checkForUpdatesTab = 
             source(file.path("server", "srv_CheckForUpdates.R"), local = TRUE)$value,
           usefulLinksTab = 
             source(file.path("server", "srv_UsefulLinks.R"), local = TRUE)$value,
           
           ReleaseNotesTab = 
             source(file.path("server", "srv_ReleaseNotes.R"), local = TRUE)$value,
           
           bugReportTab = source(file.path("server", "srv_BugReport.R"), local = TRUE)$value,
           
           testTab = source(file.path("server", "srv_HypothesisTest.R"), local = TRUE)$value
    )
    
  })
  
  
  shinyjs::hide(id = "loading_page", anim = FALSE)
  
  shinyjs::show("main_content", anim = TRUE, animType = "fade")
  
  print(ls())
})