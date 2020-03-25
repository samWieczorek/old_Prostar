rm(list=ls())
options(shiny.maxRequestSize=300*1024^2) 
options(encoding = "UTF-8")
require(compiler)
enableJIT(3)


source(file.path("./src", "commonFunc.R"),  local = TRUE)$value
source(file.path("./src", "modules/Misc/moduleInsertMarkdown.R"),  local = TRUE)$value
source(file.path("./src", "modules/Menu_Help/moduleBugReport.R"),  local = TRUE)$value
source(file.path("./src", "modules/Menu_Home/moduleHomePage.R"),  local = TRUE)$value
source(file.path("./src", "modules/Menu_Home/moduleSettings.R"),  local = TRUE)$value
source(file.path("./src", "modules/Menu_Home/moduleCheckUpdates.R"),  local = TRUE)$value


onStart = function() {
  cat("Doing application setup\n")
  
  onStop(function() {
    cat("Doing application cleanup\n")
    graphics.off()
    unlink(sessionID, recursive = TRUE)
    unlink(paste(tempdir(), "*", sep="/"),recursive = TRUE)
  })
}

source(file.path("./src", "modules/Menu_DataManager/moduleConvertData.R"),  local = TRUE)$value
source(file.path("./src", "modules/Menu_DataManager/moduleOpenMSnSet.R"),  local = TRUE)$value
source(file.path("./src", "modules/Menu_DataManager/moduleOpenDemoDataset.R"),  local = TRUE)$value


library(shinyBS)

#' Title
#'
#' @param input 
#' @param output 
#' @param session 
#'
#' @return
#' @export
#'
#' @examples
server <- function(input, output, session){
  env <- environment()
  loadLibraries()
  
  pipeline.def <- ReadPipelineConfig("src/modules/process/pipeline.conf")
  print(pipeline.def)
  
  #####
  ## Launch modules
  # files <-list.files('src',full.name = TRUE, pattern='*.R$', recursive=TRUE)
  # 
  #  for (f in files){
  #    print(paste0('sourcing ', f))
  #    if (f != 'src/core.R')
  #      source(f, local=TRUE)$value
  #  }
  
  # source(file.path("./src", "core.R"),  local = TRUE)$value
  
  source(file.path("./src", "modules/Menu_Home/moduleReleaseNotes.R"),  local = TRUE)$value
  source(file.path("./src", "modules/Misc/modulePopover.R"),  local = TRUE)$value
  source(file.path("./src", "modules/Misc/moduleStaticDataTable.R"),  local = TRUE)$value
  source(file.path("./src", "modules/Menu_DataManager/moduleInfoDataset.R"),  local = TRUE)$value
  source(file.path("./src", "modules/Misc/moduleStaticDataTable.R"),  local = TRUE)$value
  source(file.path("./src", "modules/Menu_DataManager/moduleConvertData.R"),  local = TRUE)$value
  source(file.path("./src", "modules/Menu_DataManager/moduleOpenMSnSet.R"),  local = TRUE)$value
  source(file.path("./src", "modules/Menu_DataManager/moduleOpenDemoDataset.R"),  local = TRUE)$value
  source(file.path("./src", "modules/Menu_DataManager/moduleInfoDataset.R"),  local = TRUE)$value
  
  source(file.path("./src", "modules/Menu_Home/moduleHomepage.R"),  local = TRUE)$value
  source(file.path("./src", "modules/Menu_Home/moduleCheckUpdates.R"),  local = TRUE)$value
  source(file.path("./src", "modules/Menu_Home/moduleReleaseNotes.R"),  local = TRUE)$value
  source(file.path("./src", "modules/Menu_Home/moduleSettings.R"),  local = TRUE)$value
  
  source(file.path("./src", "modules/Menu_Help/moduleBugReport.R"),  local = TRUE)$value
  
  
  
  source(file.path("./src", "srv_ReloadProstar.R"),  local = TRUE)$value
  
  ## L'appel a core.R permet d'attendre le chargement d'un dataset et de créer ensuite le pipeline correspondant
  
  
  plan(multiprocess)
  
  
  #Global reactive variables for Prostar-core
  rv.prostar <- reactiveValues(
    obj = NULL,
    settings = NULL
  )
  
  
  ## definition des variables globales liees a un pipeline
  pipeline <- reactiveValues(
    # current working data from current pipeline
    type = NULL,
    
    ## indice du dataset courant dans la liste ll.process.
    current.indice = 1,
    
    ## liste qui contiendra les noms des différents datasets enregsitres au cours 
    ## de l'execution sdu module. Il est initialisé à Original car dans tous les cas,
    ## on démarre avec un dataset intitule original
    ll.process = c('original'),
    
    # object returned by demode, openmode and convertmode
    #object that is used for modules in pipeline. C'st une instance d'une classe Pipeline
    obj = NULL,
    
    tempplot = NULL,
    loadData = NULL
    
  )
  
<<<<<<< HEAD


  ## Select submenu in sidebar
 observeEvent(input$sidebar_left, {
    switch(input$sidebar_left,
           FAQ =  toggleModal(session, "modalFAQ"),
           links = toggleModal(session, "modallinks"),
           bugReport = toggleModal(session, "modalbugreport")

=======
  source(file.path("./src", "core.R"),  local = TRUE)$value
  
  #Set up writing file for log
  logfilename <- tempfile(fileext=".log")
  print(paste0('logfilename = ',logfilename))
  con <- file(logfilename,open="wt")
  if(!interactive()){
    sink(con, append=TRUE)
    sink(con, append=TRUE, type="message")
  }
  
  print("Debut du call des modules cpte serveur")
  
  
  ## Module propres au core de Prostar et qui existent indépendamment de tout pipeline ou dataset
  #rv.prostar$settings <- callModule(moduleSettings, "modSettings",dataIn=reactive({GetCurrentMSnSet()}))
  
  callModule(moduleInsertMarkdown, "FAQ_MD",URL_FAQ)
  callModule(moduleInsertMarkdown, "links_MD",URL_links)
  callModule(module = moduleBugReport, 'bugreport', logfile=reactive({logfilename}))
  
  pipeline$loadData <- callModule(module=moduleOpenDemoDataset, 'mod_OpenDemoDataset')
  pipeline$loadData <- callModule(module=moduleOpenMSnSet, 'moduleOpenMSnSet')
  pipeline$loadData <- callModule(module=moduleConvertData, 'moduleProcess_Convert')
  
  
  observeEvent( req(input$navPage),{
    
    shinyjs::toggle('tete', condition=!(input$navPage %in% c('graphTab', 'bugReportTab', 'checkForUpdatesTab', 'faqTab')))
    print(paste0('input$navPage = ',input$navPage))
    switch(input$navPage,
           # DescriptiveStatisticsTab = source(file.path("server", "srv_DescriptiveStats.R"),  local = TRUE)$value,
           # #SessionLogsTab = source(file.path("server", "srv_LogSession.R"),  local = TRUE)$value,
           #   source(file.path("server", "srv_DemoMode.R"),  local = TRUE)$value,
           #   source(file.path("server", "srv_ConvertData.R"),  local = TRUE)$value
           #   source(file.path("server", "srv_BuildDesign.R"),  local = TRUE)$value
           # },
           # ExportTab = {
           #   source(file.path("server", "srv_Export.R"),  local = TRUE)$value
           #   source(file.path("server", "srv_SaveGraphics.R"), local = TRUE)$value
           # },
           # ReloadTab = {
           #   source(file.path("server", "srv_ReloadProstar.R"),  local = TRUE)$value
           # },
           # FilteringTab  = 
           #   source(file.path("server", "srv_Filtering.R"),  local = TRUE)$value,
           # NormalizationTab  = 
           #   source(file.path("server", "srv_Normalization.R"),  local = TRUE)$value,
           # imputationProteinLevelTabs = {
           #   source(file.path("server", "srv_Imputation_ProteinLevel.R"),  local = TRUE)$value
           # },
           # imputationPeptideLevelTabs = {
           #   source(file.path("server", "srv_Imputation_PeptideLevel.R"),  local = TRUE)$value
           # },
           # AggregationTab =
           #   source(file.path("server", "srv_Aggregation.R"),  local = TRUE)$value,
           # diffAnalysisTab = 
           #   {
           #     source(file.path("server", "srv_AnaDiff.R"),  local = TRUE)$value
           #   },
           # graphTab = 
           #   {
           #     callModule(module = moduleCC, "CC_Multi_Any", cc=reactive({rv$CC$allPep}))
           #   },
           # GoTab  = source(file.path("server", "srv_GO_enrichment.R"),  local = TRUE)$value,
           # 
           
           
           
           # Menu Help
           faqTab =  toggleModal(session, "modalFAQ"),
           usefulLinksTab =  toggleModal(session, "modallinks"),
           bugReportTab =toggleModal(session, "modalbugreport"),
           
           ## Menu Home
           HomeTab = callModule(moduleHomepage, "homepage"),
           CheckUpdatesTab =  callModule(moduleCheckUpdates, "modCheckUpdates"),
           ReleaseNotesTab =  callModule(moduleReleaseNotes, "modReleaseNotes"),
           GlobalSettingsTab = rv.prostar$settings <- callModule(moduleSettings, "modSettings",dataIn=reactive({GetCurrentMSnSet()}))
           
           #testTab = source(file.path("server", "srv_HypothesisTest.R"),  local = TRUE)$value
           #testPeptideTab = source(file.path("server", "srv_AggregateTest_Peptide.R"),  local = TRUE)$value,
           #testProteinTab = source(file.path("server", "srv_HypothesisTestProtein.R"),  local = TRUE)$value
>>>>>>> 16efda6b2b280178cfd2b9cffc6d6e9a41699eba
    )
    
  })
  
<<<<<<< HEAD
   }
=======
  
  
  shinyjs::hide(id = "loading_page", anim = FALSE)
  
  shinyjs::show("main_content", anim = TRUE, animType = "fade")
}
>>>>>>> 16efda6b2b280178cfd2b9cffc6d6e9a41699eba
