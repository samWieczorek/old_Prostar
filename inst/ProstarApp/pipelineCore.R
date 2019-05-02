source(file.path(".", "modules/process/peptide/moduleA.R"), local = TRUE)$value
source(file.path(".", "modules/process/peptide/moduleB.R"), local = TRUE)$value
source(file.path(".", "modules/process/peptide/moduleC.R"), local = TRUE)$value

source(file.path(".", "modules/process/protein/moduleD.R"), local = TRUE)$value
source(file.path(".", "modules/process/protein/moduleE.R"), local = TRUE)$value
source(file.path(".", "modules/process/protein/moduleF.R"), local = TRUE)$value
source(file.path(".", "modules/process/protein/moduleG.R"), local = TRUE)$value

source(file.path(".", "modules/process/p2p/moduleH.R"), local = TRUE)$value
source(file.path(".", "modules/process/p2p/moduleI.R"), local = TRUE)$value

source(file.path(".", "modules/DataManager/moduleOpenDataset.R"), local = TRUE)$value


pipeline <- reactiveValues(
  # current working data from current pipeline
  type = NULL,
  current.indice = 1,
  ll.process = c('original'),
  
  # object returned by demode, openmode and convertmode
  init.obj = NULL,
  #object that is used for modules in pipeline
  current.obj = NULL
  
)



obj.demomode <- callModule(module=moduleDemoMode, 'demoMode', selectedPanel = reactive({input$navPage}))


GetCurrentMSnSet <- reactive({
  pipeline$current.indice
  pipeline$current.obj
  pipeline$current.obj$datasets[[pipeline$current.indice]]
})

# observe({
#   GetCurrentMSnSet()
#   
#   callModule(module = modulePlots, 'showPlots', 
#              dataIn=reactive({GetCurrentMSnSet()}), 
#              llPlots=reactive({lstDescPlots}))
#   print("---- new value for pipeline$current.msnset ----")
# })



observeEvent(GetCurrentMSnSet(),{
  print("callModule showPlots")
  print(dim(GetCurrentMSnSet()))
  callModule(module = modulePlots, 'showPlots', 
             dataIn=reactive({GetCurrentMSnSet()}), 
             llPlots=reactive({lstDescPlots}))
})




# observeEvent(obj.demomode(),{ 
#   print("update for rv$current.obj")
#   rv$current.obj <- obj.demomode()  
#   print(rv$current.obj)
#   })


## Initialization of the pipeline
observeEvent(req(obj.demomode()),{
  print(paste0("IN observeEvent(req(obj()$initialData : ", obj.demomode()$pipeline))
  
  switch(obj.demomode()$pipeline,
         
         Peptide={
           # Load UI code for modules
           #LoadModulesUI(path2peptideModules, peptide.def)
           pipeline$ll.process <- peptide.def
           
           BuildPipelineMenu("Pipeline peptide", peptide.def)
           
           # Build and load server code for modules
           codeFile <- createWatchCode(peptide.def)
           source(file.path(".", codeFile),  local = TRUE)$value
           
           },
         Protein = {
           pipeline$ll.process <- protein.def
           #LoadModulesUI(path2proteinModules, protein.def)
           BuildPipelineMenu("Pipeline protein", protein.def)
           
           codeFile <- createWatchCode(protein.def)
           source(file.path(".", codeFile),  local = TRUE)$value
         },
         P2p = {
           pipeline$ll.process <- p2p.def
           #LoadModulesUI(path2p2pModules, p2p.def)
           BuildPipelineMenu("Pipeline p2p", p2p.def)
           
           codeFile <- createWatchCode(p2p.def)
           source(file.path(".", codeFile),  local = TRUE)$value
         }
  )
  
  pipeline$current.indice <- 1
  pipeline$current.obj <- obj.demomode()
  
  ## which processes will be part of the pipeline
  pipeline$init.obj <- list(NULL)
  pipeline$init.obj[['original']] <- obj.demomode()$datasets$original
  pipeline$init.obj[pipeline$ll.process] <- list(NULL)
  #pipeline$current.dataset <- pipeline$init.obj
  #pipeline$current.obj <- pipeline$init.obj[['original']]
})





LoadModulesUI <- function(path, ll.modules){
  
  lapply(ll.modules, function(i) {
    UIfile <- paste0(path, i, ".R")
    print(UIfile)
    source(file.path(".", UIfile), local = environment())
    })
}

BuildPipelineMenu <- function(name, def){
  
  RemoveAllPipelineTabs()
  tabs <-lapply(1:length(def), function(i) {
    tabPanel(
      title=def[i], 
      do.call(paste0(def[i],'UI'), list(def[i]))
    )
  })
  insertTab(inputId = "navPage",
            do.call(navbarMenu, c(name ,tabs)),
            target="Data manager",
            position="after")
}






RemoveAllPipelineTabs <- function(){
  removeTab(inputId = "navPage", target = "Pipeline peptide")
  removeTab(inputId = "navPage", target = "Pipeline protein")
  removeTab(inputId = "navPage", target = "Pipeline p2p")
}


GetScreenId <- reactive({
  input$navPage
  req(pipeline$current.obj)
  
  screen <- NULL
  m <-  which(names(pipeline$current.obj$datasets)==input$navPage)
  n <-  which(unlist(lapply(GetCurrentMSnSet(), function(x) length(which(x==pipeline$current.obj$datasets))))==1)
  ## test if the navPage is one of a process one
  if (length(m) ==0 || length(n) ==0) {return(NULL)}
  
  if (m >= n) { screen <- 'Initial screen'}
  else {screen <- 'Final screen'}
  print(paste0("in GetScreenId(), n = ", n, ", m = ", m, ". screen = ", screen))
  screen
})



DeleteDatasetsAfter <- function(txt){
  names <- names(pipeline$current.obj$datasets)
  indice <- which(names == txt)
  if (indice < length(names)) {
    for (i in (indice+1):length(names)){
      pipeline$current.obj$datasets[i] <- list(NULL)
    }
  }
}




printStatus <- function(){
  print("##### PrintStatus of module #####")
  print("ll.process() = ")
  print(pipeline$ll.process)
  print('init.obj = ')
  print(pipeline$init.obj)
  print(paste0('pipeline$indice= ',pipeline$current.indice))
  print(paste0('pipeline$current.obj= ',pipeline$current.obj))
  print('pipeline$current.dataset= ')
  print(pipeline$current.dataset)
  #print(paste0('ProcessX = ',ProcessX()))
}




##################
# This function is used to generate source code files qui creent 
# les observateurs pour les modules de process
##################
createWatchCode <- function(process){
  #tempfile(fileext=".R")
  filename <- 'watch.R'
  con <- file(filename,open="wt")
  
  
  for (p in process){
  txt <- paste0("Watch",p," <- callModule(module=",p,",'", p,"',  
                              dataIn=reactive({pipeline$current.obj}),
                              screen.id = reactive({GetScreenId()}))")
  writeLines(txt, con)
  
  txt <- paste0(
    "observeEvent(Watch",p,"(),{
    print(paste0('observeEvent(",p,"() : ', Watch",p,"()))
    pipeline$current.obj <- Watch",p,"()
    pipeline$current.indice <- 1 + which(pipeline$ll.process == '",p,"')
    pipeline$current.dataset$",p," <- Watch",p,"()
    DeleteDatasetsAfter('",p,"')
    printStatus()
  })")
  
  writeLines(txt, con)
  
  }
  
  close(con)
  
  return(filename)
  
}