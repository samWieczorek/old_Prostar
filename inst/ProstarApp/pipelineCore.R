source(file.path(".", "modules/pipelines/peptide/moduleA.R"), local = TRUE)$value
source(file.path(".", "modules/pipelines/peptide/moduleB.R"), local = TRUE)$value
source(file.path(".", "modules/pipelines/peptide/moduleC.R"), local = TRUE)$value

source(file.path(".", "modules/pipelines/protein/moduleD.R"), local = TRUE)$value
source(file.path(".", "modules/pipelines/protein/moduleE.R"), local = TRUE)$value
source(file.path(".", "modules/pipelines/protein/moduleF.R"), local = TRUE)$value
source(file.path(".", "modules/pipelines/protein/moduleG.R"), local = TRUE)$value

source(file.path(".", "modules/pipelines/p2p/moduleH.R"), local = TRUE)$value
source(file.path(".", "modules/pipelines/p2p/moduleI.R"), local = TRUE)$value


pipeline <- reactiveValues(
  # current working data from current pipeline
  current.obj = NULL,
  current.indice = 1,
  current.dataset = NULL,
  ll.process = c('original'),
  init.obj = NULL   
)




## Initialization of the pipeline
observeEvent(req(obj()$initialData),{
  
  switch(obj()$pipeline,
         
         Peptide={
           BuildPipelineMenu("Pipeline peptide", peptide.def)
           pipeline$ll.process <- peptide.def
           codeFile <- createWatchCode(peptide.def)
           source(file.path(".", codeFile),  local = TRUE)$value
           
           },
         Protein = {
           BuildPipelineMenu("Pipeline protein", protein.def)
           
           pipeline$ll.process <- protein.def
           codeFile <- createWatchCode(protein.def)
           source(file.path(".", codeFile),  local = TRUE)$value
         },
         P2p = {
           BuildPipelineMenu("Pipeline p2p", p2p.def)
           
           pipeline$ll.process <- p2p.def
           codeFile <- createWatchCode(p2p.def)
           source(file.path(".", codeFile),  local = TRUE)$value
         }
  )
  
  pipeline$current.indice <- 1
  
  
  ## which processes will be part of the pipeline
  
  pipeline$init.obj <- NULL
  pipeline$init.obj[['original']] <- obj()$initialData
  pipeline$init.obj[pipeline$ll.process] <- list(NULL)
  pipeline$current.dataset <- pipeline$init.obj
  pipeline$current.obj <- pipeline$init.obj[['original']]
  
  
  
})



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
  m <-  which(names(pipeline$current.dataset)==input$navPage)
  n <-  which(unlist(lapply(pipeline$current.dataset, function(x) length(which(x==pipeline$current.obj))))==1)
  ## test if the navPage is one of a process one
  if (length(m) ==0 || length(n) ==0) {return(NULL)}
  
  if (m >= n) { screen <- 'Initial screen'}
  else {screen <- 'Final screen'}
  print(paste0("in GetScreenId(), n = ", n, ", m = ", m, ". screen = ", screen))
  screen
})



DeleteDatasetsAfter <- function(txt){
  names <- names(pipeline$current.dataset)
  indice <- which(names == txt)
  if (indice < length(names)) {
    for (i in (indice+1):length(names)){
      pipeline$current.dataset[i] <- list(NULL)
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