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
  print('EVENT ON : obj()')
  print(paste0("Obj() = ", obj()$initialData))
  print(paste0("pipeline = ", obj()$pipeline))
  
  
  switch(obj()$pipeline,
         Peptide={
           BuildPipelinePeptide()
           pipeline$ll.process <- peptide.def
           codeFile <- createWatchCode(peptide.def[2:4])
           source(file.path(".", codeFile),  local = TRUE)$value
           
           },
         Protein = {
           BuildPipelineProtein()
           pipeline$ll.process <- protein.def
           },
         P2p = {
           BuildPipelineP2p()
           pipeline$ll.process <- p2p.def
           }
  )
  
  pipeline$current.indice <- 1
  
  
  ## which processes will be part of the pipeline
  
  pipeline$init.obj <- NULL
  pipeline$init.obj[pipeline$ll.process] <- list(NULL)
  pipeline$init.obj[['original']] <- obj()$initialData
  pipeline$current.dataset <- pipeline$init.obj
  pipeline$current.obj <- pipeline$init.obj[['original']]
  
  
  
})



BuildPipelinePeptide <- function(){
  RemoveAllPipelineTabs()
  insertTab(inputId = "navPage",
            navbarMenu("Pipeline peptide" ,
                       tabPanel("ProcessA",moduleAUI(peptide.def[2])),
                       tabPanel("ProcessB",moduleBUI(peptide.def[3])),
                       tabPanel("ProcessC",moduleCUI(peptide.def[4]))),
            target="Data manager",
            position="after")
}


BuildPipelineProtein <- function(){
  RemoveAllPipelineTabs()
  insertTab(inputId = "navPage",
            navbarMenu("Pipeline protein" ,
                       tabPanel("ProcessD",moduleDUI('processD')),
                       tabPanel("ProcessE",moduleEUI('processE')),
                       tabPanel("ProcessF",moduleFUI('processF')),
                       tabPanel("ProcessG",moduleGUI('processG'))),
            target="Data manager",
            position="after")
}


BuildPipelineP2p <- function(){
  RemoveAllPipelineTabs()
  insertTab(inputId = "navPage",
            navbarMenu("Pipeline p2p" ,
                       tabPanel("ProcessH",moduleHUI('processH')),
                       tabPanel("ProcessI",moduleIUI('processI'))),
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
  m <-  which(pipeline$ll.process==input$navPage)
  n <-  which(unlist(lapply(pipeline$current.dataset, function(x) length(which(x==pipeline$current.obj))))==1)
  ## test if the navPage is one of a process one
  if (length(m) ==0 || length(n) ==0) {return(NULL)}
  
  if (m >= n) { screen <- 'Initial screen'}
  else {screen <- 'Final screen'}
  print(paste0("in GetScreenId(), n = ", n, ", m = ", m, ". screen = ", screen))
  screen
})



DeleteDatasetsAfter <- function(txt){
  indice <- which(pipeline$ll.process == txt)
  if (indice < length(pipeline$ll.process)) {
    for (i in (indice+1):length(pipeline$ll.process)){
      pipeline$current.dataset[i] <- list(NULL)
    }
  }
}




printStatus <- function(){
  print("PrintStatus of module ")
  print(paste0("ll.process() = ", pipeline$ll.process))
  print(paste0('init.obj = ',pipeline$init.obj))
  print(paste0('pipeline$indice= ',pipeline$current.indice))
  print(paste0('pipeline$current.obj= ',pipeline$current.obj))
  print('pipeline$datasetl= ')
  print(pipeline$dataset)
  #print(paste0('ProcessX = ',ProcessX()))
}




##################

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
  
  txt <- paste0("observeEvent(Watch",p,"(),{
    print(paste0('observeEvent(",p,"() : ', Watch",p,"()))
    pipeline$current.obj <- Watch",p,"()
    pipeline$current.indice <- which(pipeline$ll.process == '",p,"')
    pipeline$current.dataset$",p," <- Watch",p,"()
    DeleteDatasetsAfter('",p,"')
    printStatus()
  })")
  
  writeLines(txt, con)
  
  }
  
  close(con)
  
  return(filename)
  
}