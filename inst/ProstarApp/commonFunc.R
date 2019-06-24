# 
# 
# 
# 
# GetDatasetOverview <- reactive({
#   print("In GetDatasetOverview")
#   print(pipeline$current.obj)
#   req(pipeline$current.obj)
#   
#   obj <- pipeline$current.dataset[[pipeline$current.indice]]
#   
#   columns <- c("Number of samples","Number of conditions",
#                "Number of lines", "Number of missing values", "% of missing values", 
#                "Number of empty lines")
#   
#   do <- data.frame(Definition= columns,
#                    Value=rep(0,length(columns)))
#   
#   NA.count<- length(which(is.na(Biobase::exprs(obj)==TRUE)))
#   pourcentage <- 100 * round(NA.count/(ncol(obj)*nrow(obj)), digits=4)
#   nb.empty.lines <- sum(apply(
#     is.na(as.matrix(Biobase::exprs(obj))), 1, all))
#   
#   
#   val <- c(ncol((Biobase::exprs(obj))),
#            length(unique(Biobase::pData(obj)$Condition)),
#            nrow((Biobase::exprs(obj))),
#            NA.count,
#            pourcentage,
#            nb.empty.lines)
#   do$Value <- val
#   
#   do
# })


GetDatasetOverview2 <- function(obj){
  req(obj)
  
  columns <- c("Number of samples","Number of conditions",
               "Number of lines", "Number of missing values", "% of missing values", 
               "Number of empty lines")
  
  do <- data.frame(Definition= columns,
                   Value=rep(0,length(columns)))
  
  NA.count<- length(which(is.na(Biobase::exprs(obj)==TRUE)))
  pourcentage <- 100 * round(NA.count/(ncol(obj)*nrow(obj)), digits=4)
  nb.empty.lines <- sum(apply(
    is.na(as.matrix(Biobase::exprs(obj))), 1, all))
  
  
  val <- c(ncol((Biobase::exprs(obj))),
           length(unique(Biobase::pData(obj)$Condition)),
           nrow((Biobase::exprs(obj))),
           NA.count,
           pourcentage,
           nb.empty.lines)
  do$Value <- val
  
  return(do)
}


initComplete <- function(){
  
  return (JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': 'darkgrey', 'color': 'black'});",
    "}"))
}



# function to read DT inputs
shinyValue <- function(id,num) {
  unlist(lapply(seq_len(num),function(i) {
    value <- input[[paste0(id,i)]]
    if (is.null(value)) NA else value
  }))
}










shinyOutput <- function(FUN,id,num,...) {
  inputs <- character(num)
  for (i in seq_len(num)) {
    inputs[i] <- as.character(FUN(paste0(id,i),label=NULL,...))
  }
  inputs
}


# function for dynamic inputs in DT
shinyInput <- function(FUN,id,num,...) {
  inputs <- character(num)
  for (i in seq_len(num)) {
    inputs[i] <- as.character(FUN(paste0(id,i),label=NULL,...))
  }
  inputs
}




# Call this function with all the regular navbarPage() parameters, plus a text parameter,
# if you want to add text to the navbar
navbarPageWithText <- function(..., text) {
  navbar <- navbarPage(...)
  textEl <- tags$p(class = "navbar-text", text)
  navbar[[3]][[1]]$children[[1]] <- htmltools::tagAppendChild(
    navbar[[3]][[1]]$children[[1]], textEl)
  navbar
}

# Call this function with an input (such as `textInput("text", NULL, "Search")`) if you
# want to add an input to the navbar
navbarPageWithInputs <- function(..., inputs) {
  navbar <- navbarPage(...)
  form <- tags$form(class = "navbar-form", inputs)
  navbar[[3]][[1]]$children[[1]] <- htmltools::tagAppendChild(
    navbar[[3]][[1]]$children[[1]], form)
  navbar
}







######
### Miscelllaneous functions


#' busyIndicator
busyIndicator <- function(text = "Calculation in progress..",
                          img = "images/ajax-loader.gif", wait=1000) {
  tagList(
    singleton(tags$head(
      tags$link(rel="stylesheet",
                type="text/css",href="busyIndicator/busyIndicator.css")
    ))
    ,div(class="busy-indicator",p(text),img(src=img))
    ,tags$script(sprintf(
      "	setInterval(function(){
      if ($('html').hasClass('shiny-busy')) {
      setTimeout(function() {
      if ($('html').hasClass('shiny-busy')) {
      $('div.busy-indicator').show()
      }
      }, %d)
      } else {
      $('div.busy-indicator').hide()
      }
},100)
      ",wait)
    )
  )
  }



SetCustomCSS <- function(){
  inlineCSS(".body { font-size:14px;}")
  tags$head(includeCSS("www/css/arrow.css"))
  tags$head(HTML("<script type='text/javascript' src='sbs/shinyBS.js'></script>"))
  tags$head(tags$style(".modal-dialog{ width:200px}"))
  tags$head( tags$style(HTML("hr {border-top: 1px solid #000000;}")))
  includeCSS("www/css/prostar.css")
  #,includeCSS("www/css/fontawesome.css")
  inlineCSS(".body { font-size:14px;}")
  inlineCSS(".rect {float: left;
            width: 100px;
            height: 20px;
            margin: 2px;
            border: 1px solid rgba(0, 0, 0, .2);}")
  inlineCSS(".green {background: #06AB27}")
  inlineCSS(".red {background: #C90404}")
  inlineCSS(".grey {background:lightgrey;}")
  inlineCSS(".modal-backdrop {z-index: 1000}")
  
  ## to position correctly the UI under the navbarmenu
  tags$head(tags$style(type="text/css", "body {padding-top: 70px;}"))
  }



Group2Color <- reactive({
  print(paste0("rv$settings()$whichGroup2Color = ", rv.prostar$settings()$whichGroup2Color))
  rv.prostar$settings()$whichGroup2Color
})





getDataForExprs <- function(obj){
  
  
  test.table <- as.data.frame(round(Biobase::exprs(obj),digits=rv.prostar$settings()$nDigits))
  # print(paste0("tutu:",obj@experimentData@other$OriginOfValues))
  if (!is.null(obj@experimentData@other$OriginOfValues)){ #agregated dataset
    test.table <- cbind(test.table, 
                        Biobase::fData(obj)[,obj@experimentData@other$OriginOfValues])
    # print(paste0("tutu:",head(test.table)))
    
  } else {
    test.table <- cbind(test.table, 
                        as.data.frame(matrix(rep(NA,ncol(test.table)*nrow(test.table)), nrow=nrow(test.table))))
    #print(paste0("tata:",head(test.table)))
  }
  return(test.table)
  
}




############################""

getPackagesVersions <- reactive({
  
  type <- "all"
  outOfDate <- "(Out of date)"
  dev <- "(Devel)"
  
  biocRelease <- NULL
  DAPARdata.version <- NULL
  tryCatch({
    biocRelease <- available.packages(contrib.url("http://bioconductor.org/packages/release/bioc/"))
    require(XML)
    html <- readHTMLTable("http://bioconductor.org/packages/release/data/experiment/html/DAPARdata.html")
    DAPARdata.version <- as.character(html[[3]][2][1,])
    
  }, warning = function(w) {
    return()
  }, error = function(e) {
    return()
  }, finally = {
    #cleanup-code 
  })
  
  pkgs <- c("Prostar", "DAPAR", "DAPARdata")
  loc.pkgs <-c("Prostar.loc", "DAPAR.loc", "DAPARdata.loc")
  instPkgs <- list(Prostar = installed.packages(lib.loc=Prostar.loc)["Prostar","Version"],
                   DAPAR = installed.packages(lib.loc=DAPAR.loc)["DAPAR","Version"],
                   DAPARdata = installed.packages(lib.loc=DAPARdata.loc)["DAPARdata","Version"])
  
  
  names <- c(as.character(tags$a(href="http://www.bioconductor.org/packages/release/bioc/html/Prostar.html", "Prostar")), 
             as.character(tags$a(href="http://www.bioconductor.org/packages/release/bioc/html/DAPAR.html", "DAPAR")), 
             as.character(tags$a(href="http://www.bioconductor.org/packages/release/data/experiment/html/DAPARdata.html", "DAPARdata")))
  
  
  df <- data.frame("Name" = names,
                   "Installed.packages"= rep(NA, 3), 
                   "Bioc.release" =  rep(NA, 3),
                   "NeedsUpdate"= rep(FALSE,3),
                   stringsAsFactors = FALSE)
  
  
  df[, "Installed.packages"] <- unlist(instPkgs)
  
  if (!is.null(biocRelease)) {
    biocPkgs <- list(Prostar = as.character(biocRelease["Prostar","Version"]),
                     DAPAR = as.character(biocRelease["DAPAR","Version"]),
                     DAPARdata = as.character(DAPARdata.version))
    
    if (compareVersion(instPkgs$Prostar,biocPkgs$Prostar) == 0){df[1,"Name"] <-  names[1]}
    else if (compareVersion(instPkgs$Prostar,biocPkgs$Prostar) == 1){df[1,"Name"] <-   paste(names[1],  "<strong>",dev, "</strong>", sep=" ")}
    else if (compareVersion(instPkgs$Prostar,biocPkgs$Prostar)==-1){
      df[1,"Name"] <-   paste(names[1], "<strong>", outOfDate, "</strong>", sep=" ")
      inst <- unlist(strsplit(instPkgs$Prostar, split=".", fixed=TRUE))
      bioc <- unlist(strsplit(biocPkgs$Prostar, split=".", fixed=TRUE))
      df[1,"NeedsUpdate"] <- ((inst[2]==bioc[2] && (as.numeric(inst[3]) < as.numeric(bioc[3]))))
    }
    
    if (compareVersion(instPkgs$DAPAR,biocPkgs$DAPAR) == 0){df[2,"Name"] <-  names[2]}
    else if (compareVersion(instPkgs$DAPAR , biocPkgs$DAPAR) == 1){df[2,"Name"] <-   paste(names[2],  "<strong>",dev, "</strong>", sep=" ")}
    else if (compareVersion(instPkgs$DAPAR , biocPkgs$DAPAR)==-1){
      df[2,"Name"] <-   paste(names[2],  "<strong>",outOfDate, "</strong>", sep=" ")
      inst <- unlist(strsplit(instPkgs$DAPAR, split=".", fixed=TRUE))
      bioc <- unlist(strsplit(biocPkgs$DAPAR, split=".", fixed=TRUE))
      df[2,"NeedsUpdate"] <- ((inst[2]==bioc[2] && (as.numeric(inst[3]) < as.numeric(bioc[3]))))
    }
    
    if (compareVersion(instPkgs$DAPARdata,biocPkgs$DAPARdata) == 0){df[3,"Name"] <-  names[3]}
    else if (compareVersion(instPkgs$DAPARdata , biocPkgs$DAPARdata) == 1){df[3,"Name"] <-   paste(names[3],  "<strong>",dev, "</strong>", sep=" ")}
    else if (compareVersion(instPkgs$DAPARdata , biocPkgs$DAPARdata)==-1){
      df[3,"Name"] <-   paste(names[3],  "<strong>",outOfDate, "</strong>", sep=" ")
      inst <- unlist(strsplit(instPkgs$DAPARdata, split=".", fixed=TRUE))
      bioc <- unlist(strsplit(biocPkgs$DAPARdata, split=".", fixed=TRUE))
      df[3,"NeedsUpdate"] <- ((inst[2]==bioc[2] && (as.numeric(inst[3]) < as.numeric(bioc[3]))))
    }
    df[, "Bioc.release"] <- unlist(biocPkgs)
  }
  
  
  colnames(df) <- c("Names", "Installed packages", "Bioc release","NeedsUpdate")
  
  switch(type,
         all=df <- df,
         installed = {
           df <- df[,1:2]
           df[,1] <- c('Prostar', 'DAPAR', 'DAPARdata')
         }
  )
  print(df)
  
  df
  
  
  #}
  
})








ComputeAdjacencyMatrices <- function(obj){
  req(obj@experimentData@other$proteinId)
  if (obj@experimentData@other$typeOfData != 'peptide') {return (NULL)}
  
  pId <- obj@experimentData@other$proteinId
  print(pId)
  print(paste0("class of obj : ", class(obj)))
  
  matAdj <- NULL
  matSharedPeptides <- DAPAR::BuildAdjacencyMatrix(obj, pId, FALSE)
  matUniquePeptides <- DAPAR::BuildAdjacencyMatrix(obj, pId, TRUE)
  matAdj <- list(matWithSharedPeptides=matSharedPeptides, matWithUniquePeptides=matUniquePeptides)
  
  return(matAdj)
}

ComputeConnexComposants <- function(X){
  req(X)
  CC <- NULL
  CC <- list(allPep = get.pep.prot.cc(as.matrix(X$matWithSharedPeptides)),
                onlyUniquePep = get.pep.prot.cc(as.matrix(X$matWithUniquePeptides)))
  
  CC
}


###-------------------------------------

Compute_PCA_nbDimensions <- reactive({
  # ncp should not be greater than...
  nmax <- 12  
  # pour info, ncp = nombre de composantes ou de dimensions dans les r?sultats de l'ACP
  
  y <- Biobase::exprs(rv$current.obj)
  nprot <- dim(y)[1]
  # If too big, take the number of conditions.
  n <- dim(y)[2] 
  
  if (n > nmax){
    n <- length(unique(Biobase::pData(rv$current.obj)$Condition))
  }
  
  
  ncp <- min(n, nmax)
  ncp
})





GetOnlineZipVersion <- function(){
  
  thepage <- readLines('http://prabig-prostar.univ-lyon1.fr/ProstarZeroInstall/')
  substr(thepage[12], regexpr("Prostar_",thepage[12])[1], 2+regexpr("zip",thepage[12])[1])
  
  
  thetable <- readHTMLTable('http://prabig-prostar.univ-lyon1.fr/ProstarZeroInstall/', stringsAsFactors=FALSE)
  onlineZipVersion <- thetable[[1]]$Name[3]
  
  return(onlineZipVersion)
}




launchGA <- function(){
  if (system('hostname')=="prabig-prostar"){
    tags$head(includeScript("www/google-analytics.js"))
  } else {
    #tags$head(includeScript("www/google-analytics-ProstarZeroInstall.js"))
  }
  
}

