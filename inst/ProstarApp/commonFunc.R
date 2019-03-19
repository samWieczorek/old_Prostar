



GetDatasetOverview <- reactive({
  print("In GetDatasetOverview")
  print(pipeline$current.obj)
  req(pipeline$current.obj)
  
  obj <- pipeline$current.dataset[[pipeline$current.indice]]
  
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
  
  do
})


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