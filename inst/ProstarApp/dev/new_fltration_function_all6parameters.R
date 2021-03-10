library(DAPAR)

mvFilterGetIndices <- function(obj,
                               #classData = NULL, 
                               exclude = TRUE,
                               condition = 'WholeMatrix', 
                               percent = FALSE,
                               operator = NULL,
                               threshold = NULL){
  
  
  keepThat <- NULL
  
  data <- Biobase::exprs(obj)
  
  plop_inter <- apply(is.MV(data), 1, sum)
  ind <- which(eval(parse(text=paste0("plop_inter", operator, threshold))))
  
  
  
  #############################################################################
  
  if (condition == "WholeMatrix") {
    if (isTRUE(percent)) {
      keepThat <- which(rowSums(!is.MV(data))/ncol(data) >= threshold) 
    } else {
      keepThat <- which(apply(!is.MV(data), 1, sum) >= threshold)
    }
  } else if (condition == "AtLeastOneCond" || condition == "AllCond") {
    
    conditions <- unique(Biobase::pData(obj)$Condition)
    nbCond <- length(conditions)
    keepThat <- NULL
    s <- matrix(rep(0, nrow(data)*nbCond),
                nrow=nrow(data),
                ncol=nbCond)
    
    if (isTRUE(percent)) {
      for (c in 1:nbCond) {
        ind <- which(Biobase::pData(obj)$Condition == conditions[c])
        s[,c] <- (rowSums(!is.MV(data[,ind]))/length(ind)) >= threshold
      }
    } else {
      for (c in 1:nbCond) {
        ind <- which(Biobase::pData(obj)$Condition == conditions[c])
        if (length(ind) == 1){
          s[,c] <- (!is.MV(data[,ind]) >= threshold) 
        }
        else {
          s[,c] <- (apply(!is.MV(data[,ind]), 1, sum)) >= threshold
        }
      }
    }
    
    switch(condition,
           AllCond = keepThat <- which(rowSums(s) == nbCond),
           AtLeastOneCond = keepThat <- which(rowSums(s) >= 1)
    )
  }
  
  
  #############################################################################
  
  return(keepThat)
}


plop <- read.csv('dev/example_filtration_tab_NA.txt', sep='\t')
metadata_plop <- as.data.frame(matrix(NA, nrow=6, ncol=3))
colnames(metadata_plop) <- c("Sample.name","Condition","Bio.Rep")
metadata_plop$Sample.name <- colnames(plop)
metadata_plop$Condition <- c(rep("c1",3),rep("c2",3))
metadata_plop$Bio.Rep <- c(1:6)
obj <- DAPAR::createMSnset(file = 'dev/example_filtration_tab_NA.txt', indExpData = c(1:6), metadata = metadata_plop)

mvFilterGetIndices <- function(obj,
                               percent = FALSE,
                               condition = 'WholeMatrix', 
                               threshold = 4,
                               operator = ">")