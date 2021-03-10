library(DAPAR)



filterGetIndices <- function(obj,
                             #classData = NULL, 
                             remove = TRUE,
                             condition = "WholeMatrix", 
                             percent = FALSE,
                             operator = NULL,
                             threshold = NULL) {
  
  keepThat <- NULL
  
  data <- (Biobase::fData(obj))[,1:6]
  # for direct, indirect, NA, imputed, unknown, DAPAR function controled.vocable (inOuFiles.R)
  
  if (condition == "WholeMatrix") {
    if (isTRUE(percent)) {
      inter <- rowSums(is.MV(data))/ncol(data)
      keepThat <- which(eval(parse(text=paste0("inter", operator, threshold))))
    } else {
      inter <- apply(is.MV(data), 1, sum)
      keepThat <- which(eval(parse(text=paste0("inter", operator, threshold))))
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
        inter <- rowSums(is.MV(data[,ind]))/length(ind)
        s[,c] <- eval(parse(text=paste0("inter", operator, threshold)))
      }
    } else {
      for (c in 1:nbCond) {
        ind <- which(Biobase::pData(obj)$Condition == conditions[c])
        if (length(ind) == 1){
          inter <- is.MV(data[,ind])
          s[,c] <- eval(parse(text=paste0("inter", operator, threshold)))
        }
        else {
          inter <- apply(is.MV(data[,ind]), 1, sum)
          s[,c] <- eval(parse(text=paste0("inter", operator, threshold)))
        }
      }
    }
    
    
    switch(condition,
           AllCond = keepThat <- which(rowSums(s) == nbCond),
           AtLeastOneCond = keepThat <- which(rowSums(s) >= 1)
    )
  }
  
  if (isTRUE(remove)) {
    keepThat <- c(1:nrow(data))[-keepThat]
  }
  
  return(keepThat)
}


summary_txt <- function(remove,
                        percent,
                        condition, 
                        threshold,
                        operator){
  switch(operator,
         '<=' = text_operator <- "inferior or equal",
         '<' = text_operator <- "inferior",
         '>=' = text_operator <- "superior or equal",
         '>' = text_operator <- "superior")
  
  switch(condition,
         "WholeMatrix" = text_method <- "all the matrix.",
         "AllCond" = text_method <- "every condition.",
         "AtLeastOneCond" = text_method <- "at least one condition.")
  
  if(isFALSE(percent)){
    text_threshold <- threshold
  } else {
    text_threshold <- paste(threshold*100,"%", sep="")
  }
  
  if(isFALSE(remove)){
    text_remove <- "keep"
  } else {
    text_remove <- "remove"
  }
  
  
  paste("You are going to ", text_remove, " lines where number of ", "NA",
        " data is ", text_operator, " to ", text_threshold, " in ", text_method,
        sep="")
}

##############################################################################

# plop <- read.csv('dev/example_filtration_tab_NA.txt', sep='\t')
# metadata_plop <- as.data.frame(matrix(NA, nrow=6, ncol=3))
# colnames(metadata_plop) <- c("Sample.name","Condition","Bio.Rep")
# metadata_plop$Sample.name <- colnames(plop)
# metadata_plop$Condition <- c(rep("c1",3),rep("c2",3))
# metadata_plop$Bio.Rep <- c(1:6)
# obj <- DAPAR::createMSnset(file = 'dev/example_filtration_tab_NA.txt', indExpData = c(1:6), metadata = metadata_plop)

plop <- read.table('dev/example_filtration_tab_mix.txt', sep='\t', h=T)
metadata_plop <- as.data.frame(matrix(NA, nrow=6, ncol=3))
colnames(metadata_plop) <- c("Sample.name","Condition","Bio.Rep")
metadata_plop$Sample.name <- colnames(plop)
metadata_plop$Condition <- c(rep("c1",3),rep("c2",3))
metadata_plop$Bio.Rep <- c(1:6)
obj <- DAPAR::createMSnset(file = 'dev/example_filtration_tab_mix.txt', indExpData = c(1:6), metadata = metadata_plop)


##############################################################################

remove = FALSE
percent = FALSE
condition = "WholeMatrix"
# condition = "AtLeastOneCond"
# condition = "AllCond"
threshold = 2
operator = ">="


res <- filterGetIndices(obj,
                        remove,
                        condition, 
                        percent,
                        operator,
                        threshold
)

##############################################################################

plop
summary_txt(remove, percent, condition, threshold, operator)
res