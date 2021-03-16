filterGetIndices <- function(obj,
                             metacell = NULL, 
                             remove = TRUE,
                             condition = "WholeMatrix", 
                             percent = FALSE,
                             operator = NULL,
                             threshold = NULL) {
  
  keepThat <- NULL
  
  data <- Biobase::fData(obj)[,obj@experimentData@other$names_metacell]
  level <- obj@experimentData@other$typeOfData
  
  
  if (condition == "WholeMatrix") {
    if (isTRUE(percent)) {
      inter <- rowSums(match.metacell(metadata=data, pattern=metacell, level=level))/ncol(data)
      keepThat <- which(eval(parse(text=paste0("inter", operator, threshold))))
    } else {
      inter <- apply(match.metacell(metadata=data, pattern=metacell, level=level), 1, sum)
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
        inter <- rowSums(match.metacell(metadata=data[,ind], pattern=metacell, level=level))/length(ind)
        s[,c] <- eval(parse(text=paste0("inter", operator, threshold)))
      }
    } else {
      for (c in 1:nbCond) {
        ind <- which(Biobase::pData(obj)$Condition == conditions[c])
        if (length(ind) == 1){
          inter <- match.metacell(metadata=data[,ind], pattern=metacell, level=level)
          s[,c] <- eval(parse(text=paste0("inter", operator, threshold)))
        }
        else {
          inter <- apply(match.metacell(metadata=data[,ind], pattern=metacell, level=level), 1, sum)
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
  View(data[keepThat,])
  return(keepThat)
}



summary_txt <- function(metacell,
                        remove,
                        condition,
                        percent,
                        operator,
                        threshold){
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
  
  
  paste("You are going to ", text_remove, " lines where number of ", metacell,
        " data is ", text_operator, " to ", text_threshold, " in ", text_method,
        sep="")
}



match.metacell <- function(metadata, pattern, level){
  if (missing(metadata))
    stop("'metadata' is required")
  if (missing(pattern))
    stop("'pattern' is required.")
  if (missing(level))
    stop("'level' is required.")
  
  
  if (!(pattern %in% metacell.def(level)))
    stop(paste0("'pattern' is not correct. Availablevalues are: ", paste0(metacell.def(level), collapse = ' ')))
  
  ll.res <- lapply(unname(search.metacell.tags(pattern = pattern, level)), function(x){metadata==x})
  
  res <- NULL
  for (i in 1:length(ll.res))
    if (i==1){
      res <- ll.res[[1]]
    } else {
      res <- res | ll.res[[i]]
    }
  
  return(res)
}




search.metacell.tags <- function(pattern, level){
  if(missing(pattern))
    stop("'pattern' is required.")
  if(missing(level))
    stop("'level' is required.")
  
  
  unlist(metacell.def(level)[unlist(lapply(metacell.def(level), function(x){length(grep(pattern, x))==1}))])
}




metacell.def <- function(level){
  if(missing(level))
    stop("'level' is required.")
  
  switch(level,
         peptide = setNames(nm = c('quanti',
                                   'quanti_identified',
                                   'quanti_recovered',
                                   'missing',
                                   'missing_POV',
                                   'missing_MEC',
                                   'imputed',
                                   'imputed_POV',
                                   'imputed_MEC')) ,
         
         protein = setNames(nm = c('quanti',
                                   'quanti_identified',
                                   'quanti_recovered',
                                   'missing',
                                   'missing_POV',
                                   'missing_MEC',
                                   'imputed',
                                   'imputed_POV',
                                   'imputed_MEC',
                                   'combined')
         )
         
  )
}




########################################################################
obj <- Exp1_R25_pept[1:25,]
fData(obj)[,66:71]

metacell = 'quanti'
remove = FALSE
percent = FALSE
condition = "WholeMatrix" # AtLeastOneCond AllCond
threshold = 6
operator = ">="


res <- filterGetIndices(obj,
                        metacell,
                        remove,
                        condition, 
                        percent,
                        operator,
                        threshold
)

fData(obj)[,66:71]
summary_txt(metacell, remove, percent, condition, threshold, operator)
res
fData(obj)[res,66:71]