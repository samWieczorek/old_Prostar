filterGetIndices <- function(obj,
                             metacell = NULL, 
                             remove = TRUE,
                             condition = "WholeMatrix", 
                             percent = FALSE,
                             operator = NULL,
                             threshold = NULL,
                             level = NULL) {
  
  keepThat <- NULL
  
  data <- Biobase::fData(obj)[,obj@experimentData@other$names_metacell]
  
  if (condition == "WholeMatrix") {
    if (isTRUE(percent)) {
      inter <- rowSums(match.metacell(data=data, type=metacell, level=level))/ncol(data)
      keepThat <- which(eval(parse(text=paste0("inter", operator, threshold))))
    } else {
      inter <- apply(match.metacell(data=data, type=metacell, level=level), 1, sum)
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
        inter <- rowSums(match.metacell(data=data[,ind], type=metacell, level=level))/length(ind)
        s[,c] <- eval(parse(text=paste0("inter", operator, threshold)))
      }
    } else {
      for (c in 1:nbCond) {
        ind <- which(Biobase::pData(obj)$Condition == conditions[c])
        if (length(ind) == 1){
          inter <- match.metacell(data=data[,ind], type=metacell, level=level)
          s[,c] <- eval(parse(text=paste0("inter", operator, threshold)))
        }
        else {
          inter <- apply(match.metacell(data=data[,ind], type=metacell, level=level), 1, sum)
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

match.metacell <- function(data, type, level=NULL){ # utils.R
  if (!(type %in% names(metacell.def(level))))
    stop(paste0("'type' is not correct. It must be one of the following: ", paste0(names(metacell.def()), collapse = ' ')))
  
  ll.res <- lapply(unname(search.metacell.tags(type, level)), function(x){data==x})
  
  res <- NULL
  for (i in 1:length(ll.res))
    if (i==1){
      res <- ll.res[[1]]
    } else {
      res <- res | ll.res[[i]]
    }
  
  return(res)
}



search.metacell.tags <- function(pattern, level){ # utils.R
  unlist(metacell.def(level)[unlist(lapply(metacell.def(level), function(x){length(grep(pattern, x))==1}))])
}


metacell.def <- function(level = NULL){ # inOutFiles.R
  if(is.null(level))
  stop("'level' is required")
  
  switch(level,
         peptide = list( 'quanti' =           'quantiValue',
                         'direct' =           'quantiValue-direct',
                         'indirect' =         'quantiValue-indirect',
                         'undefined' =        'quantiValue-undefined',
                         'missingValue' =     'missingValue',
                         'POV' =              'missingValue-POV',
                         'MEC' =              'missingValue-MEC',
                         'imputed' =          'imputedValue',
                         'imputed_POV' =      'imputedValue-POV',
                         'imputed_MEC' =      'imputedValue-MEC') ,
         
         protein = list( 'quanti' =           'quantiValue',
                         'direct' =           'quantiValue-direct',
                         'indirect' =         'quantiValue-indirect',
                         'undefined' =        'quantiValue-undefined',
                         'missingValue' =     'missingValue',
                         'POV' =              'missingValue-POV',
                         'MEC' =              'missingValue-MEC',
                         'imputed' =          'imputedValue',
                         'imputed_POV' =      'imputedValue-POV',
                         'imputed_MEC' =      'imputedValue-MEC',
                         'combined' =         'combinedValue')
         
  )
}
