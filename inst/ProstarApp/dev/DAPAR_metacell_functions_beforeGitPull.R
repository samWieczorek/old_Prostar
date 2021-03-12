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
