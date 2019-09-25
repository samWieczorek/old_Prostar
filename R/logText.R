
##' Build the text information for a new dataset
##' 
##' @title  Build the text information for a new dataset
##' @param l.params A list of parameters related to the process of the dataset
##' @return A string
##' @author Samuel Wieczorek
##' @examples
##' getTextForNewDataset(list(filename="foo.MSnset"))
getTextForNewDataset <- function(l.params){
  if (is.null(l.params) || length(l.params)==0) return(NULL)
  
    txt <- tags$ul(as.character(tags$li(paste("Open dataset: ",l.params$filename))))
    return (txt)
}


##' Build the text information for the filtering process
##' 
##' @title  Build the text information for the filtering process
##' @param l.params A list of parameters related to the process of the dataset
##' @return A string
##' @author Samuel Wieczorek
##' @examples
##' getTextForFiltering(list(mvFilterType="wholeMatrix",mvThNA=3))
getTextForFiltering <- function(l.params){ 
    # str(l.params) = list(mvFilterType ,
    #                 mvThNA,
    #                 stringFilter.df)
    
  if (is.null(l.params) || length(l.params)==0) {return(NULL)}
  
  
  txt <- "<ul>"
    
  txt <- paste(txt,"<li>Filter type: ", l.params$mvFilterType,"</li>")
  
  if (! (l.params$mvFilterType %in% c("None", "EmptyLines"))){
    txt <- paste(txt,"<li>Minimal nb of values: ", l.params$mvThNA,"</li>")
    }
  
  if (!is.null(l.params$stringFilter.df) && nrow(l.params$stringFilter.df) > 1){
        ll <- l.params$stringFilter.df$Filter
        txt <- paste(txt,"<li>Text filtering based on: ",  paste(ll[-1], collapse=", "),"</li>")
         }
  txt <- paste(txt,"</ul>" ) 
    return (txt)
    
}




##' Build the text information for the Normalization process
##' 
##' @title  Build the text information for the Normalization process
##' @param l.params A list of parameters related to the process of the dataset
##' @return A string
##' @author Samuel Wieczorek
##' @examples
##' getTextForNormalization(list(method="SumByColumns"))
getTextForNormalization <- function(l.params){ 
    
  # l.params <- list(method = input$normalization.method,
  #                  type = input$normalization.type,
  #                  varReduction = input$normalization.variance.reduction,
  #                  quantile = input$normalization.quantile,
  #                  spanLOESS = input$spanLOESS)
    
  if (is.null(l.params) || length(l.params)==0) return(NULL)
  
  
  txt <- "<ul>"
  
  txt <- paste(txt,"<li>Norm. method: ", l.params$method,"</li>")
  if (l.params$method != "GlobalQuantileAlignment"){
    txt <-  paste(txt,"<li>Application: ", l.params$type,"</li>")
  }
  
  switch(l.params$method,
    GlobalQuantileAlignment ={ },
    SumByColumns = {},
    MeanCentering ={ txt <-  paste(txt,"<li>Variance reduction: ", l.params$varReduction,"</li>")},
    QuantileCentering ={ txt <-  paste(txt,"<li>Quantile: ", l.params$quantile,"</li>")},
    LOESS ={ txt <-  paste(txt,"<li>Span: ", l.params$spanLOESS,"</li>")},
    vsn ={}
    )
  txt <- paste(txt,"</ul>")
    return (txt)
}



##' Build the text information for the peptide Imputation process
##' 
##' @title  Build the text information for the peptide Imputation process
##' @param l.params A list of parameters related to the process of the dataset
##' @return A string
##' @author Samuel Wieczorek
##' @examples
##' params <- list()
##' getTextForpeptideImputation(params)
getTextForpeptideImputation <- function(l.params){
# l.params <- list(pepLevel_algorithm = input$peptideLevel_missing.value.algorithm,
#                  pepLevel_basicAlgorithm = input$peptideLevel_missing.value.basic.algorithm,
#                  pepLevel_detQuantile = input$peptideLevel_detQuant_quantile,
#                  pepLevel_detQuant_factor = input$peptideLevel_detQuant_factor,
#                  pepLevel_imp4p_nbiter = input$peptideLevel_imp4p_nbiter,
#                  pepLevel_imp4p_withLapala = input$peptideLevel_imp4p_withLapala,
#                  pepLevel_imp4p_qmin = input$peptideLevel_imp4p_qmin,
#                  pepLevel_imp4pLAPALA_distrib = input$peptideLevel_imp4pLAPALA_distrib)

  if (is.null(l.params) || length(l.params)==0) return(NULL)
  
  
  txt <- "<ul>"

 
  if (l.params$pepLevel_algorithm == "imp4p"){
    txt <- paste(txt,"<li>Algorithm: ", l.params$pepLevel_algorithm,"</li>")
    txt <-  paste(txt,"<li>Number of iterations: ", l.params$pepLevel_imp4p_nbiter,"</li>")
    txt <-  paste(txt,"<li>MEC imputation: ", l.params$pepLevel_imp4p_withLapala,"</li>")
    if (l.params$pepLevel_imp4p_withLapala){
      txt <-  paste(txt,"<li>Upper lapala bound: ", l.params$pepLevel_imp4p_qmin,"</li>")
      txt <-  paste(txt,"<li>Distribution: ", l.params$pepLevel_imp4pLAPALA_distrib,"</li>")
    }
  } else {
  txt <-  paste(txt,"<li>Algorithm: ", l.params$pepLevel_basicAlgorithm,"</li>")
  if (l.params$pepLevel_basicAlgorithm == "detQuantile"){
    txt <-  paste(txt,"<li>Quantile: ", l.params$pepLevel_detQuantile,"</li>")
    txt <-  paste(txt,"<li>Factor: ", l.params$pepLevel_detQuant_factor,"</li>")
  }
  if (l.params$pepLevel_basicAlgorithm == "KNN"){
    txt <-  paste(txt,"<li>Nb neighnors: ", l.params$pepLevel_KNN_n,"</li>")
  }
}

txt <- paste(txt,"</ul>")
return (txt)




}

##' Build the text information for the Protein Imputation process
##' 
##' @title  Build the text information for the protein Imputation process
##' @param l.params A list of parameters related to the process of the dataset
##' @return A string
##' @author Samuel Wieczorek
##' @examples
##' params <- list()
##' getTextForproteinImputation(params)
getTextForproteinImputation <- function(l.params){ 
    
  if (is.null(l.params) || length(l.params)==0) return(NULL)
  
    ##############################################################
    
    # l.params <- list(POV_algorithm = input$POV_missing.value.algorithm,
    #                  POV_detQuant_quantile = input$POV_detQuant_quantile,
    #                  POV_detQuant_factor = input$POV_detQuant_factor,
    #                  POV_KNN_n = input$KNN_nbNeighbors,
    #                  MEC_algorithm = input$MEC_missing.value.algorithm,
    #                  MEC_detQuant_quantile = input$MEC_detQuant_quantile,
    #                  MEC_detQuant_factor = input$MEC_detQuant_factor,
    #                  MEC_fixedValue= input$MEC_fixedValue)
    
    txt <- "<ul>"
    
   
    txt <- paste(txt,"<li>POV imputation: ", l.params$POV_algorithm,"</li>")
    if (l.params$POV_algorithm == 'detQuantile'){
      txt <- paste(txt,"<li>Quantile: ", l.params$POV_detQuant_quantile,"</li>")
      txt <- paste(txt,"<li>Factor: ", l.params$POV_detQuant_factor,"</li>")
     }
    if (l.params$POV_algorithm == 'KNN'){
      txt <- paste(txt,"<li>N = ", l.params$POV_KNN_n,"</li>")
    }
    
    
    txt <- paste(txt,"<li>MEC imputation: ", l.params$MEC_algorithm,"</li>")
    if (l.params$MEC_algorithm == 'detQuantile'){
      txt <- paste(txt,"<li>Quantile: ", l.params$MEC_detQuant_quantile,"</li>")
      txt <- paste(txt,"<li>Factor: ", l.params$MEC_detQuant_factor,"</li>")
      } else if (l.params$MEC_algorithm == 'fixedValue'){
        txt <- paste(txt,"<li>Fixed value: ", l.params$MEC_fixedValue,"</li>")
        
    }
    
    txt <- paste(txt,"</ul>")
    return (txt)

}


##' Builds the text information for the Aggregation process
##' 
##' @title  Build the text information for the Aggregation process
##' @param l.params A list of parameters related to the process of the dataset
##' @return A string
##' @author Samuel Wieczorek
##' @examples
##' params <- list()
##' getTextForAggregation(params)
getTextForAggregation <- function(l.params){ 
    
  # l.params <- list(includeSharedPeptides = input$radioBtn_includeShared,
  #                  operator = input$AggregationOperator,
  #                  considerPeptides = input$AggregationConsider,
  #                  proteinId = input$proteinId,
  #                  topN = input$nTopn
    
  if (is.null(l.params) || length(l.params)==0) return(NULL)
  
    txt <- "<ul>"
    
    txt <- paste(txt,"<li>Protein IDs: ", l.params$proteinId,"</li>")
    txt <- paste(txt,"<li>Include shared peptides: ", l.params$includeSharedPeptides,"</li>")
    txt <- paste(txt,"<li>Which peptides to consider: ", l.params$considerPeptides,"</li>")
    txt <- paste(txt,"<li>Operator: ", l.params$operator,"</li>")
    
    if (l.params$considerPeptides == 'onlyN'){
      txt <- paste(txt,"<li>N (most abundant peptides) =", l.params$topN,"</li>")
    }
    txt <- paste(txt,"</ul>")
    
     return (txt)
    
}



##' Builds the text information for the hypothesis test process
##' 
##' @title  Build the text information for the hypothesis test process
##' @param l.params A list of parameters related to the process of the dataset
##' @return A string
##' @author Samuel Wieczorek
##' @examples
##' params <- list(design='OnevsOne', method='limma')
##' getTextForHypothesisTest(params)
getTextForHypothesisTest <- function(l.params){ 
  
  # l.params <- list(design = input$anaDiff_Design,
  #                  method = input$diffAnaMethod,
  #                  ttest_options = input$ttest_options,
  #                  th_logFC = input$seuilLogFC,
  #                  AllPairwiseCompNames = list(logFC = colnames(rv$res_AllPairwiseComparisons$logFC), 
  #                                              P_Value=colnames(rv$res_AllPairwiseComparisons$P_Value))
  # )
  if (is.null(l.params) || length(l.params)==0) return(NULL)
  
  txt <- "<ul>"
  txt <- paste(txt,"<li>Constrast: ", l.params$design,"</li>")
  if (l.params$method == "ttests"){
    txt <- paste(txt,"<li>Test: ", l.params$ttest_options,"</li>")
    
  } else {
    txt <- paste(txt,"<li>Test: ", l.params$method,"</li>")
  }
  
  txt <- paste(txt,"<li>logFC threshold: ", l.params$th_logFC,"</li>")
  txt <- paste(txt,"</ul>")
  
  return (txt)
  
}




##' Build the text information for the differential Analysis process
##' 
##' @title  Build the text information for the Aggregation process
##' @param l.params A list of parameters related to the process of the dataset
##' @return A string
##' @author Samuel Wieczorek
##' @examples
##' getTextForAnaDiff(list(design="OnevsOne",method="Limma"))
getTextForAnaDiff <- function(l.params){ 
  
  # param
  # Condition1
  # Condition2
  # Comparison
  # swapVolcano
  # filterType
  # filter_th_NA
  # calibMethod
  # numValCalibMethod
  # th_pval
  # FDR
  # NbSelected
    
  if (is.null(l.params) || length(l.params)==0) return(NULL)
  txt <- "<ul>"
  txt <- paste(txt,"<li>The comparison is ", gsub("_", " ",l.params$Comparison, fixed=TRUE),"</li>")
  txt <- paste(txt,"<li>The conditions are ", gsub("_", " ",l.params$Condition1, fixed=TRUE), " and ", gsub("_", " ",l.params$Condition2, fixed=TRUE), "</li>")
  
  if (!is.null(l.params$filterType) && (l.params$filterType != "None")){
    txt <- paste(txt,"<li>The filter used is ", l.params$filterType, 
                 "with min nb values / lines: ", l.params$filter_th_NA,"</li>")
  }
  
  if (!is.null(l.params$swapVolcano) ){
    txt <- paste(txt, "<li>Swap volcano: ",l.params$swapVolcano , "</li>")
  }
  
  
if (!is.null(l.params$calibMethod) ){
  if (!is.null(l.params$numValCalibMethod)){
    txt <- paste(txt, "<li>The calibration method is ", l.params$calibMethod, ", with num value = ", l.params$numValCalibMethod, "</li>")
  } else {
    txt <- paste(txt, "<li>The calibration method is ", l.params$calibMethod, "</li>")
  }
}

if (!is.null(l.params$th_pval)){
  txt <- paste(txt, "<li>The pvalue threshold is ", l.params$th_pval, "</li>")
}
  

if (!is.null(l.params$FDR)){
  txt <- paste(txt, "<li>FDR = ", l.params$FDR, "</li>")
}
  txt <- paste(txt,"</ul>")
  
  return (txt)
 
}


##' Build the text information for the Aggregation process
##' 
##' @title  Build the text information for the Aggregation process
##' @param l.params A list of parameters related to the process of the dataset
##' @return A string
##' @author Samuel Wieczorek
##' @examples
##' getTextForGOAnalysis(list())
getTextForGOAnalysis <-  function(l.params){
  
  if (is.null(l.params) || length(l.params)==0) return(NULL)
  
  
  
      if (is.null(l.params$whichGO2Save)){return(NULL)}
      switch(l.params$whichGO2Save,
           Both =
              {
                txt <- paste(txt, as.character(tags$li(paste(textGOParams,", GO grouping for level(s):",
                           input$GO_level))))
                txt <- paste(txt, as.character(tags$li(paste("GO enrichment with",
                           ", adj p-value cutoff = ", input$pvalueCutoff,
                           ", universe =", input$universe))))
              },
           Enrichment ={
             txt <- paste(txt, as.character(tags$li(paste(textGOParams, " GO enrichment with",
                           ", adj p-value cutoff = ", input$pvalueCutoff,
                           ", universe =", input$universe, sep= " "))))
              },
           Classification= {
             txt <- paste(txt, as.character(tags$li(paste(textGOParams,", GO grouping for level(s):",
                           input$GO_level,sep=" "))))
            }
  )
  }
