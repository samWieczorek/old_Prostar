
#------------------------------------------------------------------------
build_ParamsList_PepImputation <- reactive({
  
  ll <- list( pepLevel_algorithm = input$peptideLevel_missing.value.algorithm,
              pepLevel_basicAlgorithm = input$peptideLevel_missing.value.basic.algorithm,
              pepLevel_detQuantile = input$peptideLevel_detQuant_quantile,
              pepLevel_detQuant_factor = input$peptideLevel_detQuant_factor,
              pepLevel_imp4p_nbiter = input$peptideLevel_imp4p_nbiter,
              pepLevel_imp4p_withLapala = input$peptideLevel_imp4p_withLapala,
              pepLevel_imp4p_qmin = input$peptideLevel_imp4p_qmin,
              pepLevel_imp4pLAPALA_distrib = input$peptideLevel_imp4pLAPALA_distrib,
              pepLevel_KNN_n = input$KNN_n)
  ll
})


#------------------------------------------------------------------------
build_ParamsList_Filtering <- reactive({
  if (nrow(rv$widgets$filtering$DT_filterSummary) <=1) {
    df.string <- NULL
  } else {
    df.string <- rv$widgets$filtering$DT_filterSummary
    }
  
  if (nrow(rv$widgets$filtering$DT_numfilterSummary) <=1) {
    df.numeric <- NULL
  } else {
    df.numeric <- rv$widgets$filtering$DT_numfilterSummary}
  
  
  if (nrow(rv$widgets$filtering$metacell_Filter_SummaryDT) <=1) {
    df.metacell <- NULL
  } else {
    df.metacell <- rv$widgets$filtering$metacell_Filter_SummaryDT}
  
  
  l.params <- list(mvFilterType = input$ChooseFilters,
                   val_vs_percent = input$val_vs_percent,
                   mvThNA = as.numeric(input$seuilNA), 
                   mvThNA_percent = as.numeric(input$seuilNA_percent),
                   metacellFilter.df = df.metacell,
                   stringFilter.df = df.string,
                   numericFilter.df = df.numeric)
  
  l.params
})

#------------------------------------------------------------------------
build_ParamsList_Normalization <- reactive({
  l.params <- list(method = input$normalization.method,
                   type = input$normalization.type,
                   varReduction = input$normalization.variance.reduction,
                   quantile = input$normalization.quantile,
                   spanLOESS = input$spanLOESS)
  l.params
})


#------------------------------------------------------------------------
build_ParamsList_ProteinImputation <- reactive({
  l.params <- list(POV_algorithm = input$POV_missing.value.algorithm,
                   POV_detQuant_quantile = input$POV_detQuant_quantile,
                   POV_detQuant_factor = input$POV_detQuant_factor,
                   POV_KNN_n = input$KNN_nbNeighbors,
                   MEC_algorithm = input$MEC_missing.value.algorithm,
                   MEC_detQuant_quantile = input$MEC_detQuant_quantile,
                   MEC_detQuant_factor = input$MEC_detQuant_factor,
                   MEC_fixedValue= input$MEC_fixedValue)
  l.params
})


#------------------------------------------------------------------------
build_ParamsList_Aggregation <- reactive({
  l.params <- list(includeSharedPeptides = input$radioBtn_includeShared,
                   operator = input$AggregationOperator,
                   considerPeptides = input$AggregationConsider,
                   proteinId = input$proteinId,
                   topN = input$nTopn
  )
  l.params
})

#------------------------------------------------------------------------
build_ParamsList_HypothesisTest <- reactive({
  l.params <- list(design = input$anaDiff_Design,
                   method = input$diffAnaMethod,
                   ttest_options = input$ttest_options,
                   th_logFC = as.numeric(input$seuilLogFC),
                   AllPairwiseCompNames = list(logFC = colnames(rv$res_AllPairwiseComparisons$logFC), 
                                               P_Value=colnames(rv$res_AllPairwiseComparisons$P_Value))
  )
  l.params
})

#------------------------------------------------------------------------
build_ParamsList_AnaDiff <- reactive({})

