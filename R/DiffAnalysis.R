##' This function show the density plots of Fold Change (the same as calculated by limma) for a list 
##' of the comparisons of conditions in a differnetial analysis.
##' 
##' @title Density plots of logFC values
##' @param df_logFC A dataframe that contains the logFC values
##' @param threshold_LogFC The threshold on log(Fold Change) to
##' distinguish between differential and non-differential data 
##' @param palette xxx
##' @return A highcharts density plot
##' @author Samuel Wieczorek
##' @examples
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' obj <- Exp1_R25_pept[1:1000]
##' keepThat <- mvFilterGetIndices(obj, 'wholeMatrix', ncol(obj))
##' obj <- mvFilterFromIndices(obj, keepThat)
##' qData <- Biobase::exprs(obj)
##' sTab <- Biobase::pData(obj)
##' res <- limmaCompleteTest(qData,sTab)
##' hc_logFC_DensityPlot(res$logFC)
hc_logFC_DensityPlot <-function(df_logFC, threshold_LogFC = 0, palette=NULL){
    
    if (is.null(df_logFC)){return()}
    if (threshold_LogFC < 0){return()}
  
  
  if (is.null(palette)){
    palette <- brewer.pal(ncol(df_logFC),"Paired")[1:ncol(df_logFC)]
  }else{
    if (length(palette) != ncol(df_logFC)){
      warning("The color palette has not the same dimension as the number of samples")
      return(NULL)
    }
  }
  
  nValues <- nrow(df_logFC)*ncol(df_logFC)
  nInf <- length(which(df_logFC <= -threshold_LogFC))
  nSup <- length(which(df_logFC >= threshold_LogFC))
  nInside <- length(which(abs(df_logFC) < threshold_LogFC))
  
     hc <-  highchart() %>% 
         hc_title(text = "log(FC) repartition") %>% 
         my_hc_chart(chartType = "spline", zoomType="x") %>%
         hc_legend(enabled = TRUE) %>%
       hc_colors(palette) %>%
         hc_xAxis(title = list(text = "log(FC)"),
                  plotBands = list(list(from= -threshold_LogFC, to = threshold_LogFC, color = "lightgrey")),
                  plotLines=list(list(color= "grey" , width = 2, value = 0, zIndex = 5)))%>%
        hc_yAxis(title = list(text="Density")) %>%
         hc_tooltip(headerFormat= '',
                    pointFormat = "<b> {series.name} </b>: {point.y} ",
                    valueDecimals = 2) %>%
         my_hc_ExportMenu(filename = "densityplot") %>%
         hc_plotOptions(
             series=list(
                 animation=list(duration = 100),
                 connectNulls= TRUE,
                 marker=list(enabled = FALSE)
             )
         )
     
     maxY.inf <- NULL
     maxY.inside <- NULL
     maxY.sup <- NULL
     minX <- NULL
     maxX<- NULL
     
     
    for (i in 1:ncol(df_logFC)){
        tmp <- density(df_logFC[,i])
        ind <- tmp$y[which(tmp$x <= -threshold_LogFC)]
        maxY.inf <- max(maxY.inf, ifelse(length(ind)==0,0,ind))
        maxY.inside <- max(maxY.inf, tmp$y[intersect(which(tmp$x > -threshold_LogFC),which(tmp$x < threshold_LogFC))])
        ind <- tmp$y[which(tmp$x > threshold_LogFC)]
        maxY.sup <- max(maxY.sup, ifelse(length(ind)==0,tmp$y[length(tmp$y)],ind))
        minX <- min(minX,tmp$x)
        maxX <- max(maxX,tmp$x)
        
        
        hc <- hc_add_series(hc,
                            data.frame(x = tmp$x,  y = tmp$y), 
                            name=colnames(df_logFC)[i])
    }
     
     
     if(threshold_LogFC != 0) {
      hc <- hc %>% hc_add_annotation(
       labelOptions = list(
         shape='connector',
         backgroundColor = 'lightgrey',
         #verticalAlign = 'bottom',
         align='left',
         #distance=0,
         style=list(
           fontSize= '1.5em',
           textOutline= '1px white'
         ),
         borderWidth = 0,
         x = 20
       ),
       labels = list(
         list(
           point = list(
             xAxis = 0,
             yAxis = 0,
             x = 0,
             y = maxY.inside
           ),
           text = paste0("n Filtered out = ",nInside, "<br>(", round(100*nInside/nValues, digits=2), "%)")
         )
       )
     )
}
     if (threshold_LogFC >= minX){
       hc <- hc %>%
         hc_add_annotation(
           labelOptions = list(
             shape='connector',
             backgroundColor = 'rgba(255,255,255,0.5)',
             verticalAlign = 'top',
             borderWidth = 0,
             crop=TRUE,
             style=list(
               color = 'blue',
               fontSize= '1.5em',
               textOutline= '1px white'
             ),
             y = -10
           ),
           labels = list(
             list(
               point = list(
                 xAxis = 0,
                 yAxis = 0,
                 x = mean(c(minX,-threshold_LogFC)),
                 y = maxY.inf
               ),
               text = paste0("nInf = ",nInf, "<br>(", round(100*nInf/nValues, digits=2), ")%")
             )
           )
         )
     }
     
     if (threshold_LogFC <= maxX){
       hc <- hc %>% hc_add_annotation(
         labelOptions = list(
           shape='connector',
           backgroundColor = 'blue',
           verticalAlign = 'top',
           borderWidth = 0,
           style=list(
             color = 'blue',
             fontSize= '1.5em',
             textOutline= '1px white'
           ),
           y = -5
         ),
         labels = list(
           list(
             point = list(
               xAxis = 0,
               yAxis = 0,
               x =  mean(c(maxX,threshold_LogFC)),
               y = maxY.sup
             ),
             text = paste0("nSup = ",nSup, "<br>(", round(100*nSup/nValues, digits=2), ")%")
           )
         )
       )
       
     }
     
     
     
 return(hc)
    
}






##' This function is a wrappper to the function adjust.p from the
##' cp4p package. It returns the FDR corresponding to the p-values of the 
##' differential analysis.
##' The FDR is computed with the function \code{p.adjust}\{stats\}..
##' 
##' @title Computes the FDR corresponding to the p-values of the 
##' differential analysis using 
##' @param logFC The result (logFC values) of the differential analysis processed 
##' by \code{\link{limmaCompleteTest}} 
##' @param pval The result (p-values) of the differential analysis processed 
##' by \code{\link{limmaCompleteTest}} 
##' @param threshold_PVal The threshold on p-pvalue to
##' distinguish between differential and non-differential data 
##' @param threshold_LogFC The threshold on log(Fold Change) to
##' distinguish between differential and non-differential data 
##' @param pi0Method The parameter pi0.method of the method adjust.p 
##' in the package \code{cp4p}
##' @return The computed FDR value (floating number)
##' @author Samuel Wieczorek
##' @examples
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' obj <- Exp1_R25_pept[1:1000]
##' keepThat <- mvFilterGetIndices(obj, 'wholeMatrix', ncol(obj))
##' obj <- mvFilterFromIndices(obj, keepThat)
##' qData <- Biobase::exprs(obj)
##' sTab <- Biobase::pData(obj)
##' limma <- limmaCompleteTest(qData,sTab)
##' diffAnaComputeFDR(limma$logFC[,1],limma$P_Value[,1])
diffAnaComputeFDR <- function(logFC, pval,threshold_PVal=0, threshold_LogFC = 0, 
                            pi0Method=1){
  #require(cp4p)
    if (is.null(logFC) || is.null(pval)){return()}
    
    upItems <- which(abs(logFC) >= threshold_LogFC)
    
    selectedItems <- pval[upItems]

    padj <- cp4p::adjust.p(selectedItems,  pi0Method)
    
    items <- which(-log10(padj$adjp[,1]) >= threshold_PVal)
    
    BH.fdr <- max(padj$adjp[items,2])

    return(BH.fdr)
}




##' This method returns a class \code{MSnSet} object with the results
##' of differential analysis.
##' 
##' @title Returns a \code{MSnSet} object with the results of
##' the differential analysis performed with \code{\link{limma}} package. 
##' @param obj An object of class \code{MSnSet}.
##' @param allComp A list of two items which is the result of the function wrapper.limmaCompleteTest or xxxx 
##' @param data The result of the differential analysis processed 
##' by \code{\link{limmaCompleteTest}} 
##' @param th_pval xxx
##' @param th_logFC xxx
##' @return A MSnSet
##' @author Alexia Dorffer, Samuel Wieczorek
##' @examples
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' obj <- Exp1_R25_pept
##' keepThat <- mvFilterGetIndices(obj, 'wholeMatrix', ncol(obj))
##' obj <- mvFilterFromIndices(obj, keepThat)
##' qData <- Biobase::exprs(obj)
##' sTab <- Biobase::pData(obj)
##' allComp <- limmaCompleteTest(qData,sTab)
##' data <- list(logFC=allComp$logFC[1], P_Value = allComp$P_Value[1])
##' diffAnaSave(obj, allComp, data)
diffAnaSave <- function (obj, allComp, data=NULL,th_pval=0,th_logFC=0){
    if (is.null(allComp)){
        warning("The differential analysis has not been completed. Maybe there 
            are some missing values in the dataset. If so, please impute before
            running differential analysis")
        return(NULL)}
  
  
  ####### SAVE ALL THEPAIRWISE COMPARISON RESULTS
  
  .fc <- as.data.frame(allComp$logFC)
  .pval <- as.data.frame(allComp$P_Value)
  cnames <- c(colnames(allComp$logFC), colnames(allComp$P_Value))
  ind <- which(colnames(fData(obj)) %in% cnames)
  if (length(ind) > 0) {
      Biobase::fData(obj) <- Biobase::fData(obj)[,-ind]
  }
  for (i in 1:ncol(.fc)){
    Biobase::fData(obj) <- cbind(Biobase::fData(obj), .fc[,i], .pval[,i])
    coln <- colnames(Biobase::fData(obj))
    colnames(Biobase::fData(obj))[(length(coln)-1):length(coln)] <- c(colnames(allComp$logFC)[i],colnames(allComp$P_Value)[i])
  }
  
  text <- paste("Null hypothesis test")
  obj@processingData@processing <- c(obj@processingData@processing, text)
  #Save parameters
  
  obj@experimentData@other$RawPValues <- TRUE
  
  #### SAVE A COMPARISON ANALYSIS IF EXISTS
  if (!(is.null(data$logFC) && is.null(data$P_Value))){
  
        Biobase::fData(obj)$P_Value <- data$P_Value
        Biobase::fData(obj)$logFC <- data$logFC
        Biobase::fData(obj)$Significant <- 0

        ##setSignificant info
        x <- data$logFC
        y <- -log10(data$P_Value)
    
        ipval <- which(y >= th_pval)
        ilogfc <- which(abs(x) >= th_logFC)
        Biobase::fData(obj)[intersect(ipval, ilogfc),]$Significant <- 1
   

        
        # text <- paste("Differential analysis : Selection with the following 
        #                 threshold values :logFC =",threshold_logFC,
        #                 ", -log10(p-value) = ", threshold_pVal,
        #                 ", FDR = ", fdr, sep=" ")
        # 
        # obj@processingData@processing <- c(obj@processingData@processing, text)
        # 
    }
  
  
  return(obj)
}


##' Returns a MSnSet object with only proteins significant after 
##' differential analysis.
##' 
##' @title Returns a MSnSet object with only proteins significant after 
##' differential analysis.
##' @param obj An object of class \code{MSnSet}.
##' @return A MSnSet
##' @author Alexia Dorffer
##' @examples
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' obj <- Exp1_R25_pept
##' keepThat <- mvFilterGetIndices(obj, 'wholeMatrix', ncol(obj))
##' obj <- mvFilterFromIndices(obj, keepThat)
##' qData <- Biobase::exprs(obj)
##' sTab <- Biobase::pData(obj)
##' allComp <- limmaCompleteTest(qData,sTab)
##' data <- list(logFC=allComp$logFC[1], P_Value = allComp$P_Value[1])
##' obj <- diffAnaSave(obj, allComp, data)
##' signif <- diffAnaGetSignificant(obj)
diffAnaGetSignificant <- function (obj){
    if (is.null(obj)){
        warning("The dataset contains no data")
        return(NULL)
    }
    if (!("Significant" %in% colnames(Biobase::fData(obj)))) {
        warning ("Please Set Significant data before")
        return(NULL)
    }
    temp <- obj
    signif <- which(Biobase::fData(temp)$Significant == TRUE)
    return (temp[signif,])
}



##' This function is a wrapper to the calibration.plot method of the 
##' \code{cp4p} package for use with \code{MSnSet} objects.
##'
##' @title Performs a calibration plot on an \code{MSnSet} object, 
##' calling the \code{cp4p} package functions. 
##' @param vPVal A dataframe that contains quantitative data.
##' @param pi0Method A vector of the conditions (one condition per sample).
##' @return A plot
##' @author Samuel Wieczorek
##' @examples
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' obj <- Exp1_R25_pept[1:1000]
##' keepThat <- mvFilterGetIndices(obj, 'wholeMatrix', ncol(obj))
##' obj <- mvFilterFromIndices(obj, keepThat)
##' qData <- Biobase::exprs(obj)
##' sTab <- Biobase::pData(obj)
##' limma <- limmaCompleteTest(qData,sTab)
##' wrapperCalibrationPlot(limma$P_Value[,1])
wrapperCalibrationPlot <- function(vPVal, pi0Method="pounds"){
#require(cp4p)
if (is.null(vPVal)){return(NULL)}

p <- cp4p::calibration.plot(vPVal, pi0.method=pi0Method)

return(p)
}



##' This function plots a distribution of the p-values.
##'
##' @title Distribution of p-values (histogram) 
##' @param pval_ll A list that contains the data
##' @param bins An integer that is the number of bins of the histogram.
##' @param pi0 A float that is the threshold
##' @return A plot
##' @author Samuel Wieczorek
##' @examples
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' obj <- Exp1_R25_pept
##' keepThat <- mvFilterGetIndices(obj, 'wholeMatrix', ncol(obj))
##' obj <- mvFilterFromIndices(obj, keepThat)
##' qData <- Biobase::exprs(obj)
##' sTab <- Biobase::pData(obj)
##' allComp <- limmaCompleteTest(qData,sTab)
##' histPValue_HC(allComp$P_Value[1])
histPValue_HC <- function(pval_ll, bins=80, pi0=1){
  h <- hist(sort(unlist(pval_ll)), freq=F,breaks=bins)
  
  serieInf <- sapply(h$density, function(x)min(pi0, x) )
  serieSup <- sapply(h$density, function(x)max(0, x-pi0) )
  
  hc <- highchart() %>% 
    hc_chart(type = "column") %>%
    hc_add_series(data = serieSup, name="p-value density") %>%
    hc_add_series(data = serieInf, name="p-value density") %>%
    hc_title(text = "P-value histogram") %>% 
     hc_legend(enabled = FALSE) %>%
    hc_colors(c("#C1FFC1", "red")) %>%
    hc_xAxis(title = list(text = "P-value"), categories=h$breaks)%>%
    hc_yAxis(title = list(text="Density"),
             plotLines=list(list(color= "blue" , width = 2, value = pi0, zIndex = 5))) %>%
    hc_tooltip(headerFormat= '',
               pointFormat = "<b> {series.name} </b>: {point.y} ",
               valueDecimals = 2) %>%
    my_hc_ExportMenu(filename = "histPVal") %>%
    hc_plotOptions(
        column=list(
          groupPadding= 0,
          pointPadding= 0,
          borderWidth= 0
        ),
      series=list(
        stacking = "normal",
        animation=list(duration = 100),
        connectNulls= TRUE,
        marker=list(enabled = FALSE)
      )
    ) %>%
    hc_add_annotation(
      labelOptions = list(
        backgroundColor = 'transparent',
        verticalAlign = 'top',
        y = -30,
        borderWidth = 0,
        x = 20,
        style=list(
          fontSize= '1.5em',
          color= 'blue'
        )
        
      ),
      labels = list(
        list(
          point = list(
            xAxis = 0,
            yAxis = 0,
            x = 80,
            y = pi0
          ),
          text = paste0("pi0=", pi0)
        )
      )
    )
  return(hc)
}