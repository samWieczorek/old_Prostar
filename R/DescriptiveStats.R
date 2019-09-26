


##' Compute the PCA
##' 
##' @title Compute the PCA
##' @param obj xxx
##' @param var.scaling The dimensions to plot
##' @param ncp xxxx
##' @return A xxxxxx
##' @author Samuel Wieczorek
##' @examples
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' res.pca <- wrapper.pca(Exp1_R25_pept)
wrapper.pca <- function(obj, var.scaling=TRUE, ncp=NULL){
 # require(FactoMineR)
  
  if (is.null(var.scaling)) {var.scaling <- TRUE}
  if (length(which(is.na(Biobase::exprs(obj)))) > 0){return(NULL)}
  if (is.null(ncp)){
    nmax <- 12
    y <- Biobase::exprs(obj)
    nprot <- dim(y)[1]
    n <- dim(y)[2] # If too big, take the number of conditions.
    
    if (n > nmax){
      n <- length(unique(Biobase::pData(obj)$Condition))
    }
    
    
    ncp <- min(n, nmax)
  }
  # parameters available to the user
  variance.scaling <- TRUE
  
  res.pca <- FactoMineR::PCA(exprs(obj), scale.unit = var.scaling, ncp=ncp, graph=FALSE)
  # si warning pour les missing values, le reproduire dans l'interface graphique
  
  return(res.pca)
}



##' Plots the variables of PCA
##' 
##' @title Plots variables of PCA
##' @param res.pca xxx
##' @param chosen.axes The dimensions to plot
##' @return A plot
##' @author Samuel Wieczorek
##' @examples
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' res.pca <- wrapper.pca(Exp1_R25_pept)
##' plotPCA_Var(res.pca)
plotPCA_Var <- function(res.pca, chosen.axes=c(1,2)){
  #plot.PCA(res.pca, choix="var", axes = chosen.axes, title="Sample factor map (PCA)")
  #require(factoextra)
  # Colorer en fonction du cos2: qualit? de repr?sentation
  if (is.null(res.pca)){return(NULL)}
  factoextra::fviz_pca_var(res.pca, axes = chosen.axes, col.var = "cos2",
               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
               repel = TRUE # ?vite le chevauchement de texte
  )
  
}


##' Plots the individuals of PCA
##' 
##' @title Plots individuals of PCA
##' @param res.pca xxx
##' @param chosen.axes The dimensions to plot
##' @return A plot
##' @author Samuel Wieczorek
##' @examples
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' res.pca <- wrapper.pca(Exp1_R25_pept)
##' plotPCA_Ind(res.pca)
plotPCA_Ind <- function(res.pca, chosen.axes=c(1,2)){
  #plot.PCA(res.pca, choix="ind", axes = chosen.axes, select = 0:-1, title="Protein factor map (PCA)")
  if (is.null(res.pca)){return(NULL)}
  #require(factoextra)
  factoextra::fviz_pca_ind(res.pca,  axes = chosen.axes, geom="point")
  
  }


##' Plots the eigen values of PCA
##' 
##' @title Plots the eigen values of PCA
##' @param res.pca xxx
##' @return A histogram
##' @author Samuel Wieczorek
##' @examples
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' res.pca <- wrapper.pca(Exp1_R25_pept, ncp=6)
##' plotPCA_Eigen(res.pca)
plotPCA_Eigen <- function(res.pca){
  if (is.null(res.pca)){return(NULL)}
  eig.val <- res.pca$eig
  barplot(eig.val[, 2], 
          names.arg = 1:nrow(eig.val), 
          main = "Variances Explained by PCs (%)",
          xlab = "Principal Components",
          ylab = "Percentage of variances",
          col ="steelblue")
  # Add connected line segments to the plot
  lines(x = 1:nrow(eig.val), eig.val[, 2], 
        type = "b", pch = 19, col = "red")
  
}


##' Plots the eigen values of PCA with the highcharts library
##' 
##' @title Plots the eigen values of PCA with the highcharts library
##' @param res.pca xxx
##' @return A histogram
##' @author Samuel Wieczorek
##' @examples
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' res.pca <- wrapper.pca(Exp1_R25_pept, ncp=6)
##' plotPCA_Eigen_hc(res.pca)
plotPCA_Eigen_hc <- function(res.pca){
  if (is.null(res.pca)){return(NULL)}
  hc <- highchart() %>%
    hc_yAxis_multiples(list(title = list(text = "% of variances"),lineWidth = 0, labels = list(format = "{value}%"), max = 100), 
                       list(title = list(text = "Cumulative % of variances"), opposite = FALSE, max = 100),
                       list(title = list(text = "Eigen values"), opposite = TRUE, labels = list(format = "{value}%") )) %>%
    hc_xAxis(title = "Principal Components", categories = rownames(res.pca$eig)) %>%
    hc_add_series(data.frame(y=res.pca$eig[,2]), type="column",  name = "% of variances", yAxis = 0) %>%
    hc_add_series(data.frame(y=res.pca$eig[,3]), type="line",  color="darkblue",name = "Cumulative % of variances", marker = "diamond", color = "#FF7900", yAxis = 0) %>%
    #hc_add_series(data.frame(y=res.pca$eig[,1]),  type="line",  lineWidth = 4,name = "Eigen values", color = "orange", yAxis = 2) %>%
    #hc_tooltip(crosshairs = TRUE, headerFormat = "<b>{point.x}</b><br>") %>%
    hc_legend(enabled = TRUE)
   # hc_plotOptions(column = list(colorByPoint = TRUE, colors = SiteOTD$Colors))
  
  
}

##' Boxplot for quantitative proteomics data
##' 
##' @title Builds a boxplot from a dataframe
##' @param obj xxx
##' @param legend A vector of the conditions (one string per sample).
##' @param palette xxx
##' @return A boxplot
##' @author Florence Combes, Samuel Wieczorek
##' @seealso \code{\link{densityPlotD}}
##' @examples
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' conds <- Biobase::pData(Exp1_R25_pept)[,"Condition"]
##' boxPlotD(Exp1_R25_pept)
boxPlotD <- function(obj,legend=NULL,palette=NULL){
  qData <- Biobase::exprs(obj)
  if (is.null(palette)){
    pal <- brewer.pal(length(unique(conds)),"Dark2")[1:length(unique(conds))]
    
    for (i in 1:ncol(qData)){
      palette[i] <- pal[ which(conds[i] == unique(conds))]
    }
    
  }else{
    if (length(palette) != ncol(qData)){
      warning("The color palette has not the same dimension as the number of samples")
      return(NULL)
    }
  }

        
boxplot(qData
        ,las = 1
        , col = palette
        , cex = 2
        , axes=TRUE
        , xaxt = "n"
        , ylab = "Log (intensity)"
        , pt.cex = 4
        , horizontal = FALSE
)


if( is.null(legend)){legend <- Biobase::pData(obj)$Condition}
axis(side=1,at = 1:ncol(qData), label = legend)
#mtext("Samples", side=1,  line=(6+ncol(legend)), cex.lab=1, las=1)

abline(h=0) 

}




##' Boxplot for quantitative proteomics data using the library \code{highcharter}
##' 
##' @title Builds a boxplot from a dataframe using the library \code{highcharter}
##' @param obj xxx
##' @param legend A vector of the conditions (one condition per sample).
##' @param palette xxx
##' @param subset.view A vector of index indicating rows to highlight
##' @return A boxplot
##' @author Samuel Wieczorek, Anais Courtier
##' @seealso \code{\link{densityPlotD_HC}}
##' @examples
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' legend <- Biobase::pData(Exp1_R25_pept)[,"Sample.name"]
##' boxPlotD_HC(Exp1_R25_pept, legend, subset.view=1:10)
boxPlotD_HC <- function(obj, legend=NULL, palette = NULL,subset.view=NULL){

  qData <- Biobase::exprs(obj)
  if( is.null(legend)){legend <- Biobase::pData(obj)[,"Sample.name"]}
  
  if (is.null(palette)){palette <- rep("#FFFFFF", ncol(qData))
  } else {
    if (length(palette) != ncol(qData)){
      warning("The color palette has not the same dimension as the number of samples")
      return(NULL)
    }
  }
  
  bx <-boxplot(qData, na.rm=TRUE)
  df_outlier <- data.frame(x=bx$group-1,y = bx$out)
  
  #tmp <- NULL
  #for (i in 1:ncol(qData)){
  #  tmp <- c(tmp, rep(paste("S",i,legend[i], collapse="_"),nrow(qData)))
  #}
  tmp <- NULL
  for (i in 1:ncol(qData)){
    tmp <- c(tmp, rep(paste(paste0(rep("A", i), collapse=""),legend[i], sep='_'),nrow(qData)))
  }
  
  df <- data.frame(values = as.vector(qData,mode='numeric'),samples = tmp, stringsAsFactors = FALSE)
  
  
  hc <- hcboxplot(x=df$values, var = df$samples, colorByPoint = TRUE, outliers = TRUE) %>%
    hc_chart(type="column") %>%
    hc_yAxis(title = list(text = "Log (intensity)")) %>%
    hc_xAxis(title = list(text = "Samples"), categories=legend) %>%
    hc_colors(palette) %>%
    hc_add_series(type= "scatter",df_outlier,name="Outliers",tooltip=list(enabled=F,headerFormat ="",pointFormat="{point.y: .2f} ")) %>%  
    hc_plotOptions(
      
      boxplot= list(
        
        fillColor= c('lightgrey'),
        lineWidth= 3,
        medianColor= 'grey',
        medianWidth= 3,
        stemColor= '#A63400',
        stemDashStyle= 'dot',
        stemWidth= 1,
        whiskerColor= '#3D9200',
        whiskerLength= '20%',
        whiskerWidth= 3
      ),
      scatter = list(
        marker=list(
          fillColor = 'white',
          lineWidth = 0.5,
          lineColor = 'grey'
        )
      )
    )
  
  # Display of rows to highlight (index of row in subset.view) 
  if(!is.null(subset.view)){
    idColName<-obj@experimentData@other$proteinId
    idVector=obj@featureData@data[,idColName]
    pal=colorRampPalette(brewer.pal(8, "Set1"))(length(subset.view))    
    n=0
    for(i in subset.view){
      n=n+1
      dfSubset <- data.frame(y = as.vector(qData[i,],mode='numeric'), x = as.numeric(factor(names(qData[i,])))-1, stringsAsFactors = FALSE)
      hc<-hc %>%
        hc_add_series(type= "line",data=dfSubset,color=pal[n], dashStyle = "shortdot",name=idVector[i],
                      tooltip=list(enabled=T,headerFormat ="",pointFormat="{point.series.name} : {point.y: .2f} ") )
      
    }
  }  

  hc
  
}




##' ViolinPlot for quantitative proteomics data
##' 
##' @title Builds a violinplot from a dataframe
##' @param obj xxx
##' @param legend A vector of the conditions (one condition per sample).
##' @param palette xxx
##' @param subset.view A vector of index indicating rows to highlight
##' @return A violinplot
##' @author Samuel Wieczorek, Anais Courtier
##' @seealso \code{\link{densityPlotD}}
##' @examples
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' library(vioplot)
##' legend <- Biobase::pData(Exp1_R25_pept)[,"Condition"]
##' violinPlotD(Exp1_R25_pept, legend=legend,subset.view=20:30)
violinPlotD <- function(obj, legend=NULL, palette = NULL,subset.view=NULL){
  plot.new()
  qData <- Biobase::exprs(obj)
  
    if (!is.null(palette)) {
      if (length(palette) != ncol(qData)){
        warning("The color palette has not the same dimension as the number of samples")
      return(NULL)
      }
    } else {
      palette <- rep('#FFFFFF',ncol(qData))
    }
 
  
    plot.window(xlim=c(0,ncol(qData)+1),
                ylim=c(min(na.omit(qData)),max(na.omit(qData))))
    title( ylab="Log (intensity)")
    
    for (i in 1:ncol(qData)) {
        vioplot(na.omit(qData[,i]), col = palette[i], add=TRUE, at=i)}
    
    
    axis(2, yaxp = c(floor(min(na.omit(qData))), 
                     floor(max(na.omit(qData))), 5), las=1)
    
     if( !is.null(legend))
     {
        if (is.vector(legend) ){
            N <- 1} else{ N <- ncol(legend)}
         
        for (i in 1:N){
            axis(side=1,
                 at = 1:ncol(qData),
                 label = if (is.vector(legend) ) 
                     {legend} else {legend[,i]},
                 line= 2*i-1
            )
        }

        mtext("Samples",side=1,line=6+length(colnames(legend)), cex.lab=1, las=1)
    }
  # Display of rows to highlight (index of row in subset.view) 
  if(!is.null(subset.view)){
    idColName<-obj@experimentData@other$proteinId
    idVector=obj@featureData@data[,idColName]
    pal=colorRampPalette(brewer.pal(8, "Set1"))(length(subset.view))
    
    n=0
    for (i in subset.view) {
      n=n+1
      for (c in 1:(ncol(qData)-1)) {
        segments(y0=qData[i,c],y1=qData[i,c+1],x0=c,x1=c+1,pch=16,col=pal[n],lwd=2)
        points(y=qData[i,c],x=c,pch=16,col=pal[n])
      }
      points(y=qData[i,ncol(qData)],x=ncol(qData),pch=16,col=pal[n])
    }
    legend("topleft",legend=idVector[subset.view],lty=1,lwd=2,col=pal,pch=16,bg="transparent",bty="n")
    }

}



##' Wrapper to the function that plot to compare the quantitative proteomics 
##' data before and after normalization
##' 
##' @title Builds a plot from a dataframe
##' @param objBefore A dataframe that contains quantitative data before 
##' normalization.
##' @param objAfter A dataframe that contains quantitative data after 
##' normalization.
##' @param condsForLegend A vector of the conditions (one condition per sample).
##' @param indData2Show A vector of the indices of the columns to show in the 
##' plot. The indices are those of indices of 
##' the columns int the data.frame qDataBefore.
##' @param ... arguments for palette
##' @return A plot
##' @author Samuel Wieczorek
##' @examples
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' conds <- Biobase::pData(Exp1_R25_pept)[,"Condition"]
##' objAfter <- wrapper.normalizeD(Exp1_R25_pept, "QuantileCentering","within conditions")
##' wrapper.compareNormalizationD(Exp1_R25_pept, objAfter, conds)
wrapper.compareNormalizationD <- function(objBefore, objAfter, 
                                        condsForLegend=NULL,
                                        indData2Show=NULL,
                                        ...){

qDataBefore <- Biobase::exprs(objBefore)
qDataAfter <- Biobase::exprs(objAfter)
if (is.null(condsForLegend)){
  condsForLegend <- Biobase::pData(objBefore)[,"Condition"]}

compareNormalizationD(qDataBefore, qDataAfter, condsForLegend, indData2Show, ...)
}

##' Wrapper to the function that plot to compare the quantitative proteomics 
##' data before and after normalization. Same as the function \link{wrapper.compareNormalizationD}
##' but uses the package \code{highcharter}
##' 
##' @title Builds a plot from a dataframe
##' @param objBefore A dataframe that contains quantitative data before 
##' normalization.
##' @param objAfter A dataframe that contains quantitative data after 
##' normalization.
##' @param condsForLegend A vector of the conditions (one condition 
##' per sample).
##' @param indData2Show A vector of the indices of the columns to show in the 
##' plot. The indices are those of indices of 
##' the columns int the data.frame qDataBefore.
##' @param ... arguments for palette
##' @return A plot
##' @author Samuel Wieczorek
##' @examples
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' conds <- Biobase::pData(Exp1_R25_pept)[,"Condition"]
##' objAfter <- wrapper.normalizeD(Exp1_R25_pept, "QuantileCentering", 
##' "within conditions")
##' wrapper.compareNormalizationD_HC(Exp1_R25_pept, objAfter, conds)
wrapper.compareNormalizationD_HC <- function(objBefore, objAfter, 
                                          condsForLegend=NULL,
                                          indData2Show=NULL,
                                           ...){
    
    qDataBefore <- Biobase::exprs(objBefore)
    qDataAfter <- Biobase::exprs(objAfter)
    
    compareNormalizationD_HC(qDataBefore, qDataAfter, condsForLegend, indData2Show,...)
}

##' Plot to compare the quantitative proteomics data before and after 
##' normalization
##' 
##' @title Builds a plot from a dataframe
##' @param qDataBefore A dataframe that contains quantitative data before 
##' normalization.
##' @param qDataAfter A dataframe that contains quantitative data after 
##' normalization.
##' @param condsForLegend A vector of the conditions (one condition 
##' per sample).
##' @param indData2Show A vector of the indices of the columns to show in 
##' the plot. The indices are those of indices of 
##' the columns int the data.frame qDataBefore.
##' @param palette xxx
##' @return A plot
##' @author Samuel Wieczorek
##' @examples
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' qDataBefore <- Biobase::exprs(Exp1_R25_pept)
##' conds <- Biobase::pData(Exp1_R25_pept)[,"Condition"]
##' objAfter <- wrapper.normalizeD(Exp1_R25_pept,"QuantileCentering","within conditions")
##' compareNormalizationD(qDataBefore, Biobase::exprs(objAfter), conds)
compareNormalizationD <- function(qDataBefore,
                                qDataAfter,
                                condsForLegend=NULL,
                                indData2Show=NULL,
                                palette = NULL){

if (is.null(condsForLegend)) return(NULL)
if (is.null(indData2Show)) {indData2Show <- c(1:ncol(qDataAfter)) }

  if (is.null(palette)){
    tmp <- brewer.pal(length(unique(condsForLegend)),"Dark2")[1:length(unique(condsForLegend))]
    
    for (i in 1:ncol(qDataBefore)){
      palette[i] <- tmp[ which(condsForLegend[i] == unique(condsForLegend))]
    }
    
  }else{
    if (length(palette) != ncol(qDataBefore)){
      warning("The color palette has not the same dimension as the number of samples")
      return(NULL)
    }
  }
  
x <- qDataBefore
y <- qDataAfter/qDataBefore

lim.x <- range(min(x, na.rm=TRUE), max(x, na.rm=TRUE))
lim.y <- range(min(y, na.rm=TRUE), max(y, na.rm=TRUE))


##Colors definition
    legendColor <- unique(palette)
    txtLegend <- unique(condsForLegend)



plot(x=NULL
    ,xlim = lim.x
    ,ylim = lim.y
    , cex = 1
    , axes=TRUE
    , xlab = "Intensities before normalization"
    , ylab = "Intensities after normalization / Intensities before 
    normalization"
    ,cex.lab = 1
    ,cex.axis = 1
    ,cex.main = 3)


for (i in indData2Show){
    points(x[,i], y[,i], col = palette[i], cex = 1,pch=16)
}

legend("topleft"
        , legend = txtLegend
        , col = legendColor
        , pch = 15 
        , bty = "n"
        , pt.cex = 2
        , cex = 1
        , horiz = FALSE
        , inset=c(0,0)
)


}





##' Wrapper to the function that plot to compare the quantitative proteomics 
##' data before and after normalization
##' 
##' @title Builds a plot from a dataframe
##' @param objBefore A dataframe that contains quantitative data before 
##' normalization.
##' @param objAfter A dataframe that contains quantitative data after 
##' normalization.
##' @param condsForLegend A vector of the conditions (one condition per sample).
##' @param indData2Show A vector of the indices of the columns to show in the 
##' plot. The indices are those of indices of 
##' the columns int the data.frame qDataBefore.
##' @param ... arguments for palette
##' @return A plot
##' @author Samuel Wieczorek
##' @examples
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' conds <- Biobase::pData(Exp1_R25_pept)[,"Condition"]
##' objAfter <- wrapper.normalizeD(Exp1_R25_pept, "QuantileCentering","within conditions")
##' ids <- fData(Exp1_R25_pept)[,Exp1_R25_pept@experimentData@other$proteinId]
##' wrapper.compareNormalizationDSubset(Exp1_R25_pept, objAfter, conds, idsForLegend=ids, subset.view=1:10)
wrapper.compareNormalizationDSubset <- function(objBefore, objAfter, 
                                          condsForLegend=NULL,
                                          indData2Show=NULL,
                                          ...){
  
  qDataBefore <- Biobase::exprs(objBefore)
  qDataAfter <- Biobase::exprs(objAfter)
  #if (is.null(condsForLegend)){
    #condsForLegend <- Biobase::pData(objBefore)[,"Condition"]}
  
  compareNormalizationDSubset(qDataBefore, qDataAfter, indData2Show, ...)
}





##' Plot to compare the quantitative proteomics data before and after 
##' normalization for a subset of protein
##' 
##' @title Builds a plot from a dataframe
##' @param qDataBefore A dataframe that contains quantitative data before 
##' normalization.
##' @param qDataAfter A dataframe that contains quantitative data after 
##' normalization.
##' @param indData2Show A vector of the indices of the columns to show in 
##' the plot. The indices are those of indices of 
##' the columns int the data.frame qDataBefore.
##' @param idsForLegend A vector of the ids of the row in the data.frame qDataBefore.
##' @param palette xxx
##' @param subset.view A vector of index indicating rows to highlight
##' @return A plot
##' @author Samuel Wieczorek, Anais Courtier
##' @examples
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' qDataBefore <- Biobase::exprs(Exp1_R25_pept)
##' ids <-Exp1_R25_pept@featureData@data[,obj@experimentData@other$proteinId]
##' objAfter <- wrapper.normalizeD(Exp1_R25_pept,"QuantileCentering","within conditions")
##' compareNormalizationDSubset(qDataBefore, Biobase::exprs(objAfter), idsForLegend=ids,subset.view=1:10)
compareNormalizationDSubset <- function(qDataBefore,
                                  qDataAfter,
                                  indData2Show=NULL, idsForLegend=NULL,
                                  palette = NULL,subset.view=NULL){
  
  
  if (is.null(idsForLegend)) return(NULL)
  if (is.null(indData2Show)) {indData2Show <- c(1:ncol(qDataAfter)) }
  
  if (is.null(palette)){
    palette <- colorRampPalette(brewer.pal(8, "Set1"))(length(subset.view))
    
  }else{
    if (length(palette) != length(subset.view)){
      warning("The color palette has not the same dimension as the number of proteins")
      return(NULL)
    }
  }
  
  x <- qDataBefore
  y <- qDataAfter/qDataBefore
  
  lim.x <- range(min(x, na.rm=TRUE), max(x, na.rm=TRUE))
  lim.y <- range(min(y, na.rm=TRUE), max(y, na.rm=TRUE))
  
  
  ##Colors definition
  legendColor <- palette
  txtLegend <- idsForLegend[subset.view]
  
  
  plot(x=NULL
       ,xlim = lim.x
       ,ylim = lim.y
       , cex = 1
       , axes=TRUE
       , xlab = "Intensities before normalization"
       , ylab = "Intensities after normalization / Intensities before 
       normalization"
       ,cex.lab = 1
       ,cex.axis = 1
       ,cex.main = 3)
  
  
  for (i in indData2Show){
    points(x[subset.view,i], y[subset.view,i], col = palette, cex = 1,pch=16)
  }
  
  legend("topleft"
         , legend = txtLegend
         , col = legendColor
         , pch = 15 
         , bty = "n"
         , pt.cex = 2
         , cex = 1
         , horiz = FALSE
         , inset=c(0,0)
  )

}









##' Plot to compare the quantitative proteomics data before and after 
##' normalization using the library \code{highcharter}
##' 
##' @title Builds a plot from a dataframe. Same as compareNormalizationD but 
##' uses the library \code{highcharter}
##' @param qDataBefore A dataframe that contains quantitative data before 
##' normalization.
##' @param qDataAfter A dataframe that contains quantitative data after 
##' normalization.
##' @param condsForLegend A vector of the conditions (one condition 
##' per sample).
##' @param indData2Show A vector of the indices of the columns to show in 
##' the plot. The indices are those of indices of 
##' the columns int the data.frame qDataBefore.
##' @param palette xxx
##' @return A plot
##' @author Samuel Wieczorek
##' @examples
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' obj <- Exp1_R25_pept[1:1000]
##' qDataBefore <- Biobase::exprs(obj)
##' conds <- Biobase::pData(obj)[,"Condition"]
##' objAfter <- wrapper.normalizeD(obj,"QuantileCentering","within conditions")
##' compareNormalizationD_HC(qDataBefore, Biobase::exprs(objAfter), conds)
compareNormalizationD_HC <- function(qDataBefore,
                                  qDataAfter,
                                  condsForLegend=NULL,
                                  indData2Show=NULL,
                                  palette = NULL){
    
    if (is.null(condsForLegend)) return(NULL)
    if (is.null(indData2Show)) {indData2Show <- c(1:ncol(qDataAfter)) }
    
  
  if (is.null(palette)){
    tmp <- brewer.pal(length(unique(condsForLegend)),"Dark2")[1:length(unique(condsForLegend))]
    
    for (i in 1:ncol(qDataBefore)){
      palette[i] <- tmp[ which(condsForLegend[i] == unique(condsForLegend))]
    }
    
  }else{
    if (length(palette) != ncol(qDataBefore)){
      warning("The color palette has not the same dimension as the number of samples")
      return(NULL)
    }
  }
  
    x <- qDataBefore
    y <- qDataAfter/qDataBefore
   
    ##Colors definition
        legendColor <- unique(palette)
        txtLegend <- unique(condsForLegend)
    
    
    series <- list()
    for (i in 1:length(indData2Show)){
        tmp <- list(name=condsForLegend[i], data =list_parse(data.frame(x=x[,indData2Show[i]],
                                                                        y=y[,indData2Show[i]])))
        series[[i]] <- tmp
    }
   
    h1 <-  highchart2() %>% 
        my_hc_chart( chartType = "scatter") %>%
        hc_add_series_list(series) %>%
        hc_tooltip(enabled= "false" ) %>%
        my_hc_ExportMenu(filename = "compareNormalization")
    h1
    return(h1)

}


##' Densityplot of quantitative proteomics data over samples.
##' 
##' @title Builds a densityplot from a dataframe
##' @param obj xxx
##' @param legend A vector of the conditions (one condition per sample).
##' @param palette xxx
##' @return A density plot
##' @author Florence Combes, Samuel Wieczorek
##' @seealso \code{\link{boxPlotD}}, \code{\link{CVDistD}}
##' @examples 
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' conds <- Biobase::pData(Exp1_R25_pept)[,"Condition"]
##' densityPlotD(Exp1_R25_pept, conds)
densityPlotD <- function(obj, legend=NULL,palette = NULL){
    
  qData <- Biobase::exprs(obj)
  
  if (is.null(legend) ) { legend <- Biobase::pData(obj)[,"Condition"]}
  
  if (is.null(palette)){
    palette <- brewer.pal(length(unique(conds)),"Dark2")
  }else{
    if (length(palette) != ncol(qData)){
      warning("The color palette has not the same dimension as the number of samples")
      return(NULL)
    }
  }
  

### Range of axis definition
axis.limits <- matrix(data = 0, nrow = 4, ncol = ncol(qData))
for (i in 1:ncol(qData)){
    dens <- density(qData[,i], na.rm = TRUE)
    axis.limits[,i] <- c(min(dens$x), max(dens$x), min(dens$y), max(dens$y))
    }
lim.x <- range(min(axis.limits[1,]), max(axis.limits[2,]))
lim.y <- range(min(axis.limits[3,]), max(axis.limits[4,]))



plot(x =NULL
    , ylab ="Density"
    , xlab = "log(intensity)"
    , col = palette
    ,xlim = lim.x
    ,ylim = lim.y
    ,las = 1
    ,cex.lab = 1
    ,cex.axis = 1
    ,cex.main = 3)

for (i in ncol(qData)){
    lines(density(qData[,i], na.rm=TRUE), col = palette[i])
}


legend("topleft"         
        , legend = unique(legend)
        , col = unique(palette)
        , pch = 15 
        , bty = "n"
        , pt.cex = 2
        , cex = 1
        , horiz = FALSE
        , inset=c(0,0)
)
}




##' Densityplot of quantitative proteomics data over samples. Same as the function \code{\link{densityPlotD}}
##' but uses the package \code{highcharter}
##' 
##' @title Builds a densityplot from a dataframe
##' @param obj xxx
##' @param legend A vector of the conditions (one condition 
##' per sample).
##' @param palette xxx
##' @return A density plot
##' @author Samuel Wieczorek
##' @seealso \code{\link{boxPlotD}}, \code{\link{CVDistD}}
##' @examples 
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' densityPlotD_HC(Exp1_R25_pept)
densityPlotD_HC <- function(obj, legend=NULL, palette = NULL){
  
  qData <- Biobase::exprs(obj)
  
  if (is.null(legend) ) { legend<- Biobase::pData(obj)[,"Condition"]}
  
  # if (is.null(palette)){
  #   nbConds <- length(unique(condsForLegend))
  #   palette <- brewer.pal(nbConds, "Dark2")[1:nbConds]
  #    temp <- NULL
  #   for (i in 1:ncol(qData)){
  #     temp[i] <- palette[ which(condsForLegend[i] == unique(condsForLegend))]
  #   }
  #   palette <- temp
  #   
  # }else{
   
    
    h1 <-  highchart() %>% 
        hc_title(text = "Density plot") %>% 
        my_hc_chart(chartType = "spline", zoomType="x") %>%
         hc_legend(enabled = TRUE) %>%
        hc_xAxis(title = list(text = "log(Intensity)")) %>%
        hc_yAxis(title = list(text = "Density")) %>%
       hc_tooltip(headerFormat= '',
                   pointFormat = "<b> {series.name} </b>: {point.y} ",
                   valueDecimals = 2) %>%
      my_hc_ExportMenu(filename = "densityplot") %>%
        hc_plotOptions(
            series=list(
                animation=list(
                    duration = 100
                ),
                connectNulls= TRUE,
                marker=list(
                  enabled = FALSE)
            )
        )
    
    if (!is.null(palette)) {
      if (length(palette) != ncol(qData)){
        warning("The color palette has not the same dimension as the number of samples")
        return(NULL)
      }
      h1 <- h1 %>% hc_colors(palette)
    }
    
    if (is.null(legend)) {
      legend <- paste0("series", 1:ncol(qData))
    }
    
   for (i in 1:ncol(qData)){
      
      tmp <- data.frame(x = density(qData[,i], na.rm = TRUE)$x, 
                        y = density(qData[,i], na.rm = TRUE)$y)
      
      h1 <- h1 %>% hc_add_series(data=list_parse(tmp), name=legend[i]) 
    
    }
    
    
    return(h1)

}




##' Builds a densityplot of the CV of entities in the exprs() table
##' of an object \code{MSnSet}. The variance is calculated for each 
##' condition present
##' in the dataset (see the slot \code{'Condition'} in the \code{pData()} table).
##' 
##' @title Distribution of CV of entities
##' @param obj An object of class \code{MSnSet}.
##' @param ... arguments for palette
##' @return A density plot
##' @author Alexia Dorffer
##' @examples
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' wrapper.CVDistD(Exp1_R25_pept)
wrapper.CVDistD <- function(obj, ...){
qData <- Biobase::exprs(obj)
conds <- Biobase::pData(obj)[,"Condition"]
CVDistD(qData, conds, ...)
}


##' Builds a densityplot of the CV of entities in the exprs() table. 
##' of an object \code{MSnSet}. The variance is calculated for each 
##' condition present
##' in the dataset (see the slot \code{'Condition'} in the \code{pData()} table).
##' Same as the function \code{\link{wrapper.CVDistD}} but uses the package \code{highcharter}
##' 
##' @title Distribution of CV of entities
##' @param obj An object of class \code{MSnSet}
##' @param ... arguments for palette.
##' @return A density plot
##' @author Samuel Wieczorek
##' @examples
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' wrapper.CVDistD_HC(Exp1_R25_pept)
wrapper.CVDistD_HC <- function(obj, ...){
    qData <- Biobase::exprs(obj)
    conds <- Biobase::pData(obj)[,"Condition"]
    CVDistD_HC(qData, conds, ...)
}


##' Builds a densityplot of the CV of entities in the exprs() table
##' of a object. The CV is calculated for each condition present
##' in the dataset (see the slot \code{'Condition'} in the \code{pData()} table)
##' 
##' @title Distribution of CV of entities
##' @param qData A dataframe that contains quantitative data.
##' @param conds A vector of the conditions (one condition per sample).
##' @param palette xxx
##' @return A density plot
##' @author Florence Combes, Samuel Wieczorek
##' @seealso \code{\link{densityPlotD}}.
##' @examples
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' conds <- Biobase::pData(Exp1_R25_pept)[,"Condition"]
##' CVDistD(Biobase::exprs(Exp1_R25_pept), conds)
CVDistD <- function(qData, conds=NULL, palette = NULL){
    
if (is.null(conds)) {return(NULL)}
  if (is.null(palette)){
    palette <- brewer.pal(length(unique(conds)),"Dark2")[1:length(unique(conds))]
  }else{
    if (length(palette) != ncol(qData)){
      warning("The color palette has not the same dimension as the number of samples")
      return(NULL)
    }
  }
  
conditions <- unique(conds)
n <- length(conditions)
axis.limits <- matrix(data = 0, nrow = 4, ncol = n)
for (i in conditions){
    if (length(which(conds == i)) > 1){
    t <- density(apply(qData[,which(conds == i)], 1, 
                    function(x) 100*var(x, na.rm=TRUE)/mean(x, na.rm=TRUE)), 
                 na.rm=TRUE)

    axis.limits[,which(conditions == i)]<- c(min(t$x), max(t$x), min(t$y),max(t$y))
    }
}

lim.x <- range(min(axis.limits[1,]), max(axis.limits[2,]))
lim.y <- range(min(axis.limits[3,]), max(axis.limits[4,]))

#par(mar = c(5, 5, 6, 3))
plot(x = NULL
        , ylab ="Density"
        , xlab = "CV( log (intensity) )"
        , xlim = lim.x
        , ylim = lim.y
        , las=1
)

# density by condition
conditions <- unique(conds)
col.density = c(1:length(conditions))
for (i in conditions){
    if (length(which(conds == i)) > 1){
        t <- apply(qData[,which(conds == i)], 1, 
                function(x) 100*var(x, na.rm=TRUE)/mean(x, na.rm=TRUE))
    lines(density(t, na.rm = TRUE)
        , xlab=""
        , ylab=""
        , col=col.density[which(conditions == i)]
    )
    }
}

legend("topright"         
        , legend = conditions
        , col = col.density
        , pch = 15
        , bty = "n"
        , pt.cex = 2
        , cex = 1
        , horiz = FALSE
        , inset = c(0,0)
)

}



##' Builds a densityplot of the CV of entities in the exprs() table
##' of a object. The CV is calculated for each condition present
##' in the dataset (see the slot \code{'Condition'} in the \code{pData()} table)
##' Same as the function \code{CVDistD} but uses the package \code{highcharter}
##' @title Distribution of CV of entities
##' @param qData A dataframe that contains quantitative data.
##' @param conds A vector of the conditions (one condition per sample).
##' @param palette xxx
##' @return A density plot
##' @author Samuel Wieczorek
##' @seealso \code{\link{densityPlotD}}.
##' @examples
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' conds <- Biobase::pData(Exp1_R25_pept)[,"Condition"]
##' CVDistD_HC(Biobase::exprs(Exp1_R25_pept), conds)
CVDistD_HC <- function(qData, conds=NULL, palette = NULL){
    
    if (is.null(conds)) {return(NULL)}
  conditions <- unique(conds)
  n <- length(conditions)
  
  if (is.null(palette)){
    palette <- brewer.pal(length(unique(conds)),"Dark2")[1:n]
  }else{
    if (length(palette) != ncol(qData)){
      warning("The color palette has not the same dimension as the number of samples")
      return(NULL)
    }
  }
  
  
    
    # nbSeries = n
    # series <- list()
    # for (i in 1:length(conditions)){
    #     if (length(which(conds == conditions[i])) > 1){
    #         t <- apply(qData[,which(conds == conditions[i])], 1, 
    #                    function(x) 100*var(x, na.rm=TRUE)/mean(x, na.rm=TRUE))
    #         tmp <- data.frame(x = density(t, na.rm = TRUE)$x,
    #                           y = density(t, na.rm = TRUE)$y)
    #         series[[i]] <- list(name = conditions[i],
    #                             data = list_parse(tmp))
    #     }
    # }

    h1 <-  highchart() %>% 
        my_hc_chart(chartType = "spline", zoomType="x") %>%
        hc_colors(unique(palette)) %>%
        hc_legend(enabled = TRUE) %>%
        hc_xAxis(title = list(text = "CV(log(Intensity))")) %>%
        hc_yAxis(title = list(text = "Density")) %>%
        hc_tooltip(headerFormat= '',
                   pointFormat = "<b>{series.name}</b>: {point.y} ",
                   valueDecimals = 2) %>%
      my_hc_ExportMenu(filename = "logIntensity") %>%
        hc_plotOptions(
            series=list(
                connectNulls= TRUE,
                marker=list(
                    enabled = FALSE)
            )
        )
    
    minX <- maxX <- 0
    maxY <- 0
    for (i in 1:n){
      if (length(which(conds == conditions[i])) > 1){
        t <- apply(qData[,which(conds == conditions[i])], 1, 
                   function(x) 100*var(x, na.rm=TRUE)/mean(x, na.rm=TRUE))
        tmp <- data.frame(x = density(t, na.rm = TRUE)$x,
                          y = density(t, na.rm = TRUE)$y)
        
        ymaxY <- max(maxY,tmp$y)
        xmaxY <- tmp$x[which(tmp$y==max(tmp$y))]
        minX <- min(minX, tmp$x)
        maxX <- max(maxX, 10*(xmaxY-minX))
        
        
      h1 <- h1 %>% hc_add_series(data=tmp, name=conditions[i]) }
    }
    
    h1 <- h1 %>%
      hc_chart(
      events = list(
        load = JS(paste0("function(){
                         var chart = this;
                         this.xAxis[0].setExtremes(",minX,",",maxX, ");
                         this.showResetZoom();}"))
        )
        )

    return(h1)

}






##' Builds a correlation matrix based on a \code{MSnSet} object.
##' 
##' @title Displays a correlation matrix of the quantitative data of the
##' \code{exprs()} table
##' @param obj An object of class \code{MSnSet}.
##' @param rate A float that defines the gradient of colors.
##' @return A colored correlation matrix
##' @author Alexia Dorffer
##' @examples
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' wrapper.corrMatrixD(Exp1_R25_pept)
wrapper.corrMatrixD <- function(obj, rate=5){
qData <- Biobase::exprs(obj)
samplesData <- Biobase::pData(obj)
corrMatrixD(qData, samplesData, rate)
}

##' Builds a correlation matrix based on a \code{MSnSet} object. 
##' Same as the function \code{\link{wrapper.corrMatrixD}} but uses the package \code{highcharter}
##' 
##' @title Displays a correlation matrix of the quantitative data of the
##' \code{exprs()} table
##' @param obj An object of class \code{MSnSet}.
##' @param rate A float that defines the gradient of colors.
##' @return A colored correlation matrix
##' @author Samuel Wieczorek
##' @examples
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' wrapper.corrMatrixD_HC(Exp1_R25_pept)
wrapper.corrMatrixD_HC <- function(obj, rate=0.5){
    qData <- Biobase::exprs(obj)
    samplesData <- Biobase::pData(obj)
    data <- cor(qData,use = 'pairwise.complete.obs')
    corrMatrixD_HC(data,samplesData, rate)
}



##' Correlation matrix based on a \code{MSnSet} object
##' 
##' @title Displays a correlation matrix of the quantitative data of the
##' \code{exprs()} table.
##' @param qData A dataframe of quantitative data.
##' @param samplesData A dataframe where lines correspond to samples and 
##' columns to the meta-data for those samples.
##' @param gradientRate The rate parameter to control the exponential law for 
##' the gradient of colors
##' @return A colored correlation matrix
##' @author Florence Combes, Samuel Wieczorek
##' @examples
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' qData <- Biobase::exprs(Exp1_R25_pept)
##' samplesData <- Biobase::pData(Exp1_R25_pept)
##' corrMatrixD(qData, samplesData)
corrMatrixD <- function(qData, samplesData, gradientRate = 5){
Var1 <- Var2 <- value <- NULL

for (j in 1:length(colnames(qData))){
    colnames(qData)[j] <- paste(as.character(samplesData[j,2:ncol(samplesData)]), 
                                collapse =" ")
}

z <- cor(qData,use = 'pairwise.complete.obs')
text <- element_text(colour="black", size = 16, face = "bold")
d <- qplot(x = Var1, 
            y = Var2, 
            data = melt(z), 
            fill = value, 
            geom = "tile") +
    theme(axis.text = element_text(size=16),
        axis.title = element_text(size=20, face="bold"),
        axis.text.x = element_text(angle=30, vjust=1, hjust=1),
        legend.text = text,
        legend.title = text) +
    labs(x = "", y = "") +

    scale_fill_gradientn (
    colours=colorRampPalette (c ("white", "lightblue","darkblue")) (101),
    values = c(pexp(seq(0,1,0.01), rate=gradientRate),1), limits=c(0,1))

plot(d)
}




##' Correlation matrix based on a \code{MSnSet} object. Same as the 
##' function \link{corrMatrixD} but uses the package \code{highcharter}
##' 
##' @title Displays a correlation matrix of the quantitative data of the
##' \code{exprs()} table.
##' @param object The result of the \code{cor} function.
##' @param samplesData A dataframe in which lines correspond to samples and 
##' columns to the meta-data for those samples.
##' @param rate The rate parameter to control the exponential law for 
##' the gradient of colors
##' @return A colored correlation matrix
##' @author Samuel Wieczorek
##' @examples
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' qData <- Biobase::exprs(Exp1_R25_pept)
##' samplesData <- Biobase::pData(Exp1_R25_pept)
##' res <- cor(qData,use = 'pairwise.complete.obs')
##' corrMatrixD_HC(res, samplesData)
corrMatrixD_HC <- function(object,samplesData = NULL, rate = 0.5) {
    
    df <- as.data.frame(object)
    
    if (!is.null(samplesData)){
        for (j in 1:ncol(df)){
            names(df)[j] <- paste(as.character(samplesData[j,2:ncol(samplesData)]), 
                                        collapse =" ")
        }
        }
    is.num <- sapply(df, is.numeric)
    df[is.num] <- lapply(df[is.num], round, 2)
    dist <- NULL
    
    x <- y <- names(df)
    
    df <- tbl_df(cbind(x = y, df)) %>% 
        gather(y, dist, -x) %>% 
        mutate(x = as.character(x),
               y = as.character(y)) %>% 
        left_join(data_frame(x = y,
                             xid = seq(length(y)) - 1), by = "x") %>% 
        left_join(data_frame(y = y,
                             yid = seq(length(y)) - 1), by = "y")
    
    ds <- df %>% 
        select_("xid", "yid", "dist") %>% 
        list_parse2()
    
    fntltp <- JS("function(){
                  return this.series.xAxis.categories[this.point.x] + ' ~ ' +
                         this.series.yAxis.categories[this.point.y] + ': <b>' +
                         Highcharts.numberFormat(this.point.value, 2)+'</b>';
               ; }")
    cor_colr <- list( list(0, '#FF5733'),
                      list(0.5, '#F8F5F5'),
                      list(1, '#2E86C1')
    )
    highchart() %>% 
        my_hc_chart(chartType = "heatmap") %>% 
        hc_xAxis(categories = y, title = NULL) %>% 
        hc_yAxis(categories = y, title = NULL) %>% 
        hc_add_series(data = ds) %>% 
        hc_plotOptions(
            series = list(
                boderWidth = 0,
                dataConditions = list(enabled = TRUE),
                dataLabels = list(enabled = TRUE)
            )) %>% 
        hc_tooltip(formatter = fntltp) %>% 
        hc_legend(align = "right", layout = "vertical",
                  verticalAlign="middle") %>% 
        hc_colorAxis(  stops= cor_colr,min=rate,max=1) %>%
      my_hc_ExportMenu(filename = "corrMatrix")
}




##' Builds a heatmap of the quantitative proteomic data of a 
##' \code{MSnSet} object.
##' 
##' @title This function is a wrapper to \code{\link{heatmap.2}} that displays 
##' quantitative data in the \code{exprs()} table of an object of
##' class \code{MSnSet}
##' @param obj An object of class \code{MSnSet}.
##' @param distance The distance used by the clustering algorithm to compute 
##' the dendrogram. See \code{help(heatmap.2)}.
##' @param cluster the clustering algorithm used to build the dendrogram.
##' See \code{help(heatmap.2)}
##' @param dendro A boolean to indicate fi the dendrogram has to be displayed
##' @return A heatmap
##' @author Alexia Dorffer
##' @examples
##' \dontrun{
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' obj <- mvFilter(Exp1_R25_pept[1:1000], "wholeMatrix", 6)
##' wrapper.heatmapD(obj)
##' }
wrapper.heatmapD  <- function(obj, distance="euclidean", cluster="complete", 
                            dendro = FALSE){
qData <- Biobase::exprs(obj)
for (j in 1:length(colnames(qData))){
    colnames(qData)[j] <- paste(as.character(Biobase::pData(obj)[j,2:ncol(Biobase::pData(obj))]), 
                                collapse =" ")
}

heatmapD(qData, distance, cluster, dendro)
}




##' Heatmap of the quantitative proteomic data of a \code{MSnSet} object
##' 
##' @title This function is a wrapper to \code{\link{heatmap.2}} that displays 
##' quantitative data in the \code{exprs()} table of an object of
##' class \code{MSnSet}
##' @param qData A dataframe that contains quantitative data.
##' @param distance The distance used by the clustering algorithm to compute 
##' the dendrogram. See \code{help(heatmap.2)}
##' @param cluster the clustering algorithm used to build the dendrogram.
##' See \code{help(heatmap.2)}
##' @param dendro A boolean to indicate fi the dendrogram has to be displayed
##' @return A heatmap
##' @author Florence Combes, Samuel Wieczorek
##' @examples
##' \dontrun{
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' obj <- mvFilter(Exp1_R25_pept[1:1000], "wholeMatrix", 6)
##' qData <- Biobase::exprs(obj)
##' heatmapD(qData)
##' }
heatmapD <- function(qData, distance="euclidean", cluster="complete", dendro = FALSE){
##Check parameters
# paramdist <- c("euclidean", "manhattan") 
# if (!(distance %in% paramdist)){
#     stop("Param distance is not correct.")
#     return (NULL)
# }
# 
# paramcluster <- c("ward.D", "average")
# if (!(cluster %in%  paramcluster)){
#     stop("Param clustering is not correct.")
#     return (NULL)
# }


# if (isTRUE(dendro) && getNumberOfEmptyLines(qData) != 0)  {
#     stop("Your dataset contains empty lines: the dendrogram cannot 
# be computed.
#         Please filter or impute missing values before.")
#     return (NULL)
# }
# else {
    .data <- matrix(qData, 
                    ncol = ncol(qData), 
                    byrow = FALSE,
                    dimnames = list(rownames(qData), colnames(qData))
    )
    colors = c(seq(-3, -2, length=100),
                seq(-2, 0.5, length=100),
                seq(0.5, 6, length=100))
    heatmap.color <- colorRampPalette(c("green", "red"))(n = 1000)
    
    
    if (dendro){ .dendro = "row"} else {.dendro = "none"}
    p <- heatmap.2(
        x=t(.data),
        distfun = function(x) {
            x[is.na(x)] <- -1e5
            dist(x, method=distance)
            },
        hclustfun = function(x) {
            x[is.na(x)] <- -1e5
            hclust(x, method=cluster)
            },
        dendrogram =.dendro,
        Rowv=TRUE,
        col=heatmap.color ,
        density.info='none',
        key=TRUE,
        trace="none",
        scale="none",
        #srtCol=45,
        labCol="",
        margins=c(4,12),
        cexRow=1.5,
        keysize = 1.5,
        lhei = c(1.5, 9),
        lwid = c(1.5, 4),
        lmat = rbind(4:3, 2:1)
        
    )
#    }
}


##' Heatmap inspired by the heatmap.2 function.
##' 
##' @title This function is inspired from the function \code{\link{heatmap.2}} 
##' that displays quantitative data in the \code{exprs()} table of an object of
##' class \code{MSnSet}. For more information, please refer to the help 
##' of the heatmap.2 function.
##' @param x A dataframe that contains quantitative data.
##' @param col colors used for the image. Defaults to heat colors (heat.colors).
##' @param srtCol angle of column conds, in degrees from horizontal 
##' @param labCol character vectors with column conds to use.
##' @param labRow character vectors with row conds to use.
##' @param key logical indicating whether a color-key should be shown.
##' @param key.title main title of the color key. If set to NA no title will 
##' be plotted.
##' @param main main title; default to none.
##' @param ylab y-axis title; default to none.
##' @return A heatmap
##' @author Samuel Wieczorek
##' @examples
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' obj <- mvFilter(Exp1_R25_pept, "wholeMatrix", 6)
##' qData <- Biobase::exprs(obj)
##' heatmap.DAPAR(qData)
heatmap.DAPAR <- 
    function (x, 
              col = heat.colors(100),
              srtCol=NULL,
              labCol = NULL,
              labRow = NULL,
              key = TRUE, 
              key.title = NULL,
              main = NULL,  
              ylab = NULL) 
    {
        scale01 <- function(x, low = min(x), high = max(x)) {
            x <- (x - low)/(high - low)
            x
        }
        
        offsetCol <- 0.5
        offsetRow = 0.5
        srtRow = NULL
        colRow = NULL
        colCol = NULL 
        xlab = NULL
        key.par = list()
        margins = c(5, 5)
        sepcolor = "white"
        na.color = "white"
        keysize = 1.5
        breaks <- NULL
        na.rm = TRUE
        
        if (length(di <- dim(x)) != 2 || !is.numeric(x)) 
            stop("`x' must be a numeric matrix")
        nr <- di[1]
        nc <- di[2]
        if (nr <= 1 || nc <= 1) 
            stop("`x' must have at least 2 rows and 2 columns")
        x <- x[nr:1,]
        cellnote <- matrix("", ncol = ncol(x), nrow = nrow(x))
        cexCol = 0.2 + 1/log10(nc)
        cexRow = 0.2 + 1/log10(nr)
        iy <- 1:nr
        breaks <- length(col) + 1
        breaks <- seq(min(x, na.rm = na.rm), max(x, na.rm = na.rm), 
                      length = breaks)
        
        nbr <- length(breaks)
        ncol <- length(breaks) - 1
        
        min.breaks <- min(breaks)
        max.breaks <- max(breaks)
        x[x < min.breaks] <- min.breaks
        x[x > max.breaks] <- max.breaks
        lhei <- c(keysize, 4)
        lwid <- c(keysize, 4)
        lmat <- rbind(4:3, 2:1)
        lmat[is.na(lmat)] <- 0
        
        op <- par(no.readonly = TRUE)
        on.exit(par(op))
        layout(lmat, widths = lwid, heights = lhei, respect = FALSE)
        
        par(mar = c(margins[1], 0, 0, margins[2]))
        x <- t(x)
        
        
        image(1:nc, 1:nr, x, xlim = 0.5 + c(0, nc), ylim = 0.5 + 
                  c(0, nr), axes = FALSE, xlab = "", ylab = "", col = col, 
              breaks = breaks)
        
        
        if (!is.null(labCol)) 
        {
            axis(1, 1:nc, label = labCol, las = 2, line = -0.5 + 
                     offsetCol, tick = 0, cex.axis = cexCol, hadj = NA, 
                 padj = 0)
        }
        else {
                adjCol = c(1, NA)
                xpd.orig <- par("xpd")
                par(xpd = NA)
                xpos <- axis(1, 1:nc, label = rep("", nc), las = 2, 
                             tick = 0)
                text(x = xpos, y = par("usr")[3] - (1 + offsetCol) * 
                         strheight("M"), label = labCol, adj = adjCol, 
                     cex = cexCol, srt = srtCol, col = colCol)
                par(xpd = xpd.orig)
        }
        
        
        if (!is.null(labRow) ) {
            axis(4, iy, label = labRow, las = 5, line = -0.5 + offsetRow, 
                 tick = 0, cex.axis = cexRow, hadj = 0, padj = NA)
        }
        else {
                xpd.orig <- par("xpd")
                par(xpd = NA)
                ypos <- axis(4, iy, label = rep("", nr), las = 2, 
                             line = -0.5, tick = 0)
                text(x = par("usr")[2] + (1 + offsetRow) * strwidth("M"), 
                     y = ypos, label = labRow, adj = c(0,NA), cex = cexRow, 
                     srt = srtRow, col = colRow)
                par(xpd = xpd.orig)
        }
        
        
        
        
        par(mar = c(margins[1], 0, 0, 0))
        plot.new()
        par(mar = c(0, 0, if (!is.null(main)) 5 else 0, margins[2]))
        
        plot.new()
        if (!is.null(main)) 
            title(main, cex.main = 1.5 * op[["cex.main"]])
        
        
        if (key) {
            mar <- c(5, 4, 2, 1)
            par(mar = mar, cex = 0.75, mgp = c(2, 1, 0))
            if (length(key.par) > 0) 
                do.call(par, key.par)
            
            tmpbreaks <- breaks
            min.raw <- min.breaks
            max.raw <- max.breaks
            
            z <- seq(min.raw, max.raw, by = min(diff(breaks)/100))
            image(z = matrix(z, ncol = 1), col = col, breaks = tmpbreaks, 
                  xaxt = "n", yaxt = "n")
            par(usr = c(0, 1, 0, 1))
            lv <- pretty(breaks)
            xv <- scale01(as.numeric(lv), min.raw, max.raw)
            xargs <- list(at = xv, label = lv)
            
            xargs$side <- 1
            do.call(axis, xargs)
            key.xlab <- "Intensity value"
            
            mtext(side = 1, key.xlab, line = par("mgp")[1], padj = 0.5, 
                  cex = par("cex") * par("cex.lab"))
            
            if (is.null(key.title)) 
                title("Color Key")
        }
        
    }


# 
# rep.row <-function(x,n){
#   matrix(rep(x,each=n),nrow=n)
# }
# 
# rep.col<-function(x,n){
#   matrix(rep(x,each=n), ncol=n, byrow=TRUE)
# }

###--------------------------------------------------------------------
# heatmap_HC <- function(qData, col=heat.colors(100),labCol)
# {
#   conds_v <- c(rep.row(colnames(qData), nrow(qData)))
#   lines_v <- c(rep.col(1:nrow(qData), ncol(qData)))
#   data <- tibble(id=lines_v, condition=conds_v, value=round(c(qData), digits=2 ))
#   
# #   fntltp <- JS("function(){
# #                return this.point.x + ' ' +  this.series.yAxis.categories[this.point.y] + ':<br>' +
# #                Highcharts.numberFormat(this.point.value, 2);
# # }")
# 
#   # plotline <- list(
#   #   color = "#fde725", value = 3, width = 2, zIndex = 5,
#   #   label = list(
#   #     text = "", verticalAlign = "top",
#   #     style = list(color = "black"), textAlign = "left",
#   #     rotation = 0, y = -5)
#   # )
#   
#  # highchart2() %>%
#     hchart(data, "heatmap", hcaes(x = condition, y = id, value = value)) %>% 
#     hc_colorAxis(stops = color_stops(100, col),type = "linear") %>% 
#     # hc_yAxis(reversed = FALSE, offset = -20, tickLength = 0,
#     #          gridLineWidth = 0, minorGridLineWidth = 0,
#     #          labels = list(style = list(fontSize = "8px"))) %>% 
#     hc_tooltip(enabled = FALSE) %>% 
#     #hc_xAxis(plotLines = list(plotline)) %>%
#     hc_title(text = "MEC repartition") %>% 
#     hc_legend(layout = "vertical", verticalAlign = "top",
#               align = "left", valueDecimals = 0)
#   
# }
