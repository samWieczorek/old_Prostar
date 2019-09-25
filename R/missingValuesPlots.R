##' This method is a wrapper to plots from a \code{MSnSet} object a 
##' histogram which represents the distribution of the 
##' number of missing values (NA) per lines (ie proteins).
##' 
##' @title Histogram of missing values per lines from an object 
##' \code{MSnSet}
##' @param obj An object of class \code{MSnSet}.
##' @param indLegend The indice of the column name's in \code{pData()} tab .
##' @param showValues A logical that indicates wether numeric values should be
##' drawn above the bars.
##' @return A histogram
##' @author Alexia Dorffer
##' @examples
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' wrapper.mvPerLinesHisto(Exp1_R25_pept)
wrapper.mvPerLinesHisto <- function(obj, indLegend="auto", showValues=FALSE){
qData <- Biobase::exprs(obj)
samplesData <- Biobase::pData(obj)
mvPerLinesHisto(qData, samplesData, indLegend, showValues)
}


##' This method is a wrapper to plots from a \code{MSnSet} object a 
##' histogram which represents the distribution of the 
##' number of missing values (NA) per lines (ie proteins).
##' 
##' @title Histogram of missing values per lines from an object using highcharter
##' \code{MSnSet}
##' @param obj An object of class \code{MSnSet}.
##' @param indLegend The indice of the column name's in \code{pData()} tab .
##' @param showValues A logical that indicates wether numeric values should be
##' drawn above the bars.
##' @return A histogram
##' @author Alexia Dorffer
##' @examples
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' wrapper.mvPerLinesHisto(Exp1_R25_pept)
wrapper.mvPerLinesHisto_HC <- function(obj, indLegend="auto", showValues=FALSE){
    qData <- Biobase::exprs(obj)
    samplesData <- Biobase::pData(obj)
    hc <- mvPerLinesHisto_HC(qData, samplesData, indLegend, showValues)
    return(hc)
}

##' This method plots a bar plot which represents the distribution of the 
##' number of missing values (NA) per lines (ie proteins).
##' 
##' @title Bar plot of missing values per lines
##' @param qData A dataframe that contains the data to plot.
##' @param samplesData A dataframe which contains informations about 
##' the replicates.
##' @param indLegend The indice of the column name's in \code{pData()} tab 
##' @param showValues A logical that indicates wether numeric values should be
##' drawn above the bars.
##' @return A bar plot
##' @author Florence Combes, Samuel Wieczorek
##' @examples
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' qData <- Biobase::exprs(Exp1_R25_pept)
##' samplesData <- Biobase::pData(Exp1_R25_pept)
##' mvPerLinesHisto(qData, samplesData)
mvPerLinesHisto <- function(qData, samplesData, indLegend="auto", showValues=FALSE){

if (identical(indLegend,"auto")) { indLegend <- c(2:length(colnames(samplesData)))}
    
for (j in 1:length(colnames(qData))){
    noms <- NULL
    for (i in 1:length(indLegend)){
    noms <- paste(noms, samplesData[j,indLegend[i]], sep=" ")
    }
    colnames(qData)[j] <- noms
}

coeffMax <- .1

NbNAPerCol <- colSums(is.na(qData))
NbNAPerRow <- rowSums(is.na(qData))
#par(mar = c(10,3, 3, 3))

nb.col <- dim(qData)[2] 
nb.na <- NbNAPerRow
temp <- table(NbNAPerRow)
nb.na2barplot <- c(temp, rep(0,1+ncol(qData)-length(temp)))

if (sum(NbNAPerRow) == 0){
    nb.na2barplot <- rep(0,1+ncol(qData))
}

print(nb.na2barplot[-1])

x <- barplot(nb.na2barplot[-1], 
                main = "# lines by # of NA",
                xlab = "# NA per lines",
                names.arg = as.character(c(1:(ncol(qData)))), 
                col = c(rep("lightgrey",nb.col-1), "red"),
                ylim = c(0, 1.2*max(1,nb.na2barplot[-1])), 
                las=1,
                cex.names=1.5,
                cex.axis=1.5
)

}


##' This method plots a bar plot which represents the distribution of the 
##' number of missing values (NA) per lines (ie proteins).
##' 
##' @title Bar plot of missing values per lines using highcharter
##' @param qData A dataframe that contains the data to plot.
##' @param samplesData A dataframe which contains informations about 
##' the replicates.
##' @param indLegend The indice of the column name's in \code{pData()} tab 
##' @param showValues A logical that indicates wether numeric values should be
##' drawn above the bars.
##' @return A bar plot
##' @author Florence Combes, Samuel Wieczorek
##' @examples
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' qData <- Biobase::exprs(Exp1_R25_pept)
##' samplesData <- Biobase::pData(Exp1_R25_pept)
##' mvPerLinesHisto_HC(qData, samplesData)
mvPerLinesHisto_HC <- function(qData, samplesData, indLegend="auto", showValues=FALSE){
    
    if (identical(indLegend,"auto")) { indLegend <- c(2:length(colnames(samplesData)))}
    
    for (j in 1:length(colnames(qData))){
        noms <- NULL
        for (i in 1:length(indLegend)){
            noms <- paste(noms, samplesData[j,indLegend[i]], sep=" ")
        }
        colnames(qData)[j] <- noms
    }
    
    coeffMax <- .1
    
    NbNAPerCol <- colSums(is.na(qData))
    NbNAPerRow <- rowSums(is.na(qData))
    #par(mar = c(10,3, 3, 3))
    
    nb.col <- dim(qData)[2] 
    nb.na <- NbNAPerRow
    temp <- table(NbNAPerRow)
    nb.na2barplot <- c(temp, rep(0,1+ncol(qData)-length(temp)))
    
    if (sum(NbNAPerRow) == 0){
        nb.na2barplot <- rep(0,1+ncol(qData))
    }
    
    df <- data.frame(y=nb.na2barplot[-1])
    myColors = rep("lightgrey",nrow(df))
    myColors[nrow(df)] <- "red"
    
    #df1 <- df2 <- df
    #df2[1:(nrow(df)-1),] <- 0
    #df1 [nrow(df),] <- 0
    
    
    #, series = list( pointWidth = 50)
    
    h1 <-  highchart() %>% 
        hc_title(text = "#[lines] with X NA values") %>% 
        hc_add_series(data = df, type="column", colorByPoint = TRUE) %>%
        hc_colors(myColors) %>%
        hc_plotOptions( column = list(stacking = "normal"),
                        animation=list(duration = 100)) %>%
        hc_legend(enabled = FALSE) %>%
        hc_xAxis(categories = row.names(df), title = list(text = "#[NA values] per line")) %>%
      my_hc_ExportMenu(filename = "missingValuesPlot1") %>%
        hc_tooltip(enabled = TRUE,
                   headerFormat= '',
                   pointFormat = "{point.y} ")
    
    return(h1)
 
}



##' This method is a wrapper to plots from a \code{MSnSet} object a 
##' bar plot which represents the distribution of the 
##' number of missing values (NA) per lines (ie proteins) and per conditions.
##' 
##' @title Bar plot of missing values per lines and per conditions from an 
##' object \code{MSnSet}
##' @param obj An object of class \code{MSnSet}.
##' @param indLegend The indice of the column name's in \code{pData()} tab .
##' @param showValues A logical that indicates wether numeric values should be
##' drawn above the bars.
##' @return A bar plot
##' @author Samuel Wieczorek
##' @examples
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' wrapper.mvPerLinesHistoPerCondition(Exp1_R25_pept)
wrapper.mvPerLinesHistoPerCondition <- function(obj, indLegend="auto", 
                                            showValues=FALSE){
qData <- Biobase::exprs(obj)
samplesData <- Biobase::pData(obj)
mvPerLinesHistoPerCondition(qData, samplesData, indLegend, showValues)
}


##' This method is a wrapper to plots (using highcharts) from a \code{MSnSet} object a 
##' bar plot which represents the distribution of the 
##' number of missing values (NA) per lines (ie proteins) and per conditions.
##' 
##' @title Bar plot of missing values per lines and per conditions from an 
##' object \code{MSnSet}
##' @param obj An object of class \code{MSnSet}.
##' @param indLegend The indice of the column name's in \code{pData()} tab .
##' @param showValues A logical that indicates wether numeric values should be
##' drawn above the bars.
##' @param ... xxx
##' @return A bar plot
##' @author Samuel Wieczorek
##' @examples
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' wrapper.mvPerLinesHistoPerCondition_HC(Exp1_R25_pept)
wrapper.mvPerLinesHistoPerCondition_HC <- function(obj, indLegend="auto", 
                                                showValues=FALSE, ...){
    qData <- Biobase::exprs(obj)
    samplesData <- Biobase::pData(obj)
    mvPerLinesHistoPerCondition_HC(qData, samplesData, indLegend, showValues, ...)
}


##' This method plots a bar plot which represents the distribution of the 
##' number of missing values (NA) per lines (ie proteins) and per conditions.
##' 
##' @title Bar plot of missing values per lines and per condition
##' @param qData A dataframe that contains quantitative data.
##' @param samplesData A dataframe where lines correspond to samples and 
##' columns to the meta-data for those samples.
##' @param indLegend The indice of the column name's in \code{pData()} tab 
##' @param showValues A logical that indicates wether numeric values should be
##' drawn above the bars.
##' @param palette xxx
##' @return A bar plot
##' @author Samuel Wieczorek
##' @examples
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' qData <- Biobase::exprs(Exp1_R25_pept)
##' samplesData <- Biobase::pData(Exp1_R25_pept)
##' mvPerLinesHistoPerCondition(qData, samplesData)
mvPerLinesHistoPerCondition <- function(qData, samplesData, indLegend="auto", 
                                        showValues=FALSE, palette=NULL){

  
  if (is.null(palette)){
    palette <- brewer.pal(ncol(qData),"Dark2")[1:ncol(qData)]
  }else{
    if (length(palette) != ncol(qData)){
      warning("The color palette has not the same dimension as the number of samples")
      return(NULL)
    }
  }
if (identical(indLegend,"auto")) { indLegend <- c(2:length(colnames(samplesData)))}

nbConditions <- length(unique(samplesData[,"Condition"]))

ncolMatrix <- max(unlist(lapply(unique(samplesData[,"Condition"]), function(x){length(which(samplesData[,"Condition"]==x))})))
m <- matrix(rep(0, nbConditions*(1+ncolMatrix)), 
            ncol = nbConditions, 
            dimnames=list(seq(0:(ncolMatrix)),unique(samplesData[,"Condition"])))

for (i in unique(samplesData[,"Condition"]))
{
    nSample <- length(which(samplesData[,"Condition"] == i))
    t <- NULL
    if (nSample == 1) {
        t <- table(as.integer(is.na(qData[,which(samplesData[,"Condition"] == i)])))
    } else {t <- table(rowSums(is.na(qData[,which(samplesData[,"Condition"] == i)])))}
    
    m[as.integer(names(t))+1,i] <- t
}

m <- t(m)

x <- barplot(m, 
                main = "# lines by # of NA",
                xlab = "# NA per lines",
                names.arg = as.character(0:ncolMatrix), 
                col = palette,
                ylim = c(0, 1.2*max(m)), 
                xpd = FALSE,
                las=1,
                cex.names=1.5,
                cex.axis=1.5,
                beside=TRUE
)

}




##' This method plots a bar plot which represents the distribution of the 
##' number of missing values (NA) per lines (ie proteins) and per conditions.
##' Same as the function \link{mvPerLinesHistoPerCondition} but uses the package
##' \code{highcharter}.
##' 
##' @title Bar plot of missing values per lines and per condition
##' @param qData A dataframe that contains quantitative data.
##' @param samplesData A dataframe where lines correspond to samples and 
##' columns to the meta-data for those samples.
##' @param indLegend The indice of the column name's in \code{pData()} tab 
##' @param showValues A logical that indicates wether numeric values should be
##' drawn above the bars.
##' @param palette xxx
##' @return A bar plot
##' @author Samuel Wieczorek
##' @examples
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' qData <- Biobase::exprs(Exp1_R25_pept)
##' samplesData <- Biobase::pData(Exp1_R25_pept)
##' mvPerLinesHistoPerCondition_HC(qData, samplesData)
mvPerLinesHistoPerCondition_HC <- function(qData, samplesData, indLegend="auto", 
                                        showValues=FALSE, palette=NULL){
    
    conds <- samplesData[,"Condition"]
    if (is.null(palette)){
      palette <- brewer.pal(length(unique(conds)),"Dark2")
    }else{
      if (length(palette) != ncol(qData)){
        warning("The color palette has not the same dimension as the number of samples")
        return(NULL)
      }
    }
    
    
    if (identical(indLegend,"auto")) { indLegend <- c(2:length(colnames(samplesData)))}
    
    nbConditions <- length(unique(samplesData[,"Condition"]))
    
    ncolMatrix <- max(unlist(lapply(unique(samplesData[,"Condition"]), function(x){length(which(samplesData[,"Condition"]==x))})))
    m <- matrix(rep(0, nbConditions*(1+ncolMatrix)), 
                ncol = nbConditions, 
                dimnames=list(seq(0:(ncolMatrix)),unique(samplesData[,"Condition"])))
    
    for (i in unique(samplesData[,"Condition"]))
    {
        nSample <- length(which(samplesData[,"Condition"] == i))
        t <- NULL
        if (nSample == 1) {
            t <- table(as.integer(is.na(qData[,which(samplesData[,"Condition"] == i)])))
        } else {t <- table(rowSums(is.na(qData[,which(samplesData[,"Condition"] == i)])))}
        
        m[as.integer(names(t))+1,i] <- t
    }
    m <- as.data.frame(m)
    
     rownames(m) <- 0:(nrow(m)-1)
    
    h1 <-  highchart() %>% 
        hc_title(text = "#[lines] with X NA values (condition-wise)") %>% 
        my_hc_chart(chartType = "column") %>%
        hc_plotOptions( column = list(stacking = ""),
                        dataLabels = list(enabled = FALSE),
                        animation=list(duration = 100)) %>%
        hc_colors(unique(palette)) %>%
        hc_legend(enabled = FALSE) %>%
        hc_xAxis(categories = row.names(m), title = list(text = "#[NA values] per line (condition-wise)")) %>%
      my_hc_ExportMenu(filename = "missingValuesPlot_2") %>%
        hc_tooltip(headerFormat= '',
                   pointFormat = "{point.y} ")
    
    for (i in 1:nbConditions){
    h1 <- h1 %>% hc_add_series(data=m[,unique(samplesData[,"Condition"])[i]]) }
      
    
    return(h1)
    

    
}


##' This method plots from a \code{MSnSet} object a histogram of 
##' missing values.
##' 
##' @title Histogram of missing values from a \code{MSnSet} object
##' @param obj An object of class \code{MSnSet}.
##' @param indLegend The indices of the column name's in \code{pData()} tab.
##' @param showValues A logical that indicates wether numeric values should be
##' drawn above the bars.
##' @return A histogram
##' @author Alexia Dorffer
##' @examples
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' wrapper.mvHisto(Exp1_R25_pept, showValues=TRUE)
wrapper.mvHisto <- function(obj, indLegend="auto", showValues=FALSE){
qData <- Biobase::exprs(obj)
samplesData <- Biobase::pData(obj)
conds <- samplesData[,"Condition"]
mvHisto(qData, samplesData, conds, indLegend, showValues)
}


##' This method plots from a \code{MSnSet} object a histogram of 
##' missing values.
##' 
##' @title Histogram of missing values from a \code{MSnSet} object
##' @param obj An object of class \code{MSnSet}.
##' @param indLegend The indices of the column name's in \code{pData()} tab.
##' @param showValues A logical that indicates wether numeric values should be
##' drawn above the bars.
##' @param ... xxx
##' @return A histogram
##' @author Alexia Dorffer
##' @examples
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' wrapper.mvHisto_HC(Exp1_R25_pept, showValues=TRUE)
wrapper.mvHisto_HC <- function(obj, indLegend="auto", showValues=FALSE, ...){
    qData <- Biobase::exprs(obj)
    samplesData <- Biobase::pData(obj)
    conds <- samplesData[,"Condition"]
    mvHisto_HC(qData, samplesData, conds, indLegend, showValues, ...)
}



##' This method plots a histogram of missing values.
##' 
##' @title Histogram of missing values
##' @param qData A dataframe that contains quantitative data.
##' @param samplesData A dataframe where lines correspond to samples and 
##' columns to the meta-data for those samples.
##' @param conds A vector of the conditions (one condition per sample).
##' @param indLegend The indices of the column name's in \code{pData()} tab
##' @param showValues A logical that indicates wether numeric values should be
##' drawn above the bars.
##' @param palette xxx
##' @return A histogram
##' @author Florence Combes, Samuel Wieczorek
##' @examples
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' qData <- Biobase::exprs(Exp1_R25_pept)
##' samplesData <- Biobase::pData(Exp1_R25_pept)
##' conds <- Biobase::pData(Exp1_R25_pept)[,"Condition"]
##' mvHisto(qData, samplesData, conds, indLegend="auto", showValues=TRUE)
mvHisto <- function(qData, samplesData, conds, indLegend="auto", showValues=FALSE, palette=NULL){

  if (is.null(palette)){
    palette <- brewer.pal(length(unique(conds)),"Dark2")
  }else{
    if (length(palette) != ncol(qData)){
      warning("The color palette has not the same dimension as the number of samples")
      return(NULL)
    }
  }
  
if (identical(indLegend,"auto")) { 
    indLegend <- c(2:length(colnames(samplesData)))
}


colnames(qData) <- samplesData[,"Condition"]

coeffMax <- .1

NbNAPerCol <- colSums(is.na(qData))
NbNAPerRow <- rowSums(is.na(qData))

if (sum(NbNAPerCol) == 0) {if (sum(NbNAPerCol) == 0){
    NbNAPerCol <- rep(0,1+ncol(qData))
}} 
x <- barplot(NbNAPerCol, 
                main = "# NA per columns",
                col=palette,
                las=1,
                ylim = c(0, 1.2*max(1,NbNAPerCol)),
                names.arg = c(1:18), 
                cex.names=1.5,
                cex.axis=1.5,
                axisnames = FALSE
)

par(xpd = TRUE)
graphics::text(x, -3,
        label = colnames(qData),
        srt = 45,
        adj=1,
        cex=1.4)

}



##' This method plots a histogram of missing values. Same as the function \code{mvHisto}
##' but uses the package \code{highcharter}
##' 
##' @title Histogram of missing values
##' @param qData A dataframe that contains quantitative data.
##' @param samplesData A dataframe where lines correspond to samples and 
##' columns to the meta-data for those samples.
##' @param conds A vector of the conditions (one condition per sample).
##' @param indLegend The indices of the column name's in \code{pData()} tab
##' @param showValues A logical that indicates wether numeric values should be
##' drawn above the bars.
##' @param palette xxx
##' @return A histogram
##' @author Florence Combes, Samuel Wieczorek
##' @examples
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' qData <- Biobase::exprs(Exp1_R25_pept)
##' samplesData <- Biobase::pData(Exp1_R25_pept)
##' conds <- Biobase::pData(Exp1_R25_pept)[,"Condition"]
##' mvHisto_HC(qData, samplesData, conds, indLegend="auto", showValues=TRUE)
mvHisto_HC <- function(qData, samplesData, conds, indLegend="auto", 
                    showValues=FALSE, palette = NULL){
  if (is.null(palette)){
    palette <- brewer.pal(length(unique(conds)),"Dark2")
  }else{
    if (length(palette) != ncol(qData)){
      warning("The color palette has not the same dimension as the number of samples")
      return(NULL)
    }
  }
  
  
    if (identical(indLegend,"auto")) { 
        indLegend <- c(2:length(colnames(samplesData)))
    }
    
   
    NbNAPerCol <- colSums(is.na(qData))
    NbNAPerRow <- rowSums(is.na(qData))
    
    df <- data.frame(NbNAPerCol)
    names(df) <- 'y'
    
    
    h1 <-  highchart() %>%
         my_hc_chart(chartType = "column") %>%
         hc_title(text = "#[non-NA values] by replicate") %>%
        hc_add_series(df,type="column", colorByPoint = TRUE) %>%
      hc_colors(palette) %>%
        hc_plotOptions( column = list(stacking = "normal"),
                        animation=list(duration = 100)) %>%
        hc_legend(enabled = FALSE) %>%
        hc_xAxis(categories = conds, title = list(text = "Replicates")) %>%
      my_hc_ExportMenu(filename = "missingValuesPlot_3") %>%
        hc_tooltip(headerFormat= '',
                   pointFormat = "{point.y}")
    

    return(h1)
    
    
    

    
}



##' Plots a heatmap of the quantitative data. Each column represent one of
##' the conditions in the object of class \code{MSnSet} and 
##' the color is proportional to the mean of intensity for each line of
##' the dataset.
##' The lines have been sorted in order to vizualize easily the different
##' number of missing values. A white square is plotted for missing values.
##' 
##' @title Heatmap of missing values from a \code{MSnSet} object
##' @param obj An object of class \code{MSnSet}.
##' @return A heatmap
##' @author Alexia Dorffer
##' @examples
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' obj <- Exp1_R25_pept
##' keepThat <- mvFilterGetIndices(obj, 'wholeMatrix', 1)
##' obj <- mvFilterFromIndices(obj, keepThat)
##' wrapper.mvImage(obj)
wrapper.mvImage <- function(obj){
  qData <- Biobase::exprs(obj) 
  if (sum(is.na(qData))==0) {return(NULL)}
  

conds <- Biobase::pData(obj)[,"Condition"]
originValues <- Biobase::fData(obj)[,obj@experimentData@other$OriginOfValues]
indices <- which(apply(is.OfType(originValues, "MEC"),1,sum) >0)

mvImage(qData[indices,], conds)
}



##' Plots a heatmap of the quantitative data. Each column represent one of
##' the conditions in the object of class \code{MSnSet} and 
##' the color is proportional to the mean of intensity for each line of
##' the dataset.
##' The lines have been sorted in order to vizualize easily the different
##' number of missing values. A white square is plotted for missing values.
##' 
##' @title Heatmap of missing values
##' @param qData A dataframe that contains quantitative data.
##' @param conds A vector of the conditions (one condition per sample).
##' @return A heatmap
##' @author Samuel Wieczorek, Thomas Burger
##' @examples
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' qData <- Biobase::exprs(Exp1_R25_pept)
##' conds <- Biobase::pData(Exp1_R25_pept)[,"Condition"]
##' mvImage(qData, conds)
mvImage <- function(qData, conds){
  
### build indices of conditions
indCond <- list()
ConditionNames <- unique(conds)
for (i in ConditionNames) {
    indCond <- append(indCond, list(which(i == conds)))
}
indCond <- setNames(indCond, as.list(c("cond1", "cond2")))

nNA1 = apply(as.matrix(qData[,indCond$cond1]), 1, function(x) sum(is.na(x)))
nNA2 = apply(as.matrix(qData[,indCond$cond2]), 1, function(x) sum(is.na(x)))
o <- order(((nNA1 +1)^2) / (nNA2 +1))
exprso <- qData[o,]

for (i in 1:nrow(exprso)){
    k <- order(exprso[i,indCond$cond1])
    exprso[i,rev(indCond$cond1)] <- exprso[i, k]
    .temp <- mean(exprso[i,rev(indCond$cond1)], na.rm = TRUE)
    exprso[i,which(!is.na(exprso[i,indCond$cond1]))] <- .temp
    
    k <- order(exprso[i,indCond$cond2])
    exprso[i,indCond$cond2] <- exprso[i, k+length(indCond$cond1)]
    .temp <- mean(exprso[i,indCond$cond2], na.rm = TRUE)
    exprso[i,length(indCond$cond1) + 
            which(!is.na(exprso[i,indCond$cond2]))] <- .temp
}


heatmap.DAPAR(exprso,
                col = colorRampPalette(c("yellow", "red"))(100),
                key=TRUE,
                srtCol= 0,
                labCol=conds,
                ylab = "Peptides / proteins",
                main = "MEC heatmap"
)

#heatmap_HC(exprso,col = colfunc(100),labCol=conds)
           
           
}





##' This method is a wrapper for the function \code{\link{hc_mvTypePlot2}} adapted to objects
##' of class \code{MSnSet}).

##' @title Distribution of observed values with respect to intensity values 
##' from a \code{MSnSet} object
##' @param obj An object of class \code{MSnSet}.
##' @param ... See \code{\link{hc_mvTypePlot2}} 
##' @return A scatter plot
##' @author Florence Combes, Samuel Wieczorek
##' @examples
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' wrapper.hc_mvTypePlot2(Exp1_R25_pept)
wrapper.hc_mvTypePlot2 <- function(obj,...){
    qData <- Biobase::exprs(obj)
    conds <- Biobase::pData(obj)[,"Condition"]
    hc_mvTypePlot2(qData, conds = conds,...)
}




##' This method shows density plots which represents the repartition of
##' Partial Observed Values for each replicate in the dataset.
##' The colors correspond to the different conditions (slot Condition in in the
##' dataset of class \code{MSnSet}).
##' The x-axis represent the mean of intensity for one condition and one
##' entity in the dataset (i. e. a protein) 
##' whereas the y-axis count the number of observed values for this entity
##' and the considered condition.
##' 
##' @title Distribution of Observed values with respect to intensity values
##' @param qData A dataframe that contains quantitative data.
##' @param conds A vector of the conditions (one condition per sample).
##' @param palette The different colors for conditions
##' @param typeofMV xxx
##' @param title The title of the plot
##' @return Density plots
##' @author Samuel Wieczorek
##' @examples
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' qData <- Biobase::exprs(Exp1_R25_pept)
##' conds <- Biobase::pData(Exp1_R25_pept)[,"Condition"]
##' hc_mvTypePlot2(qData, conds, title="POV distribution")
hc_mvTypePlot2 <- function(qData, conds, palette = NULL, typeofMV=NULL, title=NULL){
  if (is.null(conds)){return(NULL)}
  
    if (is.null(palette)){
              palette <- brewer.pal(length(unique(conds)),"Dark2")[1:length(unique(conds))]
    }else{
      if (length(palette) != length(unique(conds))){
        warning("The color palette has not the same dimension as the number of conditions")
        return(NULL)
      }
    }
  
    conditions <- conds
    mTemp <- nbNA <- nbValues <- matrix(rep(0,nrow(qData)*length(unique(conditions))), nrow=nrow(qData),
                                                     dimnames=list(NULL,unique(conditions)))
    dataCond <- data.frame()
    ymax <- 0
    series <- list()
    myColors <- NULL
    j <- 1 
    
    for (iCond in unique(conditions)){
        if (length(which(conditions==iCond)) == 1){
           
            mTemp[,iCond] <- qData[,which(conditions==iCond)]
            nbNA[,iCond] <- as.integer(is.OfType(qData[,which(conditions==iCond)]))
            nbValues[,iCond] <- length(which(conditions==iCond)) - nbNA[,iCond]
        } else {
            mTemp[,iCond] <- apply(qData[,which(conditions==iCond)], 1, mean, na.rm=TRUE)
            nbNA[,iCond] <- apply(qData[,which(conditions==iCond)],1,function(x) length(which(is.na(x) == TRUE)))
            nbValues[,iCond] <- length(which(conditions==iCond)) - nbNA[,iCond]
        }
        
        
        for (i in 1:length(which(conditions==iCond))){
                data <- mTemp[which(nbValues[, iCond] == i), iCond]
                tmp <- NULL    
                    if (length(data) >= 2)
                        {
                        tmp <- density(mTemp[which(nbValues[,iCond]==i),iCond])
                        tmp$y <- tmp$y + i
                        if (max(tmp$y) > ymax) { ymax <- max(tmp$y)}
                    }
                        series[[j]] <- tmp
                        myColors <- c(myColors, palette[which(unique(conditions)==iCond)])
                j <- j+1
            }

    }
    

    hc <-  highchart() %>%
        hc_title(text = title) %>%
        my_hc_chart(chartType = "spline", zoomType="xy") %>%

        hc_legend(align = "left", verticalAlign = "top",
                  layout = "vertical") %>%
        hc_xAxis(title = list(text = "Mean of intensities")) %>%
        hc_yAxis(title = list(text = "Number ov values"),
                 #categories = c(-1:3)
                 #min = 1, 
                # max = ymax,
                 tickInterval= 0.5
                 ) %>%
       # hc_colors(palette) %>%
        hc_tooltip(headerFormat= '',
                   pointFormat = "<b> {series.name} </b>: {point.y} ",
                   valueDecimals = 2) %>%
        my_hc_ExportMenu(filename = "POV_distribution") %>%
        hc_plotOptions(
            series=list(
                showInLegend = TRUE,
                animation=list(
                    duration = 100
                ),
                connectNulls= TRUE,
                marker=list(
                    enabled = FALSE)
                
            )
        )
    
for (i in 1:length(series)){
        hc <- hc_add_series(hc,
                            data = list_parse(data.frame(cbind(x = series[[i]]$x, 
                                                               y = series[[i]]$y))), 
                            showInLegend=FALSE,
                            color = myColors[i],
                            name=conds[i])
}
    
    # add three empty series for the legend entries. Change color and marker symbol
for (c in 1:length(unique(conds))){
      hc <-  hc_add_series(hc,data = data.frame(),
                           name = unique(conds)[c],
                           color = palette[c],
                           marker = list(symbol = "circle"),
                           type = "line")
}
        
hc
 return(hc)
}


