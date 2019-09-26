


##' Returns the percentage of missing values in the quantitative
##' data (\code{exprs()} table of the dataset).
##' 
##' @title Percentage of missing values
##' @param obj An object of class \code{MSnSet}.
##' @return A floating number
##' @author Florence Combes, Samuel Wieczorek
##' @examples
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' getPourcentageOfMV(Exp1_R25_pept)
getPourcentageOfMV <- function(obj){

  df <- data.frame(Biobase::exprs(obj))
  
NA.count<-apply(df, 2, 
                function(x) length(which(is.na(data.frame(x))==TRUE)) )


pourcentage <- 100 * round(sum(NA.count) /(nrow(df)* ncol(df)), digits=4)

return(pourcentage)
}

##' Returns the number of lines, in a given column, where content matches 
##' the prefix.
##' 
##' @title Number of lines with prefix
##' @param obj An object of class \code{MSnSet}.
##' @param name The name of a column.
##' @param prefix A string
##' @return An integer
##' @author Samuel Wieczorek
##' @examples
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' getNumberOf(Exp1_R25_pept, "Potential.contaminant", "+")
getNumberOf <- function(obj, name=NULL, prefix=NULL){
if (is.null(name) || is.null(prefix) || (name=="") || (prefix=="")){
    return(0)}
if (!(is.null(name) || !is.null(name=="")) 
    && (is.null(prefix) || (prefix==""))){return(0)}

if(nchar(prefix) > 0){
    count <- length(which(substr(Biobase::fData(obj)[,name], 0, 1) == prefix))
} else { count <- 0}

return(count)
}


##' This function removes lines in the dataset based on numerical conditions.
##' 
##' @title Removes lines in the dataset based on numerical conditions.
##' @param obj An object of class \code{MSnSet}.
##' @param name The name of the column that correspond to the line to filter
##' @param value A number 
##' @param operator A string
##' @return An list of 2 items :
##' obj : an object of class \code{MSnSet} in which the lines have been deleted
##' deleted : an object of class \code{MSnSet} which contains the deleted lines 
##' @author Samuel Wieczorek
##' @examples
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' NumericalFiltering(Exp1_R25_pept, 'A_Count', '6', '>=')
NumericalFiltering <- function(obj, name=NULL, value=NULL, operator=NULL){
  if ((is.null(name) || (name == ""))) {return(NULL)}
  
  deleted <- NULL
  ind <- NULL
  ind <- NumericalgetIndicesOfLinesToRemove(obj,name, value, operator)
  
  if (!is.null(ind) && (length(ind) > 0)){
    deleted <- obj[ind]
    
    obj <- deleteLinesFromIndices(obj, ind, 
                                  paste("\"", 
                                        length(ind), 
                                        " lines were removed from dataset.\"",
                                        sep="")
    )
    
  }
  
  return(list(obj=obj, deleted=deleted))
}




##' This function returns the indice of the lines to delete, based on a 
##' prefix string
##' 
##' @title Get the indices of the lines to delete, based on a prefix string
##' @param obj An object of class \code{MSnSet}.
##' @param name The name of the column that correspond to the data to filter
##' @param value xxxx
##' @param operator A xxxx
##' @return A vector of integers.
##' @author Samuel Wieczorek
##' @examples
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' NumericalgetIndicesOfLinesToRemove(Exp1_R25_pept, "A.Count", value="6", operator='==')
NumericalgetIndicesOfLinesToRemove <- function(obj, name=NULL, value=NULL, operator=NULL)
{
  if ((value == "") || is.null(value)|| (operator=="") || is.null(operator)) {
    # warning ("No change was made")
    return (NULL)}
  
  data <- Biobase::fData(obj)[,name]
  ind <- which(eval(parse(text=paste0("data", operator, value))))
  
  return(ind)
}


##' Plots a barplot of proportion of contaminants and reverse. Same as the function
##' \code{proportionConRev} but uses the package \code{highcharter}
##' 
##' @title Barplot of proportion of contaminants and reverse
##' @param nBoth The number of both contaminants and reverse identified in the dataset.
##' @param nCont The number of contaminants identified in the dataset.
##' @param nRev The number of reverse entities identified in the dataset.
##' @param lDataset The total length (number of rows) of the dataset
##' @return A barplot
##' @author Samuel Wieczorek
##' @examples
##' proportionConRev_HC(10, 20, 100)
proportionConRev_HC <- function(nBoth = 0, nCont=0, nRev=0, lDataset=0){
    if (is.null(nCont) && is.null(nBoth) && is.null(nRev) && is.null(lDataset)){return(NULL)}
    
    total <- nBoth + nCont + nRev + lDataset
    pctGood <- 100 * round(lDataset/total,  digits=4)
    pctBoth <- 100 * round(nBoth/total,  digits=4)
    pctContaminants <- 100 * round(nCont/total,  digits=4)
    pctReverse <- 100 * round(nRev/total,  digits=4)
    
    counts <- c(lDataset, nCont, nRev, nBoth)
    slices <- c(pctGood, pctContaminants, pctReverse ,pctBoth) 
    lbls <- c("Quantitative data", "Contaminants", "Reverse", "Both contaminants & Reverse")
    #pct <- c(pctGood, pctContaminants, pctReverse  ,pctBoth)
    lbls <- paste(lbls, " (", counts, " lines)", sep="") 

    mydata <- data.frame(test=c(pctGood, pctContaminants, pctReverse ,pctBoth))
    
    highchart() %>% 
        my_hc_chart(chartType = "bar") %>% 
        hc_yAxis(title = list(text = "Pourcentage")) %>% 
        hc_xAxis(categories=lbls) %>% 
        hc_legend(enabled = FALSE) %>%
        hc_plotOptions(column = list(
            dataLabels = list(enabled = TRUE),
            stacking = "normal",
            enableMouseTracking = FALSE)
        ) %>% 
        hc_add_series(data  = mydata$test,
                      dataLabels = list(enabled = TRUE, format='{point.y}%'),
                  colorByPoint = TRUE) %>%
      my_hc_ExportMenu(filename = "contaminants")


}



##' This function removes lines in the dataset based on a prefix string.
##' 
##' @title Removes lines in the dataset based on a prefix string.
##' @param obj An object of class \code{MSnSet}.
##' @param idLine2Delete The name of the column that correspond to the 
##' data to filter
##' @param prefix A character string that is the prefix to find in the data
##' @return An object of class \code{MSnSet}.
##' @author Samuel Wieczorek
##' @examples
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' removeLines(Exp1_R25_pept, "Potential.contaminant")
##' removeLines(Exp1_R25_pept, "Reverse")
removeLines <- function(obj, idLine2Delete=NULL, prefix=NULL){
if ((prefix == "") || is.null(prefix)) {
    #warning ("No change was made")
    return (obj)}
    t <- (prefix == substring(Biobase::fData(obj)[,idLine2Delete],1,nchar(prefix)))
    ind <- which( t== TRUE)
    obj <- obj[-ind ]

return(obj)
}


##' This function removes lines in the dataset based on prefix strings (contaminants, reverse or both).
##' 
##' @title Removes lines in the dataset based on a prefix strings (contaminants, reverse or both).
##' @param obj An object of class \code{MSnSet}.
##' @param idCont2Delete The name of the column that correspond to the 
##' contaminants to filter
##' @param prefix_Cont A character string that is the prefix for the contaminants to find in the data
##' @param idRev2Delete The name of the column that correspond to the 
##' reverse data to filter
##' @param prefix_Rev A character string that is the prefix for the reverse to find in the data
##' @return An list of 4 items :
##' obj : an object of class \code{MSnSet} in which the lines have been deleted
##' deleted.both : an object of class \code{MSnSet} which contains the deleted lines 
##' corresponding to both contaminants and reverse, 
##' deleted.contaminants : n object of class \code{MSnSet} which contains the deleted lines 
##' corresponding to contaminants, 
##' deleted.reverse : an object of class \code{MSnSet} which contains the deleted lines 
##' corresponding to reverse,
##' @author Samuel Wieczorek
##' @examples
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' StringBasedFiltering(Exp1_R25_pept, 'Potential.contaminant', '+', 'Reverse', '+')
StringBasedFiltering <- function(obj, 
                                 idCont2Delete=NULL, prefix_Cont=NULL, 
                                 idRev2Delete=NULL, prefix_Rev=NULL){
    
    deleted.both <- deleted.contaminants <- deleted.reverse <- NULL
    
    ##
    ##Search for both
    ##
    if ((!is.null(idCont2Delete) || (idCont2Delete != "")) &&
        (!is.null(idRev2Delete) || (idRev2Delete != ""))) {
        indContaminants <- indReverse <- indBoth <- NULL
        indContaminants <- getIndicesOfLinesToRemove(obj,idCont2Delete,  prefix_Cont)
        indReverse <- getIndicesOfLinesToRemove(obj, idRev2Delete, prefix_Rev)
        indBoth <- intersect(indContaminants, indReverse)
        
        if (!is.null(indBoth) && (length(indBoth) > 0)){
                deleted.both <- obj[indBoth]
                obj <- deleteLinesFromIndices(obj, indBoth, 
                                               paste("\"", 
                                                     length(indBoth), 
                                                     " both contaminants and reverse were removed from dataset.\"",
                                                     sep="")
                )
            }
    }
    
    ##
    ##Search for contaminants
    ##
    if ((!is.null(idCont2Delete) || (idCont2Delete != ""))) {
        indContaminants <- NULL
        indContaminants <- getIndicesOfLinesToRemove(obj,idCont2Delete,  prefix_Cont)
        
        if (!is.null(indContaminants) && (length(indContaminants) > 0)){
                deleted.contaminants <- obj[indContaminants]

                obj <- deleteLinesFromIndices(obj, indContaminants, 
                                               paste("\"", 
                                                     length(indContaminants), 
                                                     " contaminants were removed from dataset.\"",
                                                     sep="")
                )

        }
    }
    
    
    ##
    ## Search for reverse
    ##
    if ((!is.null(idRev2Delete) || (idRev2Delete != ""))) {
        indReverse <- getIndicesOfLinesToRemove(obj, idRev2Delete, prefix_Rev)
        
        if (!is.null(indReverse)){
            if (length(indReverse) > 0)  {
                deleted.reverse <- obj[indReverse]

                obj <- deleteLinesFromIndices(obj, indReverse, 
                                               paste("\"", 
                                                     length(indReverse), 
                                                     " reverse were removed from dataset.\"",
                                                     sep="")
                )

            }
        }
    }
    
    
    return(list(obj=obj, 
                deleted.both=deleted.both, 
                deleted.contaminants=deleted.contaminants, 
                deleted.reverse=deleted.reverse))
}






##' This function removes lines in the dataset based on prefix strings.
##' 
##' @title Removes lines in the dataset based on a prefix strings.
##' @param obj An object of class \code{MSnSet}.
##' @param cname The name of the column that correspond to the line to filter
##' @param tag A character string that is the prefix for the contaminants to find in the data
##' @return An list of 4 items :
##' obj : an object of class \code{MSnSet} in which the lines have been deleted
##' deleted : an object of class \code{MSnSet} which contains the deleted lines 
##' @author Samuel Wieczorek
##' @examples
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' StringBasedFiltering2(Exp1_R25_pept, 'Potential.contaminant', '+')
StringBasedFiltering2 <- function(obj, cname=NULL, tag=NULL){
  
  deleted <- NULL
  
  ##
  ##Search for contaminants
  ##
  if ((!is.null(cname) || (cname != ""))) {
    ind <- NULL
    ind <- getIndicesOfLinesToRemove(obj,cname,  tag)
    
    if (!is.null(ind) && (length(ind) > 0)){
      deleted <- obj[ind]
      
      obj <- deleteLinesFromIndices(obj, ind, 
                                    paste("\"", 
                                          length(ind), 
                                          " contaminants were removed from dataset.\"",
                                          sep="")
      )
      
    }
  }

  return(list(obj=obj, deleted=deleted))
}








##' This function returns the indice of the lines to delete, based on a 
##' prefix string
##' 
##' @title Get the indices of the lines to delete, based on a prefix string
##' @param obj An object of class \code{MSnSet}.
##' @param idLine2Delete The name of the column that correspond to the data 
##' to filter
##' @param prefix A character string that is the prefix to find in the data
##' @return A vector of integers.
##' @author Samuel Wieczorek
##' @examples
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' getIndicesOfLinesToRemove(Exp1_R25_pept, "Potential.contaminant", prefix="+")
getIndicesOfLinesToRemove <- function(obj, idLine2Delete=NULL, prefix=NULL)
{
if ((prefix == "") || is.null(prefix)) {
   # warning ("No change was made")
    return (NULL)}
t <- (prefix == substring(Biobase::fData(obj)[,idLine2Delete],1,nchar(prefix)))
ind <- which( t== TRUE)
return(ind)
}

##' Filters the lines of \code{exprs()} table with conditions on the number
##' of missing values.
##' The user chooses the minimum amount of intensities that is acceptable and
##' the filter delete lines that do not respect this condition.
##' The condition may be on the whole line or condition by condition.
##' 
##' The different methods are :
##' "wholeMatrix": given a threshold \code{th}, only the lines that contain
##' at least \code{th} values are kept.
##' "allCond": given a threshold \code{th}, only the lines which contain
##' at least \code{th} values for each of the conditions are kept.
##' "atLeastOneCond": given a threshold \code{th}, only the lines that contain
##' at least \code{th} values, and for at least one condition, are kept.
##' 
##' @title Filter lines in the matrix of intensities w.r.t. some criteria
##' @param obj An object of class \code{MSnSet} containing
##' quantitative data.
##' @param type Method used to choose the lines to delete.
##' Values are : "None", "wholeMatrix", "allCond", "atLeastOneCond"
##' @param th An integer value of the threshold
##' @param processText A string to be included in the \code{MSnSet}
##' object for log. 
##' @return An instance of class \code{MSnSet} that have been filtered.
##' @author Florence Combes, Samuel Wieczorek
##' @examples
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' mvFilter(Exp1_R25_pept, "wholeMatrix", 2)
mvFilter <- function(obj,type, th, processText=NULL )
{
    #Check parameters
    paramtype<-c("None", "wholeMatrix", "allCond", "atLeastOneCond") 
    if (sum(is.na(match(type, paramtype)==TRUE))>0){
        warning("Param type is not correct.")
        return (NULL)
    }

    paramth<-c(seq(0, nrow(Biobase::pData(obj)), 1))
    if (sum(is.na(match(th, paramth)==TRUE))>0){
        warning("Param th is not correct.")
        return (NULL)
    }
    
    if(!is.integer(th)){th <- as.integer(th)}

    keepThat <- mvFilterGetIndices(obj,type, th)

obj <- obj[keepThat]

    obj@processingData@processing <- 
        c(obj@processingData@processing, processText)
    return(obj)
}


##' Filters the lines of \code{exprs()} table with conditions on the number
##' of missing values.
##' The user chooses the minimum amount of intensities that is acceptable and
##' the filter delete lines that do not respect this condition.
##' The condition may be on the whole line or condition by condition.
##' 
##' The different methods are :
##' "wholeMatrix": given a threshold \code{th}, only the lines that contain
##' at least \code{th} values are kept.
##' "allCond": given a threshold \code{th}, only the lines which contain
##' at least \code{th} values for each of the conditions are kept.
##' "atLeastOneCond": given a threshold \code{th}, only the lines that contain
##' at least \code{th} values, and for at least one condition, are kept.
##' 
##' @title Filter lines in the matrix of intensities w.r.t. some criteria
##' @param obj An object of class \code{MSnSet} containing
##' quantitative data.
##' @param keepThat A vector of integers which are the indices of lines to 
##' keep.
##' @param processText A string to be included in the \code{MSnSet}
##' object for log. 
##' @return An instance of class \code{MSnSet} that have been filtered.
##' @author Florence Combes, Samuel Wieczorek
##' @examples
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' mvFilterFromIndices(Exp1_R25_pept, c(1:10))
mvFilterFromIndices <- function(obj,keepThat=NULL, processText="" )
{

if (is.null(keepThat)) {return(obj)}
obj <- obj[keepThat]

# if (!is.null(obj@experimentData@other$OriginOfValues)){
#     obj@experimentData@other$OriginOfValues <- obj@experimentData@other$OriginOfValues[keepThat,]
# }
obj@processingData@processing <- 
    c(obj@processingData@processing, processText)

return(obj)
}

##' Delete the lines of \code{exprs()} table identified by their indice.
##' 
##' @title Delete the lines in the matrix of intensities and the metadata table
##' given their indice.
##' @param obj An object of class \code{MSnSet} containing
##' quantitative data.
##' @param deleteThat A vector of integers which are the indices of lines to 
##' delete.
##' @param processText A string to be included in the \code{MSnSet}
##' object for log. 
##' @return An instance of class \code{MSnSet} that have been filtered.
##' @author Florence Combes, Samuel Wieczorek
##' @examples
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' deleteLinesFromIndices(Exp1_R25_pept, c(1:10))
deleteLinesFromIndices <- function(obj,deleteThat=NULL, processText="" )
{
    
    if (is.null(deleteThat)) {return(obj)}
    obj <- obj[-deleteThat]
    
    obj@processingData@processing <-  c(obj@processingData@processing, processText)
    if (grepl("contaminants", processText)){obj@experimentData@other$contaminantsRemoved <- TRUE}
    if (grepl("reverse", processText)){obj@experimentData@other$reverseRemoved <- TRUE }
    return(obj)
}


##' Returns the indices of the lines of \code{exprs()} table to delete w.r.t. 
##' the conditions on the number of missing values.
##' The user chooses the minimum amount of intensities that is acceptable and
##' the filter delete lines that do not respect this condition.
##' The condition may be on the whole line or condition by condition.
##' 
##' The different methods are :
##' "wholeMatrix": given a threshold \code{th}, only the lines that contain
##' at least \code{th} values are kept.
##' "allCond": given a threshold \code{th}, only the lines which contain
##' at least \code{th} values for each of the conditions are kept.
##' "atLeastOneCond": given a threshold \code{th}, only the lines that contain
##' at least \code{th} values, and for at least one condition, are kept.
##' 
##' @title Filter lines in the matrix of intensities w.r.t. some criteria
##' @param obj An object of class \code{MSnSet} containing
##' quantitative data.
##' @param type Method used to choose the lines to delete.
##' Values are : "None", "EmptyLines", "wholeMatrix", "allCond", "atLeastOneCond"
##' @param th An integer value of the threshold
##' @return An vector of indices that correspond to the lines to keep.
##' @author Florence Combes, Samuel Wieczorek
##' @examples
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' mvFilterGetIndices(Exp1_R25_pept, "wholeMatrix", 2)
mvFilterGetIndices <- function(obj,type, th)
{
#Check parameters
paramtype<-c("None", "EmptyLines", "wholeMatrix", "allCond", "atLeastOneCond") 
if (sum(is.na(match(type, paramtype)==TRUE))>0){
    warning("Param type is not correct.")
    return (NULL)
}

paramth<-c(seq(0, nrow(Biobase::pData(obj)), 1))
if (sum(is.na(match(th, paramth)==TRUE))>0){
    warning("Param th is not correct.")
    return (NULL)
}

keepThat <- NULL
if (is.null(obj@experimentData@other$OriginOfValues)){
    data <- Biobase::exprs(obj)
} else {
  data <- dplyr::select(fData(obj),obj@experimentData@other$OriginOfValues)
}

if (type == "None"){
    keepThat <- seq(1:nrow(data))
} else if (type == "EmptyLines"){
    keepThat <- which(apply(!is.MV(data), 1, sum) >= 1)
} else if (type == "wholeMatrix"){
    keepThat <- which(apply(!is.MV(data), 1, sum) >= th)
} else if (type == "atLeastOneCond" || type == "allCond"){
    
    conditions <- unique(Biobase::pData(obj)$Condition)
    nbCond <- length(conditions)
    keepThat <- NULL
    s <- matrix(rep(0, nrow(data)*nbCond),nrow=nrow(data), 
                ncol=nbCond)
    
    for (c in 1:nbCond){
        ind <- which(Biobase::pData(obj)$Condition == conditions[c])
        if (length(ind) == 1){
            s[,c] <- (!is.MV(data[,ind]) >= th)}
        else {
            s[,c] <- (apply(!is.MV(data[,ind]), 1, sum) >= th)
        }
    }
    
    
    if (type == "allCond") {
        keepThat <- which(rowSums(s) == nbCond)
    }
    else if (type == "atLeastOneCond") {
        keepThat <- which(rowSums(s) >= 1)
    }
}
return(keepThat)
}
