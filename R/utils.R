##' Returns the contents of the slot processing of an object of class \code{MSnSet}
##' 
##' @title Returns the contains of the slot processing  of an object of 
##' class \code{MSnSet}
##' @param  obj An object (peptides) of class \code{MSnSet}.
##' @return The slot processing of obj@processingData
##' @author Samuel Wieczorek
##' @examples
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' getProcessingInfo(Exp1_R25_pept)
getProcessingInfo <- function(obj){
return(obj@processingData@processing)
}

##' Returns the number of empty lines in a matrix.
##' 
##' @title Returns the number of empty lines in the data
##' @param qData A matrix corresponding to the quantitative data.
##' @return An integer
##' @author Samuel Wieczorek
##' @examples
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' qData <- Biobase::exprs(Exp1_R25_pept)
##' getNumberOfEmptyLines(qData)
getNumberOfEmptyLines <- function(qData){
n <- sum(apply(is.na(as.matrix(qData)), 1, all))
return (n)
}

##' Similar to the function \code{is.na} but focused on the equality with the paramter 'type'.
##' 
##' @title Similar to the function \code{is.na} but focused on the equality with the paramter 'type'.
##' @param data A data.frame
##' @param type The value to search in the dataframe
##' @return A boolean dataframe 
##' @author Samuel Wieczorek
##' @examples
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' obj <- Exp1_R25_pept
##' data <- Biobase::fData(obj)[,obj@experimentData@other$OriginOfValues]
##' is.OfType(data, "MEC")
is.OfType <- function(data, type){
  return (type == data)
}





##' Similar to the function \code{is.na} but focused on the equality with the missing 
##' values in the dataset (type 'POV' and 'MEC')
##' 
##' @title Similar to the function \code{is.na} but focused on the equality with the missing 
##' values in the dataset (type 'POV' and 'MEC')
##' @param data A data.frame
##' @return A boolean dataframe 
##' @author Samuel Wieczorek
##' @examples
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' obj <- Exp1_R25_pept
##' data <- Biobase::fData(obj)[,obj@experimentData@other$OriginOfValues]
##' is.MV(data)
is.MV <- function(data){
  #MV=is.OfType(data, "MV")
  POV=is.OfType(data, "POV")
  MEC=is.OfType(data, "MEC")
  
  df <- POV | MEC
  return (df)
}

##' Returns the possible number of values in lines in a matrix.
##' 
##' @title Returns the possible number of values in lines in the data
##' @param obj An object of class \code{MSnSet}
##' @param type xxxxxxx
##' @return An integer
##' @author Samuel Wieczorek
##' @examples
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' getListNbValuesInLines(Exp1_R25_pept)
getListNbValuesInLines <- function(obj, type="wholeMatrix"){
  if (is.null(obj)){return()}
  
  if(is.null(obj@experimentData@other$OriginOfValues)){
    ll <- seq(0,ncol(obj))
  }
  data <- Biobase::fData(obj)[,obj@experimentData@other$OriginOfValues]
  switch(type,
         wholeMatrix= {
           ll <- unique(ncol(data) - apply(is.MV(data), 1, sum))
           },
         allCond = {
                    tmp <- NULL
                    for (cond in unique(Biobase::pData(obj)$Condition)){
                     tmp <- c(tmp, length(which(Biobase::pData(obj)$Condition== cond)))
                  }
                  ll <- seq(0,min(tmp))
                  },
         atLeastOneCond = {
                   tmp <- NULL
                  for (cond in unique(Biobase::pData(obj)$Condition)){
                       tmp <- c(tmp, length(which(Biobase::pData(obj)$Condition== cond)))
                   }
                   ll <- seq(0,max(tmp))
                    }
         )
  
  return (sort(ll))
}



##' Returns a list for the two conditions where each slot is a vector of 
##' indices for the samples.
##' 
##' @title Gets the conditions indices.
##' @param conds A vector of strings containing the column "Condition" of 
##' the \code{pData()}.
##' @param cond1 A vector of Conditions (a slot in the \code{pData()} table) for
##' the condition 1.
##' @param cond2 A vector of Conditions (a slot in the \code{pData()} table) for
##' the condition 2.
##' @return A list with two slots \code{iCond1} and \code{iCond2} containing
##' respectively the indices of samples in the \code{pData()} table of the
##' dataset. 
##' @author Florence Combes, Samuel Wieczorek
##' @examples
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' conds <- Biobase::pData(Exp1_R25_pept)[,"Condition"]
##' getIndicesConditions(conds, "25fmol", "10fmol")
getIndicesConditions <- function(conds, cond1, cond2){
indCondition1 <- indCondition2 <- NULL

for(i in 1:length(cond1)){
    indCondition1 <- c(indCondition1,
                        which(conds == cond1[i]))
}
for(i in 1:length(cond2)){
    indCondition2 <- c(indCondition2,
                        which(conds == cond2[i]))
}

return(list(iCond1 = indCondition1, iCond2 = indCondition2))
}



##' Customise the contextual menu of highcharts plots.
##' 
##' @title Customised contextual menu of highcharts plots
##' @param hc A highcharter object
##' @param filename The filename under which the plot has to be saved
##' @return A contextual menu for highcharts plots
##' @author Samuel Wieczorek
##' @examples
##' library("highcharter")
##' hc <- highchart() 
##' hc_chart(hc,type = "line") 
##' hc_add_series(hc,data = c(29, 71, 40))
##' my_hc_ExportMenu(hc,filename='foo')
my_hc_ExportMenu <- function(hc, filename){
  hc_exporting(hc, enabled=TRUE,
               filename = filename,
               buttons= list(
                 contextButton= list(
                   menuItems= list('downloadPNG', 'downloadSVG','downloadPDF')
                 )
               )
  )
}



##' Customise the resetZoomButton of highcharts plots.
##' 
##' @title Customised resetZoomButton of highcharts plots
##' @param hc A highcharter object
##' @param chartType The type of the plot
##' @param zoomType The type of the zoom (one of "x", "y", "xy", "None")
##' @return A highchart plot
##' @author Samuel Wieczorek
##' @examples
##' library("highcharter")
##' hc <- highchart() 
##' hc_chart(hc,type = "line") 
##' hc_add_series(hc,data = c(29, 71, 40))
##' my_hc_ExportMenu(hc,filename='foo')
my_hc_chart <- function(hc,  chartType,zoomType="None"){
  hc %>% 
    hc_chart(type = chartType, 
           zoomType=zoomType,
           showAxes = TRUE,
           resetZoomButton= list(
             position = list(
               align= 'left',
               verticalAlign = 'top')
           ))
}



##' This function retrieves the indices of non-zero elements in sparse matrices
##' of class dgCMatrix from package Matrix. This function is largely inspired from 
##' the package \code{RINGO}
##' 
##' @title Retrieve the indices of non-zero elements in sparse matrices
##' @param x A sparse matrix of class dgCMatrix
##' @return A two-column matrix
##' @author Samuel Wieczorek
##' @examples
##' library(Matrix)
##' mat <- Matrix(c(0,0,0,0,0,1,0,0,1,1,0,0,0,0,1),nrow=5, byrow=TRUE, sparse=TRUE)
##' res <- nonzero(mat)
nonzero <- function(x){
    ## function to get a two-column matrix containing the indices of the
    ### non-zero elements in a "dgCMatrix" class matrix
    
    stopifnot(inherits(x, "dgCMatrix"))
    if (all(x@p == 0))
        return(matrix(0, nrow=0, ncol=2,
                      dimnames=list(character(0), c("row","col"))))
    res <- cbind(x@i+1, rep(seq(dim(x)[2]), diff(x@p)))
    colnames(res) <- c("row", "col")
    res <- res[x@x != 0, , drop = FALSE]
    return(res)
}



##' This function overloads the brackets to select lines of dataframes in a MSnset file. It takes 
##' into account the slots experimentData / other / OriginOfValues
##' 
##' @title Selects lines of dataframes in a MSnset file
##' @param obj A MSnset object
##' @param lineIndices The indices of lines to be extracted
##' @return A MSnset object 
##' @author Samuel Wieczorek
##' @examples
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' res <- tabOperator(Exp1_R25_pept, c(1:10))
# tabOperator <- function(obj, lineIndices){
#     
#     tmp <- obj[lineIndices]
#     if (!is.null(tmp@experimentData@other$OriginOfValues)){
#         tmp@experimentData@other$OriginOfValues <- tmp@experimentData@other$OriginOfValues[lineIndices,]
#     }
#     
#    
#     return(tmp)
# }
