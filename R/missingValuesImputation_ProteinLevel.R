


##' This method finds the LAPALA in a dataset.
##'
##' @title Finds the LAPALA into a \code{MSnSet} object
##' @param obj An object of class \code{MSnSet}.
##' @return A data.frame that contains the indexes of LAPALA
##' @author Samuel Wieczorek
##' @examples
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' obj <- Exp1_R25_pept[1:1000]
##' lapala <- findMECBlock(obj)
findMECBlock <- function(obj){
    
    conditions <- unique(Biobase::pData(obj)$Condition)
    nbCond <- length(conditions)
    
    s <- data.frame()
    
    for (cond in 1:nbCond){
        ind <- which(Biobase::pData(obj)$Condition == conditions[cond])
        lNA <- which(apply(is.na(Biobase::exprs(obj)[,ind]), 1, sum)==length(ind))
        if (length(lNA) > 0)
        {
            tmp <- data.frame(cond,which(apply(is.na(Biobase::exprs(obj)[,ind]), 1, sum)==length(ind)))
            names(tmp) <- c("Condition", "Line")
            s <- rbind(s,tmp)
            
        }
    }
    return(s)
}


##' This method is used to put back the LAPALA that have been identified previously
##'
##' @title Put back LAPALA into  a \code{MSnSet} object
##' @param obj An object of class \code{MSnSet}.
##' @param MECIndex A data.frame that contains index of MEC (see findMECBlock) .
##' @return The object \code{obj} where LAPALA have been reintroduced
##' @author Samuel Wieczorek
##' @examples
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' obj <- Exp1_R25_pept[1:1000]
##' lapala <- findMECBlock(obj)
##' obj <- wrapper.impute.detQuant(obj)
##' obj <- reIntroduceMEC(obj, lapala)
reIntroduceMEC <- function(obj, MECIndex){
    
    for (i in 1:nrow(MECIndex))
    {
        conditions <- unique(Biobase::pData(obj)$Condition)
        replicates <- which(Biobase::pData(obj)$Condition == conditions[MECIndex[i,"Condition"]])
        Biobase::exprs(obj)[MECIndex[i,"Line"], as.vector(replicates)] <- NA
    }
    return(obj)
}



##' This method is a wrapper for
##' objects of class \code{MSnSet} and imputes missing values with a fixed value.
##' This function imputes the missing values condition by condition.
##'
##' @title KNN missing values imputation from a \code{MSnSet} object
##' @param obj An object of class \code{MSnSet}.
##' @param K the number of neighbors.
##' @return The object \code{obj} which has been imputed
##' @author Samuel Wieczorek
##' @examples
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' wrapper.impute.KNN(Exp1_R25_pept[1:1000], 3)
wrapper.impute.KNN <- function(obj, K){
    
    if (is.null(obj)){return(NULL)}
    data <- Biobase::exprs(obj)
    
    conditions <- unique(Biobase::pData(obj)$Condition)
    nbCond <- length(conditions)
    
    
    for (cond in 1:nbCond){
        ind <- which(Biobase::pData(obj)$Condition == conditions[cond])
        resKNN <- impute.knn(Biobase::exprs(obj)[,ind] ,k = K, rowmax = 0.99, colmax = 0.99, maxp = 1500, rng.seed = sample(1:1000,1))
        Biobase::exprs(obj)[,ind] <- resKNN[[1]]
    }
    
    
    return(obj)
}



##' This method is a wrapper to
##' objects of class \code{MSnSet} and imputes missing values with a fixed value.
##'
##' @title Missing values imputation from a \code{MSnSet} object
##' @param obj An object of class \code{MSnSet}.
##' @param fixVal A float .
##' @return The object \code{obj} which has been imputed
##' @author Samuel Wieczorek
##' @examples
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' wrapper.impute.fixedValue(Exp1_R25_pept[1:1000], 0.001)
wrapper.impute.fixedValue <- function(obj, fixVal){
    
    Biobase::exprs(obj)[is.na(exprs(obj))] <- fixVal
    return (obj)
}
















##' This method is a wrapper to the function \code{impute.pa} of the package
##' \code{imp4p} adapted to an object of class \code{MSnSet}.
##'
##' @title Imputation of peptides having no values in a biological condition.
##' @param obj An object of class \code{MSnSet}.
##' @param q.min Same as the function \code{impute.pa} in the package \code{imp4p}
##' @return The \code{exprs(obj)} matrix with imputed values instead of missing values.
##' @author Samuel Wieczorek
##' @examples
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' dat <- mvFilter(Exp1_R25_pept[1:1000], type="allCond", th = 1)
##' dat <- wrapper.impute.pa(dat)
wrapper.impute.pa <- function(obj, q.min = 0.025){
    cond <- as.factor(Biobase::pData(obj)$Condition)
    res <- impute.pa(Biobase::exprs(obj), conditions=cond, q.min = q.min, q.norm=3,  eps=0)
    Biobase::exprs(obj) <- res[["tab.imp"]]
    return (obj)
}




##' This method is a wrapper of the function \code{\link{impute.detQuant}} for objects
##' of class \code{MSnSet} 
##' 
##' @title Wrapper of the function \code{\link{impute.detQuant}} for objects
##' of class \code{MSnSet}
##' @param obj An instance of class \code{MSnSet}
##' @param qval An expression set containing quantitative values of various replicates
##' @param factor A scaling factor to multiply the imputation value with 
##' @return An imputed instance of class \code{MSnSet}
##' @author Samuel Wieczorek
##' @examples
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' wrapper.impute.detQuant(Exp1_R25_pept)
wrapper.impute.detQuant <- function(obj, qval=0.025, factor=1){
    if (is.null(obj)){return(NULL)}
    
    qData <- Biobase::exprs(obj)
    values <- getQuantile4Imp(qData, qval, factor)
    
    Biobase::exprs(obj) <- impute.detQuant(qData, values$shiftedImpVal)
    msg <- "Missing values imputation using deterministic quantile"
    obj@processingData@processing <- c(obj@processingData@processing,msg)
    
    obj@experimentData@other$imputation.method <- "detQuantile"
    return(obj)
}




##' This method returns the q-th quantile of each colum of an expression set, up to a scaling factor
##'
##' @title Quantile imputation value definition
##' @param qData An expression set containing quantitative values of various replicates
##' @param qval The quantile used to define the imputation value
##' @param factor A scaling factor to multiply the imputation value with
##' @return A list of two vectors, respectively containing the imputation values and the rescaled imputation values
##' @author Thomas Burger
##' @examples
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' qData <- Biobase::exprs(Exp1_R25_pept)
##' getQuantile4Imp(qData) 
getQuantile4Imp <- function(qData, qval=0.025, factor=1){
    r1 <- apply(qData, 2, quantile, qval, na.rm=TRUE)
    r2 <- r1*factor
    return(list(ImpVal = r1, shiftedImpVal = r2))
}




##' This method replaces each missing value by a given value
##'
##' @title Deterministic imputation
##' @param qData An expression set containing quantitative or missing values
##' @param values A vector with as many elements as the number of colums of qData
##' @return An imputed dataset
##' @author Thomas Burger
##' @examples
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' qData <- Biobase::exprs(Exp1_R25_pept)
##' values <- getQuantile4Imp(qData)$shiftedImpVal
##' impute.detQuant(qData, values) 
impute.detQuant <- function(qData, values){
    for(i in 1:dim(qData)[2]){
        col <- qData[,i]
        col[which(is.na(col))] <- values[i]
        qData[,i] <- col
    }
    return(qData)
}


##' This method is a wrapper to the function \code{impute.slsa} of the package
##' \code{imp4p} adapted to an object of class \code{MSnSet}.
##'
##' @title Imputation of peptides having no values in a biological condition.
##' @param obj An object of class \code{MSnSet}.
##' @return The \code{exprs(obj)} matrix with imputed values instead of missing values.
##' @author Samuel Wieczorek
##' @examples
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' dat <- mvFilter(Exp1_R25_pept[1:1000], type="allCond", th = 1)
##' dat <- wrapper.impute.slsa(dat)
wrapper.impute.slsa <- function(obj){
    cond <- as.factor(Biobase::pData(obj)$Condition)
    
    res <- impute.slsa(Biobase::exprs(obj), conditions=cond, nknn=15, selec="all", weight=1,
                       ind.comp=1)
    
    Biobase::exprs(obj) <-res
    return (obj)
}




