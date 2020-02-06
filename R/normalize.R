##' Provides several methods to normalize quantitative data from
##' a \code{MSnSet} object.
##' They are organized in six main families : GlobalQuantileAlignement, sumByColumns, QuantileCentering, MeanCentering, LOESS, vsn
##' For the first family, there is no type.
##' For the five other families, two type categories are available :
##' "Overall" which means that the value for each protein
##' (ie line in the expression data tab) is computed over all the samples ;
##' "within conditions" which means that the value for each protein
##' (ie line in the \code{exprs()} data tab) is computed condition
##' by condition.
##' Normalization can be based only on a subset of protein. Proteins with NA in this subset are ignored 
##'
##' @title Normalisation
##' @param obj An object of class \code{MSnSet}.
##' @param method One of the following : "GlobalQuantileAlignment" (for
##' normalizations of important magnitude), "SumByColumns", "QuantileCentering",
##' "Mean Centering", "LOESS" and "vsn".
##' @param type For the method "Global Alignment", the parameters are:
##' "sum by columns": operates on the original scale (not the log2 one) and propose
##' to normalize each abundance by the total abundance of the sample (so as to focus
##' on the analyte proportions among each sample).
##' "Alignment on all quantiles": proposes to align the quantiles of all the
##' replicates; practically it amounts to replace abundances by order statistics.
##' For the two other methods, the parameters are "overall" (shift all the
##' sample distributions at once) or "within conditions" (shift the sample
##' distributions within each condition at a time).
##' @param scaling A boolean that indicates if the variance of the data have to
##' be forced to unit (variance reduction) or not.
##' @param quantile A float that corresponds to the quantile used to align the
##' data.
##' @param span parameter for LOESS method
##' @param subset.norm A vector of index indicating rows to be used for normalization
##' @return An instance of class \code{MSnSet} where the quantitative
##' data in the \code{exprs()} tab has been normalized.
##' @author Samuel Wieczorek, Thomas Burger, Helene Borges, Anais Courtier
##' @examples
##' utils::data(Exp1_R25_pept, package='DAPARdata')
##' wrapper.normalizeD(Exp1_R25_pept[1:1000], "QuantileCentering", "within conditions",subset.norm=1:10)
wrapper.normalizeD <- function(obj, method, type=NULL, scaling=FALSE, quantile=0.15, span = 0.7,
                               subset.norm=NULL){
  parammethod<-c("GlobalQuantileAlignment",
                 "SumByColumns",
                 "QuantileCentering",
                 "MeanCentering",
                 "LOESS",
                 "vsn")
  
  if (sum(is.na(match(method, parammethod)==TRUE))>0){
    warning("Parameter method is not correct")
    return (NULL)
  }
  
  paramtype<-c(NULL,"overall", "within conditions")
  if (sum(is.na(match(type, paramtype)==TRUE))>0){
    warning("Parameter type is not correct")
    return (NULL)
  }
  # remove proteins with missing-value in normalization subset
  if(!is.null(subset.norm)){
    subset.norm=subset.norm[complete.cases(Biobase::exprs(obj))[subset.norm]]
  }
  # if subset.norm is null, or if it contained only protein with NA, normalization on all proteins
  if(is.null(subset.norm) | length(subset.norm)<1){
    subset.norm=1:nrow(Biobase::exprs(obj))
  }
  
  
  #.temp <- qData
  #if (!is.null(.temp)){
  #  data <- .temp
  
  
  
  
  conds <- Biobase::pData(obj)[,"Condition"]
  qData <- Biobase::exprs(obj)
  msg_method <- paste("Normalisation using method =", method,  sep="")
  msg_type <- paste("With type =", type,  sep="")
  
  switch(method,
         GlobalQuantileAlignment = {
#require(preprocessCore)
           Biobase::exprs(obj) <- preprocessCore::normalize.quantiles(qData)
           dimnames(Biobase::exprs(obj)) <- list(rownames(qData),colnames(qData))
           obj@processingData@processing <- c(obj@processingData@processing, msg_method, msg_type)
           obj@experimentData@other$normalizationMethod <- method
           
         },
         SumByColumns = {
           t <- 2^(Biobase::exprs(obj))
           
           if (type == "overall"){
             #if normalization on only one protein,colSums can't be applied
             if(length(subset.norm)==1){
               sums_cols=t[subset.norm,]
             }else{
               sums_cols <- colSums(t[subset.norm,], na.rm=TRUE)
             }
             #normalisation
             for ( i in 1:nrow(t)) {
               t[i, ] <- (t[i, ] / sums_cols)*median(sums_cols)
             }
           } else if (type == "within conditions"){
             for (l in unique(conds)) {
               indices <- which(conds== l)
               #if normalization on only one protein,colSums can't be applied
               if(length(subset.norm)==1){
                 sums_cols=t[subset.norm,indices]
               }else{
                 sums_cols <- colSums(t[subset.norm,indices], na.rm=TRUE)
               }
               for (i in 1:nrow(t)){
                 t[i,indices] <- (t[i,indices]/sums_cols) * median(sums_cols)
               }
             }
           }
           
           Biobase::exprs(obj) <- log2(t)
           
           obj@processingData@processing <- c(obj@processingData@processing, msg_method, msg_type)
           obj@experimentData@other$normalizationMethod <- method
           obj@experimentData@other$normalizationType <- type
         },
         
         QuantileCentering = {
           q <- function(x) { quantile(x, probs=quantile, na.rm=TRUE) }
           #if normalization on only one protein,q function can't be applied
           if(length(subset.norm)==1){
             quantileOverSamples=Biobase::exprs(obj)[subset.norm,]
           }else{
             quantileOverSamples <- apply(Biobase::exprs(obj)[subset.norm,], 2, q)
           }
           
           if (type == "overall"){
             cOverall <- q(quantileOverSamples)
             Biobase::exprs(obj) <- sweep(Biobase::exprs(obj), 2, quantileOverSamples)
             Biobase::exprs(obj) <- Biobase::exprs(obj) + cOverall
           } else if (type == "within conditions"){
             Biobase::exprs(obj) <- sweep(Biobase::exprs(obj), 2, quantileOverSamples)
             cCond <- NULL
             for (l in unique(conds))
             {
               indices <- which(conds== l)
               cCond[l] <- q(quantileOverSamples[indices])
               Biobase::exprs(obj)[,indices] <- Biobase::exprs(obj)[,indices] + cCond[l]
             }
           }
           
           msg_quantile <- paste("With quantile =", quantile,  sep="")
           obj@processingData@processing <- c(obj@processingData@processing, msg_method, msg_type, msg_quantile)
           obj@experimentData@other$normalizationMethod <- method
           obj@experimentData@other$normalizationType <- type
           obj@experimentData@other$normalizationQuantile <- quantile
           
         },
         MeanCentering = {
           #if normalization on only one protein,mean function can't be applied
           if(length(subset.norm)==1){
             meanOverSamples=Biobase::exprs(obj)[subset.norm,]
           }else{
             meanOverSamples <- apply(Biobase::exprs(obj)[subset.norm,], 2, mean, na.rm = TRUE)
             
           }
           
           if (type == "overall"){
             cOverall <- mean(meanOverSamples)
             Biobase::exprs(obj) <- sweep(qData, 2, meanOverSamples)
             if (scaling){
               Biobase::exprs(obj) <- scale(Biobase::exprs(obj),center=FALSE,scale=TRUE)
               attr(Biobase::exprs(obj),"scaled:scale")<-NULL
             }
             Biobase::exprs(obj) <- Biobase::exprs(obj) + cOverall
           }
           else if (type == "within conditions"){
             .temp <- sweep(Biobase::exprs(obj), 2, meanOverSamples)
             if (scaling){
               Biobase::exprs(obj) <- scale(Biobase::exprs(obj),center=FALSE, scale=TRUE)
               attr(Biobase::exprs(obj),"scaled:scale")<-NULL
             }
             cCond <- NULL
             for (l in unique(conds))
             {
               indices <- which(conds== l)
               cCond[l] <- mean(meanOverSamples[indices])
               #A confirmer
               Biobase::exprs(obj)[,indices] <- .temp[,indices] + cCond[l]
               # Biobase::exprs(obj)[,indices] <- Biobase::exprs(obj)[,indices] + cCond[l]
               
             }
           }
           
           msg_scaling <- paste("With scaling =", scaling,  sep="")
           obj@processingData@processing <- c(obj@processingData@processing, msg_method, msg_type, msg_scaling)
           obj@experimentData@other$normalizationMethod <- msg_method
           obj@experimentData@other$normalizationType <- type
           obj@experimentData@other$normalizationScaling <- scaling
           
         },
         vsn = {
           if(type == "overall"){
             vsn.fit <- vsn::vsnMatrix(2^(Biobase::exprs(obj)))
             Biobase::exprs(obj) <- vsn::predict(vsn.fit, 2^(Biobase::exprs(obj)))
           }else if(type == "within conditions"){
             for (l in unique(conds)) {
               indices <- which(conds == l)
               vsn.fit <- vsn::vsnMatrix(2^(Biobase::exprs(obj)[,indices]))
               Biobase::exprs(obj)[,indices] <- vsn::predict(vsn.fit, 2^(Biobase::exprs(obj)[,indices]))
               
             }
           }
           obj@processingData@processing <- c(obj@processingData@processing, msg_method, msg_type)
           obj@experimentData@other$normalizationMethod <- method
           obj@experimentData@other$normalizationType <- type
         },
         
         ###############
         # data must be log-expressed.
         LOESS = {
           if(type == "overall"){
             # "..." to pass the span parameter to the loess function
             Biobase::exprs(obj) <- limma::normalizeCyclicLoess(x = Biobase::exprs(obj), method = "fast", span = span)
             
           }else if(type == "within conditions"){
             for (l in unique(conds)) {
               indices <- which(conds == l)
               Biobase::exprs(obj)[,indices] <- limma::normalizeCyclicLoess(x = Biobase::exprs(obj)[,indices],method = "fast", span = span)
             }
           }
           
           msg_loess <- paste("With span =", span,  sep="")
           obj@processingData@processing <- c(obj@processingData@processing, msg_method, msg_type, msg_loess)
           obj@experimentData@other$normalizationMethod <- msg_method
           obj@experimentData@other$normalizationType <- type
           
         }
  )
  
  return(obj)
  
  
}
