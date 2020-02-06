# peptide-level t-test
##########


## Build design matrix X
# X <- as.matrix(BuildAdjacencyMatrix(obj, 'Protein_group_IDs', unique = FALSE))
# X.spec <- as.matrix(BuildAdjacencyMatrix(obj, 'Protein_group_IDs', unique = TRUE))
# 
# y <- exprs(obj)
# y <- y[rownames(X), ]
# n1 <- n2 <- 3
# n <- n1+n2 # number of samples in c1 and c2
# 
# ## Keep track of proteins that are lost in aggregation step
# unsup.prot <- !(colnames(X) %in% colnames(X.spec))
# q <- nrow(X) # Number of peptides
# p <- ncol(X) # Number of proteins
# 
# 
# # Two ways to compute it (function groupttest below)
# peptide.spec.based.tmp <- apply(X.spec, 2, FUN=function(mask) t.test(x=as.vector(y[mask == 1, 1:n1]), y=as.vector(y[mask == 1, -(1:n1)]), var.equal=TRUE)$p.value)
# peptide.spec.based.tmp <- groupttest(X.spec,y)
# 
# # then:
# peptide.spec.based.pv <- rep(1, ncol(X))
# peptide.spec.based.pv[!unsup.prot] <- peptide.spec.based.tmp
# 





# groupttest <- function(MatAdj, expr){
#   nProt <- dim(MatAdj)[2]
#   res <- rep(0,nProt)
#   
#   for(i in 1:nProt){
#     #print(i)
#     index <- names(which(MatAdj[,i]==1))
#     if(length(index)== 0){
#       res[i] <- 1
#     } else{
#       peptidestotest <- expr[index,]
#       if(length(index)== 1){
#         res[i] <- t.test(x=peptidestotest[1:3], y=peptidestotest[-(1:3)], var.equal=TRUE)$p.value
#       } else{
#         res[i] <- t.test(x=peptidestotest[,1:3], y=peptidestotest[,-(1:3)], var.equal=TRUE)$p.value
#       }
#     }
#   }
#   return(res)
# }  


#######################
### attention, à rendre générique: pour l'instant ne marche qu'avec n1=3 (car codé en dur)
##' This function is xxxxxx
##'
##' @title xxxxxx
##' @param MatAdj xxxxx.
##' @param cond1 xxxx.
##' @param cond2 xxxx.
##' @return xxxxx
##' @author Thomas Burger, Samuel Wieczorek
##' @examples
##' utils::data(Exp1_R25_pept, package='DAPARdata')
##' obj <- Exp1_R25_pept
##' protID <- "Protein_group_IDs"
##' keepThat <-  mvFilterGetIndices(obj, 'wholeMatrix', ncol(obj))
##' obj <- mvFilterFromIndices(obj, keepThat)
##' X.spec <- BuildAdjacencyMatrix(obj, protID,  unique = TRUE)
##' qData <- Biobase::exprs(obj)
##' gttest <- groupttest(X.spec, qData[,1:3], qData[,4:6])
groupttest <- function(MatAdj, cond1=NULL, cond2 = NULL){
  res <- list()
  if (is.null(cond1) || is.null(cond2)){
    warning("At least, one condition is empty.")
    return(NULL)
  }
  
  for(i in 1:dim(MatAdj)[2]){
    index <- names(which(MatAdj[,i]==1))
    if(length(index)> 0){
      res[[i]] <- t.test(x=cond1[index,], y=cond2[index,], var.equal=TRUE)
    } else {
      res[[i]] <- NA
    }
  }
  return(res)
}  




##'
##' @title xxxxxx
##' @param qData A matrix of quantitative data, without any missing values.
##' @param sTab xxxx
##' @param X xxx
##' @param X.spec A matrix of quantitative data, without any missing values.
##' @param logFC A vector (or list of vectors) xxxx
##' @param contrast Indicates if the test consists of the comparison of each 
##' biological condition versus 
##' each of the other ones (contrast=1; 
##' for example H0:"C1=C2" vs H1:"C1!=C2", etc.) 
##' or each condition versus all others (contrast=2; e.g.  H0:"C1=(C2+C3)/2" vs
##' H1:"C1!=(C2+C3)/2", etc. if there are three conditions).
##' @param type xxxxx
##' @return xxxxx
##' @author Thomas Burger, Samuel Wieczorek
##' @examples
##' utils::data(Exp1_R25_pept, package='DAPARdata')
##' obj <- Exp1_R25_pept
##' protID <- "Protein_group_IDs"
##' keepThat <-  mvFilterGetIndices(obj, 'wholeMatrix', ncol(obj))
##' obj <- mvFilterFromIndices(obj, keepThat)
##' X <- BuildAdjacencyMatrix(obj, protID,  unique = FALSE)
##' X.spec <- BuildAdjacencyMatrix(obj, protID,  unique = TRUE)
##' sTab <- Biobase::pData(obj)
##' qData <- Biobase::exprs(obj)
##' gttest <- compute.group.t.tests(qData, sTab, X, X.spec)
compute.group.t.tests <- function(qData, sTab,X,  X.spec, logFC = NULL,contrast="OnevsOne", type="Student"){
  
  switch(type,
         Student=.type <- TRUE,
         Welch=.type <- FALSE)
  
  res.tmp <- list()
  logFC <- list()
  P_Value <- list()
  
  sTab.old <- sTab
  Conditions.f <- factor(sTab$Condition, levels=unique(sTab$Condition))
  sTab <- sTab[unlist(lapply(split(sTab, Conditions.f), function(x) {x['Sample.name']})),]
  qData <- qData[,unlist(lapply(split(sTab.old, Conditions.f), function(x) {x['Sample.name']}))]
  Conditions <- sTab$Condition
  
  y <- qData
  y <- y[rownames(X), ]
  ## Keep track of proteins that are lost in aggregation step
  unsup.prot <- !(colnames(X) %in% colnames(X.spec))
  
   
  if(contrast=="OnevsOne"){
    comb <- combn(levels(Conditions.f), 2)
    for(i in 1:ncol(comb)){
      c1Indice <- which(Conditions==comb[1,i])
      c2Indice <- which(Conditions==comb[2,i])
      res.tmp <- groupttest(X.spec,qData[,c1Indice], qData[,c2Indice] )
        
      #compute logFC from the result of t.test function
      p.tmp <- unlist(lapply(res.tmp,function(x) x["p.value"]))
      m1.tmp <- unlist(lapply(res.tmp, function(x) as.numeric(unlist(x)["estimate.mean of x"])))
      m2.tmp <- unlist(lapply(res.tmp, function(x) as.numeric(unlist(x)["estimate.mean of y"])))
      m1.name <- comb[1,i]
      m2.name <- comb[2,i]
      logFC.tmp <- m1.tmp - m2.tmp
      if (grepl(comb[1,i], m2.name)){logFC.tmp <- -logFC.tmp}
      
      peptide.spec.based.pv <- rep(1, ncol(X))
      peptide.spec.based.pv[!unsup.prot] <- p.tmp
      peptide.spec.based.FC <- rep(1, ncol(X))
      peptide.spec.based.FC[!unsup.prot] <- logFC.tmp
      
        
      txt <- paste(comb[1,i],"_vs_",comb[2,i], sep="")
        
      logFC[[paste(txt, "logFC", sep="_")]] <- peptide.spec.based.FC
      P_Value[[paste(txt, "pval", sep="_")]] <- peptide.spec.based.pv
      }
  } ##end Contrast==1
  
  if(contrast=="OnevsAll"){
    nbComp <- length(levels(Conditions.f))
    
    for(i in 1:nbComp){
      
      c1Indice <- which(Conditions==levels(Conditions.f)[i])
     # Cond.t.all <- c(1:length(Conditions))
     # Cond.t.all[c1Indice] <- levels(Conditions.f)[i]
     # Cond.t.all[-c1Indice] <- "all"
      #c1Indice <- which(Conditions==levels(Conditions.f)[i])
      res.tmp <- groupttest(X.spec,qData[,c1Indice], qData[,-c1Indice] )
      
      
      p.tmp <- unlist(lapply(res.tmp,function(x) x["p.value"]))
      m1.tmp <- unlist(lapply(res.tmp, function(x) as.numeric(unlist(x)["estimate.mean of x"])))
      m2.tmp <- unlist(lapply(res.tmp, function(x) as.numeric(unlist(x)["estimate.mean of y"])))
      m1.name <- levels(Conditions.f)[i]
      m2.name <-  paste("all-(",levels(Conditions.f)[i],")", sep="")
      logFC.tmp <- m1.tmp - m2.tmp
      if (grepl(levels(Conditions.f)[i], m2.name)){logFC.tmp <- -logFC.tmp}
      
      peptide.spec.based.pv <- rep(1, ncol(X))
      peptide.spec.based.pv[!unsup.prot] <- p.tmp
      peptide.spec.based.FC <- rep(1, ncol(X))
      peptide.spec.based.FC[!unsup.prot] <- logFC.tmp
      
      
      
      txt <- paste(levels(Conditions.f)[i],"_vs_(all-",levels(Conditions.f)[i],")", sep="")
      logFC[[paste(txt, "logFC", sep="_")]] <- peptide.spec.based.FC
      P_Value[[paste(txt, "pval", sep="_")]] <- peptide.spec.based.pv
    }
  } # End Contrast=2
  
  
  res.l <- list(
    logFC = as.data.frame(logFC),
    P_Value = as.data.frame(P_Value)
  )
  colnames(res.l$logFC) <- names(logFC)
  colnames(res.l$P_Value) <- names(P_Value)
  
  return(res.l) 
  
}