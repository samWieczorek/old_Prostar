##' This function computes the number of proteins that are only defined by 
##' specific peptides, shared peptides or a mixture of two. 
##' 
##' @title Computes the number of proteins that are only defined by 
##' specific peptides, shared peptides or a mixture of two.
##' @param matShared The adjacency matrix with both specific and 
##' shared peptides.
##' @return A list
##' @author Samuel Wieczorek
##' @examples
##' utils::data(Exp1_R25_pept, package='DAPARdata')
##' protID <- "Protein_group_IDs"
##' obj <- Exp1_R25_pept[1:1000]
##' MShared <- BuildAdjacencyMatrix(obj, protID, FALSE)
##' getProteinsStats(MShared)
getProteinsStats <- function(matShared){
    if (is.null(matShared)){return(NULL)}
    #if(!is.matrix(matUnique) || !is.matrix(matShared)){return(NULL)}
    
    
    ind.shared.Pep <- which(rowSums(as.matrix(matShared))>1)
    ind.unique.Pep <- which(rowSums(as.matrix(matShared))==1)
    
    M.shared.Pep <- matShared[ind.shared.Pep,]
    M.shared.Pep <- M.shared.Pep[,-which(colSums(as.matrix(M.shared.Pep))==0)]
    
    M.unique.Pep <- matShared[ind.unique.Pep,]
    M.unique.Pep <- M.unique.Pep[,-which(colSums(as.matrix(M.unique.Pep))==0)]
    
    
    pep.names.shared <- colnames(M.shared.Pep)
    pep.names.unique <- colnames(M.unique.Pep)
    protOnlyShared <- setdiff(pep.names.shared, intersect(pep.names.shared, pep.names.unique))
    protOnlyUnique <- setdiff(pep.names.unique, intersect(pep.names.shared, pep.names.unique))
    protMix <- intersect(pep.names.shared, pep.names.unique)
    

    return (list(nbPeptides = nrow(M.unique.Pep)+nrow(M.shared.Pep),
                 nbSpecificPeptides = nrow(M.unique.Pep),
                 nbSharedPeptides = nrow(M.shared.Pep),
                 nbProt = length(protOnlyShared)+length(protOnlyUnique)+length(protMix),
                 protOnlyUniquePep =protOnlyUnique,
                  protOnlySharedPep =protOnlyShared,
                  protMixPep = protMix))
}




##' This function creates a column for the protein dataset after aggregation 
##' by using the previous peptide dataset.
##' 
##' @title creates a column for the protein dataset after agregation by
##'  using the previous peptide dataset.
##' @param peptideData A data.frame of meta data of peptides. It is the fData 
##' of the MSnset object.
##' @param matAdj The adjacency matrix used to agregate the peptides data.
##' @param columnName The name of the column in fData(peptides_MSnset) that 
##' the user wants to keep in the new protein data.frame.
##' @param proteinNames The names of the protein in the new dataset (i.e. rownames)
##' @return A vector
##' @author Samuel Wieczorek
##' @examples
##' utils::data(Exp1_R25_pept, package='DAPARdata')
##' protID <- "Protein_group_IDs"
##' obj.pep <- Exp1_R25_pept[1:1000]
##' M <- BuildAdjacencyMatrix(obj.pep, protID, FALSE)
##' data <- Biobase::fData(obj.pep)
##' protData <- Prostar::aggregateMean(obj.pep, M)
##' name <- "Protein_group_IDs"
##' proteinNames <- rownames(Biobase::fData(protData))
##' BuildColumnToProteinDataset(data, M, name,proteinNames )
BuildColumnToProteinDataset <- function(peptideData, matAdj, columnName, proteinNames){
nbProt <- ncol(matAdj)
newCol <- rep("", nbProt)

#print(head(rownames(peptideData)))
i <- 1
for (p in proteinNames){
    listeIndicePeptides <- names(which(matAdj[,p] == 1))
    listeData <- unique(as.character(peptideData[listeIndicePeptides,columnName], ";"))
    newCol[i] <- paste0(listeData, collapse = ", ")
    i <- i +1
}
return(newCol)
}


##' This function creates a column for the protein dataset after agregation 
##' by using the previous peptide dataset. It is a parallel version of the function
##' \code{BuildColumnToProteinDataset}
##' 
##' @title creates a column for the protein dataset after agregation by
##'  using the previous peptide dataset.
##' @param peptideData A data.frame of meta data of peptides. It is the fData 
##' of the MSnset object.
##' @param matAdj The adjacency matrix used to agregate the peptides data.
##' @param columnName The name of the column in fData(peptides_MSnset) that 
##' the user wants to keep in the new protein data.frame.
##' @param proteinNames The names of the protein in the new dataset (i.e. rownames)
##' @return A vector
##' @author Samuel Wieczorek
##' @examples
##' \dontrun{
##' utils::data(Exp1_R25_pept, package='DAPARdata')
##' protID <- "Protein_group_IDs"
##' obj.pep <- Exp1_R25_pept[1:1000]
##' M <- BuildAdjacencyMatrix(obj.pep, protID, FALSE)
##' data <- Biobase::fData(obj.pep)
##' protData <- Prostar::aggregateSum(obj.pep, M)
##' name <- "Protein_group_IDs"
##' proteinNames <- rownames(Biobase::fData(protData))
##' BuildColumnToProteinDataset_par(data, M, name,proteinNames )
##' }
BuildColumnToProteinDataset_par <- function(peptideData, matAdj, columnName, proteinNames){
    doParallel::registerDoParallel()
    
    nbProt <- ncol(matAdj)
    newCol <- rep("", nbProt)
    i <- 1
    newCol <- foreach (i=1:length(proteinNames), .combine=rbind) %dopar% {
        listeIndicePeptides <- names(which(matAdj[,proteinNames[i]] == 1))
        listeData <- unique(as.character(peptideData[listeIndicePeptides,columnName], ";"))
        paste0(listeData, collapse = ", ")
    }
    return(as.vector(newCol))
}



##' This function computes the number of peptides used to aggregate proteins.
##' 
##' @title Compute the number of peptides used to aggregate proteins
##' @param M A "valued" adjacency matrix in which lines and columns correspond 
##' respectively to peptides and proteins.
##' @return A vector of boolean which is the adjacency matrix 
##' but with NA values if they exist in the intensity matrix.
##' @author Alexia Dorffer
##' @examples
##' library(DAPARdata)
##' utils::data(Exp1_R25_pept)
##' protID <- "Protein_group_IDs"
##' M <- BuildAdjacencyMatrix(Exp1_R25_pept[1:1000], protID, FALSE)
##' CountPep(M)
CountPep <- function (M) {
    z <- M
    z[z!=0] <- 1
    return(z)
}



##' Method to create a plot with proteins and peptides on
##' a MSnSet object (peptides)
##' 
##' @title Function to create a histogram that shows the repartition of
##' peptides w.r.t. the proteins
##' @param mat An adjacency matrix.
##' @return A histogram  
##' @author Alexia Dorffer, Samuel Wieczorek
##' @examples
##' utils::data(Exp1_R25_pept, package='DAPARdata')
##' mat <- BuildAdjacencyMatrix(Exp1_R25_pept[1:1000], "Protein_group_IDs")
##' GraphPepProt(mat)
GraphPepProt <- function(mat){
    if (is.null(mat)){return (NULL)} 

    #mat <- as.matrix(mat)
    t <- t(mat)
    t <- apply(mat, 2, sum, na.rm=TRUE)
    tab <- table(t)
    position <- seq(1, length(tab),by=3)
    conds <- names(tab)

    #par(mar=c(6,4,4,8) + 0.1)#, mgp=c(3,0.5,0)
    barplot(tab, 
            xlim=c(1, length(tab)),
            xlab="Nb of peptides", 
            ylab="Nb of proteins",
            names.arg=conds, 
            xaxp=c(1, length(tab), 3), 
            las=1
            , col = "orange")

}




##' Method to create a binary matrix with proteins in columns and peptides 
##' in lines on a \code{MSnSet} object (peptides)
##' 
##' @title Function matrix of appartenance group
##' @param obj.pep An object (peptides) of class \code{MSnSet}.
##' @param protID The name of proteins ID column 
##' @param unique A boolean to indicate whether only the unique peptides must 
##' be considered (TRUE) or if the shared peptides have to 
##' be integrated (FALSE).
##' @return A binary matrix  
##' @author Florence Combes, Samuel Wieczorek, Alexia Dorffer
##' @examples
##' utils::data(Exp1_R25_pept, package='DAPARdata') 
##' BuildAdjacencyMatrix(Exp1_R25_pept[1:1000], "Protein_group_IDs", TRUE)
BuildAdjacencyMatrix <- function(obj.pep, protID, unique=TRUE){
   
  
  data <- Biobase::exprs(obj.pep)
  PG <- Biobase::fData(obj.pep)[,protID]
  PG.l <- strsplit(as.character(PG), split=";", fixed=TRUE)
  
  t <- table(data.frame(A=rep(seq_along(PG.l), lengths(PG.l)), B=unlist(PG.l)))
  
  if (unique == TRUE){
    ll <- which(rowSums(t)>1)
    if (length(ll) > 0) {
      t[ll,] <- 0
    }
  }
  
  
  ## Delete empty columns
  # ll <- which(colSums(t)==0)
  # if (length(ll) > 0){
  #   X <- X[,-t]
  # }
  # 
  X <- Matrix::Matrix(t, sparse=T,
              dimnames = list(rownames(obj.pep), colnames(t))
  )
  
  
  
  
  return(X)
}





##' Method to split an adjacency matrix into specific and shared
##' 
##' @title splits an adjacency matrix into specific and shared 
##' @param X An adjacency matrix
##' @return A list of two adjacency matrices
##' @author Samuel Wieczorek
##' @examples
##' utils::data(Exp1_R25_pept, package='DAPARdata') 
##' X <- BuildAdjacencyMatrix(Exp1_R25_pept[1:1000], "Protein_group_IDs", FALSE)
##' X.ll <- splitAdjacencyMat(X)
splitAdjacencyMat <- function(X){
  hasShared <- length( which(rowSums(X) > 1)) > 0
  hasSpec <- length( which(rowSums(X) == 1)) > 0
  
  
  if (hasShared && !hasSpec){
    tmpShared <- X
    tmpSpec <- X
    tmpSpec[which(rowSums(tmpSpec) > 1),] <- 0
  }
  else if (!hasShared && hasSpec){
    tmpSpec <- X
    tmpShared <- X
    tmpShared[which(rowSums(tmpShared) == 1),] <- 0
  }
  else if (hasShared && hasSpec){
    tmpSpec <- X
    tmpShared <- X
    tmpShared[which(rowSums(tmpShared) == 1),] <- 0
    tmpSpec[which(rowSums(tmpSpec) > 1),] <- 0
  } else {
    tmpSpec <- X
    tmpShared <- X
  }
  
  
  return (list(Xshared = tmpShared, Xspec = tmpSpec))
  
}

##' This function computes the intensity of proteins based on the sum of the 
##' intensities of their peptides.
##' 
##' @title Compute the intensity of proteins with the sum of the intensities
##' of their peptides.
##' @param obj.pep A matrix of intensities of peptides
##' @param X An adjacency matrix in which lines and columns correspond 
##' respectively to peptides and proteins.
##' @return A matrix of intensities of proteins
##' @author Alexia Dorffer
##' @examples
##' utils::data(Exp1_R25_pept, package='DAPARdata')
##' protID <- "Protein_group_IDs"
##' obj.pep <- Exp1_R25_pept[1:1000]
##' X <- BuildAdjacencyMatrix(obj.pep, protID, FALSE)
##' Prostar::aggregateSum(obj.pep, X)
aggregateSum <- function(obj.pep, X){
  pepData <- 2^(Biobase::exprs(obj.pep))
  protData <- inner.sum(pepData, X)
  obj.prot <- finalizeAggregation(obj.pep, pepData, protData, X)
  return(obj.prot)
}



##' Method to xxxxx
##' 
##' @title xxxx 
##' @param obj.pep xxxxx
##' @param X xxxx
##' @param init.method xxxxx
##' @param method xxxxx
##' @param n xxxx
##' @return xxxxx
##' @author Samuel Wieczorek
##' @examples
##' utils::data(Exp1_R25_pept, package='DAPARdata')
##' protID <- "Protein_group_IDs"
##' obj.pep <- Exp1_R25_pept[1:1000]
##' X <- BuildAdjacencyMatrix(obj.pep, protID, FALSE)
##' aggregateIterParallel(obj.pep, X)
aggregateIterParallel <- function(obj.pep, X, init.method='Sum', method='Mean', n=NULL){
  
  doParallel::registerDoParallel()
  
  qData.pep <- 2^(Biobase::exprs(obj.pep))
  protData <- matrix(rep(0,ncol(X)*nrow(X)), nrow=ncol(X))
  
  protData <- foreach(cond=1:length(unique(Biobase::pData(obj.pep)$Condition)), .combine=cbind, .packages = "MSnbase") %dopar% {
    
    condsIndices <- which(Biobase::pData(obj.pep)$Condition == unique(Biobase::pData(obj.pep)$Condition)[cond])
    qData <- qData.pep[,condsIndices]
    Prostar::inner.aggregate.iter(qData, X, init.method, method, n)
   }
  
  protData <- protData[,colnames(Biobase::exprs(obj.pep))]
  obj.prot <- finalizeAggregation(obj.pep, qData.pep, protData, X)
  
  return(obj.prot)

}


##' Method to xxxxx
##' 
##' @title xxxx 
##' @param pepData xxxxx
##' @param X xxxx
##' @param init.method xxx
##' @param method xxx
##' @param n xxxx
##' @return xxxxx
##' @author Samuel Wieczorek
##' utils::data(Exp1_R25_pept, package='DAPARdata')
##' protID <- "Protein_group_IDs"
##' obj.pep <- Exp1_R25_pept[1:1000]
##' X <- BuildAdjacencyMatrix(obj.pep, protID, FALSE)
##' Prostar::inner.aggregate.iter(exprs(obj.pep), X)
inner.aggregate.iter <- function(pepData, X,init.method='Sum', method='Mean', n=NULL){
  
  
  if (!(init.method %in% c("Sum", "Mean"))) {
    warning("Wrong parameter init.method")
    return(NULL)
  }
  
  if (!(method %in% c("onlyN", "Mean"))){
        warning("Wrong parameter method")
        return(NULL)
  }
  
  
  if (method=='onlyN' && is.null(n)){
    warning("Parameter n is null")
    return(NULL)
  }
      
  yprot <- NULL
  switch(init.method,
         Sum= yprot <- inner.sum(pepData, X),
         Mean= yprot <- inner.mean(pepData, X)
  )
  conv <- 1
  
  while(conv > 10**(-10)){
    mean.prot <- rowMeans(as.matrix(yprot), na.rm = TRUE)
    mean.prot[is.na(mean.prot)] <- 0
    
    X.tmp <- mean.prot*X
    X.new <- X.tmp/rowSums(as.matrix(X.tmp), na.rm = TRUE)
    X.new[is.na(X.new)] <- 0
    
    # l'appel ? la fonction ci-dessous d?pend des param?tres choisis par l'utilisateur
    switch(method,
           Mean = yprot <- inner.mean(pepData, X.new),
           onlyN = yprot <- inner.aggregate.topn(pepData,X.new,'Mean', n)
    )
    
    mean.prot.new <- rowMeans(as.matrix(yprot), na.rm = TRUE)
    mean.prot.new[is.na(mean.prot.new)] <- 0
    
    conv <- mean(abs(mean.prot.new - mean.prot))
    print(paste0("conv : ", conv))
  }
  return(as.matrix(yprot))
}



##' Method to xxxxx
##' 
##' @title xxxx 
##' @param obj.pep xxxxx
##' @param X xxxx
##' @param init.method xxxxx
##' @param method xxxxx
##' @param n xxxx
##' @return A protein object of class \code{MSnset}
##' @author Samuel Wieczorek
##' @examples
##' utils::data(Exp1_R25_pept, package='DAPARdata')
##' protID <- "Protein_group_IDs"
##' X <- BuildAdjacencyMatrix(Exp1_R25_pept[1:1000], protID, FALSE)
##' aggregateIter(Exp1_R25_pept[1:1000],X=X)
aggregateIter <- function(obj.pep, X, init.method='Sum', method='Mean', n=NULL){
  
  ### a reproduire iterativement pour chaque condition
    # Initialisation: presque aucune d?pendance ? l'initialisation prendre "sum overall" et  matAdj = X par simplicit?
    #X <- as.matrix(X)
  qData.pep <- 2^(Biobase::exprs(obj.pep))
  
    protData <- matrix(rep(0,ncol(X)*ncol(obj.pep)), nrow=ncol(X))
    
    for (cond in unique(pData(obj.pep)$Condition)){
      condsIndices <- which(pData(obj.pep)$Condition == cond)
      qData <- qData.pep[,condsIndices]
      print(paste0("Condition ", cond))
      protData[,condsIndices]  <- inner.aggregate.iter(qData, X, init.method, method, n)
     }
    obj.prot <- finalizeAggregation(obj.pep, qData.pep, protData, X)
    return(obj.prot)
    
  #return(yprot)
}



##' Method to compute the number of quantified peptides used for aggregating each protein
##' 
##' @title Computes the number of peptides used for aggregating each protein 
##' @param X An adjacency matrix
##' @param pepData A data.frame of quantitative data
##' @return A data.frame
##' @author Samuel Wieczorek
GetNbPeptidesUsed <- function(X, pepData){
   pepData[!is.na(pepData)] <- 1
  pepData[is.na(pepData)] <- 0
  pep <- t(X) %*% pepData
  
  return(pep)
}

##' This function computes the intensity of proteins as the mean of the 
##' intensities of their peptides.
##' 
##' @title Compute the intensity of proteins as the mean of the intensities
##' of their peptides.
##' @param obj.pep A peptide object of class \code{MSnset}
##' @param X An adjacency matrix in which lines and columns correspond 
##' respectively to peptides and proteins.
##' @return A matrix of intensities of proteins
##' @author Alexia Dorffer
##' @examples
##' utils::data(Exp1_R25_pept, package='DAPARdata')
##' obj.pep <- Exp1_R25_pept[1:1000]
##' protID <- "Protein_group_IDs"
##' X <- BuildAdjacencyMatrix(obj.pep, protID, FALSE)
##' aggregateMean(obj.pep, X)
aggregateMean <- function(obj.pep, X){
  pepData <- 2^(Biobase::exprs(obj.pep))
  protData <- inner.mean(pepData, X)
  obj.prot <- finalizeAggregation(obj.pep, pepData, protData, X)
  return(obj.prot)
}



##' Method to compute the detailed number of quantified peptides used for aggregating each protein
##' 
##' @title Computes the detailed number of peptides used for aggregating each protein 
##' @param X An adjacency matrix
##' @param pepData A data.frame of quantitative data
##' @return A list of two items
##' @author Samuel Wieczorek
##' utils::data(Exp1_R25_pept, package='DAPARdata')
##' obj.pep <- Exp1_R25_pept[1:1000]
##' protID <- "Protein_group_IDs"
##' X <- BuildAdjacencyMatrix(obj.pep, protID, FALSE)
##' GetDetailedNbPeptidesUsed(X, obj.pep)
GetDetailedNbPeptidesUsed <- function(X, pepData){
  pepData[!is.na(pepData)] <- 1
  pepData[is.na(pepData)] <- 0
  
  mat <- splitAdjacencyMat(X)
  return(list(nShared=t(mat$Xshared) %*% pepData, 
              nSpec=t(mat$Xspec) %*% pepData))
  
}


##' Method to compute the detailed number of quantified peptides for each protein
##' 
##' @title Computes the detailed number of peptides for each protein 
##' @param X An adjacency matrix
##' @return A data.frame
##' @author Samuel Wieczorek
##' @examples
##' utils::data(Exp1_R25_pept, package='DAPARdata')
##' obj.pep <- Exp1_R25_pept[1:1000]
##' protID <- "Protein_group_IDs"
##' X <- BuildAdjacencyMatrix(obj.pep, protID, FALSE)
##' GetDetailedNbPeptides(X)
GetDetailedNbPeptides <- function(X){
  
  mat <- splitAdjacencyMat(as.matrix(X))
  
  
  return(list(nTotal = rowSums(t(as.matrix(X))),
              nShared=rowSums(t(mat$Xshared)), 
              nSpec=rowSums(t(mat$Xspec)))
  )
  
}



##' Method to xxxxx
##' 
##' @title xxxx 
##' @param pepData A data.frame of quantitative data
##' @param X An adjacency matrix
##' @return A matrix
##' @author Samuel Wieczorek
inner.sum <- function(pepData, X){
  pepData[is.na(pepData)] <- 0
  Mp <- t(X) %*% pepData
  return(Mp)
}


##' Method to xxxxx
##' 
##' @title xxxx 
##' @param pepData A data.frame of quantitative data
##' @param X An adjacency matrix
##' @return xxxxx
##' @author Samuel Wieczorek
inner.mean <- function(pepData, X){
  Mp <- inner.sum(pepData, X)
  Mp <- Mp / GetNbPeptidesUsed(X, pepData)
  
  return(Mp)
  
}




##' Method to xxxxx
##' 
##' @title xxxx 
##' @param pepData A data.frame of quantitative data
##' @param X An adjacency matrix
##' @param method xxxxx
##' @param n xxxxx
##' @return xxxxx
##' @author Samuel Wieczorek
inner.aggregate.topn <-function(pepData,X, method='Mean', n=10){
  
  med <- apply(pepData, 1, median)
  xmed <- as(X * med, "dgCMatrix")
  for (c in 1:ncol(X)){
    v <- order(xmed[,c],decreasing=TRUE)[1:n]
    l <- v[which((xmed[,c])[v] != 0)]
    
    if (length(l) > 0){
      diff <- setdiff( which(X[,c] == 1), l)
      if (length(diff)) {X[diff,c] <- 0}
    }
  }
  
  Mp <- NULL
  switch(method,
         Mean= Mp <- inner.mean(pepData, X),
         Sum= Mp <- inner.sum(pepData, X)
  )
  
  return(Mp)
}

##' This function computes the intensity of proteins as the sum of the 
##' intensities of their n best peptides.
##' 
##' @title Compute the intensity of proteins as the sum of the 
##' intensities of their n best peptides.
##' @param obj.pep A matrix of intensities of peptides
##' @param X An adjacency matrix in which lines and columns correspond 
##' respectively to peptides and proteins.
##' @param method xxx
##' @param n The maximum number of peptides used to aggregate a protein.
##' @return A matrix of intensities of proteins
##' @author Alexia Dorffer, Samuel Wieczorek
##' @examples
##' utils::data(Exp1_R25_pept, package='DAPARdata')
##' obj.pep <- Exp1_R25_pept[1:1000]
##' protID <- "Protein_group_IDs"
##' X <- BuildAdjacencyMatrix(obj.pep, protID, FALSE)
##' Prostar::aggregateTopn(obj.pep, X, n=3)
aggregateTopn <- function(obj.pep,X,  method='Mean', n=10){
  pepData <- 2^(Biobase::exprs(obj.pep))
  
  protData <- inner.aggregate.topn(pepData, X, method=method, n)
  
  obj.prot <- finalizeAggregation(obj.pep, pepData, protData, X)
  return(obj.prot)
}




##' Method to finalize the aggregation process
##' 
##' @title Finalizes the aggregation process 
##' @param obj.pep A peptide object of class \code{MSnset}
##' @param pepData xxxx
##' @param X An adjacency matrix in which lines and columns correspond 
##' respectively to peptides and proteins.
##' @param protData xxxxx
##' @param lib.loc A list of two items (lib.loc$Prostar.loc and lib.loc$DAPAR.loc) to provide the 
##' location of the installed packages
##' @return A protein object of class \code{MSnset}
##' @author Samuel Wieczorek
finalizeAggregation <- function(obj.pep, pepData, protData,X, lib.loc=NULL){
 
  protData <- as.matrix(protData)
  X <- as.matrix(X)
  protData[protData==0] <- NA
  protData[is.nan(protData)] <- NA
  protData[is.infinite(protData)] <-NA
  
  temp <- GetDetailedNbPeptidesUsed(X, pepData)
  
  pepSharedUsed <- as.matrix(temp$nShared)
  colnames(pepSharedUsed) <- paste("pepShared.used.", colnames(pepData), sep="")
  rownames(pepSharedUsed) <- colnames(X)
  
  pepSpecUsed <- as.matrix(temp$nSpec)
  colnames(pepSpecUsed) <- paste("pepSpec.used.", colnames(pepData), sep="")
  rownames(pepSpecUsed) <- colnames(X)
  
  pepTotalUsed <- as.matrix(GetNbPeptidesUsed(X, pepData))
  colnames(pepTotalUsed) <- paste("pepTotal.used.", colnames(pepData), sep="")
  rownames(pepTotalUsed) <- colnames(X)
  
  n <- GetDetailedNbPeptides(X)
  
   fd <- data.frame(colnames(X), 
                    nPepTotal = n$nTotal,
                    nPepShared = n$nShared, 
                    nPepSpec = n$nSpec, 
                    pepSpecUsed, 
                    pepSharedUsed, 
                    pepTotalUsed)
  
  obj.prot <- MSnSet(exprs = log2(protData), 
                fData = fd, 
                pData = Biobase::pData(obj.pep))
  obj.prot@experimentData@other <- obj.pep@experimentData@other
  obj.prot@experimentData@other$typeOfData <-"protein"
  #obj.prot <- addOriginOfValue(obj.prot)
  obj.prot@experimentData@other$OriginOfValues <- NULL
  obj.prot@experimentData@other$Prostar_Version <- installed.packages(lib.loc = lib.loc$Prostar.loc)["Prostar","Version"]
  obj.prot@experimentData@other$DAPAR_Version <- installed.packages(lib.loc = lib.loc$DAPAR.loc)["DAPAR","Version"]
  return (obj.prot)
}

