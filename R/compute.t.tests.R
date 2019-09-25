


##' This function is a wrapper xxxxx
##'
##' @title xxxxx
##' @param obj An object of class \code{MSnSet} with no missing values
##' @param ... See \code{compute.t.tests}
##' @return xxxxxxx
##' @author Samuel Wieczorek
##' @examples
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' obj <- Exp1_R25_pept[1:1000]
##' keepThat <- mvFilterGetIndices(obj, 'wholeMatrix', ncol(obj))
##' obj <- mvFilterFromIndices(obj, keepThat)
##' ttest <- wrapper.t_test_Complete(obj, 1)
wrapper.t_test_Complete <- function(obj,...){
    
    qData <- Biobase::exprs(obj)
    sTab <- pData(obj)
    
    ttest <- compute.t.tests(qData,sTab=pData(obj),...)
    
    return (ttest)
}




##' This function is xxxxxx
##'
##' @title xxxxxx
##' @param qData A matrix of quantitative data, without any missing values.
##' @param sTab xxxx 
##' @param contrast Indicates if the test consists of the comparison of each 
##' biological condition versus 
##' each of the other ones (contrast=1; 
##' for example H0:"C1=C2" vs H1:"C1!=C2", etc.) 
##' or each condition versus all others (contrast=2; e.g.  H0:"C1=(C2+C3)/2" vs
##' H1:"C1!=(C2+C3)/2", etc. if there are three conditions).
##' @param type xxxxx
##' @return A list of two items : logFC and P_Value; both are dataframe. The first one contains
##' the logFC values of all the comparisons (one column for one comparison), the second one contains
##' the pvalue of all the comparisons (one column for one comparison). The names of the columns for those two dataframes
##' are identical and correspond to the description of the comparison. 
##' @author Florence Combes, Samuel Wieczorek
##' @examples
##' require(DAPARdata)
##' data(Exp1_R25_pept)
##' obj <- Exp1_R25_pept[1:1000]
##' keepThat <- mvFilterGetIndices(obj, 'wholeMatrix', ncol(obj))
##' obj <- mvFilterFromIndices(obj, keepThat)
##' sTab <- Biobase::pData(obj)
##' qData <- Biobase::exprs(obj)
##' ttest <- compute.t.tests(qData,sTab ,"OnevsOne")
compute.t.tests <- function(qData,sTab, contrast="OnevsOne", type="Student"){
    switch(type,
           Student=.type <- TRUE,
           Welch=.type <- FALSE)

res<-list()
logFC <- list()
P_Value <- list()

nbComp <- NULL

sTab.old <- sTab
Conditions.f <- factor(sTab$Condition, levels=unique(sTab$Condition))
sTab <- sTab[unlist(lapply(split(sTab, Conditions.f), function(x) {x['Sample.name']})),]
qData <- qData[,unlist(lapply(split(sTab.old, Conditions.f), function(x) {x['Sample.name']}))]
Conditions <- sTab$Condition

Cond.Nb<-length(levels(Conditions.f))


    if(contrast=="OnevsOne"){
        nbComp <- Cond.Nb*(Cond.Nb-1)/2

        for(i in 1:(Cond.Nb-1)){
            for (j in (i+1):Cond.Nb){
    
                c1Indice <- which(Conditions==levels(Conditions.f)[i])
                c2Indice <- which(Conditions==levels(Conditions.f)[j])
    
                res.tmp <- apply(qData[,c(c1Indice,c2Indice)], 1, 
                                 function(x) {
                   t.test(x~Conditions[c(c1Indice,c2Indice)],  var.equal=.type)
                })
                p.tmp <- unlist(lapply(res.tmp,function(x)x$p.value))
                m1.tmp <- unlist(lapply(res.tmp,function(x)as.numeric(x$estimate[1])))
                m2.tmp <- unlist(lapply(res.tmp,function(x)as.numeric(x$estimate[2])))
                m1.name <- names(unlist(lapply(res.tmp,function(x)x$estimate[1])))[1]
                m2.name <- names(unlist(lapply(res.tmp,function(x)x$estimate[2])))[1]
                logFC.tmp <- m1.tmp - m2.tmp
                if (grepl(levels(Conditions.f)[1], m2.name)){logFC.tmp <- -logFC.tmp}
                
                txt <- paste(levels(Conditions.f)[i],"_vs_",levels(Conditions.f)[j], sep="")
                
                logFC[[paste(txt, "logFC", sep="_")]] <- logFC.tmp
                P_Value[[paste(txt, "pval", sep="_")]] <- p.tmp
            }
        }
    } ##end Contrast==1

    if(contrast=="OnevsAll"){
        nbComp <- Cond.Nb
        
        for(i in 1:nbComp){
            
            c1 <- which(Conditions==levels(Conditions.f)[i])
           
            Cond.t.all<-c(1:length(Conditions))
            Cond.t.all[c1]<-levels(Conditions.f)[i]
            Cond.t.all[-c1]<-"all"
            
            res.tmp <- apply(qData, 1, 
                             function(x) {
                                 t.test(x~Cond.t.all, var.equal=.type)
                             })
            
            p.tmp <- unlist(lapply(res.tmp,function(x)x$p.value))
            m1.tmp <- unlist(lapply(res.tmp,function(x)as.numeric(x$estimate[1])))
            m2.tmp <- unlist(lapply(res.tmp,function(x)as.numeric(x$estimate[2])))
            m1.name <- names(unlist(lapply(res.tmp,function(x)x$estimate[1])))[1]
            m2.name <- names(unlist(lapply(res.tmp,function(x)x$estimate[2])))[1]
            logFC.tmp <- m1.tmp - m2.tmp
            
            if (grepl(levels(Conditions.f)[i], m2.name)){logFC.tmp <- -logFC.tmp}
            
            txt <- paste(levels(Conditions.f)[i],"_vs_(all-",levels(Conditions.f)[i],")", sep="")
            logFC[[paste(txt, "logFC", sep="_")]] <- logFC.tmp
            P_Value[[paste(txt, "pval", sep="_")]] <- p.tmp
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
