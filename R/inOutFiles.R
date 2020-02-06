
##' Saves the parameters of a tool in the pipeline of Prostar
##' @title Saves the parameters of a tool in the pipeline of Prostar
##' @param obj An object of class \code{MSnSet}
##' @param name.dataset The name of the dataset
##' @param name The name of the tool. Available values are: "Norm, Imputation, anaDiff, GOAnalysis,Aggregation"
##' @param l.params A list that contains the parameters
##' @return An instance of class \code{MSnSet}.
##' @author Samuel Wieczorek
##' @examples 
##' utils::data(Exp1_R25_pept, package='DAPARdata')
##' l.params=list(method="Global quantile alignment", type="overall")
##' saveParameters(Exp1_R25_pept, "Filtered.peptide", "Imputation",l.params)
saveParameters <- function(obj,name.dataset=NULL,name=NULL,l.params=NULL){
  if ( is.null(name) || is.null(name.dataset)) {
    warning("No operation has been applied to the dataset.")
    return(obj)
  }
  tmp <- list()
  if(is.null(l.params)){
    tmp[[name]] <- list()
  } else {
    tmp[[name]] <- l.params
  }
  
  obj@experimentData@other$Params[[name.dataset]] <- tmp
  #obj@processingData@processing <- c(obj@processingData@processing , buildLogText(name, l.params, level=obj@experimentData@other$typeOfData))
  
  return(obj)
}



##' Sets the MEC tag in the OriginOfValues
##' @title Sets the MEC tag in the OriginOfValues
##' @param obj An object of class \code{MSnSet}
##' @return An instance of class \code{MSnSet}.
##' @author Samuel Wieczorek
##' @examples 
##' utils::data(Exp1_R25_pept, package='DAPARdata')
##' setMEC(Exp1_R25_pept)
setMEC <- function(obj){
  
  if (is.null( obj@experimentData@other$OriginOfValues)){return()}
  
  conditions <- unique(Biobase::pData(obj)$Condition)
  nbCond <- length(conditions)
  
  for (cond in 1:nbCond){
    ind <- which(Biobase::pData(obj)$Condition == conditions[cond])
    if (length(ind) == 1) {
      lNA <- which(is.na(Biobase::exprs(obj)[,ind]))
      } else {
        lNA <- which(apply(is.na(Biobase::exprs(obj)[,ind]), 1, sum)==length(ind))
      }
    if (length(lNA) > 0)
    {
      Biobase::fData(obj)[lNA,obj@experimentData@other$OriginOfValues[ind]] <- "MEC"
    }
  }
  return(obj)
}



##' Sets the OriginOfValues dataframe in the fData table
##' @title Sets the OriginOfValues dataframe
##' @param obj An object of class \code{MSnSet}
##' @param index A list of integer xxxxxxx
##' @return An instance of class \code{MSnSet}.
##' @author Samuel Wieczorek
##' @examples 
##' utils::data(Exp1_R25_pept, package='DAPARdata')
##' addOriginOfValue(Exp1_R25_pept)
addOriginOfValue <- function(obj,index=NULL){

if (!is.null(obj@experimentData@other$OriginOfValues)){
 print("Dataframe already exists. No modification has been made to the MSnset object.")
  return (obj)
  }


if (!is.null(index))
{
  OriginOfValues <- Biobase::fData(obj)[,index]
}else {   
  OriginOfValues <- data.frame(matrix(rep("unknown", nrow(Biobase::exprs(obj))*ncol(Biobase::exprs(obj))), 
                                      nrow=nrow(Biobase::exprs(obj)),
                                      ncol=ncol(Biobase::exprs(obj))),
                               stringsAsFactors = FALSE)
}
    
OriginOfValues[is.na(obj)] <-  "POV"
rownames(OriginOfValues) <- rownames(Biobase::fData(obj))
colnames(OriginOfValues) <- paste0("OriginOfValue",colnames(Biobase::exprs(obj)))
colnames(OriginOfValues) <- gsub(".", "_", colnames(OriginOfValues), fixed=TRUE)

#indMin <- length(colnames(fData(obj)))
#indMax <- length(colnames(fData(obj))) + length(OriginOfValues)
Biobase::fData(obj) <- cbind(Biobase::fData(obj), OriginOfValues, deparse.level = 0)

obj@experimentData@other$OriginOfValues <- colnames(OriginOfValues)

obj <- setMEC(obj)

return(obj)
}



##' Builds an object of class \code{MSnSet} from a 
##' single tabulated-like file for quantitative and meta-data and a dataframe 
##' for the samples description. It differs from
##' the original \code{MSnSet} builder which requires three separated files 
##' tabulated-like quantitative proteomic data into a \code{MSnSet} object,
##' including metadata.
##' 
##' @title Creates an object of class \code{MSnSet} from text file
##' @param file The name of a tab-separated file that contains the data.
##' @param metadata A dataframe describing the samples (in lines).
##' @param indExpData A vector of string where each element is the name
##' of a column in designTable that have to be integrated in
##' the \code{fData()} table
##' of the \code{MSnSet} object.
##' @param indFData The name of column in \code{file} that will be the name of
##' rows for the \code{exprs()} and \code{fData()} tables
##' @param indiceID The indice of the column containing the ID of entities 
##' (peptides or proteins)
##' @param indexForOriginOfValue xxxxxxxxxxx
##' @param logData A boolean value to indicate if the data have to be
##' log-transformed (Default is FALSE)
##' @param replaceZeros A boolean value to indicate if the 0 and NaN values of
##' intensity have to be replaced by NA (Default is FALSE)
##' @param pep_prot_data A string that indicates whether the dataset is about
##' @param proteinId xxxx
##' @param versions A list of the following items: Prostar_Version, DAPAR_Version
##' peptides or proteins.
##' @return An instance of class \code{MSnSet}.
##' @author Florence Combes, Samuel Wieczorek
##' @examples 
##' require(Matrix)
##' exprsFile <- system.file("extdata", "Exp1_R25_pept.txt", package="DAPARdata")
##' metadataFile <- system.file("extdata", "samples_Exp1_R25.txt", package="DAPARdata")
##' metadata = read.table(metadataFile, header=TRUE, sep="\t", as.is=TRUE)
##' indExpData <- c(56:61)
##' indFData <- c(1:55,62:71)
##' indiceID <- 64
##' createMSnset(exprsFile, metadata,indExpData,  indFData, indiceID, indexForOriginOfValue = NULL, pep_prot_data = "peptide")
createMSnset <- function(file,metadata=NULL,indExpData,indFData,indiceID=NULL,
                         indexForOriginOfValue = NULL,
                         logData=FALSE, replaceZeros=FALSE,
                         pep_prot_data=NULL,
                         proteinId = NULL,
                         versions=NULL){
    
    if (!is.data.frame(file)){ #the variable is a path to a text file
        data <- read.table(file, header=TRUE, sep="\t", stringsAsFactors = FALSE)
    } else {data <- file}
    
    ## replace all blanks by a dot
    ##   cols <- gsub(" ","\\.",  colnames(data)[indExpData])
    ##   dotIndice <- regexpr(pattern = '.',cols, fixed=TRUE) [1]
    ##   pattern <- substr(cols,1,dotIndice)
    ##   cols <- sub(pattern[1], replacement="", cols)
    #intensities <- as.matrix(data[,indExpData])
    #intensities <- gsub(",", ".", intensities)
    ##building exprs Data of MSnSet file
    Intensity <- matrix(as.numeric(gsub(",", ".",as.matrix(data[,indExpData] )))
                        , ncol=length(indExpData)
                        , byrow=FALSE)
    
    colnames(Intensity) <- gsub(".", "_", colnames(data)[indExpData], fixed=TRUE)
    rownames(Intensity) <- rownames(data)
    ##the name of lines are the same as the data of the first column
    # if (is.null(indiceID)) {
    #     rownames(Intensity) <- rep(paste(pep_prot_data, "_", 1:nrow(Intensity), sep=""))
    # }else{rownames(Intensity) <- data[,indiceID]}
    
    ##building fData of MSnSet file
    fd <- data.frame( data[,indFData],stringsAsFactors = FALSE)
    
    if (is.null(indiceID)) {
        rownames(fd) <- rep(paste(pep_prot_data, "_", 1:nrow(fd), sep=""))
        rownames(Intensity) <- rep(paste(pep_prot_data, "_", 1:nrow(Intensity), sep=""))
    }else{
        rownames(fd) <- data[,indiceID]
        rownames(Intensity) <- data[,indiceID]
    }
    
    colnames(fd) <- gsub(".", "_", colnames(data)[indFData], fixed=TRUE)
    
     pd <- as.data.frame(metadata,stringsAsFactors = FALSE)
    rownames(pd) <- gsub(".", "_", pd$Sample.name, fixed=TRUE)
    pd$Sample.name <- gsub(".", "_", pd$Sample.name, fixed=TRUE)
    
    ##Integrity tests
    if(identical(rownames(Intensity), rownames(fd))==FALSE)
        stop("Problem consistency between
             row names expression data and featureData")
    
    if(identical(colnames(Intensity), rownames(pd))==FALSE) 
        stop("Problem consistency between column names 
             in expression data and row names in phenoData")
    
    obj <- MSnSet(exprs = Intensity, fData = fd, pData = pd)
    
    if (logData) {
        Biobase::exprs(obj) <- log2(Biobase::exprs(obj))
        obj@processingData@processing <- 
            c(obj@processingData@processing, "Data has been Log2 tranformed")
    }
    
    if (replaceZeros) {
        Biobase::exprs(obj)[Biobase::exprs(obj) == 0] <- NA
        Biobase::exprs(obj)[is.nan(Biobase::exprs(obj))] <- NA
        Biobase::exprs(obj)[is.infinite(Biobase::exprs(obj))] <-NA
        obj@processingData@processing <- c(obj@processingData@processing, "All zeros were replaced by NA")
    }
    
    
    if (!is.null(pep_prot_data)) {
        obj@experimentData@other$typeOfData <- pep_prot_data
    }
    
    obj@experimentData@other$Prostar_Version <- versions$Prostar_Version
    obj@experimentData@other$DAPAR_Version <- versions$DAPAR_Version
    obj@experimentData@other$proteinId <- proteinId
    
    
    obj@experimentData@other$RawPValues <- FALSE
    
    obj <- addOriginOfValue(obj,indexForOriginOfValue)
    
    return(obj)
}


##' This function exports a \code{MSnSet} data object to a Excel file.
##' Each of the 
##' three data.frames in the \code{MSnSet} object (ie experimental data,
##' phenoData
##' and metaData are respectively integrated into separate sheets in
##' the Excel file).
##' The colored cells in the experimental data correspond to the original missing values
##' which have been imputed.
##' 
##' @title This function exports a \code{MSnSet} object to a Excel file.
##' @param obj An object of class \code{MSnSet}.
##' @param filename A character string for the name of the Excel file.
##' @return A Excel file (.xlsx)
##' @author Samuel Wieczorek
##' @examples
##' \donttest{
##' Sys.setenv("R_ZIPCMD"= Sys.which("zip"))
##' utils::data(Exp1_R25_pept, package='DAPARdata')
##' obj <- Exp1_R25_pept[1:1000]
##' writeMSnsetToExcel(obj, "foo")
##' }
writeMSnsetToExcel <- function(obj, filename)
{
    #require(Matrix)
    POV_Style <- openxlsx::createStyle(fgFill = "lightblue")
    MEC_Style <- openxlsx::createStyle(fgFill = "orange")
    
    #require(openxlsx)
    name <- paste(filename, ".xlsx", sep="")
    wb <- openxlsx::createWorkbook(name)
    n <- 1
    openxlsx::addWorksheet(wb, "Quantitative Data")
    openxlsx::writeData(wb, sheet=n, cbind(ID = rownames(Biobase::exprs(obj)),
                                           Biobase::exprs(obj)), rowNames = FALSE)
    
   
    if (is.null(obj@experimentData@other$OriginOfValues)){
      listPOV <-  which(is.na(exprs(obj)), arr.ind=TRUE)
    } else {
        mat <- fData(obj)[,obj@experimentData@other$OriginOfValues]
        listPOV <- which(mat=="POV", arr.ind=TRUE)
        listMEC <- which(mat=="MEC", arr.ind=TRUE)
    }
    
    openxlsx::addStyle(wb, sheet=n, cols = listPOV[,"col"]+1, rows = listPOV[,"row"]+1, style = POV_Style)
    openxlsx::addStyle(wb, sheet=n, cols = listMEC[,"col"]+1, rows = listMEC[,"row"]+1, style = MEC_Style)
    
    #bodyStyleNumber <- createStyle(numFmt = "NUMBER")
    #addStyle(wb, sheet=1, bodyStyleNumber, rows = 2:nrow(Biobase::exprs(obj)), 
    #cols=2:ncol(Biobase::exprs(obj)),gridExpand = TRUE)
    
    openxlsx::addWorksheet(wb, "Samples Meta Data")
    n <- n +1
    openxlsx::writeData(wb, sheet=n, Biobase::pData(obj), rowNames = FALSE)
    n <- n +1
    if (dim(Biobase::fData(obj))[2] != 0){
        openxlsx::addWorksheet(wb, "Feature Meta Data")
        #numericCols <- which(sapply(Biobase::fData(obj), is.numeric))
        #Biobase::fData(obj)[,numericCols] <- 
        #format(Biobase::fData(obj)[,numericCols])
        
        openxlsx::writeData(wb, sheet=n, cbind(ID = rownames(Biobase::fData(obj)),
                                               Biobase::fData(obj)), rowNames = FALSE)
        #bodyStyleNumber <- createStyle(numFmt = "NUMBER")
        #addStyle(wb, sheet=3, bodyStyleNumber, 
        #rows = 2:nrow(Biobase::exprs(obj)), cols=numericCols, 
        #gridExpand = TRUE, stack=TRUE)
        
    }
    
    if (!is.null(obj@experimentData@other$GGO_analysis))
    {
        l <- length(obj@experimentData@other$GGO_analysis$ggo_res)
        for (i in 1:l){
            n <- n +1
            level <- as.numeric(obj@experimentData@other$GGO_analysis$levels[i])
            openxlsx::addWorksheet(wb, paste("Group GO - level ", level, sep=""))
            openxlsx::writeData(wb, sheet=n, obj@experimentData@other$GGO_analysis$ggo_res[[i]]$ggo_res@result)
        }
    }
    
    if (!is.null(obj@experimentData@other$EGO_analysis))
    {
        n <- n +1
        openxlsx::addWorksheet(wb, "Enrichment GO")
        openxlsx::writeData(wb, sheet=n, obj@experimentData@other$EGO_analysis$ego_res@result)
        
    }
    
    openxlsx::saveWorkbook(wb, name, overwrite=TRUE)
    return(name)
    
    
}

##' This function reads a sheet of an Excel file and put the data into a data.frame.
##' 
##' @title This function reads a sheet of an Excel file and put the data into a data.frame.
##' @param file The name of the Excel file.
##' @param extension The extension of the file
##' @param sheet The name of the sheet
##' @return A data.frame
##' @author Samuel Wieczorek
readExcel <- function(file, extension, sheet){
  # data <- NULL
  # if (extension=="xls") {
  #     data <- readxl::read_xls(file, sheet)
  # }
  # else if (extension=="xlsx") {
  #     data <- readxl::read_xlsx(file, sheet)
  # }
  # return(as.data.frame(data,asIs=T))
  
  #options(digits=10)
  data <- NULL
  data <- readxl::read_excel(file, sheet)
  
  return(as.data.frame(data,asIs=T, stringsAsFactors=F))
    
}


##' This function lists all the sheets of an Excel file.
##' 
##' @title This function returns the list of the sheets names in a Excel file.
##' @param file The name of the Excel file.
##' @return A vector
##' @author Samuel Wieczorek
listSheets <- function(file){
    #require(openxlsx)
    return(openxlsx::getSheetNames(file))
    
}


##' This function exports a MSnset dataset into three csv files compressed in a zip file
##' 
##' @title Exports a MSnset dataset into a zip archive containing three zipped CSV files.
##' @param obj An object of class \code{MSnSet}.
##' @param fname The name of the archive file.
##' @return A compressed file
##' @author Samuel Wieczorek
##' @examples
##' \donttest{
##' utils::data(Exp1_R25_pept, package='DAPARdata')
##' obj <- Exp1_R25_pept[1:1000]
##' writeMSnsetToCSV(obj, "foo")
##' }
writeMSnsetToCSV <- function(obj, fname){
    
    #fname <- paste(tempdir(),fname,  sep="/")
    write.csv(Biobase::exprs(obj), paste(tempdir(), "exprs.csv", sep='/'))
    write.csv(Biobase::fData(obj), paste(tempdir(), "fData.csv", sep='/'))
    write.csv(Biobase::pData(obj), paste(tempdir(), "pData.csv", sep='/'))
    files <- c(paste(tempdir(), "exprs.csv", sep='/'),
               paste(tempdir(), "fData.csv", sep='/'),
               paste(tempdir(), "pData.csv", sep='/'))
    zip(fname, files, zip = Sys.getenv("R_ZIPCMD", "zip"))
    
    return(fname)
}


##' Similar to the function \code{rbind} but applies on two subsets of the same \code{MSnSet} object.
##' 
##' @title Similar to the function \code{rbind} but applies on two subsets of the same \code{MSnSet} object.
##' @param df1 An object (or subset of) of class \code{MSnSet}. May be NULL
##' @param df2 A subset of the same object as df1
##' @return An instance of class \code{MSnSet}.
##' @author Samuel Wieczorek
##' @examples
##' utils::data(Exp1_R25_pept, package='DAPARdata')
##' df1 <- Exp1_R25_pept[1:100]
##' df2 <- Exp1_R25_pept[200:250]
##' rbindMSnset(df1, df2)
rbindMSnset <- function(df1=NULL, df2){
  
  if (is.null(df1)){
    obj <- df2
    return(obj)
  }
  if (is.null(df1) && is.null(df2)){return(NULL)}
    
  tmp.exprs <- rbind(exprs(df1), exprs(df2))
  tmp.fData <- rbind(fData(df1), fData(df2))
  tmp.pData <- pData(df1)
  
  obj <-  MSnSet(exprs = tmp.exprs, fData = tmp.fData, pData = tmp.pData)
  obj@protocolData <- df1@protocolData
  obj@experimentData <- df1@experimentData
  
  return(obj)
  
}
