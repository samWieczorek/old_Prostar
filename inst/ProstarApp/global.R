DAPAR.loc <- DAPARdata.loc <- Prostar.loc <- NULL
#DAPARdata.loc <- DAPAR.loc <- Prostar.loc <- "/home/shiny/Rlibs_test"




#5lstDescPlots <- c("intensity", "mv")
lstDescPlots <- c("intensity", "pca", "varDist", "corrMatrix", "heatmap", "mv", "quantiTable")



#library(shinycssloaders)

#library(rclipboard)


#library(shinyBS)
#library(shinyTree)
#library(shinyWidgets)
#library(vioplot)
# library(colourpicker)
#library(data.table)
#library(MSnbase)
#library(rhandsontable)
#library(data.table)
#library(shinyjqui)
#library(RColorBrewer)
#library(DT)


loadLibraries <- function(){
  
  library(gplots)
  library(tidyverse)
  library(DAPAR, lib.loc = DAPAR.loc)
  library(R.utils)
  library(highcharter)
  
  library(future)
  library(promises)
  
}

imputationAlgorithms <- c("None" = "None",
                          "imp4p" = "imp4p",
                          "Basic methods" = "BasicMethods")

basicMethodsImputationAlgos <- c("None" = "None",
                                 "Det. quantile" = "detQuantile",
                                 "KNN" = "KNN",
                                 "MLE" = "MLE"
)

imputationAlgorithmsPeptides_POV <- list("None" = "None",
                                         "imp4p" = "imp4p",
                                         "slsa" = "slsa",
                                         "Det. quantile" = "detQuantile",
                                         "KNN" = "KNN")

imputationAlgorithmsProteins_POV <- list("None" = "None",
                                         "slsa" = "slsa",
                                         "Det quantile" = "detQuantile",
                                         "KNN" = "KNN")

imputationAlgorithmsPeptides_MEC<- list("None" = "None",
                                        "Det quantile" = "detQuantile",
                                        "Fixed value" = "fixedValue")

imputationAlgorithmsProteins_MEC <- list("None" = "None",
                                         "Det quantile" = "detQuantile",
                                         "Fixed value" = "fixedValue")

JSCSSTags <- function() 
{ 
  list(
    tags$script(src="////code.highcharts.com/highcharts.js",type="text/javascript"),
    
    tags$script(src="js/jquery.js",type="text/javascript"),
    tags$script(src="js/jquery.dataTables.js",type="text/javascript"),
    tags$link(href='css/jquery.dataTables.css', rel="stylesheet", 
              type="text/css"), 
    tags$link(href='css/dataTables.tableTools.css', rel="stylesheet", 
              type="text/css"), 
    tags$script(src='js/dataTables.tableTools.js'),
    tags$link(rel="stylesheet", type="text/css",href="css/style.css"),
    tags$script(type="text/javascript", src = "busy.js")
  )
}
WaitMsgPlot <- "Building plot. Please wait..."
WaitMsgCalc <- "Calculation in progress"


group2ColorByDefault <- "Condition"

listBrewerPalettes <- c("Dark2 (qualit.)" = "Dark2",
                        "Accent (qualit.)"="Accent",
                        "Paired (qualit.)" = "Paired",
                        "Pastel1 (qualit.)" = "Pastel1",
                        "Pastel2 (qualit.)" = "Pastel2",
                        "Set1 (qualit.)" = "Set1",
                        "Set2 (qualit.)" = "Set2", 
                        "Set3 (qualit.)" = "Set3",
                        "BrBG (diverging)"="BrBG",
                        "PiYG (diverging)"=  "PiYG",
                        "PRGn (diverging)" ="PRGn",
                        "PuOr (diverging)" ="PuOr",
                        "RdBu (diverging)"="RdBu",
                        "RdGy (diverging)" ="RdGy",
                        "RdYlBu (diverging)" ="RdYlBu",
                        "RdYlGn (diverging)" ="RdYlGn",
                        "Spectral (diverging)"="Spectral")




defaultGradientRate <- 0.9
actionBtnClass <- "btn-primary"

PrevNextBtnClass <- "btn-info"
optionsBtnClass <- "info"
plotWidth <- "800px"
plotHeight <- "600px"
spinnerType <- 4

limitHeatmap <- 20000
G_heatmapDistance_Choices <- list("Euclidean" ="euclidean",
                                  "Manhattan"="manhattan",
                                  "Maximum" = "maximum",
                                  "Canberra" = "canberra",
                                  "Binary" = "binary",
                                  "Minkowski" = "minkowski")

G_heatmapLinkage_Choices <- list("Complete" = "complete",
                                 "Average"="average",
                                 "Ward.D"="ward.D",
                                 "Ward.D2"="ward.D2",
                                 "Single" = "single",
                                 "Centroid" = "centroid",
                                 "Mcquitty" = "mcquitty",
                                 "Median" = "median")



## URLs for the .md files stored in the website github directory
base_URL <- "http://www.prostar-proteomics.org/md/"
URL_FAQ <- paste0(base_URL, "FAQ.md")
URL_links <- paste0(base_URL, "links.md")
URL_ProstarPresentation <- paste0(base_URL, "presentation.md")
URL_formerReleases <-paste0(base_URL, "formerReleases.md")
URL_versionNotes <- paste0(base_URL, "versionNotes.md")



## gestion des couleurs

grey <- "#FFFFFF"
orangeProstar <- "#E97D5E"






G_noneStr <- "None"
G_heatmapDistance_Choices <- list("Euclidean" ="euclidean",
                                  "Manhattan"="manhattan",
                                  "Maximum" = "maximum",
                                  "Canberra" = "canberra",
                                  "Binary" = "binary",
                                  "Minkowski" = "minkowski")

G_heatmapLinkage_Choices <- list("Complete" = "complete",
                                 "Average"="average",
                                 "Ward.D"="ward.D",
                                 "Ward.D2"="ward.D2",
                                 "Single" = "single",
                                 "Centroid" = "centroid",
                                 "Mcquitty" = "mcquitty",
                                 "Median" = "median")

def.progress.loadDataset <- c('Clear memory', 'Load dataset', 'Configure object', 'Load in memory')


GetExtension <- function(name){
  temp <- unlist(strsplit(name,'.', fixed=T))
  return(last(temp))
}


#--------------------------------------------------------
DeleteFileExtension <- function(name){
  return(strsplit(name,'.', fixed=T)[[1]][1])}

#--------------------------------------------------------
GetExtension <- function(name){
  temp <- unlist(strsplit(name,'.', fixed=T))
  return(last(temp))
}



gFiltersList <- c("None" = "None",
                  "Empty lines" = "EmptyLines",
                  "Whole matrix" = "wholeMatrix",
                  "For every condition" = "allCond",
                  "At least one condition" = "atLeastOneCond")

gFilterNone <- gFiltersList[["None"]]
gFilterEmptyLines <- gFiltersList[["Empty lines"]]
gFilterWholeMat <- gFiltersList[["Whole matrix"]]
gFilterAllCond <- gFiltersList[["For every condition"]]
gFilterOneCond <- gFiltersList[["At least one condition"]]

# variables for filtering the data
gReplaceAllZeros <- "replaceAllZeros"
gLogTransform <- "Log2 tranformed data"
gFilterTextPrefix <- "Filtered with"



calibMethod_Choices <- c("Benjamini-Hochberg", 
                         "st.boot", "st.spline", 
                         "langaas","jiang", "histo", 
                         "pounds", "abh","slim", 
                         "numeric value")

anaDiffMethod_Choices <- c("None"="None",
                           "Limma"="Limma", 
                           "t-tests"="ttests")


G_noneStr <- "None"
G_emptyStr <- ""
G_heatmapDistance_Choices <- list("Euclidean" ="euclidean",
                                  "Manhattan"="manhattan",
                                  "Maximum" = "maximum",
                                  "Canberra" = "canberra",
                                  "Binary" = "binary",
                                  "Minkowski" = "minkowski")

G_heatmapLinkage_Choices <- list("Complete" = "complete",
                                 "Average"="average",
                                 "Ward.D"="ward.D",
                                 "Ward.D2"="ward.D2",
                                 "Single" = "single",
                                 "Centroid" = "centroid",
                                 "Mcquitty" = "mcquitty",
                                 "Median" = "median")


G_logFC_Column <- "logFC"


G_sourceOfProtID_Choices <- c("Select a column in dataset" = "colInDataset",
                              "Choose a file" = "extFile")

G_ontology_Choices <- c("Molecular Function (MF)"="MF" , 
                        "Biological Process (BP)" = "BP", 
                        "Cellular Component (CC)" = "CC")

G_universe_Choices <- c("Entire organism" = "Entire organism",
                        "Entire dataset" = "Entire dataset",
                        "Custom" = "Custom")

G_pAdjustMethod_Choices <- c("BH", "fdr", "None")

G_imp4PDistributionType_Choices <- c("uniform" = "unif", "beta" = "beta")

G_ConvertDataID_Choices <- c("Auto ID" = "Auto ID", "Custom ID" = "custom ID")
G_exportFileFormat_Choices <- c( "MSnset","Excel", "zip")
gFileFormatExport <- list(msnset = "MSnset",excel = "Excel", zip="zip")
gFileExtension <- list(txt = ".txt",
                       tsv = ".tsv",
                       msnset = ".MSnset",
                       excel = ".xlsx",
                       zip = ".zip")


bsButtonRight <- function(...) {
  btn <- bsButton(...)
  # Directly inject the style into the shiny element.
  btn$attribs$style <- "float: right;"
  btn
}

actionBtnClass <- "btn-primary"

PrevNextBtnClass <- "btn-info"
optionsBtnClass <- "info"
