DAPAR.loc <- DAPARdata.loc <- Prostar.loc <- NULL
#DAPARdata.loc <- DAPAR.loc <- Prostar.loc <- "/home/shiny/Rlibs_test"

source(file.path(".", "commonFunc.R"),  local = TRUE)$value



#5lstDescPlots <- c("intensity", "mv")
lstDescPlots <- c("intensity", "pca", "varDist", "corrMatrix", "heatmap", "mv", "quantiTable")



#library(shinycssloaders)

library(rclipboard)




loadLibraries <- function(){
  #library(shinyBS)
  #library(shinyTree)
  #library(shinyWidgets)
  #library(vioplot)
 # library(colourpicker)
  library(gplots)
  #library(data.table)
  #library(MSnbase)
  library(tidyverse)
  #library(RColorBrewer)
  library(DAPAR, lib.loc = DAPAR.loc)
  library(R.utils)
  #library(rhandsontable)
  #library(data.table)
  #library(shinyjqui)
  
  #library(DT)
  library(highcharter)
  
  library(future)
  library(promises)
  
}




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


source(file.path(".", "modules/moduleInsertMarkdown.R"),  local = TRUE)$value
source(file.path(".", "modules/moduleBugReport.R"),  local = TRUE)$value


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



