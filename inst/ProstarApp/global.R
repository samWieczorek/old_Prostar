DAPAR.loc <- DAPARdata.loc <- Prostar.loc <- NULL
#DAPARdata.loc <- DAPAR.loc <- Prostar.loc <- "/home/shiny/Rlibs_test"

source(file.path(".", "commonFunc.R"),  local = TRUE)$value


#######
## Definition des noms des modules qui composent les pipelines
## Ces noms doivent etre strictement identiques aux noms des modules
## dans les fichiers source sans quoi, Prostar ne pourra pas les trouver
## TODO : faire une gestion d'erreur en cas d'absence d'un module
######
peptide.def <- c('moduleC','moduleB','moduleA')
protein.def <- c('moduleD','moduleE','moduleF','moduleG')
p2p.def <- c('moduleH','moduleI')

path2peptideModules <- 'modules/process/peptide/'
path2proteinModules <- 'modules/process/protein/'
path2p2pModules <- 'modules/process/p2p/'


#5lstDescPlots <- c("intensity", "mv")
lstDescPlots <- c("intensity", "pca", "varDist", "corrMatrix", "heatmap", "mv", "quantiTable")

loadLibraries <- function(){
  library(shinyAce)
  library(shinyWidgets)
  library(vioplot)
  library(colourpicker)
  library(gplots)
  library(data.table)
  library(MSnbase)
  library(tidyverse)
  library(RColorBrewer)
  library(DAPAR, lib.loc = DAPAR.loc)
  library(R.utils)
  library(rhandsontable)
  library(data.table)
  library(shinyjqui)
}



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


library(shinycssloaders)
library(shinythemes)
library(rclipboard)
library(DT)
library(highcharter)
library(shinyBS)
library(shinyTree)
library(future)
library(promises)
plan(multiprocess) 


source(file.path(".", "modules/moduleInsertMarkdown.R"),  local = TRUE)$value
source(file.path(".", "modules/moduleBugReport.R"),  local = TRUE)$value


## URLs for the .md files stored in the website github directory
base_URL <- "https://samwieczorek.github.io/samWieczorek/md/"
#base_URL <- "https://raw.githubusercontent.com/samWieczorek/Prostar/master/inst/ProstarApp/md/"
URL_FAQ <- paste0(base_URL, "FAQ.md")
URL_links <- paste0(base_URL, "links.md")
URL_ProstarPresentation <- paste0(base_URL, "presentation.md")
URL_formerReleases <-paste0(base_URL, "formerReleases.md")
URL_versionNotes <- paste0(base_URL, "versionNotes.md")









G_noneStr <- "None"

def.progress.loadDataset <- c('Clear memory', 'Load dataset', 'Configure object', 'Load in memory')
def.progress.openMSnset <- c('Step 1', 'Step 2', 'Step 3', 'Step 4')