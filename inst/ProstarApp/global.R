DAPAR.loc <- DAPARdata.loc <- Prostar.loc <- NULL
#DAPARdata.loc <- DAPAR.loc <- Prostar.loc <- "/home/shiny/Rlibs_test"



#######
## Definition des noms des modules qui composent les pipelines
## Ces noms doivent etre strictement identiques aux noms des modules
## dans les fichiers source sans quoi, Prostar ne pourra pas les trouver
## TODO : faire une gestion d'erreur en cas d'absence d'un module
######
peptide.def <- c('moduleA','moduleB','moduleC')
protein.def <- c('moduleD','moduleE','moduleF','moduleG')
p2p.def <- c('moduleH','moduleI')


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
}



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



base_URL <- "https://samwieczorek.github.io/samWieczorek/md/"

#base_URL <- "https://raw.githubusercontent.com/samWieczorek/Prostar/master/inst/ProstarApp/md/"
URL_FAQ <- paste0(base_URL, "FAQ.md")
URL_links <- paste0(base_URL, "links.md")
URL_ProstarPresentation <- paste0(base_URL, "presentation.md")
URL_formerReleases <-paste0(base_URL, "formerReleases.md")
URL_versionNotes <- paste0(base_URL, "versionNotes.md")

actionBtnClass <- "btn-primary"
