DAPAR.loc <- DAPARdata.loc <- Prostar.loc <- NULL
#DAPARdata.loc <- DAPAR.loc <- Prostar.loc <- "/home/shiny/Rlibs_test"



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

SetCustomCSS <- function(){
  inlineCSS(".body { font-size:14px;}")
  tags$head(includeCSS("www/css/arrow.css"))
  tags$head(HTML("<script type='text/javascript' src='sbs/shinyBS.js'></script>"))
  tags$head(tags$style(".modal-dialog{ width:200px}"))
  tags$head( tags$style(HTML("hr {border-top: 1px solid #000000;}")))
  includeCSS("www/css/prostar.css")
  #,includeCSS("www/css/fontawesome.css")
  inlineCSS(".body { font-size:14px;}")
  inlineCSS(".rect {float: left;
            width: 100px;
            height: 20px;
            margin: 2px;
            border: 1px solid rgba(0, 0, 0, .2);}")
  inlineCSS(".green {background: #06AB27}")
  inlineCSS(".red {background: #C90404}")
  inlineCSS(".grey {background:lightgrey;}")
  inlineCSS(".modal-backdrop {z-index: 1000}")
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


## URLs for the .md files stored in the website github directory
base_URL <- "https://samwieczorek.github.io/samWieczorek/md/"
#base_URL <- "https://raw.githubusercontent.com/samWieczorek/Prostar/master/inst/ProstarApp/md/"
URL_FAQ <- paste0(base_URL, "FAQ.md")
URL_links <- paste0(base_URL, "links.md")
URL_ProstarPresentation <- paste0(base_URL, "presentation.md")
URL_formerReleases <-paste0(base_URL, "formerReleases.md")
URL_versionNotes <- paste0(base_URL, "versionNotes.md")

actionBtnClass <- "btn-primary"








######
### Miscelllaneous functtions


# Call this function with all the regular navbarPage() parameters, plus a text parameter,
# if you want to add text to the navbar
navbarPageWithText <- function(..., text) {
  navbar <- navbarPage(...)
  textEl <- tags$p(class = "navbar-text", text)
  navbar[[3]][[1]]$children[[1]] <- htmltools::tagAppendChild(
    navbar[[3]][[1]]$children[[1]], textEl)
  navbar
}

# Call this function with an input (such as `textInput("text", NULL, "Search")`) if you
# want to add an input to the navbar
navbarPageWithInputs <- function(..., inputs) {
  navbar <- navbarPage(...)
  form <- tags$form(class = "navbar-form", inputs)
  navbar[[3]][[1]]$children[[1]] <- htmltools::tagAppendChild(
    navbar[[3]][[1]]$children[[1]], form)
  navbar
}


