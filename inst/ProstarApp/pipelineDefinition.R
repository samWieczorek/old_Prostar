#######
## Definition des noms des modules qui composent les pipelines
## Ces noms doivent etre strictement identiques aux noms des modules
## dans les fichiers source sans quoi, Prostar ne pourra pas les trouver
## TODO : faire une gestion d'erreur en cas d'absence d'un module
######
peptide <- c('moduleFiltering', 'moduleNormalization', 'modulePepImputation', 'moduleHypothesisTest')
protein <- c('moduleD','moduleE','moduleF','moduleG')
p2p <- c('moduleH','moduleI')


pipeline.def <- list(
  peptide = c('moduleFiltering', 'moduleNormalization', 'modulePepImputation', 'moduleHypothesisTest'),
  protein = c('moduleD','moduleE','moduleF','moduleG'),
  p2p = c('moduleH','moduleI')
  
)
path2peptideModules <- 'modules/process/peptide/'
path2proteinModules <- 'modules/process/protein/'
path2p2pModules <- 'modules/process/p2p/'

