# Declaration of global variables

logfilename <- "log.txt"
gFileExtension <- list(txt = ".txt",
                        msnset = ".MSnset",
                        excel = ".xls",
                        zip = ".zip")

gAgregateMethod <- list(sum = "sum overall",
                        mean = "mean",
                        topn = "sum on top n")

gFiltersList <- list()
gFiltersList[["None"]] <- "none"
gFiltersList[["Whole matrix"]] <- "wholeMatrix"
gFiltersList[["For every condition"]] <- "allCond"
gFiltersList[["At least one condition"]] <- "atLeastOneCond"

gDatasets <- list()
gDatasets[["NA"]] <- "none"

gFilterNone <- gFiltersList[["None"]]
gFilterWholeMat <- gFiltersList[["Whole matrix"]]
gFilterAllCond <- gFiltersList[["For every condition"]]
gFilterOneCond <- gFiltersList[["At least one condition"]]

# variables for filtering the data
gReplaceAllZeros <- "replaceAllZeros"
gLogTransform <- "Log2 tranformed data"
gFilterTextPrefix <- "Filtered with"

pData.complete.list <- list("Label" = "Label", 
                            "Bio.Rep" = "Bio. rep.",
                            "Tech.Rep" = "Tech. rep.", 
                            "Analyt.Rep" = "An. Rep.")


normMethods <- list("None" = "None",
                    "Global Rescaling - sum by columns" = 
                    "Global Rescaling - sum by columns",
                    
                    "Global Rescaling - quantiles" =  
                    "Global Rescaling - quantiles",
                    
                    "Median Centering - overall" = 
                    "Median Centering - overall",
                    
                    "Median Centering - within conditions" = 
                    "Median Centering - overall",
                    
                    "Mean Centering - overall" = 
                    "Mean Centering - overall", 
                    
                    "Mean Centering - within conditions" = 
                    "Mean Centering - within conditions", 
                    
                    "Mean Centering Scaling - overall" = 
                    "Mean Centering Scaling - overall",
                    
                    "Mean Centering Scaling - within conditions" = 
                    "Mean Centering Scaling - within conditions")


imputationAlgorithms <- list("None" = "None",
                            "LeftCensored - QRILC" = 
                            "LeftCensored - QRILC",

                            "RandomOccurence - BPCA" = 
                            "RandomOccurence - BPCA",

                            "RandomOccurence - KNN" = 
                            "RandomOccurence - KNN",

                            "RandomOccurence - MLE" = 
                            "RandomOccurence - MLE"
)

JSCSSTags <- function() 
{ 
list(
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


reactiveDataTable <- function(func, ...) 
{
    reactive(function() 
    {
    classNames <- 
        getOption("shiny.table.class", "table table-striped table-bordered")
        classID = getOption("shiny.table.id", "example")
    data <- func()
    if (is.null(data) || is.na(data)) 
        return("")
    return(paste(
        capture.output(
        print(xtable(data, ...), 
        type = "html", 
        html.table.attributes = 
            paste("class=\"", classNames, "\" id=\"", classID, "\"", sep = ""),
            ...)), collapse = "\n"))

})
}


# for layout
resizeComponents <- function(){
tags$head(
    tags$style(type="text/css", "textarea { max-width: 400px; }"),
    tags$style(type='text/css', 
            ".well { max-width: 300px; max-height=300px;}"),
    tags$style(type='text/css', ".span4 { max-width: 300px; }")
)
}


GetFilterText <- function(type, seuil){
return (
    paste(gFilterTextPrefix," ",type , " (threshold = ", seuil, " ).", sep=""))
}


# variables for different extensions files format
gFileFormatExport <- list(msnset = "MSnset",excel = "Excel")

# Not used yet 
GetLogFilename <- function(){
return(paste("loG__",Sys.getpid(), ".txt", sep=""))
}

#---------------------------------------------------------
GetChoices <- function(){

choix <- list.dirs(path=dir, recursive=FALSE)
names <- c()
for (i in 1:length(choix)){
    names[i] <-
    unlist(strsplit(choix[i], '/'))[length(unlist(strsplit(choix[i], '/')))]
}
return(setNames(names, names))
}


#--------------------------------------------------------
DeleteFileExtension <- function(name){
return(strsplit(name,'.', fixed=T)[[1]][1])}

#--------------------------------------------------------
GetExtension <- function(name){
return(strsplit(name,'.', fixed=T)[[1]][2])}



#' busyIndicator
#' 
#' A busy indicator
#' 
#' @param text The text to show
#' @param img An anitmated gif
#' @param wait The amount of time to wait before showing the busy indicator. The
#'   default is 1000 which is 1 second.
#'   
#' @export
# busyIndicator <- function(text = "Calculation in progress..",img = "ajax-loader.gif", wait=1000) {
#   
#   conditionalPanel(
#     condition="($('html').hasClass('shiny-busy'))",
#     strong(div(text, style = "color:blue")),
#    # h4(text,style="color:red"),
#     img(src="images/ajax-loader.gif")
#   )
#   }

busyIndicator <- function(text = "Calculation in progress..",img = "images/ajax-loader.gif", wait=1000) {
  tagList(
    singleton(tags$head(
      tags$link(rel="stylesheet", type="text/css",href="busyIndicator/busyIndicator.css")
    ))
    ,div(class="busy-indicator",p(text),img(src=img))
    ,tags$script(sprintf(
      "	setInterval(function(){
      if ($('html').hasClass('shiny-busy')) {
      setTimeout(function() {
      if ($('html').hasClass('shiny-busy')) {
      $('div.busy-indicator').show()
      }
      }, %d)  		    
      } else {
      $('div.busy-indicator').hide()
      }
},100)
      ",wait)
    )
  )	
}

