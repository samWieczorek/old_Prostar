# Declaration of global variables

commandLogFile <- "cmdLog.R"
logfilename <- "log.txt"
gFileExtension <- list(txt = ".txt",
                       tsv = ".tsv",
                        msnset = ".MSnset",
                        excel = ".xlsx",
                        zip = ".zip")

gAgregateMethod <- list("none" = "none",
                        "sum overall" = "sum overall",
                        "mean" = "mean",
                        "sum on top n" = "sum on top n")

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
                            "LeftCensored - QRILC" = "QRILC",
                            "RandomOccurence - BPCA" = "BPCA",
                            "RandomOccurence - KNN" = "KNN",
                            "RandomOccurence - MLE" = "MLE",
                            "imp4p" = "imp4p"
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

DT_pagelength <- 15



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



gGraphicsFilenames <- list(
    histoMV_DS = "histoMV_DS.png",
    histoMVPerLines_DS = "histoMissvaluesPerLines_DS.png",
    histoMVPerLinesConditions_DS = "histoMVPerLinesPerConditions_DS.png",
    histoMV = "histoMV_DS.png",
    histoMVPerLines = "histoMissvaluesPerLines.png",
    histoMVPerLinesConditions = "histoMVPerLinesPerConditions.png",
    corrMatrix = "corrMatrix.png",
    heatmap = "heatmap.png",
    boxplot = "boxplot.png",
    varDist = "varDist.png",
    densityPlot = "densityPlot.png",
    densityPlotNorm = "densityPlotNorm.png",
    propContRev = "propContRev.png",
    boxplotNorm = "boxplotNorm.png",
    compareNorm = "compareNorm.png",
    MVtypePlot = "MVtypePlot.png",
    imageNA = "imageNA.png",
    AgregMatUniquePeptides = "AgregMatUniquePeptides.png",
    AgregMatSharedPeptides = "AgregMatSharedPeptides.png",
    volcanoPlot_1 = "volcanoPlot_1.png",
    volcanoPlot_3 = "volcanoPlot_3.png",
    calibrationPlot = "calibrationPlot.png",
    calibrationPlotAll = "calibrationPlotAll.png"

    
)

defaultGradientRate <- 5




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
    temp <- unlist(strsplit(name,'.', fixed=T))
    return(last(temp))
    }



#' busyIndicator
busyIndicator <- function(text = "Calculation in progress..",
                        img = "images/ajax-loader.gif", wait=1000) {
tagList(
    singleton(tags$head(
    tags$link(rel="stylesheet", 
            type="text/css",href="busyIndicator/busyIndicator.css")
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

########################################################
# FROM :http://stackoverflow.com/questions/35271661/update-shiny-r-custom-progressbar/39265225#39265225
progressBar2 <- function(inputId=NULL, value=0, label=FALSE, color="info", size=NULL,
                         striped=FALSE, active=FALSE, vertical=FALSE) {
    stopifnot(is.numeric(value))
    if (value < 0 || value > 100)
        stop("'value' should be in the range from 0 to 100", call. = FALSE)
    #if (!(color %in% shinydashboard:::validColors || color %in% shinydashboard:::validStatuses))
    #    stop("'color' should be a valid status or color.", call. = FALSE)
    if (!is.null(size))
        size <- match.arg(size, c("sm", "xs", "xxs"))
    text_value <- paste0(value, "%")
    if (vertical)
        style <- htmltools::css(height = text_value, `min-height` = "2em")
    else
        style <- htmltools::css(width = text_value, `min-width` = "2em")
    htmltools::tags$div(
        class = "progress",
        class = if (!is.null(size)) paste0("progress-", size),
        class = if (vertical) "vertical",
        class = if (active) "active",
        htmltools::tags$div(
            class = "progress-bar",
            class = paste0("progress-bar-", color),
            class = if (striped) "progress-bar-striped",
            id = paste0(inputId),
            role = "progressbar",
            style = style,
            `aria-valuemax` = 100,
            `aria-valuemin` = 0,
            `aria-valuenow` = value,
            htmltools::tags$span(
                id = "text_value",
                class = if (!label) "sr-only", 
                text_value)
        )
    )
}


updatePB <- function(session,inputId=NULL,value=NULL,label=NULL,color=NULL,text_value = NULL,size=NULL,striped=NULL,active=NULL,vertical=NULL) {
    data <- dropNulls(list(id=inputId,value=value,label=label,color=color,text_value=text_value,size=size,striped=striped,active=active,vertical=vertical))
    
    session$sendCustomMessage("updateprogress",data)
}

dropNulls <-function(x) {
    x[!vapply(x,is.null,FUN.VALUE=logical(1))]
}




########################################################

# Author: https://jackolney.github.io/2016/shiny/
progressGroup <- function(text, value, min = 0, max = value, color = "aqua") {
    stopifnot(is.character(text))
    stopifnot(is.numeric(value))
    if (value < min || value > max)
        stop(sprintf("'value' should be in the range from %d to %d.", min, max), call. = FALSE)
    tags$div(
        class = "progress-group",
        tags$span(class = "progress-text", text),
        tags$span(class = "progress-number", sprintf("%d / %d", value, max)),
        prgoressBar(round(value / max * 100), color = color, size = "sm")
    )
}


findSequences <- function(v){
    diff <- v[2:length(v)] - v[1:(length(v)-1)]
    
    if (!all(diff != 1)){
        s <- rle(diff == 1)
        begin <- c(0, cumsum(s$lengths))[which(s$values)] + 1
        end <- cumsum(s$lengths)[which(s$values)] +1

        seq <- "c("
        temp <- NULL
        i <- 1
    
        while(i < begin[1] && i < length(v))
        {
            seq <- paste(seq, v[i], ",", sep="")
            i <- i + 1
        }
    
        for (i in 1:length(begin)){
         seq <- paste(seq, v[begin[i]], ":", v[end[i]], sep="")
            if (i < length(begin)) {seq <- paste(seq, ",", sep="")}
            if (i < length(begin) && ((begin[i+1] - end[i]) > 1)){
                for(j in c((end[i]+1):(begin[i+1]-1))) {
                    seq <- paste(seq, v[j], ",", sep="")
                }
            }
        }
    
        i <- last(end) +1
        if (i <= length(v)) {seq <- paste(seq, ",", sep="")}
        while(i <= length(v))
        {
            seq <- paste(seq, v[i])
            if (i < length(v)) {seq <- paste(seq, ",", sep="")}
            i <- i +1
        }
    
    
        seq <- paste(seq, ")", sep="")
    
        }
    else 
        {
        seq <- paste("c(", paste(diff, collapse=","),")", sep="")
        }
    return(seq)
}





