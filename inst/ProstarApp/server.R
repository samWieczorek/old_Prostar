options(shiny.maxRequestSize=100*1024^2) 
options(shiny.trace=FALSE)
options(shiny.reactlog=TRUE)
#if (!interactive()) sink(stderr(), type = "output")

library(R.utils)
library(highcharter)
require(compiler)
enableJIT(3)
###
source(file.path(".", "global.R"),  local = TRUE)$value

source(file.path("ui", "uiConfigure.R"),  local = TRUE)$value

# initialize data with colnames
df <- data.frame(matrix(c("0","0"), 1, 2))
colnames(df) <- c("Input1", "Input2")

port <- data.table(Experiment=list(),
                    Label=list(),
                    Bio.Rep=list(),
                    Tech.Rep=list(),
                    Analyt.Rep=list())


shinyServer(function(input, output, session) {
    cat(file=stderr())
    Sys.setlocale("LC_ALL", 'en_GB.UTF-8')
    Sys.setenv("R_ZIPCMD"= Sys.which("zip"))
    sessionID <- Sys.getpid()
    
    serverAdmin <- FALSE
    
    if (isTRUE(serverAdmin)){
        hname <- System$getHostname()
        print(hname)
        
        clientdataText <- observe({
            rv$IP_Client = session$clientData$url_hostname
        })
        
        #verbose <- TRUE
        #print(cdata$url_hostname)
        sessionLogFile <- paste("www/sessionLogs_", gsub("\\.", "_", rv$IP_Client), "_", sessionID, ".txt",sep="")
        if (!interactive()) sink(sessionLogFile, type = "output")
    }
    # Simulate work being done for 1 second
    Sys.sleep(1)

    # Hide the loading message when the rest of the server function has executed
    hide(id = "loading-content", anim = TRUE, animType = "fade")
    
    env <- environment()
    source(file.path("server", "general.R"), local = TRUE)$value
    source(file.path("server", "filtering.R"),  local = TRUE)$value
    source(file.path("server", "imputation.R"),  local = TRUE)$value
    source(file.path("server", "normalization.R"),  local = TRUE)$value
    source(file.path("server", "anaDiff.R"),  local = TRUE)$value
    source(file.path("server", "descriptiveStats.R"),  local = TRUE)$value
    source(file.path("server", "aggregation.R"),  local = TRUE)$value
    source(file.path("server", "datasetManager.R"),  local = TRUE)$value
    source(file.path("server", "help.R"),  local = TRUE)$value
    source(file.path("server", "GO_enrichment.R"),  local = TRUE)$value

    outputOptions(output, 'currentObjLoaded', suspendWhenHidden=FALSE)
})
