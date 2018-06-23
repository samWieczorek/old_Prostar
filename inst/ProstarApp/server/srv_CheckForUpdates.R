

getPackagesVersions <- reactive({
  outOfDate <- "(Out of date)"
  dev <- "(Devel)"
  
  biocRelease <- NULL
  tryCatch({
    biocRelease <-available.packages(contrib.url("http://bioconductor.org/packages/release/bioc/"))
    require(XML)
    html <- readHTMLTable("http://bioconductor.org/packages/release/data/experiment/html/DAPARdata.html")
    DAPARdata.version <- as.character(html[[3]][2][1,])
    
  }, warning = function(w) {
    return()
  }, error = function(e) {
    return()
  }, finally = {
    #cleanup-code 
  })
  
  pkgs <- c("Prostar", "DAPAR", "DAPARdata")
  loc.pkgs <-c("Prostar.loc", "DAPAR.loc", "DAPARdata.loc")
  instPkgs <- list(Prostar = installed.packages(lib.loc=Prostar.loc)["Prostar","Version"],
                   DAPAR = installed.packages(lib.loc=DAPAR.loc)["DAPAR","Version"],
                   DAPARdata = installed.packages(lib.loc=DAPARdata.loc)["DAPARdata","Version"])
  
  
  names <- c(as.character(tags$a(href="http://www.bioconductor.org/packages/release/bioc/html/Prostar.html", "Prostar")), 
             as.character(tags$a(href="http://www.bioconductor.org/packages/release/bioc/html/DAPAR.html", "DAPAR")), 
             as.character(tags$a(href="http://www.bioconductor.org/packages/release/data/experiment/html/DAPARdata.html", "DAPARdata")))
  
  
  df <- data.frame("Name" = names,
                   "Installed.packages"= rep(NA, 3), 
                   "Bioc.release" =  rep(NA, 3))
  
  
  df[, "Installed.packages"] <- unlist(instPkgs)
  
  if (!is.null(biocRelease)) {
    biocPkgs <- list(Prostar = as.character(biocRelease["Prostar","Version"]),
                     DAPAR = as.character(biocRelease["DAPAR","Version"]),
                     DAPARdata = as.character(DAPARdata.version))
    
    if (compareVersion(instPkgs$Prostar,biocPkgs$Prostar) == 0){df[1,"Name"] <-  names[1]}
    else if (compareVersion(instPkgs$Prostar,biocPkgs$Prostar) == 1){df[1,"Name"] <-   paste(names[1],  "<strong>",dev, "</strong>", sep=" ")}
    else if (compareVersion(instPkgs$Prostar,biocPkgs$Prostar)==-1){df[1,"Name"] <-   paste(names[1], "<strong>", outOfDate, "</strong>", sep=" ")}
    
    if (compareVersion(instPkgs$DAPAR,biocPkgs$DAPAR) == 0){df[2,"Name"] <-  names[2]}
    else if (compareVersion(instPkgs$DAPAR , biocPkgs$DAPAR) == 1){df[2,"Name"] <-   paste(names[2],  "<strong>",dev, "</strong>", sep=" ")}
    else if (compareVersion(instPkgs$DAPAR , biocPkgs$DAPAR)==-1){df[2,"Name"] <-   paste(names[2],  "<strong>",outOfDate, "</strong>", sep=" ")}
    
    if (compareVersion(instPkgs$DAPARdata,biocPkgs$DAPARdata) == 0){df[3,"Name"] <-  names[3]}
    else if (compareVersion(instPkgs$DAPARdata , biocPkgs$DAPARdata) == 1){df[3,"Name"] <-   paste(names[3],  "<strong>",dev, "</strong>", sep=" ")}
    else if (compareVersion(instPkgs$DAPARdata , biocPkgs$DAPARdata)==-1){df[3,"Name"] <-   paste(names[3],  "<strong>",outOfDate, "</strong>", sep=" ")}
    
    
    df[, "Bioc.release"] <- unlist(biocPkgs)
  }
  
  
  colnames(df) <- c("Names", "Installed packages", "Bioc release")
  df
})



output$tab_versions <- renderDataTable({
  dt <- DT::datatable(getPackagesVersions(), 
                      escape = FALSE,
                      rownames= FALSE,
                      option=list(initComplete = initComplete(),
                                  dom = 't',
                                  autoWidth=TRUE,
                                  columnDefs = list(list(width='200px',targets= "_all"))
                      )
  )
  dt
})
