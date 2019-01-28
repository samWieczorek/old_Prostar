





output$tab_versions <- DT::renderDataTable({
  dt <- DT::datatable(getPackagesVersions(), 
                      escape = FALSE,
                      rownames= FALSE,
                      extensions = c('Scroller', 'Buttons'),
                      option=list(initComplete = initComplete(),
                                  dom = 'Brt',
                                  autoWidth=TRUE,
                                  ordering = F,
                                  columnDefs = list(list(visible=FALSE,targets=c(3)),
                                                    list(width='200px', targets="_all"),
                                                    list(className = 'dt-center', targets=c(1,2))
                                  )
                      )
  )
  dt
})


# 
# observeEvent(input$updateProstar,{
#   
#   BiocManager::install("Prostar", update=TRUE, ask=FALSE)
# })
# 
# 
# 
# output$update <- renderUI({
#   
#   # if(!file.exists(file.path(".", "prostar.conf"))){
#   #   tags$p("Unable to find the file prostar.conf")
#   #   return(NULL)
#   # }
#   # conf <- read.table("prostar.conf", header=FALSE)
#   df <- getPackagesVersions()
#   if (sum(df[,"NeedsUpdate"])==0) {return(NULL)}
#   #if (!(conf[which(conf[1,]=="R-Portable"),2])) {return(NULL)}
#   
#   tagList(
#     tags$p(class="body",
#            "Some of the packages of Prostar needs to be updated (for bug fixing). You can update Prostar by clicking on the Update button.
#            This will attempt to update packages from CRAN and/or Bioconductor repositories."),
#     actionButton("updateProstar", "Update Prostar", class = actionBtnClass)
#     )
#   
#   
# })
# 



output$baseVersions <- renderUI({
  
  tagList(
    
    tags$p(R.version.string, style="font-size: 16px"),
    tags$p(paste0("Bioconductor Release ",as.character(BiocManager::version())), style="font-size: 16px"),
  
    tags$br()
  )
  
})