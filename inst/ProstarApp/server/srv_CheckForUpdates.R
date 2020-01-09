





output$tab_versions <- DT::renderDataTable(server=TRUE,{
  dt <- DT::datatable(getPackagesVersions2(), 
                      escape = FALSE,
                      rownames= FALSE,
                      extensions = c('Scroller', 'Buttons'),
                      option=list(initComplete = initComplete(),
                                  dom = 'Brt',
                                  autoWidth=TRUE,
                                  ordering = F
                      )
  )
  dt
})


output$infoForNewVersions <- renderUI({
  df <- getPackagesVersions2()
  if (sum(grepl("(Out of date)",df[,1])) >= 1) {
   
  tagList(
    p(style="font-size: 16px", "Even though it remains possible to work with the current package versions, updates are advised. 
         If you use the server or the stand-alone versions, please proceed via the Bioconductor."),

      zipVersion <- substr(GetOnlineZipVersion(), 9, regexpr(".zip",GetOnlineZipVersion())[1] -1),
      prostarVersion <- installed.packages(lib.loc=Prostar.loc)["Prostar","Version"],
      if (compareVersion(zipVersion,prostarVersion) == 1){
          p(style="font-size: 16px", "If you use the Zero-install version, please download the latest zip file on our website ",
            tags$a("(www.prostar-proteomics.org)", href="http://www.prostar-proteomics.org", target="_blank")
          )
      } else {
        p(style="font-size: 16px", "If you use the Zero-install version, the new zip file (Prostar Zero Install) will be available soon on our website ",
          tags$a("(www.prostar-proteomics.org)", href="http://www.prostar-proteomics.org", target="_blank")
        )
      }
    )
  }
})




output$baseVersions <- renderUI({
  
  tagList(
    
    tags$p(R.version.string, style="font-size: 16px"),
    tags$p(paste0("Bioconductor Release ",as.character(BiocManager::version())), style="font-size: 16px"),
  
    tags$br()
  )
  
})