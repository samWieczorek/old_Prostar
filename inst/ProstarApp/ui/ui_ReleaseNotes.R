tabPanel("Release notes",
         value="ReleaseNotesTab",
         shinyBS::bsCollapse(id = "collapseFormerReleases", open = "Current release",multiple = TRUE,
                             shinyBS::bsCollapsePanel("Current release", moduleInsertMarkdownUI("versionNotes_MD"),style = "info"),
                             shinyBS::bsCollapsePanel("Former releases", moduleInsertMarkdownUI("formerReleases_MD"),style = "info")),
         #uiOutput("warningDependanciesVersion")
         )