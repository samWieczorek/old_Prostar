tabPanel("Release notes",
         value="ReleaseNotesTab",
         bsCollapse(id = "collapseFormerReleases", open = "Current release",multiple = TRUE,
                    bsCollapsePanel("Current release", moduleInsertMarkdownUI("versionNotes_MD"),style = "info"),
                    bsCollapsePanel("Former releases", moduleInsertMarkdownUI("formerReleases_MD"),style = "info")),
         uiOutput("warningDependanciesVersion"))