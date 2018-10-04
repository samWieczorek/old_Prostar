tabPanel("Release notes",
         value="ReleaseNotesTab",
         bsCollapse(id = "collapseFormerReleases", open = "Current release",multiple = TRUE,
                    bsCollapsePanel("Current release", includeMarkdown("server/versionNotes.md"),style = "info"),
                    bsCollapsePanel("Former releases", includeMarkdown("server/formerReleases.md"),style = "info")),
         uiOutput("warningDependanciesVersion"))