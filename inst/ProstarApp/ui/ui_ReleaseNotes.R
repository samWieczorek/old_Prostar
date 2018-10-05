tabPanel("Release notes",
         value="ReleaseNotesTab",
         bsCollapse(id = "collapseFormerReleases", open = "Current release",multiple = TRUE,
                    bsCollapsePanel("Current release", includeMarkdown(URL_versionNotes),style = "info"),
                    bsCollapsePanel("Former releases", includeMarkdown(URL_formerReleases),style = "info")),
         uiOutput("warningDependanciesVersion"))