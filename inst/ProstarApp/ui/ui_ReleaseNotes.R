tabPanel("Release notes",
         value="ReleaseNotesTab",
         #uiOutput("versionNotes"),
         includeMarkdown("server/versionNotes.md"),
         includeMarkdown("server/formerReleases.md"),
         uiOutput("warningDependanciesVersion"))