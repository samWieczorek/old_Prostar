tabPanel("Release notes",
         value="ReleaseNotesTab",
         #uiOutput("versionNotes"),
         includeMarkdown("server/versionNotes.md"),
         uiOutput("warningDependanciesVersion"))