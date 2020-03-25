moduleReleaseNotesUI <- function(id)
{
  ns <- NS(id)
  
tagList(
         shinyBS::bsCollapse(id = "collapseFormerReleases", open = "Current release",multiple = TRUE,
                             shinyBS::bsCollapsePanel("Current release", moduleInsertMarkdownUI(ns("versionNotes_MD")),style = "info"),
                             shinyBS::bsCollapsePanel("Former releases", moduleInsertMarkdownUI(ns("formerReleases_MD")),style = "info"))
)

}


moduleReleaseNotes <- function(input, output, session){
  ns <- session$ns
  
callModule(moduleInsertMarkdown, "versionNotes_MD",URL_versionNotes)
callModule(moduleInsertMarkdown, "formerReleases_MD",URL_formerReleases)

}