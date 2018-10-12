

callModule(moduleInsertMarkdown, "versionNotes_MD",URL_versionNotes)
callModule(moduleInsertMarkdown, "formerReleases_MD",URL_formerReleases)

output$warningDependanciesVersion <- renderUI({
  
  DTVersion <- installed.packages()["DT","Version"]
  highcharterVersion <- installed.packages()["highcharter","Version"]
  
  if (DTVersion != "0.4.11" || highcharterVersion != "0.6.0"){
    tagList(
      tags$h4("Notes"),
      
      tags$p("For a better experience with Prostar, we advice you to install the development version of the following
             packages : DT and highcharter. To do so, type and execute the followings commands in a R console:"),
      tags$ul(
        tags$li("devtools::install_github('rstudio/DT')"),
        tags$li("devtools::install_github('jbkunst/highcharter')")
      )
      )
  }
})




# 
# output$versionNotes <- renderUI({
#   
#   
#   tagList(
#     tags$h4("News in Prostar 1.13.6"),
#     tags$h5("Bug fixed:"),
#     tags$ol(
#       tags$li("Auto reset of dropdown menu in differential analysis.")
#     ),
#     
#     tags$h5("New features:"),
#     
#     tags$ol(
#       tags$li("In the feature metadata table, the FC tag has been replaced by 'logFC'. In the 
#               experimental design table, the column names 'Experiment' and 'Label' have been replaced
#               respectively by 'Sample.name' and 'Condition'."),
#       tags$li("Better managment of dropdown menus"),
#       tags$li("Add a Bug report tab in the 'Help' menu"),
#       tags$li("Reorganisation of the menus: Data preprocessing, Data mining"),
#       tags$li("Add proportions in logFC distribution plot"),
#       tags$li("Delete the dependency to the package imputeLCMD"),
#       tags$li("Add LOESS and vsn as new normalisation methods")
#       )
#     
#     
#     )
# })
