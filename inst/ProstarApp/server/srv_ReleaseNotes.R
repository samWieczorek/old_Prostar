
output$warningDependanciesVersion <- renderUI({
  
  DTVersion <- installed.packages()["DT","Version"]
  highcharterVersion <-installed.packages()["highcharter","Version"]
  
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





output$versionLog <- renderUI({
  
  txt <- "<strong><font size=\"4\" color=\"red\">Note:</font></strong> <br>
  <font color=\"red\">For a better experience with Prostar, we advice you to install the development version of the following
  packages : DT and highcharter. <br>
  To do so, type and execute the followings commands in a R console:<br>
  <ul>
  <li> devtools::install_github('rstudio/DT')</li>
  <li> devtools::install_github('jbkunst/highcharter')</li>
  </ul> </font>"
  
  tagList(
    tags$h4("News in Prostar 1.13.x"),
    tags$h5("Bug fixed:"),
    tags$ol(
      tags$li("Auto reset of dropdown menu in differential analysis.")
    ),
    
    tags$h5("New features:"),
    
    tags$ol(
      tags$li("The FC tag has been replaced by 'logFC'")
      )
    
    
      )
})
