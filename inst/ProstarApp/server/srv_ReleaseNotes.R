
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



