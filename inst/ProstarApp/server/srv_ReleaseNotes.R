
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
    tags$h4("News in Prostar 1.12.9"),
    tags$h5("Bug fixed:"),
    tags$ol(
      tags$li("Normalization: \"Sum by columns\" has been modified to provide log-abundances compatible with the other treatments. It can be
              done \"for each condition independantly\" or \"globally\".")
    ),
    
    tags$h5("New features:"),
    
    tags$ol(
      tags$li("Descriptive statistics: The expression datasets are colored w.r.t
              the nature of missing value (POV or MEC) even when the value has been imputed"),
      
      tags$li("Filtering: Manage designs with more than 2 conditions and with
              conditions containing different number of samples"),
      
      tags$li("Filtering: UI more user friendly for the string-based filtering (Tab 2)"),
      
      
      tags$li("Imputation (protein level): Distinction between missing values on an
              entire condition (Missing on the Entire Condition) and the other
              ones (Partially Observed Value)"),
      
      tags$li("Imputation (protein level): for the POV, it is possible to use SLSA
              which take into account the experimentaldesign experimental"),
      
      tags$li("Imputation (protein level): imputations are all processed condition
              by condition"),
      
      tags$li("Differential analysis: All tests can process datasets with
              conditions of different number of samples"),
      
      tags$li("Differential analysis: Limma takes into account all the hierarchical experimental designs"),
      
      tags$li("GO analysis: Add the GeneID nomenclature.")
      )
    
    
      )
})
