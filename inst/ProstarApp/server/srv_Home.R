<<<<<<< HEAD



output$citationText <- renderUI({
  tagList(
    tags$div(
      style="background-color: lightgrey;",
tags$p(class="body",tags$b("Maintaining ProStaR as free software is a heavy and time-consuming
                duty. If you use it, please cite the following reference:"),
           tags$br(),
           "S. Wieczorek, F. Combes, C. Lazar, Q. Giai-Gianetto, 
                L. Gatto, A. Dorffer, A.-M. Hesse, Y. Coute, M. Ferro, 
                C. Bruley and T. Burger.",tags$br(),tags$u("DAPAR & ProStaR: software to perform statistical 
                analyses in quantitative discovery"),
              tags$i("Bioinformatics 33(1), 135-136"),
           tags$b("2017")),
  tags$a("http://doi.org/10.1093/bioinformatics/btw580", href="http://doi.org/10.1093/bioinformatics/btw580", target="_blank")
  
    )
  )
  })


output$versionsText <- renderUI({
  t <- sessionInfo()
  daparVersion <- installed.packages(lib.loc=DAPAR.loc)["DAPAR","Version"]
  ProstarVersion <- installed.packages(lib.loc=Prostar.loc)["Prostar","Version"]
  
  tagList(
     tags$p(class="body",
tags$b("DAPAR")," and ", tags$b("ProStaR"), " form a 
                software suite for quantitative analysis of mass spectrometry 
           based proteomics. More specifically it is designed to process 
           relative quantitative data from discovery experiments.", tags$br(),"
           It is composed of two distinct R packages :",
    tags$ul(
      tags$li(tags$p(tags$a("Prostar", href="http://www.bioconductor.org/packages/release/bioc/html/Prostar.html", target="_blank"),paste0(" (version ",
                             ProstarVersion,"): the web based graphical user interface to DAPAR"))),
      tags$li(tags$p(tags$a("DAPAR", href="http://www.bioconductor.org/packages/release/bioc/html/DAPAR.html", target="_blank"),paste0(" (version ",
                              daparVersion,"): a collection of tools and graphs dedicated to proteomic analysis")))
    )
            )
)
})


#-------------------------------------------------------------------
output$descriptionText <- renderUI({
  
  tagList(
    tags$p(class="body",
        "The package DAPAR includes wrappers to numerous other R packages, either available on the ",
           tags$a("CRAN", href="https://cran.r-project.org", target="_blank"), " or the ",
           tags$a("Bioconductor", href="http://www.bioconductor.org", target="_blank"),
           tags$br(),
           "Here is a brief overview of the available functionalities:"),
    tags$p(style=" font-size:20px;",tags$b("Data processing")),
    tags$ul(
      style="list-style-type:disc;",
      tags$li(tags$p(class="body",tags$b("Filtering"), "options allows pruning the protein or peptide list according to 
                various criteria (missing values, contaminants, reverse sequences);")),
      tags$li(tags$p(class="body",tags$b("Cross replicate normalization"), ", so as to make the quantitative 
                values comparable between the different analyzed samples;")),
      tags$li(tags$p(class="body",tags$b("Missing values imputation"), " with different methods, depending 
                on the nature of  the missing values;")),
      tags$li(tags$p(class="body",tags$b("Aggregation"), " from peptide to protein intensity values;")),
      tags$li(tags$p(class="body",tags$b("Significance test"), ", which includes null hypothesis 
                significance testing."))
    ),
      
    tags$br(),
    tags$p(style=" font-size:20px;",tags$b("Data mining")),
    tags$ul(
      style="list-style-type:disc;",
      tags$li(tags$p(class="body",tags$b("Descriptive statistics"), "are available, for exploration and visualization of the 
                     quantitative dataset;")),
      tags$li(tags$p(class="body",tags$b("Differential analysis"), " which includes multiple testing correction 
                     (for false discovery rate estimation).")),
      tags$li(tags$p(class="body",tags$b("Gene Ontology (GO) analysis"), " allows to map protein list onto GO terms and 
                     to test category enrichment."))
    )
  )
   
  
=======



output$citationText <- renderUI({
  tagList(
    tags$div(
      style="background-color: lightgrey;",
tags$p(class="body",tags$b("Maintaining ProStaR as free software is a heavy and time-consuming
                duty. If you use it, please cite the following reference:"),
           tags$br(),
           "S. Wieczorek, F. Combes, C. Lazar, Q. Giai-Gianetto, 
                L. Gatto, A. Dorffer, A.-M. Hesse, Y. Coute, M. Ferro, 
                C. Bruley and T. Burger.",tags$br(),tags$u("DAPAR & ProStaR: software to perform statistical 
                analyses in quantitative discovery"),
              tags$i("Bioinformatics 33(1), 135-136"),
           tags$b("2017")),
  tags$a("http://doi.org/10.1093/bioinformatics/btw580", href="http://doi.org/10.1093/bioinformatics/btw580", target="_blank")
  
    )
  )
  })


output$versionsText <- renderUI({
  t <- sessionInfo()
  daparVersion <- installed.packages(lib.loc=DAPAR.loc)["DAPAR","Version"]
  ProstarVersion <- installed.packages(lib.loc=Prostar.loc)["Prostar","Version"]
  
  tagList(
     tags$p(class="body",
tags$b("DAPAR")," and ", tags$b("ProStaR"), " form a 
                software suite for quantitative analysis of mass spectrometry 
           based proteomics. More specifically it is designed to process 
           relative quantitative data from discovery experiments.", tags$br(),"
           It is composed of two distinct R packages :",
    tags$ul(
      tags$li(tags$p(tags$a("Prostar", href="http://www.bioconductor.org/packages/release/bioc/html/Prostar.html", target="_blank"),paste0(" (version ",
                             ProstarVersion,"): the web based graphical user interface to DAPAR"))),
      tags$li(tags$p(tags$a("DAPAR", href="http://www.bioconductor.org/packages/release/bioc/html/DAPAR.html", target="_blank"),paste0(" (version ",
                              daparVersion,"): a collection of tools and graphs dedicated to proteomic analysis")))
    )
            )
)
})


#-------------------------------------------------------------------
output$descriptionText <- renderUI({
  
  tagList(
    tags$p(class="body",
        "The package DAPAR includes wrappers to numerous other R packages, either available on the ",
           tags$a("CRAN", href="https://cran.r-project.org", target="_blank"), " or the ",
           tags$a("Bioconductor", href="http://www.bioconductor.org", target="_blank"),
           tags$br(),
           "Here is a brief overview of the available functionalities:"),
    tags$p(style=" font-size:20px;",tags$b("Data processing")),
    tags$ul(
      style="list-style-type:disc;",
      tags$li(tags$p(class="body",tags$b("Filtering"), "options allows pruning the protein or peptide list according to 
                various criteria (missing values, contaminants, reverse sequences);")),
      tags$li(tags$p(class="body",tags$b("Cross replicate normalization"), ", so as to make the quantitative 
                values comparable between the different analyzed samples;")),
      tags$li(tags$p(class="body",tags$b("Missing values imputation"), " with different methods, depending 
                on the nature of  the missing values;")),
      tags$li(tags$p(class="body",tags$b("Aggregation"), " from peptide to protein intensity values;")),
      tags$li(tags$p(class="body",tags$b("Significance test"), ", which includes null hypothesis 
                significance testing."))
    ),
      
    tags$br(),
    tags$p(style=" font-size:20px;",tags$b("Data mining")),
    tags$ul(
      style="list-style-type:disc;",
      tags$li(tags$p(class="body",tags$b("Descriptive statistics"), "are available, for exploration and visualization of the 
                     quantitative dataset;")),
      tags$li(tags$p(class="body",tags$b("Differential analysis"), " which includes multiple testing correction 
                     (for false discovery rate estimation).")),
      tags$li(tags$p(class="body",tags$b("Gene Ontology (GO) analysis"), " allows to map protein list onto GO terms and 
                     to test category enrichment."))
    )
  )
   
  
>>>>>>> ceee6a0719f73dbf86eb71708e3099eee6d98083
})