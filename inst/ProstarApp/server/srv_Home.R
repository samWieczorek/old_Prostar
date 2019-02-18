
output$citationText <- renderUI({
  tagList(
    tags$div(style="background-color: lightgrey;",
    tags$p(class="body",tags$b("Maintaining ProStaR as free software is a heavy and time-consuming
                duty. If you use it, please cite the following reference:")),
    tags$p(tags$i("S. Wieczorek, F. Combes, C. Lazar, Q. Giai-Gianetto, 
                L. Gatto, A. Dorffer, A.-M. Hesse, Y. Coute, M. Ferro, 
                C. Bruley and T. Burger."),
          tags$u("DAPAR & ProStaR: software to perform statistical 
                analyses in quantitative discovery."),
                tags$b("Bioinformatics"),", 33(1), 135-136, 2017.",
                tags$a("http://doi.org/10.1093/bioinformatics/btw580", href="http://doi.org/10.1093/bioinformatics/btw580", target="_blank")
                )
      )
  )
  })


output$versionsText <- renderUI({
  t <- sessionInfo()
  daparVersion <- installed.packages(lib.loc=DAPAR.loc)["DAPAR","Version"]
  ProstarVersion <- installed.packages(lib.loc=Prostar.loc)["Prostar","Version"]
  
  tagList(
     tags$p(class="body",
            tags$b("DAPAR"),
            " and ", 
            tags$b("Prostar"), 
            " form a software suite devoted to the differential analysis of 
              quantitative data resulting from discovery proteomics experiments.", 
            tags$br(),
            "It is composed of two distinct ",
            tags$b("R"),
            " packages:",
            tags$ul(
              tags$li(tags$p(tags$a("Prostar", href="http://www.bioconductor.org/packages/release/bioc/html/Prostar.html", target="_blank"),paste0(" (version ",
                             ProstarVersion,"), which proposes a web-based graphical user interface to DAPAR."))),
               tags$li(tags$p(tags$a("DAPAR", href="http://www.bioconductor.org/packages/release/bioc/html/DAPAR.html", target="_blank"),paste0(" (version ",
                              daparVersion,"), which contains all the routines to analyze and visualize proteomics data.")))
            )
     )
)
})
