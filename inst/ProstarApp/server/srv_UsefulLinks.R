output$References2 <- renderUI({
  
  tagList(
    
    tags$br(),
    tags$h4(tags$b("User manuals and tutorials:")),
    
    tags$ul(
      tags$li(a("ProStaR user manual", href="https://www.bioconductor.org/packages/release/bioc/vignettes/Prostar/inst/doc/Prostar_UserManual.pdf?attredirects=0", target="_blank")),
      tags$li(a("ProStaR Tutorial", href="https://sites.google.com/site/thomasburgerswebpage/download/Prostar_Tutorial.pdf?attredirects=0", target="_blank")),
      tags$li(a("CP4P Tutorial", href="https://sites.google.com/site/thomasburgerswebpage/download/tutorial-CP4P-4.pdf?attredirects=0", target="_blank"))
    ),
    
    tags$br(),
    tags$h4(tags$b("Contact:")),
    tags$p("If you need any help, but also if you wish to make comments 
  or suggestions, please contact Samuel Wieczorek or Thomas Burger (firstname.lastname@cea.fr)."),
    tags$br(),tags$br(),
    
    tags$h4(tags$b("Reference manuals:")),
    tags$ul(
      tags$li(a("ProStaR reference manual", href="https://www.bioconductor.org/packages/release/bioc/manuals/Prostar/man/Prostar.pdf", target="_blank")),
      tags$li(a("DAPAR reference manual", href="https://www.bioconductor.org/packages/release/bioc/manuals/DAPAR/man/DAPAR.pdf?attredirects=0", target="_blank")),
      tags$li(a("MSnbase package webpage", href="https://www.bioconductor.org/packages/release/bioc/html/MSnbase.html", target="_blank")),
      tags$li(a("CP4P reference manual", href="https://cran.r-project.org/web/packages/cp4p/cp4p.pdf?attredirects=0", target="_blank")),
      tags$li(a("IMP4P reference manual", href="https://cran.r-project.org/web/packages/imp4p/imp4p.pdf?attredirects=0", target="_blank"))
     ),
    
    tags$br(),
    tags$h4(tags$b("References:")),
    tags$ol(
      tags$h4(tags$b("Our referenced works:")),
      tags$li(a(p("Q. Giai Gianetto, F. Combes, C. Ramus, C. Bruley, Y. Coute and 
  T. Burger. Calibration Plot for Proteomics (cp4p): A graphical tool 
                to visually check the assumptions underlying FDR control in 
                quantitative experiments.",tags$i("Proteomics"),", 16(1):29-32, 2016."), 
                href="https://sites.google.com/site/thomasburgerswebpage/download/calib-final.pdf?attredirects=0", target="_blank")),
      tags$li(a(p("C. Lazar, L. Gatto, M. Ferro, C. Bruley, T. Burger. Accounting 
  for the multiple natures of missing values in label-free quantitative 
                proteomics datasets to compare imputation strategies.", tags$i("Journal of 
                Proteome Research"), ", 15(4):1116-1125, 2016."), 
                href="https://sites.google.com/site/thomasburgerswebpage/download/natureOfMV-Vsubmited2.pdf?attredirects=0", target="_blank")),
      tags$li(a(p("Q. Giai Gianetto, Y. Coute, C. Bruley and T. Burger. Uses and 
  misuses of the fudge factor in quantitative discovery proteomics.", tags$i("Proteomics"), ", 16(14):1955-60, 2016."), 
                href="https://sites.google.com/site/thomasburgerswebpage/download/OnTheMissuseOfFudgeFactorInProteomics_FV.pdf?attredirects=0", target="_blank")),
      tags$li(a(p("S. Wieczorek, F. Combes, C. Lazar, Q. Giai-Gianetto, L. Gatto, 
  A. Dorffer, A.-M. Hesse, Y. Coute, M. Ferro, C. Bruley and T. Burger. 
                DAPAR & ProStaR: software to perform statistical analyses in 
                quantitative discovery proteomics.", tags$i("Bioinformatics"), ", 33(1):135-136, 2017"), 
                href="https://sites.google.com/site/thomasburgerswebpage/download/prostar.pdf?attredirects=0", target="_blank")),
      tags$li(a(p("T. Burger. Gentle introduction to the statistical 
  foundations of false discovery rate in quantitative proteomics.", tags$i("Journal of Proteome Research"), ", 17(1):12-22, 2017."), 
                href="https://sites.google.com/site/thomasburgerswebpage/download/fdrtuto.pdf?attredirects=0", target="_blank")),
      tags$li(a(p("L. Jacob, F. Combes and T. Burger. PEPA test: fast and powerful differential analysis
    from relative quantitative proteomics data using shared peptides.", tags$i("Biostatistics"), ", kxy021, 2018."), 
                href="https://sites.google.com/site/thomasburgerswebpage/download/revised-biostat-proteom-preprint.pdf?attredirects=0", target="_blank")),
      tags$li(p("Q. Giai Gianetto, C. Lazar, S. Wieczorek, C. Bruley, Y. Coute and 
    T. Burger. Multiple imputation strategy for mass spectrometry-based 
                proteomic data. (in preparation).")),
     tags$h4(tags$b("Other references:")),
     tags$li(p("Bolstad BM (2017). preprocessCore: A collection of pre-processing functions. R package version 1.38.1"), 
             a("link",href="https://github.com/bmbolstad/preprocessCore", target="_blank")),
     tags$li(p("Hastie T, Tibshirani R, Narasimhan B and Chu G (2017). impute: Imputation for microarray data. R package version 1.50.1")),
     tags$li(p("Gatto L and Lilley K (2012). 
    MSnbase - an R/Bioconductor package for isobaric tagged mass spectrometry data visualization, processing and quantitation.", 
        tags$i("Bioinformatics"), ", 28, pp. 288-289.")),
     tags$li(p("Ritchie ME, Phipson B, Wu D, Hu Y, Law CW, Shi W and Smyth GK (2015). 
    Limma powers differential expression analyses for RNA-sequencing and microarray studies.", tags$i("Nucleic Acids Research"), ", 43(7), pp. e47.")),
     
     
     
     tags$li(p("Cleveland, William S., and Susan J. Devlin. Locally weighted regression: an approach to regression analysis by local fitting.", tags$i("Journal of the American statistical association"), " 83.403 (1988): 596-610.")),
     tags$li(p("Huber, Wolfgang, et al. Variance stabilization applied to microarray data calibration and to the quantification of differential expression.", tags$i("Bioinformatics"), " 18.suppl_1 (2002): S96-S104."))
   )
   
   
  )
  
  
  
  })
