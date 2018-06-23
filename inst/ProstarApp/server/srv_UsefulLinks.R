output$References <- renderText({
  
  
  txt<- "<br>
  
  <strong><font size=\"4\">User manuals and tutorials:</font></strong>
  <ul>
  <li>  
  <a href=\"https://www.bioconductor.org/packages/release/bioc/vignettes/Prostar/inst/doc/Prostar_UserManual.pdf?attredirects=0\">ProStaR user manual</a>
  </li>
  <li> <a href=\"https://sites.google.com/site/thomasburgerswebpage/download/Prostar_Tutorial.pdf?attredirects=0\">ProStaR Tutorial</a>
  </li>
  <li> <a href=\"https://sites.google.com/site/thomasburgerswebpage/download/tutorial-CP4P-4.pdf?attredirects=0\">CP4P Tutorial</a>
  </li>
  </ul>
  <br>
  
  <strong><font size=\"4\">Contact:</font></strong><br>
  If you need any help, but also if you wish to make comments 
  or suggestions, please contact Samuel Wieczorek, Florence Combes or 
  Thomas Burger (firstname.lastname@cea.fr).<br><br>
  
  
  
  <strong><font size=\"4\">Reference manuals:</font></strong>
  <ul>
  <li>  
  <a href=\"https://www.bioconductor.org/packages/release/bioc/manuals/Prostar/man/Prostar.pdf\">ProStaR reference manual</a>
  </li>
  <li> <a href=\"https://www.bioconductor.org/packages/release/bioc/manuals/DAPAR/man/DAPAR.pdf?attredirects=0\">DAPAR reference manual</a>
  </li>
  <li> <a href=\"https://www.bioconductor.org/packages/release/bioc/html/MSnbase.html\">MSnbase package webpage</a>
  </li>
  <li> <a href=\"https://cran.r-project.org/web/packages/cp4p/cp4p.pdf?attredirects=0\">CP4P reference manual</a>
  </li>
  <li> <a href=\"https://cran.r-project.org/web/packages/imp4p/imp4p.pdf?attredirects=0\">IMP4P reference manual</a>
  </li>
  </ul>
  <br>
  
  
  <strong><font size=\"4\">Our referenced works:</font></strong>
  <ol>
  <li>
  <a href=\"https://sites.google.com/site/thomasburgerswebpage/download/calib-final.pdf?attredirects=0\">
  Q. Giai Gianetto, F. Combes, C. Ramus, C. Bruley, Y. Coute and 
  T. Burger. Calibration Plot for Proteomics (cp4p): A graphical tool 
  to visually check the assumptions underlying FDR control in 
  quantitative experiments. <i>Proteomics</i>, 16(1):29-32, 2016. 
  </a></li>
  <li>
  <a href=\"https://sites.google.com/site/thomasburgerswebpage/download/natureOfMV-Vsubmited2.pdf?attredirects=0\">
  C. Lazar, L. Gatto, M. Ferro, C. Bruley, T. Burger. Accounting 
  for the multiple natures of missing values in label-free quantitative 
  proteomics datasets to compare imputation strategies. <i>Journal of 
  Proteome Research</i>, 15(4):1116-1125, 2016. 
  </a></li>         
  <li>
  <a href=\"https://sites.google.com/site/thomasburgerswebpage/download/OnTheMissuseOfFudgeFactorInProteomics_FV.pdf?attredirects=0\">
  Q. Giai Gianetto, Y. Coute, C. Bruley and T. Burger. Uses and 
  misuses of the fudge factor in quantitative discovery proteomics. 
  <i>Proteomics</i>, 16(14):1955-60, 2016. 
  </a></li>
  <li>
  <a href=\"https://sites.google.com/site/thomasburgerswebpage/download/prostar.pdf?attredirects=0\">
  S. Wieczorek, F. Combes, C. Lazar, Q. Giai-Gianetto, L. Gatto, 
  A. Dorffer, A.-M. Hesse, Y. Coute, M. Ferro, C. Bruley and T. Burger. 
  DAPAR & ProStaR: software to perform statistical analyses in 
  quantitative discovery proteomics, <i>Bioinformatics</i>, 33(1):135-136, 2017
  </a></li>
  <li>
  <a href=\"https://sites.google.com/site/thomasburgerswebpage/download/fdrtuto.pdf?attredirects=0\">
  T. Burger. Gentle introduction to the statistical 
  foundations of false discovery rate in quantitative proteomics. 
  <i>Journal of Proteome Research</i>, 17(1):12-22, 2017. 
  </a></li>
  <li> <a href=\"https://sites.google.com/site/thomasburgerswebpage/download/revised-biostat-proteom-preprint.pdf?attredirects=0\">
    L. Jacob, F. Combes and T. Burger. PEPA test : fast and powerful differential analysis
    from relative quantitative proteomics data using shared peptides. (accepted for publication, 2018). 
    </a></li>
    <li> Q. Giai Gianetto, C. Lazar, S. Wieczorek, C. Bruley, Y. Coute and 
    T. Burger. Multiple imputation strategy for mass spectrometry-based 
    proteomic data. (in preparation).
    </li>
    </ol>
    <br>
   
    
    <strong><font size=\"4\">Other references:</font></strong>
    <ol start=8>
    <li> Bolstad BM (2017). preprocessCore: A collection of pre-processing 
    functions. R package version 1.38.1, 
    <a href=\"https://github.com/bmbolstad/preprocessCore\"
    title=\"here\" target=\"_blank\">
    https://github.com/bmbolstad/preprocessCore</a>
    <li> Hastie T, Tibshirani R, Narasimhan B and Chu G (2017). 
    impute: Imputation for microarray data. R package version 1.50.1
    </li>
    <li> Gatto L and Lilley K (2012). 
    MSnbase - an R/Bioconductor package for isobaric tagged mass spectrometry data visualization, processing and quantitation.
    Bioinformatics, 28, pp. 288-289.
    </li>
    <li> Ritchie ME, Phipson B, Wu D, Hu Y, Law CW, Shi W and Smyth GK (2015). 
    Limma powers differential expression analyses for RNA-sequencing and microarray studies. 
    Nucleic Acids Research, 43(7), pp. e47.
    </li>
    </ol>"
  
  
  HTML(txt)
  
})

