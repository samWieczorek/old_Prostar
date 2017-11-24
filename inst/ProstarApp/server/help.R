output$References <- renderText({
    
    
    
    # <strong><font size=\"4\">User manual:</font></strong>
    #      <a href=\"https://www.bioconductor.org/packages/release/bioc/vignettes/
    # Prostar/inst/doc/Prostar_UserManual.pdf\"
    # title=\"here\" target=\"_blank\">here</a>
    # <br><br>

    # <strong><font size=\"4\">Tutorial:</font></strong>
    #      <a href=\"http://bioconductor.org/packages/release/bioc/vignettes/
    # Prostar/inst/doc/Prostar_Tutorial.pdf\"
    # title=\"here\" target=\"_blank\">here</a>
    # <br><br>


    txt<- "<strong><font size=\"5\">HELP</font></strong>
         <br><hr color:\"blue\"><br>
         
         <strong><font size=\"4\"><a href=\"https://www.bioconductor.org/packages/release/bioc/vignettes/Prostar/inst/doc/Prostar_UserManual.pdf\">User manual</a></font></strong>
          <br><br>
         <strong><font size=\"4\"><a href=\"http://bioconductor.org/packages/release/bioc/vignettes/
Prostar/inst/doc/Prostar_Tutorial.pdf\">Tutorial</a></font></strong>
         
        <br><br>
         
         <strong><font size=\"4\">Contact:</font></strong><br>
         If you need any help, but also if you wish to make comments 
         or suggestions, please contact Samuel Wieczorek, Florence Combes or 
         Thomas Burger (firstname.lastname@cea.fr).<br><br>
         
         
         <strong><font size=\"4\">Additional ressources:</font></strong>
         <ul>
         <li>  
         <a href=\"https://www.bioconductor.org/packages/release/bioc/manuals/
Prostar/man/Prostar.pdf\">ProStaR reference manual</a>
         </li>
         <li> <a href=\"https://www.bioconductor.org/
packages/release/bioc/manuals/DAPAR/man/DAPAR.pdf\">DAPAR reference manual</a>
         
         
         </li>
         <li> <a href=\"https://www.bioconductor.org/packages/
release/bioc/html/MSnbase.html\">MSnbase package</a>
         </li>
         <li> <a href=\"https://cran.r-project.org/web/packages/cp4p/cp4p.pdf\">Calibration of p-values (CP4P tutorial)</a>
         </li>
         </ul>
         <br>
         
         <strong><font size=\"4\">Our referenced works:</font></strong>
         <ol>
         <li> S. Wieczorek, F. Combes, C. Lazar, Q. Giai-Gianetto, L. Gatto, 
        A. Dorffer, A.-M. Hesse, Y. Coute, M. Ferro, C. Bruley and T. Burger. 
        DAPAR & ProStaR: software to perform statistical analyses in 
        quantitative discovery proteomics, <i>Bioinformatics</i>, 33(1):135-136, 2017
         </li>
         <li> C. Lazar, L. Gatto, M. Ferro, C. Bruley, T. Burger. Accounting 
         for the multiple natures of missing values in label-free quantitative 
         proteomics datasets to compare imputation strategies. <i>Journal of 
         Proteome Research</i>, 15(4):1116-1125, 2016. 
         </li>
         <li> Q. Giai Gianetto, F. Combes, C. Ramus, C. Bruley, Y. Coute and 
         T. Burger. Calibration Plot for Proteomics (cp4p): A graphical tool 
         to visually check the assumptions underlying FDR control in 
         quantitative experiments. <i>Proteomics</i>, 16(1):29-32, 2016. 
         </li>
         
         <li> Q. Giai Gianetto, Y. Coute, C. Bruley and T. Burger. Uses and 
         misuses of the fudge factor in quantitative discovery proteomics. 
         <i>Proteomics</i>, 16(14):1955-60, 2016. 
         </li>

         <li> T. Burger. A gentle introduction to the statistical 
         foundations of false discovery rate in quantitative proteomics. 
         (under review). 
         </li>

        <li> L. Jacob, F. Combes and T. Burger. PEPA test : fast and powerful differential analysis
        from relative quantitative proteomics data using shared peptides. (under review).

         <li> Q. Giai Gianetto, C. Lazar, S. Wieczorek, C. Bruley, Y. Coute and 
         T. Burger. Multiple imputation strategy for mass spectrometry-based 
         proteomic data. (in preparation).
         </li>
         
         </ol>
         
         <br>
         
         <strong><font size=\"4\">Other references:</font></strong>
         <ol start=6>
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



#-------------------------------------------------------------------
output$aboutText <- renderUI({
    busyIndicator(WaitMsgCalc,wait = 0)
    
    t <- sessionInfo()
    daparVersion <- installed.packages(lib.loc = DAPAR.loc)["DAPAR","Version"]
    ProstarVersion <- installed.packages(lib.loc = Prostar.loc)["Prostar","Version"]
    
    
    text <- paste("<strong>To cite DAPAR and ProStaR software:</strong><br> 
                  S. Wieczorek, F. Combes, C. Lazar, Q. Giai-Gianetto, 
                L. Gatto, A. Dorffer, A.-M. Hesse, Y. Coute, M. Ferro, 
                C. Bruley and T. Burger. <br>
                <u>\"DAPAR & ProStaR: software to perform statistical 
                analyses in quantitative discovery 
                proteomics\"</u><br>
                <i>Bioinformatics 33(1), 135–136</i>, <strong>2017</strong><br>
                <a href=\"http://doi.org/10.1093/bioinformatics/btw580\"
                  title=\"here\" target=\"_blank\">http://doi.org/10.1093/bioinformatics/btw580</a>
                  
                  <br><br><br>
                  <strong>DAPAR</strong> and <strong>ProStaR</strong> form a 
                  software suite for quantitative analysis of mass spectrometry 
                  based proteomics.<br>
                  More specifically it is designed to process 
                  relative quantitative data from discovery experiments.<br>
                  It is composed of two distinct R packages : <br>", 
                  "<ul style=\"list-style-type:disc;\">
                  <li>
                  <a href=\"http://www.bioconductor.org/packages/release/bioc/html/Prostar.html\"
                  title=\"here\" target=\"_blank\">Prostar</a> (version ",
                  ProstarVersion, "): the web based graphical user interface to DAPAR 
                  </li>
                  <li>
                  <a href=\"http://www.bioconductor.org/packages/release/bioc/html/DAPAR.html\"
                  title=\"here\" target=\"_blank\">DAPAR</a> (version ",daparVersion,"): a 
                  collection of tools and graphs dedicated to proteomic analysis
                  </li>
                  </ul> 
                  DAPAR includes wrappers to numerous other R packages, either available on 
                  <a href=\"the https://cran.r-project.org/\" title=\"here\" target=\"_blank\">
                  CRAN</a> or on the <a href=\"http://www.bioconductor.org\"
                  title=\"here\" target=\"_blank\">Bioconductor</a>.
                  <br>
                  Here is a brief overview of the available functionalities:
                  <ul style=\"list-style-type:disc;\">
                  <li>  
                  <strong>Descriptive statistics</strong> are available, for exploration and visualization of the 
                  quantitative dataset;
                  </li>
                  <li>  
                <strong>Filtering</strong> options allows pruning the protein or peptide list according to 
                various criteria (missing values, contaminants, reverse sequences);
                </li>
                  
                <li>
                <strong>Cross replicate normalization</strong>, so as to make the quantitative 
                values comparable between the different analyzed samples;
                </li>
                  
                <li>  
                 <strong>Missing values imputation</strong> with different methods, depending 
                on the nature of  the missing values;
                  </li>

                <li>  
                 <strong>Aggregation</strong> from peptide to protein intensity values;
                  </li>
                  
                  <li>
                  <strong>Differential analysis</strong>, which includes null hypothesis 
                significance testing as well as multiple testing correction 
                (for false discovery rate estimation).
                  </li>

                <li>
                  <strong>Gene Ontology (GO) analysis</strong> allows is to map protein list onto GO terms and to test category enrichment.
                  </li>
                  </ul>
                  
                  <br>
                  For more details, please refer to the \"Help\" tab.", sep="")
    
    HTML(text)
    
})
