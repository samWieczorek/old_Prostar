output$References <- renderText({
    
    
    txt<- "<strong><font size=\"5\">HELP</font></strong>
         <br><hr color:\"blue\"><br>
         
         <strong><font size=\"4\">User manual:</font></strong>
         <a href=\"https://www.bioconductor.org/packages/release/bioc/vignettes/
Prostar/inst/doc/Prostar_UserManual.pdf\"
         title=\"here\" target=\"_blank\">here</a>
         <br><br>
         
         
         
         <strong><font size=\"4\">Tutorial:</font></strong>
         <a href=\"http://bioconductor.org/packages/release/bioc/vignettes/
Prostar/inst/doc/Prostar_Tutorial.pdf\"
         title=\"here\" target=\"_blank\">here</a>
         <br><br>
         
         <strong><font size=\"4\">Contact:</font></strong><br>
         If you need any help, but also if you wish to make comments 
         or suggestions, please contact Samuel Wieczorek, Florence Combes or 
         Thomas Burger (firstname.lastname@cea.fr)<br><br>
         
         
         <strong><font size=\"4\">Additional ressources:</font></strong>
         <ul>
         <li> ProStaR reference manual:  
         <a href=\"https://www.bioconductor.org/packages/release/bioc/manuals/
Prostar/man/Prostar.pdf\"
         title=\"here\" target=\"_blank\">here</a>
         </li>
         <li> DAPAR reference manual: <a href=\"https://www.bioconductor.org/
packages/release/bioc/manuals/DAPAR/man/DAPAR.pdf\"
         title=\"here\" target=\"_blank\">here</a>
         
         
         </li>
         <li> MSnbase package: <a href=\"https://www.bioconductor.org/packages/
release/bioc/html/MSnbase.html\" title=\"here\" target=\"_blank\">here</a>
         </li>
         <li> Cp4p tutorial: <a href=\"https://cran.r-project.org/web/
packages/cp4p/cp4p.pdf\"
         title=\"here\" target=\"_blank\">here</a>
         </li>
         </ul>
         <br><br>
         
         <strong><font size=\"4\">Our referenced works:</font></strong>
         <ol>
         <li> S. Wieczorek, F. Combes, C. Lazar, Q. Giai-Gianetto, L. Gatto, 
        A. Dorffer, A.-M. Hesse, Y. Coute, M. Ferro, C. Bruley, T. Burger. 
        DAPAR & ProStaR: software to perform statistical analyses in 
        quantitative discovery proteomics, <i>Bioinformatics</i>, 2016
         </li>
         <li> C. Lazar, L. Gatto, M. Ferro, C. Bruley, T. Burger. Accounting 
         for the multiple natures of missing values in label-free quantitative 
         proteomics datasets to compare imputation strategies. <i>Journal of 
         Proteome Research</i>, 15(4):1116-25, 2016. 
         </li>
         <li> Q. Giai Gianetto, F. Combes, C. Ramus, C. Bruley, Y. Coute, 
         T. Burger. Calibration Plot for Proteomics (cp4p): A graphical tool 
         to visually check the assumptions underlying FDR control in 
         quantitative experiments. <i>Proteomics</i>, 16(1):29-32, 2016. 
         </li>
         
         <li> Q. Giai Gianetto, Y. Coute, C. Bruley, T. Burger. Uses and 
         misuses of the fudge factor in quantitative discovery proteomics. 
         <i>Proteomics</i>, 16(14):1955-60, 2016. 
         </li>

         <li> Q. Giai Gianetto, C. Lazar, S. Wieczorek, C. Bruley, Y. Coute and 
         T. Burger. Multiple imputation strategy for mass spectrometry-based 
         proteomic data (under preparation)
         </li>
         
         </ol>
         
         <br><br>
         
         <strong><font size=\"4\">Other references:</font></strong>
         <ol start=6>
         <li> Bolstad BM (2016). preprocessCore: A collection of pre-processing 
        functions. R package version 1.36.0, 
        <a href=\"https://github.com/bmbolstad/preprocessCore\"
         title=\"here\" target=\"_blank\">
        https://github.com/bmbolstad/preprocessCore</a>

         <li> Hastie T, Tibshirani R, Narasimhan B and Chu G (2016). 
        impute: Imputation for microarray data. R package version 1.48.0
         </li>

         <li> Ported to R by Alvaro A. Novo. Original by Joseph L. Schafer 
        <jls@stat.psu.edu>. (2013). norm: Analysis of
        multivariate normal datasets with missing values. R package 
        version 1.0-9.5.
        <a href=\"https://CRAN.R-project.org/package=norm\"
         title=\"norm on CRAN\" target=\"_blank\">norm on CRAN</a>
         </li>
         </ol>"
         
         
         HTML(txt)

})



#-------------------------------------------------------------------
output$aboutText <- renderUI({
    busyIndicator("Calculation in progress",wait = 0)
    
    t <- sessionInfo()
    daparVersion <- installed.packages()["DAPAR","Version"]
    ProstarVersion <- installed.packages()["Prostar","Version"]
    
    
    text <- paste("<strong>To cite DAPAR and ProStaR software:</strong><br> 
                  S. Wieczorek, F. Combes, C. Lazar, Q. Giai-Gianetto, 
                L. Gatto, A. Dorffer, A.-M. Hesse, Y. Coute, M. Ferro, 
                C. Bruley, T. Burger. <i>\"DAPAR & ProStaR: software to 
                perform statistical analyses in quantitative discovery 
                proteomics\"</i>, <i>Bioinformatics</i>, 2016
                  
                  <br><br><br>
                  <strong>DAPAR</strong> and <strong>ProStaR</strong> form a 
                  software suite for quantitative analysis of mass spectrometry 
                  based proteomics, more specifically designed to process 
                  relative quantitative data from discovery experiments.<br> <br>
                  
                  
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
                  
                  In addition, it is bind to numerous other R packages available on 
                  <a href=\"the https://cran.r-project.org/\" title=\"here\" target=\"_blank\">
                  CRAN</a> or on the <a href=\"http://www.bioconductor.org\"
                  title=\"here\" target=\"_blank\">Bioconductor</a>, among which 
                  <a href=\"http://www.bioconductor.org/packages/release/bioc/html/MSnbase.html\"
                  title=\"here\" target=\"_blank\">MSnbase</a>, which has introduced Msnsets, 
                  the data structure on which all the processing are based.
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
                  <strong>Differential analysis</strong>, which includes null hypothesis 
                significance testing as well as multiple testing correction 
                (for false discovery rate estimation).
                  </li>

                <li>
                  <strong>Gene Ontology (GO) analysis</strong>, XXXXXXXXXXXXXX.
                  </li>
                  </ul>
                  
                  <br>
                  <br>
                  For more details, please refer to the \"Help\" tab.", sep="")
    
    HTML(text)
    
})
