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
    <i>Journal of Proteome Research</i>, accepted for publication, 2017. 
    </a></li>
    <li> L. Jacob, F. Combes and T. Burger. PEPA test : fast and powerful differential analysis
    from relative quantitative proteomics data using shared peptides. (under review).
    </li>
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



getPackagesVersions <- reactive({
    outOfDate <- "(Out of date)"
    dev <- "(Devel)"
    
    biocRelease <- NULL
    tryCatch({
        biocRelease <-available.packages(contrib.url("http://bioconductor.org/packages/release/bioc/"))
        require(XML)
        html <- readHTMLTable("http://bioconductor.org/packages/release/data/experiment/html/DAPARdata.html")
        DAPARdata.version <- as.character(html[[3]][2][1,])
        
    }, warning = function(w) {
        return()
    }, error = function(e) {
        return()
    }, finally = {
        #cleanup-code 
    })
    
    pkgs <- c("Prostar", "DAPAR", "DAPARdata")
    loc.pkgs <-c("Prostar.loc", "DAPAR.loc", "DAPARdata.loc")
    instPkgs <- list(Prostar = installed.packages(lib.loc=Prostar.loc)["Prostar","Version"],
                     DAPAR = installed.packages(lib.loc=DAPAR.loc)["DAPAR","Version"],
                     DAPARdata = installed.packages(lib.loc=DAPARdata.loc)["DAPARdata","Version"])
    
    
    names <- c(as.character(tags$a(href="http://www.bioconductor.org/packages/release/bioc/html/Prostar.html", "Prostar")), 
               as.character(tags$a(href="http://www.bioconductor.org/packages/release/bioc/html/DAPAR.html", "DAPAR")), 
               as.character(tags$a(href="http://www.bioconductor.org/packages/release/data/experiment/html/DAPARdata.html", "DAPARdata")))
    
    
df <- data.frame("Name" = names,
                     "Installed.packages"= rep(NA, 3), 
                     "Bioc.release" =  rep(NA, 3))
    
    
    df[, "Installed.packages"] <- unlist(instPkgs)
    
    if (!is.null(biocRelease)) {
        biocPkgs <- list(Prostar = biocRelease["Prostar","Version"],
                         DAPAR = biocRelease["DAPAR","Version"],
                         DAPARdata = DAPARdata.version)
        
        if (instPkgs$Prostar == biocPkgs$Prostar){df[1,"Name"] <-  names[1]}
        else if (instPkgs$Prostar > biocPkgs$Prostar){df[1,"Name"] <-   paste(names[1],  "<strong>",dev, "</strong>", sep=" ")}
        else if (instPkgs$Prostar < biocPkgs$Prostar){df[1,"Name"] <-   paste(names[1], "<strong>", outOfDate, "</strong>", sep=" ")}
        
        if (instPkgs$DAPAR == biocPkgs$DAPAR){df[2,"Name"] <-  names[2]}
        else if (instPkgs$DAPAR > biocPkgs$DAPAR){df[2,"Name"] <-   paste(names[2],  "<strong>",dev, "</strong>", sep=" ")}
        else if (instPkgs$DAPAR < biocPkgs$DAPAR){df[2,"Name"] <-   paste(names[2],  "<strong>",outOfDate, "</strong>", sep=" ")}
        
        if (instPkgs$DAPARdata == biocPkgs$DAPARdata){df[3,"Name"] <-  names[3]}
        else if (instPkgs$DAPARdata > biocPkgs$DAPARdata){df[3,"Name"] <-   paste(names[3],  "<strong>",dev, "</strong>", sep=" ")}
        else if (instPkgs$DAPARdata < biocPkgs$DAPARdata){df[3,"Name"] <-   paste(names[3],  "<strong>",outOfDate, "</strong>", sep=" ")}
        
        
        df[, "Bioc.release"] <- unlist(biocPkgs)
    }

    
    colnames(df) <- c("Names", "Installed packages", "Bioc release")
    #rownames <- c(as.character(tags$a(href="www.rstudio.com", "Click here!")), "DAPAR", "DAPARdata")
    df
})



output$tab_versions <- renderDataTable({
    dt <- DT::datatable(getPackagesVersions(), 
                        escape = FALSE,
                        rownames= FALSE,
                        option=list(initComplete = initComplete(),
                            dom = 't',
                            autoWidth=TRUE,
                            columnDefs = list(list(width='200px',targets= "_all"))
                            )
                    )
    dt
})

output$checkUpdates <- renderUI({
   
    df <- getPackagesVersions()
    instPkgs <- df[,"Installed packages"]
    biocPkgs <- df[,"Bioc release"]
    if ((instPkgs$Prostar == biocPkgs$Prostar) && (instPkgs$DAPAR == biocPkgs$DAPAR) && (instPkgs$DAPARdata == biocPkgs$DAPARdata)){
            h3("All the Prostar packages are in the newest version.")
        } else{
            h3("The Prostar packages are out of date. Please check the bioconductor.")
            }

})


output$warningDependanciesVersion <- renderUI({
    
    DTVersion <- installed.packages()["DT","Version"]
    highcharterVersion <-installed.packages()["highcharter","Version"]
    
    
txt <- "<strong><font size=\"4\" color=\"red\">Note:</font></strong> <br>
   <font color=\"red\">For a better experience with Prostar, we advice you to install the development version of the following
    packages : DT and highcharter. <br>
   To do so, type and execute the followings commands in a R console:<br>
    <ul>
    <li> devtools::install_github('rstudio/DT')</li>
    <li> devtools::install_github('jbkunst/highcharter')</li>
    </ul> </font>"

    if (DTVersion != "0.4.11" || highcharterVersion != "0.6.0")
    HTML(txt)
    
})
#-------------------------------------------------------------------
output$aboutText <- renderUI({
    busyIndicator(WaitMsgCalc,wait = 0)
    
    t <- sessionInfo()
    daparVersion <- installed.packages(lib.loc=DAPAR.loc)["DAPAR","Version"]
    ProstarVersion <- installed.packages(lib.loc=Prostar.loc)["Prostar","Version"]
    
    
    text <- paste("<strong>Maintaining ProStaR as free software is a heavy and time-consuming
                  duty. If you use it, please cite the following reference</strong><br> 
                  S. Wieczorek, F. Combes, C. Lazar, Q. Giai-Gianetto, 
                  L. Gatto, A. Dorffer, A.-M. Hesse, Y. Coute, M. Ferro, 
                  C. Bruley and T. Burger. <br>
                  <u>\"DAPAR & ProStaR: software to perform statistical 
                  analyses in quantitative discovery 
                  proteomics\"</u><br>
                  <i>Bioinformatics 33(1), 135-136</i>, <strong>2017</strong><br>
                  <a href=\"http://doi.org/10.1093/bioinformatics/btw580\"
                  title=\"here\" target=\"_blank\">http://doi.org/10.1093/bioinformatics/btw580</a>
                  
                  <br><hr>
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


output$FAQ_output <- renderUI({
    
    tagList(
    tags$br(),tags$br(),tags$br(),
    tags$h4("1 - Why the table in experimental design blinks when I am editing it?"),
    tags$p("When you edit the experimental design (during converting a text file to MSnset or during the update of the design),
           it may happen that the cells begin to blink in a random order. Then, no more operation is possible in the table. 
           This happens if you edit the cells too fast w.r.t. the speed of update of the table. We apologize for this caveat : this is a known bug of the package used to
           provide the table. No fix is available yet. The only workaround is to close then reopen Prostar."),
    tags$br(),
    tags$h4("2 - How to build a valid experimental design?"),
    tags$p("The differential analysis of Prostar now integrates hierarchical designs when using limma. TODO")
    
    
    )
})