![alt text](https://raw.githubusercontent.com/samWieczorek/Prostar/master/inst/ProstarApp/www/images/LogoProstarComplet.png 
"Prostar logo")

# Menu

| ~~ [Home](#home) ~~ | ~~ [Prostar](#prostar) ~~  | ~~ [Support & Resources](#support-resources) ~~ | ~~ [Community](#community) ~~ |
| :------------: | :------------: | :------------: | :------------: |
| [About](#about)  | [Zero-install](#zero-install)  | [Useful links](#useful-links)  | [Team presentation](#team-presentation)  |
| [Citation](#citation) | [Bioconductor installs](#bioconductor-installs)  | [FAQ](#frequently-asked-questions)  | [Bug report](#bug-report)  |
| [Version](#version)  |  [Online demo](#online-demo) | [Forum](#forum)  | [Happiness report](#happiness-report)  |
| [Presentation](#presentation) | [Release Notes](#release-notes) | | |



**Direct access to [Zero-install version of Prostar](#zero-install) or to the [Online demo](#online-demo).**

------------

# Home
## About
Prostar is a software tool devoted to the differential analysis of quantitative data resulting from discovery proteomics experiments.

Prostar is easy to install (see our [Zero-install page](#zero-install)), 
easy to use (thanks to its [Shiny](https://shiny.rstudio.com/)-based click-button interface) 
and well-documented (see our [reference page](#useful-links)). 
Moreover, it has been regularly updated along years to provide state-of-the-art data science methodologies.
<div align="right"><a href="#menu">↥ menu</a></div>

## Citation
**Maintaining Prostar as free software is a heavy duty. Please cite the following reference**

*S. Wieczorek, F. Combes, C. Lazar, Q. Giai-Gianetto, L. Gatto, A. Dorffer, A.-M. Hesse, Y. Coute, M. Ferro, C. Bruley and T. Burger*.  
DAPAR & ProStaR: software to perform statistical analyses in quantitative discovery proteomics.  
**Bioinformatics** 33(1), 135-136, 2017.
<a href="http://doi.org/10.1093/bioinformatics/btw580" target="_blank">http://doi.org/10.1093/bioinformatics/btw580.</a>
<div align="right"><a href="#menu">↥ menu</a></div>

## Version
**dd/mm/yyyy:**  
Prostar version 1.XX has been released on the Bioconductor.

**dd/mm/yyyy:**  
Prostar version 1.YY is available through the Zero-install.
<div align="right"><a href="#menu">↥ menu</a></div>

## Presentation
### Data management
* **Conversion**: To import a tabulated file containing quantitative data and convert it into an MSnset structure.
* **Loading**: To open an Msnset structure that has been previously constructed.
* **Exporting**: To save a partially/completely processed dataset and to download the data analysis results.
* **Demo data**: Toy datasets are available to discover Prostar potential in the simplest way.

### Data processing
* **Filtering**: To prune the protein or peptide list according to various criteria (missing values, string matching);
* **Normalization**: To correct batch or group effects.
* **Imputation**: By taking into account the very nature of each missing value.
* **Aggregation**: For peptide-level datasets, it is possible to estimate protein abundances.
* **Hypothesis testing**: To compute the significance of each protein differential abundance.

### Data mining
* **Descriptive statistics**: Available at any stage of the anayslys, for data exploration and visualization.
* **Differential analysis**: To select a list of differentially abundant proteins with a controlled false discovery rate.
* **Gene Ontology analysis**: To  map a protein list onto GO terms and test category enrichment.
<div align="right"><a href="#menu">↥ menu</a></div>

# Prostar
1. [**Zero-install**](#zero-install)  
The easiest way, so far only available on Microsoft Windows desktop machines.
2. [**Stand-alone Bioconductor install**](#bioconductor-installs)  
The standard method to install Bioconductor distributed software. 
This method works for any operating systems (Unix/Linux, Mac OS X and Windows) as long as R is installed.
3. **Server install**  
When one wants Prostar to run on a Unix server, on which remote users connect. 
This more advanced type of install is detailed in the [user manual](#useful-links).
4. [**Online demo**](#online-demo)  
Before installing Prostar on your desktop machine, test our online demo!

## Zero-install

Just download the zip file below and unzip it !

<a href="http://live.prostar-proteomics.org/" target="_blank">**Download Prostar 1.X.X zip file**</a> (Release date: XX/XX/XXXX)

The unzipped folder contains an executable file which directly launches Prostar.

**Remarks:**
1. It is not necessary to have R already installed.
2. For now, Zero-install is only available for Microsoft Windows machines.
3. At first launch, an internet connection is necessary to finish the install.
4. To ensure full compatibility and debugging, the zip file is available up to one month after each Bioconductor release.
<div align="right"><a href="#menu">↥ menu</a></div>

## Bioconductor installs

Only stand-alone install is detailed below. For server install, please refer to the user manual. 
This type of install works with any operating system among Unix/Linux, Mac OS X and Windows. 
However, it is necessary to have the latest version of R installed in a directory where the user has read/write permissions. 

1. Install Bioconductor package manager by copy-paste of teh following commands (see Bioconductor webpage for details):
```r
chooseCRANmirror()
install.packages("BiocManager")
```
2. Install Prostar:
```r
BiocManager::install("Prostar")
```

3. Launch Prostar:
```r
library(Prostar)
Prostar()
```

4. For a better experience, it is advised to install the development version DT and highcharter packages. 
To do so, install the devtools package and execute the following commands:
```r
install.packages("devtools")
devtools::install_github('rstudio/DT')
devtools::install_github('jbkunst/highcharter')
```
<div align="right"><a href="#menu">↥ menu</a></div>

## Online demo

An online demo of Prostar software is available at:
<a href="http://live.prostar-proteomics.org/" target="_blank">**http://live.prostar-proteomics.org**</a>

**Remark:** The server hosting the online demo has limiting capacities. 
Thus, uploading your own dataset may lead to server overload. 
To test the online demo, please rely on the toy datasets that are available in "Demo data" (from "Data manager" menu).
<div align="right"><a href="#menu">↥ menu</a></div>

## Release notes
### News in Prostar 1.13

#### Bug fixed
* Auto reset of dropdown menu in differential analysis.
* In the feature metadata table, the FC tag has been replaced by 'logFC'.
* In the experimental design table, the column names 'Experiment' and 'Label' have been replaced respectively by 'Sample.name' and 'Condition'.
* Delete the dependency to the package imputeLCMD.
* Tooltip persistance dealt with.

#### New features
* Better managment of dropdown menus in the main menu.
* Add a Bug report tab in the 'Help' menu.
* Reorganization of the menus into Data preprocessing and Data mining.
* Add proportions in logFC distribution plot.
* Add LOESS normalization.
* Add VSN normalization.
* Improve automatic report generation.
* New peptide-to-protein aggregation with fair account of shared peptides.
* Peptide visualization on protein volcano plots.
* Add customisation of colors for plots.


### News in Prostar 1.12

#### Bug fixed
* Normalization: "Sum by columns" has been modified to provide log-abundances compatible with subsequent processing.
* Normalization: Any normalization can now be applied "for each condition independantly" or "globally".
* Imputation: All methods are now only applied "for each condition independantly".

#### New features
* The entire pipeline is now compatible with datasets with more than 2 conditions.
* Descriptive statistics: The expression datasets are colored with respect to the nature of missing value 
(either POV or MEC, see below), even when the value has been imputed.
* Filtering: Manage designs with more than 2 conditions and with conditions containing different number of samples.
* Filtering: More user friendly interface for the string-based filtering (Tab 2).
* Imputation (protein level): Distinction between missing values on an entire condition 
(Missing on the Entire Condition - MEC) and the other ones (Partially Observed Value - POV).
* Imputation (protein level): for POV, it is possible to use SLSA which take into account the experimental design.
* Differential analysis: All tests can be applied on datasets with different number of samples in each condition.
* Differential analysis: Limma takes into account all the hierarchical experimental designs.
* GO analysis: the GeneID nomenclature is now available.

### News in Prostar 1.10

#### Bug fixed
* not traced.

#### New features
* Gene Ontology (GO) analysis (Beta version).
* Automatic report generation (Beta).
* Preliminary separation between peptide and protein level pipelines.
* IMP4P method for peptide level imputation.
* DetQuantile method for protein level imputation.
* Tooltip implementation.
<div align="right"><a href="#menu">↥ menu</a></div>

# Support & resources
## Useful links
### User manuals and tutorials

1. <a href="https://www.bioconductor.org/packages/release/bioc/vignettes/Prostar/inst/doc/Prostar_UserManual.pdf" target="_blank">Prostar user manual</a>
2. <a href="https://sites.google.com/site/thomasburgerswebpage/download/Prostar_Tutorial.pdf"  target="_blank">Prostar tutorial</a>  (not up-to-date)
3. <a href="https://sites.google.com/site/thomasburgerswebpage/download/tutorial-CP4P-4.pdf" target="_blank">cp4p tutorial</a>
4. Prostar protein-level protocol (tba)

### Contact

If you need any help, but also if you wish to make comments or suggestions, please contact Samuel Wieczorek or Thomas Burger (firstname.lastname@cea.fr).

### Reference manuals

1. <a href="https://www.bioconductor.org/packages/release/bioc/manuals/Prostar/man/Prostar.pdf" target="_blank">Prostar reference manual</a>
2. <a href="https://www.bioconductor.org/packages/release/bioc/manuals/DAPAR/man/DAPAR.pdf" target="_blank">DAPAR reference manual</a>
3. <a href="https://www.bioconductor.org/packages/release/bioc/html/MSnbase.html" target="_blank">MSnbase package webpage</a>
4. <a href="https://cran.r-project.org/web/packages/cp4p/cp4p.pdf" target="_blank">CP4P reference manual</a>
5. <a href="https://cran.r-project.org/web/packages/imp4p/imp4p.pdf" target="_blank">IMP4P reference manual</a>


### Bibliographical references

#### Our referenced works


1. <a href="http://prabig-prostar.univ-lyon1.fr/Articles/calib-final.pdf" target="_blank">_Q. Giai Gianetto, F. Combes, C. Ramus, C. Bruley, Y. Coute and T. Burger_. Calibration Plot for Proteomics (cp4p): A graphical tool to visually check the assumptions underlying FDR control in quantitative experiments. **Proteomics**, 16(1):29-32, 2016.</a>
2. <a href="http://prabig-prostar.univ-lyon1.fr/Articles/natureOfMV-Vsubmited2.pdf" target="_blank">_C. Lazar, L. Gatto, M. Ferro, C. Bruley, T. Burger_. Accounting for the multiple natures of missing values in label-free quantitative proteomics datasets to compare imputation strategies. **Journal of Proteome Research**, 15(4):1116-1125, 2016.</a>
3. <a href="http://prabig-prostar.univ-lyon1.fr/Articles/OnTheMissuseOfFudgeFactorInProteomics_FV.pdf" target="_blank">_Q. Giai Gianetto, Y. Coute, C. Bruley and T. Burger_. Uses and misuses of the fudge factor in quantitative discovery proteomics. **Proteomics**, 16(14):1955-60, 2016.</a>
4. <a href="http://prabig-prostar.univ-lyon1.fr/Articles/prostar.pdf" target="_blank">_S. Wieczorek, F. Combes, C. Lazar, Q. Giai-Gianetto, L. Gatto, A. Dorffer, A.-M. Hesse, Y. Coute, M. Ferro, C. Bruley and T. Burger_. DAPAR & ProStaR: software to perform statistical analyses in quantitative discovery proteomics, **Bioinformatics**, 33(1):135-136, 2017</a>
5. <a href="http://prabig-prostar.univ-lyon1.fr/Articles/fdrtuto.pdf" target="_blank">_T. Burger_. Gentle introduction to the statistical foundations of false discovery rate in quantitative proteomics. **Journal of Proteome Research**, 17(1):12-22, 2017.</a>
6. <a href="http://prabig-prostar.univ-lyon1.fr/Articles/revised-biostat-proteom-preprint.pdf" target="_blank">_L. Jacob, F. Combes and T. Burger_. PEPA test : fast and powerful differential analysis from relative quantitative proteomics data using shared peptides. **Biostatistics**, kxy021, 2018.</a>
7. _Q. Giai Gianetto, C. Lazar, S. Wieczorek, C. Bruley, Y. Coute and T. Burger_. Multiple imputation strategy for mass spectrometry-based proteomic data. (in preparation).


#### Other references

8. _Bolstad BM_ (2017). preprocessCore: A collection of pre-processing functions. **R package** version 1.38.1
9. _Hastie T, Tibshirani R, Narasimhan B and Chu G_ (2017). impute: Imputation for microarray data. **R package** version 1.50.1
10. _Gatto L and Lilley K_ (2012). MSnbase - an R/Bioconductor package for isobaric tagged mass spectrometry data visualization, processing and quantitation. **Bioinformatics**, 28, pp. 288-289.
11. _Ritchie ME, Phipson B, Wu D, Hu Y, Law CW, Shi W and Smyth GK_ (2015). Limma powers differential expression analyses for RNA-sequencing and microarray studies. **Nucleic Acids Research**, 43(7), pp. e47.
12. _Cleveland, W. S., & Devlin, S. J._ (1988). Locally weighted regression: an approach to regression analysis by local fitting. **Journal of the American statistical association**, 83(403), 596-610.
13. _Huber, W., Von Heydebreck, A., Sültmann, H., Poustka, A., & Vingron, M._ (2002). Variance stabilization applied to microarray data calibration and to the quantification of differential expression. **Bioinformatics**, 18(suppl_1), S96-S104.
<div align="right"><a href="#menu">↥ menu</a></div>

## Frequently asked questions
### FAQ table of contents

* [Why does the table in experimental design blink during edition?](#why-does-the-table-in-experimental-design-blink-during-edition)
* [How to build a valid experimental design?](#how-to-build-a-valid-experimental-design)
* [Why do the items of the contextual menus for plots remain 'undefined'?](#why-do-the-items-of-the-contextual-menus-for-plots-remain-undefined)
* [Why does my volcano plot look so aligned?](#why-does-my-volcano-plot-look-so-aligned)
* [How to recover differential analysis results?](#how-to-recover-differential-analysis-results)

### Why does the table in experimental design blink during edition?

When you edit the experimental design (during converting a text file to MSnset or during the update of the design),
it may happen that the cells begin to blink in a random order. Then, no more operation is possible in the table. 
This happens if you edit the cells too fast with respect to the table update speed. We apologize for this caveat : 
this is a known bug of the package used to provide the table. No patch is available yet. The only workaround is to close then reopen Prostar.


### How to build a valid experimental design? 
In Prostar, the differential analysis is devoted is devoted to the processing of hierarchical unpaired experimental designs . 
However, in former versions, this was not explicit enough, so that users with paired samples could used Prostar with wrong assumptions. 
To clear this out, we have changed the experimental design construction step so that its explicitly appears unpaired.

As a result, the samples must now be numbered as in the following example:

* Condition 1: 1 - 2 - 3 - 4,
* Condition 2: 5 - 6 - 7 - 8

As opposed to:

* Condition 1: 1 - 2 - 3 - 4,
* Condition 2: 1 - 2 - 3 - 4

Which, depending on the context, could suggest that the 8 samples comes only from 4 different biological subjects, 
and thus leading to paired tests - for instance, patients that are compared between Before (Condition 1) and After (Condition 2) some treatment.

However, one should note that even if the experimental design now looks different, 
this is just due to a numbering convention, and the statistical test is not impacted.


### Why do the items of the contextual menus for plots remain 'undefined'?
This happens if the version of the package 'highcharter' is less or equal to 0.5.0. 
To fix this issue, you should install the devel version of the package by typing the following command in a R console: 
```r
devtools::install_github('jbkunst/highcharter')
```

### Why does my volcano plot look so aligned?

![alt text](https://raw.githubusercontent.com/samWieczorek/Prostar/master/inst/ProstarApp/www/images/dfPriorIssue.png "Volcano plot resulting from a Limma issue")

In very uncommun situations, one may obtain a bowl shape volcano plot such as depicted above. This is due to using Limma on a dataset for which it is not adapted: Briefly, the numerical values in the quantitative matrix appears to have a repetitive pattern that prevent Limma routines to compute the number of degrees of freedom of the Chi2 distribution on which the protein variances should be fitted. As a result, Limma returns a result directly proportional to the fold-change, and the p-values are none-informative. In such cases, which are fortunately extremely odd, we advise to replace Limma test by a classical t-test.

### How to recover differential analysis results?

From Prostar 1.14, the differential analysis results are not exported anymore when using the "Export fo file" functionality, regardless the format (MSnset, excel or zipped CSV). This is due to the separate management of the "data mining" and "data processing outputs. As a result, after performing the differential analysis, the results must be downloaded thanks to the devoted buttons (otherwise, they will be lost when closing the Prostar session). However, all the p-value computatations (from the "hypothesis testing" menu) can be exported and recover from one session to another one.
<div align="right"><a href="#menu">↥ menu</a></div>
 
## Forum
 
Our community forum is hosted by the Bioconductor:
<a href="https://support.bioconductor.org/t/prostar/" target="_blank">**https://support.bioconductor.org/t/prostar/**</a>
<div align="right"><a href="#menu">↥ menu</a></div>


# Community

## Team presentation

### Core team

**Samuel Wiezcorek**  
After a first career as IT support technician, Sam obtained an engineering degree (2004) at "Conservatoire National des Arts et Métiers", 
followed by a MS degree in computer sciences and a PhD in machine learning (2009) at Grenoble-Alpes University. 
Since then, he has been working as a research engineer at EDyP-lab, where he has been developing and maintaining software tools for proteomics. 
Sam has been involved in Prostar project since its beginning. 
He is the code guru and supervises all the software aspects of the project, such as coding, packaging, deployment, 
debugging, graphical user interfaces, etc.


<a href="https://sites.google.com/site/thomasburgerswebpage" target="_blank">**Thomas Burger**</a>  
After two MS degrees in computer sciences and in applied mathematics (2004), 
Thomas defended a PhD in pattern recognition (2007) at Grenoble-Alpes University. 
From 2008 to 2011, he held an associate professor position in statistics (South Britany University). 
In November 2011, he obtained a full time CNRS researcher position at EDyP-lab, and started addressing computational proteomics questions. 
He is the principal investigator of Prostar project. 
His expertise focuses on the statistical, methodological and algorithmic aspects of proteomics data analysis.  


**Contact us** - firstname.lastname@cea.fr

### Occasional contributors

* Florence Combes
* <a href="https://research.pasteur.fr/fr/member/quentin-giai-gianetto/" target="_blank">Quentin Giai-Gianetto</a> (Institut Pasteur, France)
* <a href="https://lgatto.github.io/" target="_blank">Laurent Gatto</a> 
(Université catholique de Louvain, Belgique)
* Cosmin Lazar
* Hélène Borges
* <a href="https://scholar.google.fr/citations?user=pUQIR6wAAAAJ&hl=fr" target="_blank">Yohann Couté</a>
* Christophe Bruley
* Anne-Marie Hesse
* Alexia Dorffer​

### Beta-testing & co.
The entire EDyP proteomics platform (see <a href="http://www.edyp.fr" target="_blank">www.edyp.fr</a>): 
Prostar being permanently hosted by EDyP lab, the first users (the original ones, but also the testers) are naturally the lab members. 
They are all warmly acknowledged for their contributions.
<div align="right"><a href="#menu">↥ menu</a></div>

## Bug report

To report any issue with Prostar, it is best to use the devoted tab in Prostar software (click on **Bug report** in the **Help menu**), 
as it allows easy sharing of the session logs and data (essential to efficient debugging).

However, it is also possible to the development team by email (see [team presentation](#team-presentation)).
<div align="right"><a href="#menu">↥ menu</a></div>

## Happiness report

**If you are pleased with your Prostar experience**, you can also send us a message (messages are not restricted to bug reports)!  
:-)

Do not forget to [cite Prostar in your publications](#citation)!
<div align="right"><a href="#menu">↥ menu</a></div>
