

# Prostar <img src="./logo.png" width=100 alt="Stan Logo"/>



[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![years in bioc](http://bioconductor.org/shields/years-in-bioc/Prostar.svg)](https://bioconductor.org/packages/release/bioc/html/Prostar.html)
[![bioc downloads](http://bioconductor.org/shields/downloads/Prostar.svg)](https://bioconductor.org/packages/stats/bioc/Prostar/)

Release: [![build release](http://bioconductor.org/shields/build/release/bioc/Prostar.svg)](https://bioconductor.org/checkResults/release/bioc-LATEST/Prostar/)
Devel: [![build devel](http://bioconductor.org/shields/build/devel/bioc/Prostar.svg)](https://bioconductor.org/checkResults/devel/bioc-LATEST/Prostar/)



DAPAR and ProStaR are software tools to perform the statistical analysis of label-free XIC-based quantitative discovery proteomics experiments. DAPAR contains procedures to filter, normalize, impute missing value, aggregate peptide intensities, perform null hypothesis significance tests and select the most likely differentially abundant proteins with a corresponding false discovery rate. ProStaR is a graphical user interface that allows friendly access to the DAPAR functionalities through a web browser.


## Installation

DAPAR and ProStaR are implemented in the R language and are available on the website of the Bioconductor project (http://www.bioconductor.org/). A complete tutorial and a toy dataset are accompanying the packages.


```r
BiocManager::install("DAPAR2")

library(prostar)
prostar()
```

Install of the standalone version

cd ~; mkdir rlib
mkdir rlib/www
chmod -R 777 rlib/www

R CMD INSTALL Prostar_0.97.0.tar.gz --library=rlib
If you would prefer to install the package from R, do this:

### From CRAN
install.packages("Prostar", lib="~/rlib")

## Usage

### Example of a small size protein dataset.
Copy/paste to format your own dataset.
```r
id	IntCond1	IntCond1	IntCond2	IntCond2
0	40679000	33460000	23513000	47514000
1	22472000	20529000	18878000	27066000
2	24181000	22307000	21765000	29650000
3	17831000	15288000	21161000	24255000
4	25557000	29942000	22358000	22992000
5	NA	NA	NA	NA
6	26948000	20690000	20022000	32127000
7	145130000	152030000	161860000	174660000
8	3750600	3680100	9091600	3803000
9	2288700000	2308300000	2457900000	2481200000
10	56216000	56733000	57767000	57121000
11	55116000	55531000	55141000	55985000
12	NA	NA	NA	10604000
13	NA	24846000	21564000	19663000
14	43910000	41853000	42121000	53375000
15	64318000	64018000	69304000	68975000
16	33044000	30683000	44981000	30866000
17	154570000	160330000	161860000	170290000
18	67461000	65409000	63535000	64638000
```

## Contact

samuel.wieczorek@cea.fr, thomas.burger@cea.fr.