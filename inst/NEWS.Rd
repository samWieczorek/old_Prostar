\name{Prostar-NEWS}
\title{Prostar News}




\section{CHANGES IN VERSION 1.11.13}{
\subsection{BUG FIXES}{
  \itemize{
        \item Normalization: "Sum by columns" has been modified to provide log-abundances compatibles with the other treatments. It can be done "for each condition independantly" or "globaly".
        }
        
        }
        
        
\subsection{NEW FEATURES}{
    \itemize{
        \item Descriptive statistics: The expression datasets are colored w.r.t the nature of missing value (POV or MEC) even when the value has been imputed
        \item  Filtering: Manage designs with more than 2 conditions and with conditions containing different number of samples
        \item  Filtering: UI more user friendly for the string-based filtering (Tab 2)
      \item Normalization: A few modifications in the UI and 
      \item Imputation (protein level): Distinction between missing values on an entire condition (Missing on the Entire Condition) and the other ones (Partially Observed Value)
      \item Imputation (protein level): for the POV, it is possible to use SLSA which take into account the experimentaldesign experimental
      \item Imputation (protein level): imputations are all processed condition by condition
    \item Differential analysis: All tests can process datasets with conditions of different number of samples
    \item Differential analysis: Limma takes into account all the hierarchical experimental designs

    \item GO analysis: Add the GeneID nomenclature.
}
       }
 }       
 
 
 
 
\section{CHANGES IN VERSION 1.11.4}{
\subsection{BUG FIXES}{
  \itemize{
        \item A bug in the string-based filtering tool was fixed. The case where an entity could be both contaminants and reverse was not takien into account.This lead to wrong number in the plot.
        \item Correction of the beahviour of the table in the experimental design (convert Data tool). When the user copy-paste some lines it may add unneeded rows. These rows can be deleted with an option in the contextual menu.
        }
        
        }
 }       
        
\section{CHANGES IN VERSION 1.9.15}{

\subsection{BUG FIXES}{
  \itemize{
  \item When the aggregation step has been performed, the interface switches to the first tab of the 'Descriptive Statistics' in order to view informations aout the new dataset (the protein one).
   \item Implementation of a parallel version of the function which saves the (new) protein dataset after the aggregation step.
   \item Disable the extra row appearing in the metadata table when convertinga text file to a MSnSet file.
   \item Disable the extra row appearing in the metadata table when convertinga text file to a MSnSet file.
    \item  A new package (readxl) is used to read xls or xlxs files. In certain circumstances, the functions of the previsous package openxlsx is not able to decode properly Excel files.
    \item When converting a new (text or Excel) file in Prostar : the missing values were not registered as expected. Especially, they did not appear in blue in the table above the volcanoplot. Bug fixed
    \item A bug occured when the user load successively several datasets in Prostar. The previous ones were note correctly erased and this has lead to side effects. This bug is now fixed
   }
   }
   
  \subsection{NEW FEATURES}{
    \itemize{
        \item  Enhancement of the string-based filtering UI
      \item The automatic generation of an analysis report has been integrated in the Dataset Manager (menu 'Export'). It allows the user to download plots and parameters used in Prostar ont their dataset. 
      \item  Added a Gene Ontology (GO) analysis module in Data Processing. This module allows to perform GO grouping and GO Enrichment.
      \item  Several plots are now based on the package highcharter which is a wrapper to the highcharts graphical library. It provides interactivity with the user.
}
       }
       }
  
  