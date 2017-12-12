\name{Prostar-NEWS}
\title{Prostar News}

\section{CHANGES IN VERSION 1.11.4}{
\subsection{BUG FIXES}{
  \itemize{
        \item A bug in the string-based filtering tool was fixed. The case where an entity could be both contaminants and reverse was not takien into account.This lead to wrong number in the plot.
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
  
  