
## News in Prostar 1.12

### Bug fixed
* Normalization: "Sum by columns" has been modified to provide log-abundances compatible with subsequent processing.
* Normalization: Any normalization can now be applied "for each condition independantly" or "globally".
* Imputation: All methods are now only applied "for each condition independantly".

### New features
* The entire pipeline is now compatible with datasets with more than 2 conditions.
* Descriptive statistics: The expression datasets are colored with respect to the nature of missing value (either POV or MEC, see below), even when the value has been imputed.
* Filtering: Manage designs with more than 2 conditions and with conditions containing different number of samples.
* Filtering: More user friendly interface for the string-based filtering (Tab 2).
* Imputation (protein level): Distinction between missing values on an entire condition (Missing on the Entire Condition - MEC) and the other ones (Partially Observed Value - POV).
* Imputation (protein level): for POV, it is possible to use SLSA which take into account the experimental design.
* Differential analysis: All tests can be applied on datasets with different number of samples in each condition.
* Differential analysis: Limma takes into account all the hierarchical experimental designs.
* GO analysis: the GeneID nomenclature is now available.

## News in Prostar 1.10

### Bug fixed
* not traced.

### New features
* Gene Ontology (GO) analysis (Beta version).
* Automatic report generation (Beta).
* Preliminary separation between peptide and protein level pipelines.
* IMP4P method for peptide level imputation.
* DetQuantile method for protein level imputation.
* Tooltip implementation.
