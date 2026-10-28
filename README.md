# From acoustic patterns to acoustic niche processes 

This repository contains the data and code required to reproduce the results presented in the manuscript:
"From acoustic patterns to acoustic niche processes: evidence from a hyperdiverse tropical bird assemblage"

## Manuscript Summary

This study tests specific predictions of the Acoustic Niche Hypothesis (ANH) and the Acoustic Adaptation Hypothesis (AAH) in a hyperdiverse tropical bird community along a forest recovery gradient in the Ecuadorian Chocó. We analyzed more than 330 bird species, combining expert-annotated species occurrences within two-minute passive acoustic recordings with species-level peak frequencies, morphological traits, phylogenetic relationships, and three-dimensional forest structure.

This combination of broad community coverage and locally resolved co-occurrence information allowed us to test temporal associations among species, whether recurrent temporal co-occurrence is associated with greater spectral differentiation, and how community-level acoustic trait distributions vary with forest structural complexity. 

We additionally use these results to consider the inferential scope of ANH-related tests: what a particular test resolves depends on the specific prediction, observational level, and spatiotemporal scale at which it is evaluated.

## Key Findings

- **Temporal associations**: Contrary to the temporal segregation pattern predicted under the ANH at the two-minute grain examined here, positive temporal associations were more prevalent than negative associations.

- **Spectral differentiation**: Positive temporal associations were not accompanied by greater differentiation in peak frequency. Positively associated species pairs were more similar in ecological and morphological traits, but their peak-frequency distances were no larger than those of negatively associated pairs, even after accounting for body-mass and phylogenetic distances.

- **Environmental structuring of acoustic traits**: Contrary to the directional prediction derived from the AAH, mean community peak frequency did not decrease with increasing forest structural complexity. However, community peak-frequency dispersion increased systematically with forest structural complexity, both before and after accounting for body mass.

- **From patterns to processes**: These results provide no support for the specific temporal, spectral, and directional frequency-shift patterns tested here, but they do not imply an absence of acoustic interference more generally. Conversely, an observed pattern consistent with acoustic partitioning would not by itself identify acoustic interference as the process that generated it. The interpretation of ANH-related evidence therefore depends on the inferential scope of the specific test and on the extent to which alternative processes can be distinguished.

## Requirements

### Software

- R (version 4.4.0 or higher)
- RStudio (recommended, project managed via ```Habitat_Complexity_Clustering.Rproj``` file)

### Key R Packages

- **Data Access**: ```httr```, ```jsonlite```
- **Data Handling**: ```dplyr```, ```stringr```, ```tidyr```
- **Co-Occurrence Analysis**: ```cooccur```
- **Audio Processing**: ```seewave```, ```tuneR```
- **Phylogeny**: ```ape```, ```purrr```
- **Modeling & Statistics**: ```lme4```, ```multcomp```, ```ARTool```
- **Plotting**: ```ggplot2```, ```ggrepel```

## Code Structure

The analysis is organized in numbered scripts reflecting the workflow hierarchy. Scripts should be executed sequentially in ascending order.

### Level 1: Major Analytical Steps

- ```01_compute-cooccurrence```: Processing of detection data to identify species co-occurring within the same 2 minutes.
  
- ```02_measure-peak-frequencies```: Compilation of species-level peak frequencies from Sagar et al. (2024) and automated estimation of peak frequencies from acoustic recordings for species lacking suitable published values.
  
- ```03_analyze-cooccurrence-distances```: Calculation of acoustic, phylogenetic and ecological distances between co-occurring species.
  
- ```04_test-aah```: Statistical analyses testing the Acoustic Adaptation Hypothesis (AAH).

Note: Each Level 1 folder contains **Level 2** scripts for specific tasks like data filtering, modeling, and plotting.


## Data Structure

The folder structure reflects conceptual data sources rather than processing stages. All transformations are handled in the scripts.

- ```Co-occurrence```: Temporal overlap within 2-minute intervals and associated distance metrics for co-occurring species pairs.

- ```Dis_matrices```: Calculated distance matrices (acoustic, phylogenetic, ecological, and morphological).

- ```Expert_detections```: Detections based on manually labelled species identifications.

- ```Peak_frequencies```: Species-specific peak frequencies (acoustic traits) derived from literature and our automated spectral analysis.

- ```Testing_AAH```: Community-level acoustic metrics (mean and standard deviation of peak frequency) aggregated per plot.
  
- ```Traits_phylo```: Functional trait and phylogenetic data for all recorded species.
  



