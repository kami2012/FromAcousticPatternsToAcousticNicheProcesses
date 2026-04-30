# Habitat complexity allows temporal clustering of similar species 

This repository contains the data and code required to reproduce the results presented in the manuscript:
"Habitat complexity expands acoustic space of tropical bird communities and allows temporal clustering of similar species"

## Manuscript Summary

Understanding how species partition limited resources is a central question in ecology and evolution.
This study addresses the Acoustic Niche Hypothesis (ANH) and the Acoustic Adaptation Hypothesis (AAH) 
by adopting a multi-level approach that explicitly distinguishes between levels of ecological organization. 
Using a comprehensive dataset from the Ecuadorian Chocó, we analyzed over 340 bird species along a forest recovery gradient. 
By integrating high-resolution co-occurrence data (2-minute intervals) with trait and phylogenetic analyses, 
we disentangle how habitat complexity, environmental constraints, and evolutionary history shape acoustic community assembly.

## Key Findings

- **Scale-Dependency & Process Decoupling**: Acoustic partitioning does not scale consistently across levels of organization.
Local behavioural patterns are decoupled from community-level structure, which is primarily shaped by environmental constraints,
allometric structure, and phylogenetic history. This scale mismatch may explain inconsistent results in previous niche studies.

- **Acoustic Space Expansion**: Contrary to AAH predictions, mean community peak frequency did not decrease with forest age.
Instead, habitat complexity expands the available acoustic space (quantified as the standard deviation of peak frequency).

- **Temporal Clustering**: Contrary to ANH predictions, positive co-occurrences (temporal overlap) outnumber negative ones.
In this hyperdiverse system, ecologically similar species tend to cluster temporally rather than avoid overlap.

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
  
- ```02_measure-peak-frequencies```: Automated extraction of peak frequencies and validation against Mikula et al. (2021) and Sagar et al. (2024).
  
- ```03_analyze-cooccurrence-distances```: Calculation of acoustic, phylogenetic and ecological distances between co-occurring species.
  
- ```04_test-aah```: Statistical analyses testing the Acoustic Adaptation Hypothesis (AAH).

Note: Each Level 1 folder contains **Level 2** scripts for specific tasks like data filtering, modeling, and plotting.


## Data Structure

The folder structure reflects conceptual data sources rather than processing stages. All transformations are handled in the scripts.

- ```BirdNET_detections```: Automated species detections derived from acoustic recordings.

- ```Expert_detections```: Detections based on manually labelled species identifications.

- ```Co-occurrence```: Temporal overlap within 2-minute intervals and associated distance metrics for co-occurring species pairs.
  
- ```Peak_frequencies```: Species-specific peak frequencies (acoustic traits) derived from literature and our automated spectral analysis.
  
- ```Traits_phylo```: Functional trait and phylogenetic data for all recorded species.
  
- ```Dis_matrices```: Calculated distance matrices (acoustic, phylogenetic, ecological, and morphological).

- ```Testing_AAH```: Community-level acoustic metrics (mean and standard deviation of peak frequency) aggregated per plot.

