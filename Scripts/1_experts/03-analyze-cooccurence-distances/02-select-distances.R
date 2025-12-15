rm(list=ls(all=TRUE))
Sys.setenv(LANG = "en")

# Working directory
setwd("E:/Manuscripts/4_co-occurence") 

library(ape)
library(ARTool)
library(purrr)
library(dplyr)

## ============================================================================================================ ##
## Select pairwise distance values for each co-occurring species pair across all trait and phylogenetic matrices


#--------------
## Load data ##
#--------------

load("Data/1_experts/co-occurrence/cooccurrence_results.RData") # cooccurrence pairs

# Distance matrices
load("Data/1_experts/dist_matrices/distM.rda") # Traits
dist_traits <- distM
dist_peak <- as.matrix(read.csv2("Data/1_experts/dist_matrices/dist_peak_freq_sagar/dist_peak_matrix.csv", row.names = 1)) # peak frequencies
dist_mass <- as.matrix(read.csv2("Data/1_experts/dist_matrices/dist_mass_matrix.csv", row.names = 1)) # body mass
dist_traits_wo_mass <- as.matrix(read.csv2("Data/1_experts/dist_matrices/dist_traits_wo_mass_matrix.csv", row.names = 1)) # Gower distance trait matrix (ecological and morphological traits) without body mass
dist_phylo <- as.matrix(read.csv2("Data/1_experts/dist_matrices/dist_phylogeny.csv", row.names = 1)) 
dist_traits_phylo_corr <- as.matrix(read.csv2("Data/1_experts/dist_matrices/dist_trait_matrix_phylo_corrected.csv", row.names = 1))
dist_peak_phylo_corr <- as.matrix(read.csv2("Data/1_experts/dist_matrices/dist_peak_freq_sagar/dist_peak_matrix_phylo_corrected.csv", row.names = 1))

#--------------
## Filter significant co-occurrence results ##
#--------------

cooccurrence_pairs <- map_dfr(names(results_cooccurrence), function(p) {
  res <- results_cooccurrence[[p]]$results
  if (is.null(res)) return(NULL)
  res %>%
    dplyr::filter(p_lt <= 0.05 | p_gt <= 0.05) %>%
    mutate(plot = p,
           type = ifelse(p_gt <= 0.05, "positive", "negative")) %>%
    mutate(type = as.character(type)) %>%   # ensure consistent type
    dplyr::select(plot, sp1_name, sp2_name, type, p_lt, p_gt)
})

extract_dist <- function(mat, sp1, sp2) {
  rn <- rownames(mat)
  
  if (sp1 %in% rn && sp2 %in% rn) {
    return(mat[sp1, sp2])
  } else {
    return(NA)
  }
}

cooccurrence_pairs$traits_distance <-
  mapply(extract_dist, mat = list(dist_traits),
         sp1 = cooccurrence_pairs$sp1_name,
         sp2 = cooccurrence_pairs$sp2_name)

cooccurrence_pairs$peak_distance <-
  mapply(extract_dist, mat = list(dist_peak),
         sp1 = cooccurrence_pairs$sp1_name,
         sp2 = cooccurrence_pairs$sp2_name)

cooccurrence_pairs$mass_distance <-
  mapply(extract_dist, mat = list(dist_mass),
         sp1 = cooccurrence_pairs$sp1_name,
         sp2 = cooccurrence_pairs$sp2_name)

cooccurrence_pairs$traits_wo_mass_distance <-
  mapply(extract_dist, mat = list(dist_traits_wo_mass),
         sp1 = cooccurrence_pairs$sp1_name,
         sp2 = cooccurrence_pairs$sp2_name)

cooccurrence_pairs$phylo_distance <-
  mapply(extract_dist, mat = list(dist_phylo),
         sp1 = cooccurrence_pairs$sp1_name,
         sp2 = cooccurrence_pairs$sp2_name)

cooccurrence_pairs$trait_distance_phylo_corr <-
  mapply(extract_dist, mat = list(dist_traits_phylo_corr),
         sp1 = cooccurrence_pairs$sp1_name,
         sp2 = cooccurrence_pairs$sp2_name)

cooccurrence_pairs$peak_distance_phylo_corr <-
  mapply(extract_dist, mat = list(dist_peak_phylo_corr),
         sp1 = cooccurrence_pairs$sp1_name,
         sp2 = cooccurrence_pairs$sp2_name)

write.csv2(cooccurrence_pairs, "Data/1_experts/co-occurrence/cooccurrence_pairs_distances.csv", row.names = FALSE)

