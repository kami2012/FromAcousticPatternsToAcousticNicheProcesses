rm(list=ls(all=TRUE))
Sys.setenv(LANG = "en")

# Working directory
setwd("E:/Manuscripts/4_co-occurence") 

library(ape)
library(ARTool)
library(purrr)
library(dplyr)

## ============================================================================================================ ##
## Compute pairwise distances between co-occurring species based on phylogeny and trait data


#--------------
## Load data ##
#--------------


load("Data/1_experts/results_cooccurence.RData") # cooccurence pairs

# Phylogenetic Tree
load("Data/1_experts/bird_tree.rda") # Phylogenetic Tree

# Traits
load("Data/1_experts/traits/distM.rda") # Gower distance trait matrix (ecological and morphological traits)
dist_peak <- read.csv2("Data/1_experts/traits/dist_peak_matrix.csv", row.names = 1) # peak frequencies
dist_mass <- read.csv2("Data/1_experts/traits/dist_mass_matrix.csv", row.names = 1) # body mass
dist_traits_wo_mass <- read.csv2("Data/1_experts/traits/traits_wo_mass_matrix.csv", row.names = 1) # Gower distance trait matrix (ecological and morphological traits) without body mass

# Recovery gradient categories
plot_categories <- read.csv("Data/plot_categories.csv", header=T, check.names = F)
plot_categories <- plot_categories[, 1:7]

#--------------
## Filter significant co-occurence results ##
#--------------

cooccurence_pairs <- map_dfr(names(results_cooccurence), function(p) {
  res <- results_cooccurence[[p]]$results
  if (is.null(res)) return(NULL)
  res %>%
    dplyr::filter(p_lt <= 0.05 | p_gt <= 0.05) %>%
    mutate(plot = p,
           type = ifelse(p_gt <= 0.05, "positive", "negative")) %>%
    mutate(type = as.character(type)) %>%   # ensure consistent type
    dplyr::select(plot, sp1_name, sp2_name, type, p_lt, p_gt)
})


#--------------
## Extract ecological and morphological trait distances for each species pair ##
#--------------

cooccurence_pairs$traits_distance <- mapply(function(sp1, sp2) {
  if (sp1 %in% rownames(distM) && sp2 %in% colnames(distM)) {
    distM[sp1, sp2]
  } else {
    NA
  }
}, cooccurence_pairs$sp1_name, cooccurence_pairs$sp2_name)


#--------------
## Extract peak frequency distances for each species pair ##
#--------------

cooccurence_pairs$peak_distance <- mapply(function(sp1, sp2) {
  if (sp1 %in% rownames(dist_peak) && sp2 %in% colnames(dist_peak)) {
    dist_peak[sp1, sp2]
  } else {
    NA
  }
}, cooccurence_pairs$sp1_name, cooccurence_pairs$sp2_name)


#--------------
## Extract body mass distances for each species pair ##
#--------------

cooccurence_pairs$mass_distance <- mapply(function(sp1, sp2) {
  if (sp1 %in% rownames(dist_mass) && sp2 %in% colnames(dist_mass)) {
    dist_mass[sp1, sp2]
  } else {
    NA
  }
}, cooccurence_pairs$sp1_name, cooccurence_pairs$sp2_name)


#--------------
## Extract trait wo body mass distances for each species pair ##
#--------------

cooccurence_pairs$traits_wo_mass_distance <- mapply(function(sp1, sp2) {
  if (sp1 %in% rownames(dist_traits_wo_mass) && sp2 %in% colnames(dist_traits_wo_mass)) {
    dist_traits_wo_mass[sp1, sp2]
  } else {
    NA
  }
}, cooccurence_pairs$sp1_name, cooccurence_pairs$sp2_name)


#--------------
## Extract phylogenetic distances for each species pair ##
#--------------

# Create phylogenetic distances matrix
phylo_dist <- cophenetic(tr)

cooccurence_pairs$phylo_distance <- mapply(function(sp1, sp2) {
  if (sp1 %in% rownames(phylo_dist) && sp2 %in% colnames(phylo_dist)) phylo_dist[sp1, sp2] else NA
}, cooccurence_pairs$sp1_name, cooccurence_pairs$sp2_name)


#--------------
## Compute distances corrected for phylogenetic relatedness ##
#--------------

peak_mod <- lm(peak_distance ~ phylo_distance, cooccurence_pairs)
cooccurence_pairs$peak_res <- residuals(peak_mod)

traits_mod <- lm(traits_distance ~ phylo_distance, cooccurence_pairs)
cooccurence_pairs$traits_res <- residuals(traits_mod)


#--------------
## Merge cooccurence_pairs with plot_categories ##
#--------------

cooccurence_pairs_categories <- merge(
  cooccurence_pairs,
  plot_categories[, c("Plot_ID", "Label")],   
  by.x = "plot",                         
  by.y = "Plot_ID",                      
  all.x = TRUE                           
)

categories_order <- c(
  "A_P",
  "A_C",
  "F_P",
  "F_C",
  "F_P_RegI",
  "F_C_RegI",
  "A_Old",
  "F_P_RegII",
  "F_C_RegII",
  "F_Old"
)


# New Column Category Label as factor
cooccurence_pairs_categories$Label <- factor(cooccurence_pairs_categories$Label, levels = categories_order)

# Sort by Category Label 
cooccurence_pairs_categories <- cooccurence_pairs_categories[order(cooccurence_pairs_categories$Label), ]



write.csv2(cooccurence_pairs_categories, "Data/1_experts/cooccurence_pairs.csv", row.names = FALSE)

