rm(list=ls(all=TRUE))
Sys.setenv(LANG = "en")

# Working directory
setwd("E:/Manuscripts/4_co-occurence") 

library(ape)
library(ARTool)
library(dplyr)
library(purrr)

## ============================================================================================================ ##
## Evaluate and plot probabilistic co-occurrence pairs 


#--------------
## Load data ##
#--------------


load("Data/4_birdnet_retrained/threshold_05/results_cooccurence.RData") # cooccurence pairs

load("Data/4_birdnet_retrained/threshold_05/distM.rda") # Gower distance trait matrix
rownames(distM) <- gsub(" ", "_", rownames(distM))
colnames(distM) <- gsub(" ", "_", colnames(distM))

load("Data/4_birdnet_retrained/threshold_05/bird_tree.rda") # Phylogenetic Tree
#"Tolmomyias_assimilis" %in% tr$tip.label => has to be fixed
#[1] FALSE

plot_categories <- read.csv("Data/plot_categories.csv", header=T, check.names = F)
plot_categories <- plot_categories[, 1:7]

#--------------
## Filter significant cooccurence results ##
#--------------

cooccurence_pairs <- map_dfr(names(results_cooccurence), function(p) {
  res <- results_cooccurence[[p]]$results
  if (is.null(res)) return(NULL)
  res %>%
    filter(p_lt <= 0.05 | p_gt <= 0.05) %>%
    mutate(plot = p,
           type = ifelse(p_gt <= 0.05, "positive", "negative")) %>%
    mutate(type = as.character(type)) %>%   # ensure consistent type
    select(plot, sp1_name, sp2_name, type, p_lt, p_gt)
})


#--------------
## Extract trait distances for each species pair ##
#--------------

cooccurence_pairs$trait_distance <- mapply(function(sp1, sp2) {
  if (sp1 %in% rownames(distM) && sp2 %in% colnames(distM)) {
    distM[sp1, sp2]
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
  "A_Old",
  "F_P",
  "F_C",
  "F_P_RegI",
  "F_C_RegI",
  "F_P_RegII",
  "F_C_RegII",
  "F_Old"
)

# New Column Category Label as factor
cooccurence_pairs_categories$Label <- factor(cooccurence_pairs_categories$Label, levels = categories_order)

# Sort by Category Label 
cooccurence_pairs_categories <- cooccurence_pairs_categories[order(cooccurence_pairs_categories$Label), ]


write.csv(cooccurence_pairs_categories, "Data/4_birdnet_retrained/threshold_05/cooccurence_pairs.csv", row.names = FALSE)

#--------------
## Plot distances ##
#--------------

cooccurence_pairs_categories <- read.csv("Data/4_birdnet_retrained/threshold_05/cooccurence_pairs.csv", header=T, check.names = F)

# Trait distance
p1 <- ggplot(cooccurence_pairs_categories, aes(x = Label, y = trait_distance, fill = type)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.8) +
  geom_vline(xintercept = 3.5, color = "black", linewidth = 0.6) +  # optional
  labs(
    title = "Trait Distances based on retrained BirdNET Threshold 0.5",
    x = "Plot Category", 
    y = "Trait Distance", 
    fill = "Co-occurrence type"
  ) +
  scale_fill_manual(values = c(
    "positive" = "#1b9e77",
    "negative" = "#d95f02"
  )) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save as PNG
ggsave("Plots/4_birdnet_retrained/threshold_05/trait_distances_boxplot.png", plot = p1, width = 10, height = 6, dpi = 300)

# Phylogenetic distance
p2 <- ggplot(cooccurence_pairs_categories, aes(x = Label, y = phylo_distance, fill = type)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.8) +
  geom_vline(xintercept = 3.5, color = "black", linewidth = 0.6) +  # optional
  labs(
    title = "Phylogenetic Distances based on retrained BirdNET Threshold 0.5",
    x = "Plot Category", 
    y = "Phylogenetic Distance", 
    fill = "Co-occurrence type"
  ) +
  scale_fill_manual(values = c(
    "positive" = "#1b9e77",
    "negative" = "#d95f02"
  )) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save as PNG
ggsave("Plots/4_birdnet_retrained/threshold_05/phylo_distances_boxplot.png", plot = p2, width = 10, height = 6, dpi = 300)


#--------------
## Significance tests ##
#--------------


cooccurence_pairs_categories %>%
  group_by(Label) %>%
  summarise(p_value = wilcox.test(trait_distance ~ type)$p.value)

cooccurence_pairs_categories %>%
  group_by(Label) %>%
  summarise(p_value = wilcox.test(phylo_distance ~ type)$p.value)

# Example using trait distances
# Ensure columns: trait_distance, type, Label
cooccurence_pairs_categories$type <- factor(cooccurence_pairs_categories$type, levels = c("positive", "negative"))
cooccurence_pairs_categories$Label <- factor(cooccurence_pairs_categories$Label, levels = categories_order)  # your ordered categories



# Keep only Labels NOT starting with "A"
pairs_filtered <- cooccurence_pairs_categories %>%
  filter(!grepl("^A", Label))

# Fit ART model
art_trait <- art(trait_distance ~ type * Label, data = pairs_filtered)

# ANOVA table
anova(art_trait)


art_phylo <- art(phylo_distance ~ type * Label, data = pairs_filtered)
anova(art_phylo)
