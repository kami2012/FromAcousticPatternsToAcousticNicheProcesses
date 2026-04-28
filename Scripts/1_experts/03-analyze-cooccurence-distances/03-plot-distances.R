rm(list=ls(all=TRUE))
Sys.setenv(LANG = "en")

# Working directory
setwd("E:/Manuscripts/4_co-occurence") 


## ============================================================================================================ ##
## Plot pairwise distances between co-occurring species based on phylogeny and trait data 


#--------------
## Load data ##
#--------------

cooccurrence_pairs <- read.csv2("Data/1_experts/co-occurrence/cooccurrence_pairs_distances.csv")

# Recovery gradient categories
plot_categories <- read.csv("Data/plot_categories.csv", header=T, check.names = F)
plot_categories <- plot_categories[, 1:7]

cooccurrence_pairs_categories <- merge(
  cooccurrence_pairs,
  plot_categories[, c("Plot_ID", "Label")],   
  by.x = "plot",                         
  by.y = "Plot_ID",                      
  all.x = TRUE                           
)

# Set factor levels of 'Label' to reflect the recovery gradient ##
cooccurrence_pairs_categories$Label <- factor(
    cooccurrence_pairs_categories$Label,
    levels = c("A_P", "A_C", "F_P", 
               "F_C", "F_P_RegI", "F_C_RegI", "A_Old",
               "F_P_RegII", "F_C_RegII", "F_Old"
               )
)

#--------------
## Plot distances (Boxplot) ##
#--------------

# Ecological and morphological traits distance
p1 <- ggplot(cooccurrence_pairs_categories, aes(x = Label, y = traits_distance, fill = type)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.8) +
  #geom_vline(xintercept = 3.5, color = "black", linewidth = 0.6) +  # optional
  labs(
    title = "",
    x = "Recovery Gradient", 
    y = "Ecological and Morphological Traits Distance", 
    fill = "Co-occurrence Type"
  ) +
  scale_fill_manual(values = c(
    "positive" = "#1b9e77",
    "negative" = "#d95f02"
  )) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("Plots/1_experts/distances_gradient_boxplot_sagar/trait_distances_boxplot.png", plot = p1, width = 10, height = 6, dpi = 300)

# Peak frequency distance
p2 <- ggplot(cooccurrence_pairs_categories, aes(x = Label, y = peak_distance, fill = type)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.8) +
  #geom_vline(xintercept = 3.5, color = "black", linewidth = 0.6) +  # optional
  labs(
    title = "",
    x = "Recovery Gradient", 
    y = "Peak Frequency Distance", 
    fill = "Co-occurrence Type"
  ) +
  scale_fill_manual(values = c(
    "positive" = "#1b9e77",
    "negative" = "#d95f02"
  )) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("Plots/1_experts/distances_gradient_boxplot_sagar/peak_distances_boxplot.png", plot = p2, width = 10, height = 6, dpi = 300)

# Body mass distance
p3 <- ggplot(cooccurrence_pairs_categories, aes(x = Label, y = mass_distance, fill = type)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.8) +
  #geom_vline(xintercept = 3.5, color = "black", linewidth = 0.6) +  # optional
  labs(
    title = "",
    x = "Recovery Gradient", 
    y = "Body Mass Distance", 
    fill = "Co-occurrence Type"
  ) +
  scale_fill_manual(values = c(
    "positive" = "#1b9e77",
    "negative" = "#d95f02"
  )) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("Plots/1_experts/distances_gradient_boxplot_sagar/mass_distances_boxplot.png", plot = p3, width = 10, height = 6, dpi = 300)

# Traits without body mass distance
p4 <- ggplot(cooccurrence_pairs_categories, aes(x = Label, y = traits_wo_mass_distance, fill = type)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.8) +
  #geom_vline(xintercept = 3.5, color = "black", linewidth = 0.6) +  # optional
  labs(
    title = "",
    x = "Recovery Gradient", 
    y = "Traits without Boody Mass Distance", 
    fill = "Co-occurrence Type"
  ) +
  scale_fill_manual(values = c(
    "positive" = "#1b9e77",
    "negative" = "#d95f02"
  )) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("Plots/1_experts/distances_gradient_boxplot_sagar/traits_wo_mass_distances_boxplot.png", plot = p4, width = 10, height = 6, dpi = 300)

# Phylogenetic distance
p5 <- ggplot(cooccurrence_pairs_categories, aes(x = Label, y = phylo_distance, fill = type)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.8) +
  #geom_vline(xintercept = 3.5, color = "black", linewidth = 0.6) +  # optional
  labs(
    title = "",
    x = "Recovery Gradient", 
    y = "Phylogenetic Distance", 
    fill = "Co-occurrence Type"
  ) +
  scale_fill_manual(values = c(
    "positive" = "#1b9e77",
    "negative" = "#d95f02"
  )) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("Plots/1_experts/distances_gradient_boxplot_sagar/phylo_distances_boxplot.png", plot = p5, width = 10, height = 6, dpi = 300)

# Peak frequency distance (phylo corrected)
p6 <- ggplot(cooccurrence_pairs_categories, aes(x = Label, y = peak_distance_phylo_corr, fill = type)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.8) +
  #geom_vline(xintercept = 3.5, color = "black", linewidth = 0.6) +  # optional
  labs(
    title = "",
    x = "Recovery Gradient", 
    y = "Phylogenetically Corrected Peak Frequency Distance", 
    fill = "Co-occurrence Type"
  ) +
  scale_fill_manual(values = c(
    "positive" = "#1b9e77",
    "negative" = "#d95f02"
  )) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("Plots/1_experts/distances_gradient_boxplot_sagar/peak_frequency_phylo-corr_boxplot.png", plot = p6, width = 10, height = 6, dpi = 300)

# Peak frequency distance (mass corrected)
p7 <- ggplot(cooccurrence_pairs_categories, aes(x = Label, y = peak_distance_mass_corr, fill = type)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.8) +
  #geom_vline(xintercept = 3.5, color = "black", linewidth = 0.6) +  # optional
  labs(
    title = "",
    x = "Recovery Gradient", 
    y = "Body-mass-corrected Peak Frequency Distance", 
    fill = "Co-occurrence Type"
  ) +
  scale_fill_manual(values = c(
    "positive" = "#1b9e77",
    "negative" = "#d95f02"
  )) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("Plots/1_experts/distances_gradient_boxplot_sagar/peak_frequency_mass-corr_boxplot.png", plot = p7, width = 10, height = 6, dpi = 300)

# Peak frequency distance (phylo and mass corrected)
p8 <- ggplot(cooccurrence_pairs_categories, aes(x = Label, y = peak_distance_phylo_mass_corr, fill = type)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.8) +
  #geom_vline(xintercept = 3.5, color = "black", linewidth = 0.6) +  # optional
  labs(
    title = "",
    x = "Recovery Gradient", 
    y = "Phylogenetically and Body-mass-corrected Peak Frequency Distance", 
    fill = "Co-occurrence Type"
  ) +
  scale_fill_manual(values = c(
    "positive" = "#1b9e77",
    "negative" = "#d95f02"
  )) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("Plots/1_experts/distances_gradient_boxplot_sagar/peak_frequency_phylo-mass-corr_boxplot.png", plot = p8, width = 10, height = 6, dpi = 300)

# Traits distance (phylo corrected)
p9 <- ggplot(cooccurrence_pairs_categories, aes(x = Label, y = trait_distance_phylo_corr, fill = type)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.8) +
  #geom_vline(xintercept = 3.5, color = "black", linewidth = 0.6) +  # optional
  labs(
    title = "",
    x = "Recovery Gradient", 
    y = "Phylogenetically Corrected Traits Distance", 
    fill = "Co-occurrence Type"
  ) +
  scale_fill_manual(values = c(
    "positive" = "#1b9e77",
    "negative" = "#d95f02"
  )) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("Plots/1_experts/distances_gradient_boxplot_sagar/traits_phylo-corr_boxplot.png", plot = p9, width = 10, height = 6, dpi = 300)



