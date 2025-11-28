rm(list=ls(all=TRUE))
Sys.setenv(LANG = "en")

# Working directory
setwd("E:/Manuscripts/4_co-occurence") 


## ============================================================================================================ ##
## Plot pairwise distances between co-occurring species based on phylogeny and trait data 


#--------------
## Load data ##
#--------------

cooccurence_pairs_categories <- read.csv2("Data/1_experts/cooccurence_pairs.csv")


#--------------
## Set factor levels of 'Label' to reflect the recovery gradient ##
#--------------

#cooccurence_pairs_categories$Label <- factor(
#  cooccurence_pairs_categories$Label,
#  levels = c("A_P", "A_C", "A_Old", "F_P", 
#             "F_C", "F_P_RegI", "F_C_RegI", 
#             "F_P_RegII", "F_C_RegII", "F_Old"
#             )
#)

cooccurence_pairs_categories$Label <- factor(
    cooccurence_pairs_categories$Label,
    levels = c("A_P", "A_C", "F_P", 
               "F_C", "F_P_RegI", "F_C_RegI", "A_Old",
               "F_P_RegII", "F_C_RegII", "F_Old"
               )
)

#--------------
## Plot distances (Boxplot) ##
#--------------

# Ecological and morphological traits distance
p1 <- ggplot(cooccurence_pairs_categories, aes(x = Label, y = traits_distance, fill = type)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.8) +
  #geom_vline(xintercept = 3.5, color = "black", linewidth = 0.6) +  # optional
  labs(
    title = "Ecological and Morphological Traits Distances",
    x = "Plot Category", 
    y = "Traits Distance", 
    fill = "Co-occurrence type"
  ) +
  scale_fill_manual(values = c(
    "positive" = "#1b9e77",
    "negative" = "#d95f02"
  )) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("Plots/1_experts/distances_gradient_boxplot/trait_distances_boxplot.png", plot = p1, width = 10, height = 6, dpi = 300)

# Peak frequency distance
p2 <- ggplot(cooccurence_pairs_categories, aes(x = Label, y = peak_distance, fill = type)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.8) +
  #geom_vline(xintercept = 3.5, color = "black", linewidth = 0.6) +  # optional
  labs(
    title = "Peak Frequency Distances",
    x = "Plot Category", 
    y = "Peak Frequency Distance", 
    fill = "Co-occurrence type"
  ) +
  scale_fill_manual(values = c(
    "positive" = "#1b9e77",
    "negative" = "#d95f02"
  )) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("Plots/1_experts/distances_gradient_boxplot/peak_distances_boxplot.png", plot = p2, width = 10, height = 6, dpi = 300)

# Body mass distance
p3 <- ggplot(cooccurence_pairs_categories, aes(x = Label, y = mass_distance, fill = type)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.8) +
  #geom_vline(xintercept = 3.5, color = "black", linewidth = 0.6) +  # optional
  labs(
    title = "Body Mass Distances",
    x = "Plot Category", 
    y = "Boody Mass Distance", 
    fill = "Co-occurrence type"
  ) +
  scale_fill_manual(values = c(
    "positive" = "#1b9e77",
    "negative" = "#d95f02"
  )) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("Plots/1_experts/distances_gradient_boxplot/mass_distances_boxplot.png", plot = p3, width = 10, height = 6, dpi = 300)

# Traits without body mass distance
p3 <- ggplot(cooccurence_pairs_categories, aes(x = Label, y = traits_wo_mass_distance, fill = type)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.8) +
  #geom_vline(xintercept = 3.5, color = "black", linewidth = 0.6) +  # optional
  labs(
    title = "Traits without Body Mass Distances",
    x = "Plot Category", 
    y = "Traits without Boody Mass Distance", 
    fill = "Co-occurrence type"
  ) +
  scale_fill_manual(values = c(
    "positive" = "#1b9e77",
    "negative" = "#d95f02"
  )) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("Plots/1_experts/distances_gradient_boxplot/traits_wo_mass_distances_boxplot.png", plot = p3, width = 10, height = 6, dpi = 300)

# Phylogenetic distance
p4 <- ggplot(cooccurence_pairs_categories, aes(x = Label, y = phylo_distance, fill = type)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.8) +
  #geom_vline(xintercept = 3.5, color = "black", linewidth = 0.6) +  # optional
  labs(
    title = "Phylogenetic Distances",
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

ggsave("Plots/1_experts/distances_gradient_boxplot/phylo_distances_boxplot.png", plot = p4, width = 10, height = 6, dpi = 300)

# Peak frequency distance (residuals)
p5 <- ggplot(cooccurence_pairs_categories, aes(x = Label, y = peak_res, fill = type)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.8) +
  #geom_vline(xintercept = 3.5, color = "black", linewidth = 0.6) +  # optional
  labs(
    title = "Phylogenetically Corrected Peak Frequency Distance",
    x = "Plot Category", 
    y = "Residual Peak Frequency Distance", 
    fill = "Co-occurrence type"
  ) +
  scale_fill_manual(values = c(
    "positive" = "#1b9e77",
    "negative" = "#d95f02"
  )) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("Plots/1_experts/distances_gradient_boxplot/peak_frequency_residuals_boxplot.png", plot = p5, width = 10, height = 6, dpi = 300)

# Traits distance (residuals)
p6 <- ggplot(cooccurence_pairs_categories, aes(x = Label, y = traits_res, fill = type)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.8) +
  #geom_vline(xintercept = 3.5, color = "black", linewidth = 0.6) +  # optional
  labs(
    title = "Phylogenetically Corrected Traits Distance",
    x = "Plot Category", 
    y = "Residual Traits Distance", 
    fill = "Co-occurrence type"
  ) +
  scale_fill_manual(values = c(
    "positive" = "#1b9e77",
    "negative" = "#d95f02"
  )) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("Plots/1_experts/distances_gradient_boxplot/traits_residuals_boxplot.png", plot = p6, width = 10, height = 6, dpi = 300)



