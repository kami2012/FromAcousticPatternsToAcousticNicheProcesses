rm(list=ls(all=TRUE))
Sys.setenv(LANG = "en")

library(ggplot2)


## ============================================================================================================ ##
## Plot pairwise distances between co-occurring species based on phylogeny and trait data 


#--------------
## Load data ##
#--------------

cooccurrence_pairs <- read.csv2("Data/Co-occurrence/cooccurrence_pairs_distances.csv")

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

# Peak-frequency distance
p1 <- ggplot(cooccurrence_pairs_categories, aes(x = Label, y = peak_distance, fill = type)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.8) +
  #geom_vline(xintercept = 3.5, color = "black", linewidth = 0.6) +  # optional
  labs(
    title = "",
    x = "Recovery Gradient", 
    y = "Peak-Frequency Distance", 
    fill = "Temporal Association Type"
  ) +
  
  scale_fill_manual(values = c(
    "positive" = "#1b9e77",
    "negative" = "#d95f02",
    "random" = "grey70" # Neutral baseline
  ), 
  labels = c(
    "positive" = "Positive",
    "negative" = "Negative",
    "random" = "Non-significant"
  )) +
  
  theme_bw() +
  theme(
    axis.title = element_text(size = 14), 
    axis.text = element_text(size = 12, color = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
    legend.position = c(0.98, 0.98), # x=0.95 (far right), y=0.95 (top)
    legend.justification = c("right", "top"), # Anchors the corner of the legend box
    legend.background = element_rect(fill = alpha("white", 0.5)), # Semi-transparent background)
    legend.title = element_text(size = 12), 
    legend.text = element_text(size = 10)
  )

ggsave("Plots/Distances/peak_frequency_distances.png", plot = p1, width = 10, height = 6, dpi = 300)


# Peak frequency distance (phylo and mass corrected)
p2 <- ggplot(cooccurrence_pairs_categories, aes(x = Label, y = peak_distance_phylo_mass_corr, fill = type)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.8) +
  #geom_vline(xintercept = 3.5, color = "black", linewidth = 0.6) +  # optional
  labs(
    title = "",
    x = "Recovery Gradient", 
    y = "Adjusted Peak-Frequency Distance", 
    fill = "Temporal Association Type"
  ) +
  
  scale_fill_manual(values = c(
    "positive" = "#1b9e77",
    "negative" = "#d95f02",
    "random" = "grey70" # Neutral baseline
  ), 
  labels = c(
    "positive" = "Positive",
    "negative" = "Negative",
    "random" = "Non-significant"
  )) +
  
  theme_bw() +
  theme(
    axis.title = element_text(size = 14), 
    axis.text = element_text(size = 12, color = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
    legend.position = c(0.98, 0.98), # x=0.95 (far right), y=0.95 (top)
    legend.justification = c("right", "top"), # Anchors the corner of the legend box
    legend.background = element_rect(fill = alpha("white", 0.5)), # Semi-transparent background)
    legend.title = element_text(size = 12), 
    legend.text = element_text(size = 10)
  )

ggsave("Plots/Distances/adjusted_peak_frequency_distances.png", plot = p2, width = 10, height = 6, dpi = 300)


# Traits without body-mass distance
p3 <- ggplot(cooccurrence_pairs_categories, aes(x = Label, y = traits_wo_mass_distance, fill = type)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.8) +
  #geom_vline(xintercept = 3.5, color = "black", linewidth = 0.6) +  # optional
  labs(
    title = "",
    x = "Recovery Gradient", 
    y = "Ecological–Morphological Trait Distance", 
    fill = "Temporal Association Type"
  ) +
  
  scale_fill_manual(values = c(
    "positive" = "#1b9e77",
    "negative" = "#d95f02",
    "random" = "grey70" # Neutral baseline
  ), 
  labels = c(
    "positive" = "Positive",
    "negative" = "Negative",
    "random" = "Non-significant"
  )) +
  
  theme_bw() +
  theme(
    axis.title = element_text(size = 14), 
    axis.text = element_text(size = 12, color = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
    legend.position = c(0.98, 0.98), # x=0.95 (far right), y=0.95 (top)
    legend.justification = c("right", "top"), # Anchors the corner of the legend box
    legend.background = element_rect(fill = alpha("white", 0.5)), # Semi-transparent background)
    legend.title = element_text(size = 12), 
    legend.text = element_text(size = 10)
  )

ggsave("Plots/Distances/ecological–morphological_trait_distances.png", plot = p3, width = 10, height = 6, dpi = 300)









