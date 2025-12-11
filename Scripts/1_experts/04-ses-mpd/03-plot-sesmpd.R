rm(list=ls(all=TRUE))
Sys.setenv(LANG = "en")

# Working directory
setwd("E:/Manuscripts/4_co-occurence") 

library(ggplot2)

## ============================================================================================================ ##
## Plot Standardized Effect Size of Mean Pairwise Distance (SES-MPD) based on trait data and peak frequencies

#--------------
## Load data ##
#--------------

# SES-MPD results
SES_CORR_POS <- read.csv2("Data/1_experts/ses-mpd/ses-mpd_results_phylo-corrected_positive.csv", row.names = 1)
SES_CORR_NEG <- read.csv2("Data/1_experts/ses-mpd/ses-mpd_results_phylo-corrected_negative.csv", row.names = 1)
SES_UNCORR_POS <- read.csv2("Data/1_experts/ses-mpd/ses-mpd_results_positive.csv",row.names = 1)
SES_UNCORR_NEG <- read.csv2("Data/1_experts/ses-mpd/ses-mpd_results_negative.csv", row.names = 1)

# Recovery gradient categories
plot_categories <- read.csv("Data/plot_categories.csv", header=T, check.names = F)
plot_categories <- plot_categories[, 1:7]

#--------------
## Data Preparation: Transform SES-MPD to Long Format ##
#--------------

# Merge and transform Phylogeny-Corrected Data
df_corr <- SES_CORR_POS %>%
  dplyr::select(plot, SES_MPD_Peak_POS = SES_MPD_Peak, SES_MPD_Trait_POS = SES_MPD_Trait)

# MERGE with NEG data (Full Outer Join to keep all plots)
df_corr <- merge(
  df_corr,
  SES_CORR_NEG %>% dplyr::select(plot, SES_MPD_Peak_NEG = SES_MPD_Peak, SES_MPD_Trait_NEG = SES_MPD_Trait),
  by = "plot",
  all = TRUE 
)

# Merge with plot categories
df_corr <- merge(df_corr, plot_categories[, c("Plot_ID", "Label")], by.x = "plot", by.y = "Plot_ID", all.x = TRUE)

# Pivot to long format for CORR
df_corr_long <- df_corr %>%
  pivot_longer(
    cols = starts_with("SES_MPD"),
    names_to = c("Trait_Type", "Cooccurrence_Type"),
    names_pattern = "SES_MPD_(.*)_(.*)", 
    values_to = "SES_MPD"
  ) %>%
  mutate(
    Phylogeny = "Phylo-Corrected",
    Cooccurrence_Type = factor(Cooccurrence_Type, levels = c("POS", "NEG"), labels = c("Positive", "Negative"))
  )



# Merge and transform NOT-Phylogeny-Corrected Data
df_uncorr <- SES_UNCORR_POS %>%
  dplyr::select(plot, SES_MPD_Peak_POS = SES_MPD_Peak, SES_MPD_Trait_POS = SES_MPD_Trait)

# MERGE with NEG data (Full Outer Join)
df_uncorr <- merge(
  df_uncorr,
  SES_UNCORR_NEG %>% dplyr::select(plot, SES_MPD_Peak_NEG = SES_MPD_Peak, SES_MPD_Trait_NEG = SES_MPD_Trait),
  by = "plot",
  all = TRUE
)

df_uncorr <- merge(df_uncorr, plot_categories[, c("Plot_ID", "Label")], by.x = "plot", by.y = "Plot_ID", all.x = TRUE)

# Pivot to long format for UNCORR
df_uncorr_long <- df_uncorr %>%
  pivot_longer(
    cols = starts_with("SES_MPD"),
    names_to = c("Trait_Type", "Cooccurrence_Type"),
    names_pattern = "SES_MPD_(.*)_(.*)", 
    values_to = "SES_MPD"
  ) %>%
  mutate(
    Phylogeny = "Not Corrected",
    Cooccurrence_Type = factor(Cooccurrence_Type, levels = c("POS", "NEG"), labels = c("Positive", "Negative"))
  )


# Create the overall dataframe and set factor levels
df_all_long <- bind_rows(df_corr_long, df_uncorr_long)

# Set factor levels of 'Label' to reflect the recovery gradient
df_all_long$Label <- factor(
  df_all_long$Label,
  levels = c("A_P", "A_C", "F_P",
             "F_C", "F_P_RegI", "F_C_RegI", "A_Old",
             "F_P_RegII", "F_C_RegII", "F_Old"
  )
)

# Convert Trait_Type labels for cleaner plotting
df_all_long$Trait_Type <- factor(
  df_all_long$Trait_Type,
  levels = c("Peak", "Trait"), 
  labels = c("Peak Frequency Distance", "Non-Vocal Trait Distance")
)


write.csv2(df_all_long, "Data/1_experts/ses-mpd/ses-mpd_results.csv", row.names = FALSE)


# 2.1. Boxplots for Phylogeny-Corrected SES-MPD Results
# Filter: Only corrected data
df_plot1 <- df_all_long %>% filter(Phylogeny == "Phylo-Corrected")

plot_corr <- ggplot(df_plot1, aes(x = Label, y = SES_MPD, fill = Cooccurrence_Type)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.8) +
  facet_wrap(~ Trait_Type, scales = "free_y") + # Separated by Peak and Trait
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") + # Null expectation
  labs(
    title = "Phylogeny-Corrected SES-MPD (Positive vs. Negative Co-occurrence)",
    x = "Plot Category (Recovery Gradient)", 
    y = "SES-MPD (Clustering < 0 | Overdispersion > 0)", 
    fill = "Co-occurrence Type"
  ) +
  scale_fill_manual(values = c(
    "Positive" = "#1b9e77",
    "Negative" = "#d95f02"
  )) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("Plots/1_experts/ses-mpd_boxplot/sesmpd_phylo-corr_boxplot.png", plot = plot_corr, width = 10, height = 6, dpi = 300)

# 2.2. Boxplots for NOT-Phylogeny-Corrected SES-MPD Results
# Filter: Only uncorrected data
df_plot2 <- df_all_long %>% filter(Phylogeny == "Not Corrected")

plot_uncorr <- ggplot(df_plot2, aes(x = Label, y = SES_MPD, fill = Cooccurrence_Type)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.8) +
  facet_wrap(~ Trait_Type, scales = "free_y") + # Separated by Peak and Trait
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") + # Null expectation
  labs(
    title = "Non-Phylogeny-Corrected SES-MPD (Positive vs. Negative Co-occurrence)",
    x = "Plot Category (Recovery Gradient)", 
    y = "SES-MPD (Clustering < 0 | Overdispersion > 0)", 
    fill = "Co-occurrence Type"
  ) +
  scale_fill_manual(values = c(
    "Positive" = "#1b9e77",
    "Negative" = "#d95f02"
  )) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("Plots/1_experts/ses-mpd_boxplot/sesmpd_phylo-uncorr_boxplot.png", plot = plot_uncorr, width = 10, height = 6, dpi = 300)


# Aggregate by Cooccurrence Type and Trait vs. Peak Frequency
plot_aggregated_plots <- ggplot(df_all_long, aes(x = Cooccurrence_Type, y = SES_MPD, fill = Cooccurrence_Type)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.8) +
  facet_grid(Phylogeny ~ Trait_Type, scales = "free_y") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") + # Null expectation
  labs(
    title = "SES-MPD",
    x = "Co-occurrence Type",
    y = "SES-MPD (Clustering < 0 | Overdispersion > 0)",
    fill = "Co-occurrence Type"
  ) +
  scale_fill_manual(values = c(
    "Positive" = "#1b9e77",
    "Negative" = "#d95f02"
  )) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text.y = element_text(angle = 0), # Phylogeny-Label horizontal
    legend.position = "none" 
  )

ggsave("Plots/1_experts/ses-mpd_boxplot/sesmpd_aggregated_plots_4panels.png", plot = plot_aggregated_plots, width = 8, height = 7, dpi = 300)