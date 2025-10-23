rm(list=ls(all=TRUE))
Sys.setenv(LANG = "en")

# Working directory
setwd("E:/Manuscripts/4_co-occurence") 

library(forcats)
library(ggplot2)
library(tidyr)

## ============================================================================================================ ##
## Evaluate and plot probabilistic co-occurrence analysis 


#--------------
## Load data ##
#--------------

summary_df <- read.table("Data/2_rfc/threshold_05/results_cooccurrence_summary.csv", header=T, check.names = F,sep=";")

categories <- read.csv("Data/categories_nmds1.csv", header=T, check.names = F)
categories <- categories[, 1:7]


#--------------
## Add categories ##
#--------------

summary_df_categories <- merge(
  summary_df,
  categories[, c("Plot_ID", "Label")],   
  by.x = "Plot",                         
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
summary_df_categories$Label <- factor(summary_df_categories$Label, levels = categories_order)

# Sort by Category Label 
summary_df_categories <- summary_df_categories[order(summary_df_categories$Label), ]


# long format
df_long <- summary_df_categories %>%
  pivot_longer(cols = c("Species", "Positive", "Negative"), 
               names_to = "Type", values_to = "Count")

# Boxplot by Type
p <- ggplot(df_long, aes(x = Label, y = Count, fill = Type)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.8) +
  facet_wrap(~ Type, scales = "free_y") +
  geom_vline(xintercept = 3.5, color = "black", linewidth = 0.6) +
  labs(
    title = "Co-occurrence based on RFCx Threshold 0.5",
    x = "Plot Category", 
    y = "Count", 
    fill = "Type") +
  scale_fill_manual(values = c(
    "Species" = "#D3D3D3",
    "Positive" = "#1b9e77",
    "Negative" = "#d95f02"
  )) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save as PNG
ggsave("Plots/2_rfc/threshold_05/co-occurrences_boxplot.png", plot = p, width = 10, height = 6, dpi = 300)
