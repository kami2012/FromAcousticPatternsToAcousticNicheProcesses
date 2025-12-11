rm(list=ls(all=TRUE))
Sys.setenv(LANG = "en")

# Working directory
setwd("E:/Manuscripts/4_co-occurence") 

library(ggplot2)
library(purrr)
library(dplyr)

## ============================================================================================================ ##
## Plot probabilistic co-occurrence results 


#--------------
## Load data ##
#--------------

# Cooccurrence results
load("Data/1_experts/co-occurrence/cooccurrence_results.RData")

# Recovery gradient categories
plot_categories <- read.csv("Data/plot_categories.csv", header=T, check.names = F)
plot_categories <- plot_categories[, 1:7]

#--------------
## Preprocess and aggregate co-occurrence counts ##
#--------------

# Use map_dfr to iterate over all plots in results_cooccurrence and bind the results by row.
cooccurrence <- map_dfr(names(results_cooccurrence), function(plot) {
  res <- results_cooccurrence[[plot]]
  
  if (is.null(res)) {
    return(data.frame(
      plot = plot,
      species = NA,
      pairs = NA,
      positive = NA,
      negative = NA,
      random = NA,
      unclassifiable = NA,
      stringsAsFactors = FALSE
    ))
  }
  
  counts <- data.frame(
    plot = plot,
    species = res$species,
    pairs = res$pairs,
    positive = res$positive,
    negative = res$negative,
    random = res$random,
    unclassifiable = res$unclassifiable,
    stringsAsFactors = FALSE
  )
  
  return(counts)
})

write.csv2(cooccurrence, "Data/1_experts/co-occurrence/cooccurrence_summary.csv", row.names = FALSE)


#--------------
## Compute portions ##
#--------------

cooccurrence_portions <- cooccurrence %>%
  mutate(
    classified_pairs = positive + negative + random,
    
    Positive_portion = positive / classified_pairs,
    
    Negative_portion = negative / classified_pairs) %>%
  rename(Species = species)

#--------------
## Merge with gradient metadata ##
#--------------

cooccurrence_gradient <- merge(
  cooccurrence_portions,
  plot_categories[, c("Plot_ID", "Label")], # Select only Plot_ID (merge key) and Label (gradient category)  
  by.x = "plot",                            # Key in cooccurrence data
  by.y = "Plot_ID",                         # Key in plot_categories data
  all.x = TRUE                              # Keep all rows from cooccurrence data
)

# Convert the 'Label' column to an ordered factor to ensure plots are displayed 
# in the correct sequence reflecting the recovery gradient in subsequent plots/analyses.
cooccurrence_gradient$Label <- factor(
  cooccurrence_gradient$Label,
  levels = c("A_P", "A_C", "F_P", 
             "F_C", "F_P_RegI", "F_C_RegI", "A_Old",
             "F_P_RegII", "F_C_RegII", "F_Old"
  )
)

# Sort by category label 
cooccurrence_gradient <- cooccurrence_gradient[order(cooccurrence_gradient$Label), ]


# Convert to long format
df_long <- cooccurrence_gradient %>%
  pivot_longer(cols = c("Positive_portion", "Negative_portion"), 
               names_to = "Type", values_to = "Portion")

# Boxplot by Type
p <- ggplot(df_long, aes(x = Label, y = Portion, fill = Type)) +
  geom_boxplot(outlier.shape = 1, alpha = 0.8) +
  facet_wrap(~ Type, scales = "fixed") +
  scale_y_continuous(
    breaks = seq(0.0, 0.15, by = 0.025),
    name = "Proportion of Non-Randomly Associated Pairs"
  ) +
  # Begrenzung der Ansicht (Ansicht beschneiden, Daten behalten)
  coord_cartesian(ylim = c(0.0, 0.15)) +
  labs(
    title = "",
    x = "Recovery Gradient", 
    fill = "Co-occurrence Type") +
  scale_fill_manual(values = c(
    "Positive_portion" = "#1b9e77",
    "Negative_portion" = "#d95f02"
  )) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save as PNG
ggsave("Plots/1_experts/co-occurrences_proportion_boxplot.png", plot = p, width = 10, height = 6, dpi = 300)


wilcox.test(cooccurrence_gradient$Positive_portion, 
            cooccurrence_gradient$Negative_portion, 
            paired = TRUE)

#Wilcoxon signed rank test with continuity correction

#data:  cooccurrence_gradient$Positive_portion and cooccurrence_gradient$Negative_portion
#V = 2400, p-value = 1.026e-12
#alternative hypothesis: true location shift is not equal to 0

cooccurrence_gradient$Difference <- cooccurrence_gradient$Positive_portion - cooccurrence_gradient$Negative_portion

hist(cooccurrence_gradient$Difference)

shapiro.test(cooccurrence_gradient$Difference)