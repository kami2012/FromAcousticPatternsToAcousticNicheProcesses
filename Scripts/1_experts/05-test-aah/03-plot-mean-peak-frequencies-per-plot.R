rm(list=ls(all=TRUE))
Sys.setenv(LANG = "en")

# Working directory
setwd("E:/Manuscripts/4_co-occurence") 

library(ggplot2)
library(dplyr)

## ============================================================================================================ ##
## Plot the results of testing the Acoustic Adaptation Hypothesis (AAH) 


#--------------
## Load data ##
#--------------

dat <- read.csv2("Data/1_experts/mean_peak_frequency_per_plot_mass_corrected_gradient_succession.csv")


#--------------
## Preprocess data ##
#--------------

# Filter Reassembly plots
dat <- dat %>%
  dplyr::filter(grepl("^F_", Label))


# Filter Old-growth plots for separate visualization (boxplot)
dat_old_growth <- dat %>%
  filter(Label == "F_Old")

# Filter all other plots (excluding F_Old) for linear regression
dat_succession <- dat %>%
  filter(Label != "F_Old")

# Perform Linear Regression (lm) on Acoustic Diversity (SD Peak Freq) 
# against numerical Succession Time, using only the non-Old-growth plots.
model_sd_succ_plot <- lm (sd_freq_UNWEIGHTED ~ Succession, data = dat_succession)
summary_sd_succ_plot <- summary(model_sd_succ_plot)


model_mean_succ_plot <- lm (mean_freq_UNWEIGHTED ~ Succession, data = dat_succession)
summary_mean_succ_plot <- summary(model_mean_succ_plot)



#--------------
## Plot data ##
#--------------

# Initialize ggplot object: Map Succession Time (x) to Acoustic Diversity (y)
plot_aah_sd <- ggplot(dat_succession, aes(x = Succession, y = sd_freq_UNWEIGHTED)) +
  
  # A) Scatterplot (Succession plots)
  geom_point(aes(color = Succession), size = 2, alpha = 0.7) +
  
  # B) Regression line and confidence interval
  geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 1, fill = "grey70", alpha = 0.2) +
  
  # Define the gradual color scale for the scatterplot points
  scale_color_gradient(
    low = "gold",        
    high = "forestgreen",
    name = "Succession Time"
  ) +
  
  # C) Boxplot of the old-growth plots
  geom_boxplot(data = dat_old_growth, 
               aes(x = max(dat_succession$Succession, na.rm = TRUE) + 5, 
                   y = sd_freq_UNWEIGHTED, 
                   group = Label),
               width = 5, fill = "darkgreen") +
  
  # D) Labels and titles
  labs(
    x = "Succession Time (Years)",
    y = "Acoustic Diversity (SD of Mass-corrected Peak Freq.)",
    title = "Acoustic Diversity (Mass-corrected) vs. Succession Time",
    subtitle = paste0("Linear Regression (excluding F_Old): R2=", 
                      round(summary_sd_succ_plot$r.squared, 3), 
                      ", p=", 
                      round(summary_sd_succ_plot$coefficients[2, 4], 4))
  ) +
  
  # E) Adapt x-axis scaling
  xlim(min(dat$Succession, na.rm = TRUE), max(dat_succession$Succession, na.rm = TRUE) + 10) +
  
  # Set base theme
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

#print(plot_aah_sd)

plot_final_sd <- plot_aah_sd + theme_classic()

ggsave("Plots/1_experts/aah/acoustiv_diversity_vs_succession_time_mass_corrected.png", plot = plot_final_sd, width = 10, height = 6, dpi = 300)



# Initialize ggplot object: Map Succession Time (x) to Acoustic Diversity (y)
plot_aah_mean <- ggplot(dat_succession, aes(x = Succession, y = mean_freq_UNWEIGHTED)) +
  
  # A) Scatterplot (Succession plots)
  geom_point(aes(color = Succession), size = 2, alpha = 0.7) +
  
  # B) Regression line and confidence interval
  geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 1, fill = "grey70", alpha = 0.2) +
  
  # Define the gradual color scale for the scatterplot points
  scale_color_gradient(
    low = "gold",        
    high = "forestgreen",
    name = "Succession Time"
  ) +
  
  # C) Boxplot of the old-growth plots
  geom_boxplot(data = dat_old_growth, 
               aes(x = max(dat_succession$Succession, na.rm = TRUE) + 5, 
                   y = mean_freq_UNWEIGHTED, 
                   group = Label),
               width = 5, fill = "darkgreen") +
  
  # D) Labels and titles
  labs(
    x = "Succession Time (Years)",
    y = "Mean Peak Freq. (Mass-corrected)",
    title = "Peak Frequency (Mass-corrected) vs. Succession Time",
    subtitle = paste0("Linear Regression (excluding F_Old): R2=", 
                      round(summary_mean_succ_plot$r.squared, 3), 
                      ", p=", 
                      round(summary_mean_succ_plot$coefficients[2, 4], 4))
  ) +
  
  # E) Adapt x-axis scaling
  xlim(min(dat$Succession, na.rm = TRUE), max(dat_succession$Succession, na.rm = TRUE) + 10) +
  
  # Set base theme
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

#print(plot_aah_sd)

plot_final_mean <- plot_aah_mean + theme_classic()

ggsave("Plots/1_experts/aah/peak_frequency_vs_succession_time_mass_corrected.png", plot = plot_final_mean, width = 10, height = 6, dpi = 300)
