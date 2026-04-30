rm(list=ls(all=TRUE))
Sys.setenv(LANG = "en")

library(ggplot2)
library(dplyr)

## ============================================================================================================ ##
## Plot the results of testing the Acoustic Adaptation Hypothesis (AAH) 


#--------------
## Load data ##
#--------------

dat <- read.csv2("Data/Testing_AAH/mean_peak_frequency_per_plot_mass_corrected_gradient_succession.csv")


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

# Perform Linear Regression (lm) on Mean Peak Frequency 
# against numerical Succession Time, using only the non-Old-growth plots.
model_mean_succ_plot <- lm (mean_freq_UNWEIGHTED ~ Succession, data = dat_succession)
summary_mean_succ_plot <- summary(model_mean_succ_plot)



#--------------
## Plot data ##
#--------------

# 1. Calculate stats
r_sq_sd <- round(summary_sd_succ_plot$r.squared, 3)
p_val_sd <- round(summary_sd_succ_plot$coefficients[2, 4], 4)

# 2. Build the Plot
plot_final_sd <- ggplot(dat_succession, aes(x = Succession, y = sd_freq_UNWEIGHTED)) +
  
  # A) Helping Lines
  geom_vline(xintercept = 38, linetype = "dotted", color = "grey80") +
  
  # B) Regression Line
  geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 1, fill = "grey70", alpha = 0.2) +
  
  # C) Succession Data Points
  geom_point(aes(color = Succession), size = 2.5, alpha = 0.7) +
  scale_color_gradient(low = "gold", high = "forestgreen") +
  
  # D) Old-growth Boxplot (Empty)
  geom_boxplot(data = dat_old_growth, 
               aes(x = 50, y = sd_freq_UNWEIGHTED),
               width = 5, fill = NA, color = "black",
               linewidth = 0.8, outlier.shape = NA) +
  
  # E) Old-growth Jittered Dots
  geom_jitter(data = dat_old_growth,
              aes(x = 50, y = sd_freq_UNWEIGHTED),
              color = "darkgreen", width = 1.5, size = 2.5, alpha = 0.6) +
  
  # F) Scales & Labels
  scale_x_continuous(
    breaks = c(0, 10, 20, 30, 40, 50),
    labels = c("0\nAgriculture", "10\n ", "20\n", "30\n ", "40\n ", "\nOld-growth"),
    limits = c(-1, 55)
  ) +
  labs(
    x = "Succession Time (Years)",
    y = "SD of Mass-corrected Peak Frequency",
    subtitle = NULL
  ) +
  
  # G) Theme and Styling
  theme_classic() +
  theme(
    axis.title = element_text(size = 16), 
    axis.text = element_text(size = 14, color = "black"),
    axis.text.x = element_text(lineheight = 0.8),
    axis.title.x = element_text(margin = margin(t = 15)),
    legend.position = "none"
  ) +
  
  # H) Annotation
  annotate(
    "text", 
    x = 0, 
    y = min(dat_succession$sd_freq_UNWEIGHTED, na.rm = TRUE) + 0.005, 
    label = paste0("'Linear Regression (excluding Old-growth):' ~ R^2 == ", r_sq_sd, " ~~~ p == ", p_val_sd),
    parse = TRUE, hjust = 0, vjust = 0, size = 5, fontface = "italic"
  )

# 3. Print
print(plot_final_sd)

# 4. Save
ggsave("Plots/Testing_AAH/acoustiv_diversity_vs_succession_time_mass_corrected.png", plot = plot_final_sd, width = 10, height = 6, dpi = 300)



# 1. Calculate stats
r_sq_mean <- round(summary_mean_succ_plot$r.squared, 3)
p_val_mean <- round(summary_mean_succ_plot$coefficients[2, 4], 4)

# 2. Build the Plot
plot_final_mean <- ggplot(dat_succession, aes(x = Succession, y = mean_freq_UNWEIGHTED)) +
  
  # A) Helping Lines
  geom_vline(xintercept = 38, linetype = "dotted", color = "grey80") +
  
  # B) Regression Line
  geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 1, fill = "grey70", alpha = 0.2, linetype = "dashed") +
  
  # C) Succession Data Points
  geom_point(aes(color = Succession), size = 2.5, alpha = 0.7) +
  scale_color_gradient(low = "gold", high = "forestgreen") +
  
  # D) Old-growth Boxplot (Empty)
  geom_boxplot(data = dat_old_growth, 
               aes(x = 50, y = mean_freq_UNWEIGHTED),
               width = 5, fill = NA, color = "black",
               linewidth = 0.8, outlier.shape = NA) +
  
  # E) Old-growth Jittered Dots
  geom_jitter(data = dat_old_growth,
              aes(x = 50, y = mean_freq_UNWEIGHTED),
              color = "darkgreen", width = 1.5, size = 2.5, alpha = 0.6) +
  
  # F) Scales & Labels
  scale_x_continuous(
    breaks = c(0, 10, 20, 30, 40, 50),
    labels = c("0\nAgriculture", "10\n ", "20\n", "30\n ", "40\n ", "\nOld-growth"),
    limits = c(-1, 55)
  ) +
  labs(
    x = "Succession Time (Years)",
    y = "Mean Peak Frequency (Mass-corrected residuals)",
    subtitle = NULL
  ) +
  
  # G) Theme and Styling
  theme_classic() +
  theme(
    axis.title = element_text(size = 16), 
    axis.text = element_text(size = 14, color = "black"),
    axis.text.x = element_text(lineheight = 0.8),
    axis.title.x = element_text(margin = margin(t = 15)),
    legend.position = "none"
  ) +
  
  # H) Annotation
  annotate(
    "text", 
    x = 0, 
    y = min(dat_succession$mean_freq_UNWEIGHTED, na.rm = TRUE) + 0.005, 
    label = paste0("'Linear Regression (excluding Old-growth):' ~ R^2 == ", r_sq_mean, " ~~~ p == ", p_val_mean),
    parse = TRUE, hjust = 0, vjust = 0, size = 5, fontface = "italic"
  )

# 3. Print
print(plot_final_mean)

ggsave("Plots/Testing_AAH/peak_frequency_vs_succession_time_mass_corrected.png", plot = plot_final_mean, width = 10, height = 6, dpi = 300)
