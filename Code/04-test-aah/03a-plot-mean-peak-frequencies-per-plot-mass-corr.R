rm(list=ls(all=TRUE))
Sys.setenv(LANG = "en")

library(ggplot2)
library(dplyr)

## ============================================================================================================ ##
## Plot the results of testing the Acoustic Adaptation Hypothesis (AAH) 


#--------------
## Load data ##
#--------------

dat <- read.csv2("Data/Testing_AAH/mean_peak_frequency_per_plot_mass_corrected_tls.csv")


#--------------
## Preprocess data ##
#--------------

# Perform Linear Regression (lm) on Acoustic Diversity (SD Peak Freq) 
# against SSCI, using only the non-Old-growth plots.
model_sd_ssci <- lm (sd_freq_UNWEIGHTED ~ ssci, data = dat)
summary_sd_ssci <- summary(model_sd_ssci)

# Perform Linear Regression (lm) on Mean Peak Frequency 
# against SSCI, using only the non-Old-growth plots.
model_mean_ssci <- lm (mean_freq_UNWEIGHTED ~ ssci, data = dat)
summary_mean_ssci <- summary(model_mean_ssci)



#--------------
## Plot data ##
#--------------

r_sq_sd <- round(summary_sd_ssci$r.squared, 3)
p_val_sd <- round(summary_sd_ssci$coefficients["ssci", "Pr(>|t|)"], 3)

plot_sd_ssci <- ggplot(dat, aes(x = ssci, y = sd_freq_UNWEIGHTED)) +
  geom_smooth(method = "lm", se = TRUE,
              color = "black", linewidth = 1,
              fill = "grey70", alpha = 0.2) +
  geom_point(color = "black", size = 2.5, alpha = 1) +
  labs(
    x = "Forest Structural Complexity (SSCI)",
    y = "SD of Body-Mass-Adjusted Peak-Frequency Residuals"
  ) +
  theme_classic() +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14, color = "black"),
    axis.title.x = element_text(margin = margin(t = 15)),
    legend.position = "none"
  ) +
  annotate(
    "text",
    x = min(dat$ssci, na.rm = TRUE),
    y = max(dat$sd_freq_UNWEIGHTED, na.rm = TRUE),
    label = paste0("R^2 == ", r_sq_sd, " * ',' ~ p == ", p_val_sd),
    parse = TRUE,
    hjust = 0,
    vjust = 0,
    size = 5,
    fontface = "italic"
  )

print(plot_sd_ssci)

ggsave(
  "Plots/Testing_AAH/sd_peak_frequency_vs_ssci_mass_adjusted.png",
  plot = plot_sd_ssci,
  width = 10,
  height = 6,
  dpi = 300
)


#--------------------------

r_sq_mean <- round(summary_mean_ssci$r.squared, 3)
p_val_mean <- signif(summary_mean_ssci$coefficients["ssci", "Pr(>|t|)"], 3)


plot_mean_ssci <- ggplot(dat, aes(x = ssci, y = mean_freq_UNWEIGHTED)) +
  geom_smooth(method = "lm", se = TRUE,
              color = "black", linewidth = 1, linetype="dashed",
              fill = "grey70", alpha = 0.2) +
  geom_point(color = "black", size = 2.5, alpha = 1) +
  labs(
    x = "Forest Structural Complexity (SSCI)",
    y = "Mean Body-Mass-Adjusted Peak-Frequency Residual"
  ) +
  theme_classic() +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14, color = "black"),
    axis.title.x = element_text(margin = margin(t = 15)),
    legend.position = "none"
  ) +
  annotate(
    "text",
    x = min(dat$ssci, na.rm = TRUE),
    y = max(dat$mean_freq_UNWEIGHTED, na.rm = TRUE),
    label = paste0("R^2 == ", r_sq_mean, " * ',' ~ p == ", p_val_mean),
    parse = TRUE,
    hjust = 0,
    vjust = 0,
    size = 5,
    fontface = "italic"
  )

print(plot_mean_ssci)

ggsave(
  "Plots/Testing_AAH/mean_peak_frequency_vs_ssci_mass_adjusted.png",
  plot = plot_mean_ssci,
  width = 10,
  height = 6,
  dpi = 300
)


