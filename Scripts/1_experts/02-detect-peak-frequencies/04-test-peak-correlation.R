rm(list=ls(all=TRUE))
Sys.setenv(LANG = "en")

# Working directory
setwd("E:/Manuscripts/4_co-occurence")

library(ggplot2)
library(ggrepel)

## ============================================================================================================ ##
## Test correlation between our measured peak frequencies and those reported by Mikula


#--------------
## Load data ##
#--------------

mean_peak_frequency <- read.csv2("Data/1_experts/peak_frequency_detections/data_from_xeno-canto/mean_peak_frequency.csv")
freq_combined <- read.csv2("Data/1_experts/peak_frequency_detections/peak_frequencies_mikula_sagar.csv")

#--------------
## Merge median peak frequency with Mikula`s and Sagar`s peak frequency values
#--------------

# Merge by matching scinam
freq_combined <- freq_combined %>%
  left_join(
    # The right table: Select and rename columns from mean_peak_frequency
    mean_peak_frequency %>%
      select(full_name, median_peak) %>% # Select the two columns needed
      rename(peak_freq_kuemmet = median_peak), # Rename the frequency column for clarity
    # The join condition: Match 'scinam' in freq_combined to 'species' in sagar_freq
    by = c("scinam" = "full_name")
  )

write.csv2(freq_combined, "Data/1_experts/peak_frequency_detections/peak_frequencies_mikula_sagar_kuemmet.csv", row.names = FALSE)



#--------------
## Test correlation ##
#--------------

cor(freq_combined$peak_freq_mikula, freq_combined$peak_freq_kuemmet, use = "complete.obs", method = "spearman")
# 0.9000716

cor(freq_combined$dom_freq_sagar, freq_combined$peak_freq_kuemmet, use = "complete.obs", method = "spearman")
# 0.8233517


m <- ggplot(freq_combined, aes(x = peak_freq_mikula, y = peak_freq_kuemmet, label = scinam)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  geom_text_repel(
    data = subset(freq_combined, abs(peak_freq_mikula - peak_freq_kuemmet) > 2000)
  ) +
  labs(x = "Mikula", y = "Kuemmet")

ggsave(filename = "Plots/1_experts/peak_frequency_correlations/mikula.png", plot = m, device = "png", width = 7, height = 5, units = "in", dpi = 300)


s <- ggplot(freq_combined, aes(x = dom_freq_sagar, y = peak_freq_kuemmet, label = scinam)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  geom_text_repel(
    data = subset(freq_combined, abs(dom_freq_sagar - peak_freq_kuemmet) > 2000)
  ) +
  labs(x = "Sagar", y = "Kuemmet")

ggsave(filename = "Plots/1_experts/peak_frequency_correlations/sagar.png", plot = s, device = "png", width = 7, height = 5, units = "in", dpi = 300)


