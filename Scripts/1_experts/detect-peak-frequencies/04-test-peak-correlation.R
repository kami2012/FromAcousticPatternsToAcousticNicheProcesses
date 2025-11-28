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

peak_frequencies <- read.csv2("Data/1_experts/peak_frequency_detections/peak_frequencies.csv")

#--------------
## Test correlation ##
#--------------

cor(peak_frequencies$peak_frequency_mikula, peak_frequencies$peak_frequency_xc, use = "complete.obs", method = "spearman")
# 0.938142


ggplot(peak_frequencies, aes(x = peak_frequency_mikula, y = peak_frequency_xc, label = scinam)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  geom_text_repel(
    data = subset(peak_frequencies, abs(peak_frequency_mikula - peak_frequency_xc) > 2000)
  ) +
  labs(x = "Mikula", y = "Xeno-Canto")



