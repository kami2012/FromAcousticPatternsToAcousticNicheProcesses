rm(list=ls(all=TRUE))
Sys.setenv(LANG = "en")

# Working directory
setwd("E:/Manuscripts/4_co-occurence") 

library(ggplot2)
library(dplyr)

## ============================================================================================================ ##
## Number of birds per file detected by experts and CNN models 


#--------------
## Load data ##
#--------------

# Experts
experts <- read.csv2("Data/Experts_detections/detections_freile_gelis_2021_2022_birds_dummy_pivot_reordered.csv", stringsAsFactors = FALSE)

# BirdNET
birdnet_05 <- read.csv2("Data/BirdNET_detections/birdNET_expert-set_threshold05.csv", stringsAsFactors = FALSE)
birdnet_09 <- read.csv2("Data/BirdNET_detections/birdNET_expert-set_threshold09.csv", stringsAsFactors = FALSE)


#--------------
## Calculate birds per file ##
#--------------

experts$birds_per_file <- rowSums(experts[, 3:ncol(experts)], na.rm = TRUE)

birdnet_05$birds_per_file <- rowSums(birdnet_05[, 3:ncol(birdnet_05)], na.rm = TRUE)
birdnet_09$birds_per_file <- rowSums(birdnet_09[, 3:ncol(birdnet_09)], na.rm = TRUE)


#--------------
## Plot birds per file ##
#--------------

# Combine data
df_list <- list(
  Experts = experts$birds_per_file,
  BirdNET_05 = birdnet_05$birds_per_file,
  BirdNET_09 = birdnet_09$birds_per_file
)

df_long <- bind_rows(lapply(names(df_list), function(name) {
  data.frame(Model = name, birds_per_file = df_list[[name]])
}))

df_long$Model <- factor(df_long$Model, 
                        levels = c("Experts", "BirdNET_05", "BirdNET_09"))

# Define colors for each model
colors <- c(
  "Experts" = "#E69F00",
  "BirdNET_05" = "#0072B2",
  "BirdNET_09" = "#56B4E9"
)

# Plot boxplot
p <- ggplot(df_long, aes(x = Model, y = birds_per_file, fill = Model)) +
  geom_boxplot(alpha = 0.7, outlier.colour = "grey40", outlier.size = 1) +
  theme_bw() +
  labs(title = "Number of bird species detected per 2-min recording",
       x = "Model",
       y = "Number of species detected") +
  scale_fill_manual(values = colors) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

ggsave("Plots/birds_per_file_boxplot.png", plot = p, width = 8, height = 5, dpi = 300)


