rm(list=ls(all=TRUE))
Sys.setenv(LANG = "en")

# Working directory
setwd("E:/Manuscripts/4_co-occurence") 



## ============================================================================================================ ##
## Number of birds per file detected by experts and CNN models 


#--------------
## Load data ##
#--------------

# Experts
experts <- read.csv("Data/1_experts/bird_presence_absence_per_file.csv", stringsAsFactors = FALSE)

# Rainforest Connection
rfc_05 <- read.csv("Data/2_rfc/threshold_05/bird_presence_absence_per_file.csv", stringsAsFactors = FALSE)
rfc_09 <- read.csv("Data/2_rfc/threshold_09/bird_presence_absence_per_file.csv", stringsAsFactors = FALSE)

# BirdNET
birdnet_05 <- read.csv("Data/3_birdnet/threshold_05/bird_presence_absence_per_file.csv", stringsAsFactors = FALSE)
birdnet_09 <- read.csv("Data/3_birdnet/threshold_09/bird_presence_absence_per_file.csv", stringsAsFactors = FALSE)

# Retrained BirdNET
birdnetRe_05 <- read.csv("Data/4_birdnet_retrained/threshold_05/bird_presence_absence_per_file.csv", stringsAsFactors = FALSE)
birdnetRe_09 <- read.csv("Data/4_birdnet_retrained/threshold_09/bird_presence_absence_per_file.csv", stringsAsFactors = FALSE)

#--------------
## Calculate birds per file ##
#--------------

experts$birds_per_file <- rowSums(experts[, 3:ncol(experts)], na.rm = TRUE)

rfc_05$birds_per_file <- rowSums(rfc_05[, 3:ncol(rfc_05)], na.rm = TRUE)
rfc_09$birds_per_file <- rowSums(rfc_09[, 3:ncol(rfc_09)], na.rm = TRUE)

birdnet_05$birds_per_file <- rowSums(birdnet_05[, 3:ncol(birdnet_05)], na.rm = TRUE)
birdnet_09$birds_per_file <- rowSums(birdnet_09[, 3:ncol(birdnet_09)], na.rm = TRUE)

birdnetRe_05$birds_per_file <- rowSums(birdnetRe_05[, 3:ncol(birdnetRe_05)], na.rm = TRUE)
birdnetRe_09$birds_per_file <- rowSums(birdnetRe_09[, 3:ncol(birdnetRe_09)], na.rm = TRUE)


#--------------
## Plot birds per file ##
#--------------

# Combine data
df_list <- list(
  Experts = experts$birds_per_file,
  RFCx_05 = rfc_05$birds_per_file,
  RFCx_09 = rfc_09$birds_per_file,
  BirdNET_05 = birdnet_05$birds_per_file,
  BirdNET_09 = birdnet_09$birds_per_file,
  Retrained_BirdNET_05 = birdnetRe_05$birds_per_file,
  Retrained_BirdNET_09 = birdnetRe_09$birds_per_file
)

df_long <- bind_rows(lapply(names(df_list), function(name) {
  data.frame(Model = name, birds_per_file = df_list[[name]])
}))

df_long$Model <- factor(df_long$Model, 
                        levels = c("Experts", "RFCx_05", "RFCx_09",
                                   "BirdNET_05", "BirdNET_09",
                                   "Retrained_BirdNET_05", "Retrained_BirdNET_09"))

# Define colors for each model
colors <- c(
  "Experts" = "#cb181d",
  "RFCx_05" = "#238b45",
  "RFCx_09" = "#74c476",
  "BirdNET_05" = "#2171b5",
  "BirdNET_09" = "#6baed6",
  "Retrained_BirdNET_05" = "#ae017e",
  "Retrained_BirdNET_09" = "#dd3497"
)

# Plot boxplot
p <- ggplot(df_long, aes(x = Model, y = birds_per_file, fill = Model)) +
  geom_boxplot() +
  theme_bw() +
  labs(title = "Number of Birds per File",
       x = "Model",
       y = "Birds per File") +
  scale_fill_manual(values = colors) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")


ggsave("Plots/birds_per_file_boxplot.png", plot = p, width = 8, height = 5, dpi = 300)


