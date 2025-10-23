rm(list=ls(all=TRUE))
Sys.setenv(LANG = "en")

# Working directory
setwd("E:/Manuscripts/4_co-occurence") 

library("dplyr")
library("mgcv")
library("stringr")

## ============================================================================================================ ##
## Compute daily activity patterns with GAM 


#--------------
## Load data ##
#--------------

data_complete <- read.table("Data/4_birdnet_retrained/threshold_05/birdNET_retrained_expert-set_threshold05.csv", header=T, check.names = F,sep=";")

# Remove plots with all zero values (ACN1) and plots without context information (CR16)
data_filtered <- data_complete[!data_complete$plot %in% c("ACN1", "CR16"), ] # => 85 plots

# Replace spaces with underscores
colnames(data_filtered)[3:ncol(data_filtered)] <- gsub(" ", "_", colnames(data_filtered)[3:ncol(data_filtered)])


plot_categories <- read.csv("Data/categories_nmds1.csv", header=T, check.names = F)
plot_categories <- plot_categories[, 1:7]



#---------------------------------------------
## Preprocessing ##
#---------------------------------------------

data_filtered$bird_presence <- as.integer(rowSums(data_filtered[ , !(names(data_filtered) %in% c("filename", "plot"))]) > 0)

bird_presence <- data_filtered[ , c("filename", "plot", "bird_presence")]

# Add plot categories
bird_presence <- merge(
  bird_presence,
  plot_categories[, c("Plot_ID", "Label", "Latitude", "Longitude")],   
  by.x = "plot",                         
  by.y = "Plot_ID",                      
  all.x = TRUE                           
)

# Assign each plot to either "Agricultural Matrix" or "Forest Matrix"
# If the 'label' starts with "A" → Agricultural, else → Forest
bird_presence$matrix <- ifelse(str_starts(bird_presence$Label, "A"),
                                    "Agricultural Matrix",
                                    "Forest Matrix")

# Assign habitat based on the end of the 'label' string
bird_presence <- bird_presence %>%
  mutate(habitat = case_when(
    str_ends(Label, "P")     ~ "Agriculture",
    str_ends(Label, "C")     ~ "Agriculture",
    str_ends(Label, "RegI")  ~ "Regeneration I",
    str_ends(Label, "RegII") ~ "Regeneration II",
    str_ends(Label, "Old")   ~ "Old-growth",
    TRUE                      ~ NA_character_  # fallback for unmatched labels
  ))

bird_presence$habitat <- ordered(
  bird_presence$habitat,
  levels = c("Agriculture", "Regeneration I", "Regeneration II", "Old-growth")
)

# Extract year from filename
bird_presence$year <- ifelse(str_starts(bird_presence$filename, "2022"),
                             2022,
                             2021)

# Extract time_of_day from filename
bird_presence <- bird_presence %>%
  mutate(time_str = ifelse(str_starts(filename, "2022"),
                           substr(filename, 10, 15),
                           str_extract(filename, "(?<=_)\\d{6}(?=_)")),
         time_minutes = as.numeric(substr(time_str, 1, 2)) * 60 +
           as.numeric(substr(time_str, 3, 4)) +
           as.numeric(substr(time_str, 5, 6)) / 60)

# Convert time to hours
bird_presence$time_hours <- bird_presence$time_minutes / 60


# Change column names to lowercase
names(bird_presence) <- tolower(names(bird_presence))


# Add distinct colors for the different habitat types
bird_presence <- bird_presence %>%
  mutate(habitat_color = case_when(
    habitat == "Agriculture"     ~ "#c2e699",
    habitat == "Regeneration I"  ~ "#78c679",
    habitat == "Regeneration II" ~ "#31a354",
    habitat == "Old-growth"      ~ "#006837"
  ))


# Filter only Forest Matrix
forest_data <- subset(bird_presence, matrix == "Forest Matrix")


# Check number of observations per habitat per hour
table(forest_data$habitat)

# Agriculture  Old-growth  Regeneration I Regeneration II 
# 18720           24960           16224           21216  


#------------------------
# Predict bird activity along the gradient
#------------------------

# Fit GAM model: smooth for time, categorical habitat, linear elevation, spatial smooth
gam_bird <- gam(
  bird_presence ~ s(time_minutes) + habitat + s(latitude, longitude, bs = "tp"),
  family = binomial, method = "REML", data = forest_data
)

summary(gam_bird)

# Predicted probabilities
forest_data$pred <- predict(gam_bird, type = "response")

#------------------------
# Plot predicted probabilities by habitat
#------------------------

p1 <- ggplot(forest_data, aes(x = habitat, y = pred, fill = habitat_color)) +
  geom_boxplot(outlier.shape = 1, width = 0.5, color = "black") + 
  labs(
    title = "Estimated Probability of Bird Activity (Retrained BirdNET 0.5, expert-set)",
    x = "Habitat", 
    y = "Probability of Bird Vocalization"
    ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white")
  ) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.25), limits = c(0, 1)) +
  scale_fill_identity(name = "Habitat Type", guide = "legend")
  

# Save to file
ggsave("Plots/4_birdnet_retrained/threshold_05/probability_of_bird_vocalization_by_habitat_expert-set.png", plot = p1, width = 8, height = 6, dpi = 300)


#------------------------
# Daily pattern of activity along the recovery gradient
#------------------------


unique_habitats <- unique(forest_data$habitat)

png("Plots/4_birdnet_retrained/threshold_05/daily_pattern_of_bird_activity_partialEffect_expert-set.png", width = 1600, height = 1200, res = 200)
# Create 2x2 layout
par(mfrow = c(2, 2), mar = c(5, 5, 3, 2), oma = c(0, 0, 4, 0))  # oma: bottom, left, top, right

# Loop over habitats
for (h in unique_habitats) {
  data_h <- subset(forest_data, habitat == h)
  color_h <- unique(data_h$habitat_color)
  
  # Fit GAM
  gam_base <- gam(bird_presence ~ s(time_hours),
                  family = binomial, method = "REML", data = data_h)
  
  
  # Plot partial effect
  plot(gam_base, 
       shade = TRUE, 
       shade.col = adjustcolor(color_h, alpha.f = 0.4),
       col = color_h,
       main = h,
       xlab = "Time of the day (hour)",
       ylab = "Partial effect of daily pattern",
       ylim = c(-5, 5),
       cex.lab = 1.2, cex.main = 1.7, font.main = 1, las = 1)
  abline(h = 0, col = "darkgrey")
}

mtext("Daily Pattern of Bird Vocalization by Habitat (Retrained BirdNET 0.5, expert-set)", outer = TRUE, line = 1.5, cex = 1.5)

dev.off()

# Partial Effects with Response-Scale () (probabilities)
get_partial <- function(df) {
  gam_fit <- gam(bird_presence ~ s(time_hours), family = binomial, data = df)
  newdata <- data.frame(time_hours = seq(min(df$time_hours), max(df$time_hours), length.out = 200))
  pred <- predict(gam_fit, newdata = newdata, type = "response", se.fit = TRUE)
  data.frame(
    habitat = rep(unique(df$habitat), times = nrow(newdata)),
    time_hours = newdata$time_hours,
    fit = pred$fit,
    lower = pred$fit - 1.96 * pred$se.fit,
    upper = pred$fit + 1.96 * pred$se.fit,
    color = rep(unique(df$habitat_color), times = nrow(newdata))
  )
}

# Compute Partial Effects
partial_df <- forest_data %>%
  group_by(habitat) %>%
  do(get_partial(.)) %>%
  ungroup()

# Plot
p2 <- ggplot(partial_df, aes(x = time_hours, y = fit, color = habitat, fill = habitat)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, color = NA) +
  geom_line(size = 1) +
  scale_color_manual(
    name = "Habitat Type",  # <- Legend title
    values = setNames(partial_df$color, partial_df$habitat)
  ) +
  scale_fill_manual(
    name = "Habitat Type",  # <- Legend title
    values = setNames(partial_df$color, partial_df$habitat)
  ) +
  labs(
    title = "Daily Pattern of Bird Vocalization by Habitat (Retrained BirdNET 0.5, expert-set)",
    x = "Time of the day (hour)", 
    y = "Probability of Bird Vocalization"
    ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white")
  ) +
  theme(legend.position = "bottom") +
  facet_wrap(~ habitat, ncol = 2)   # <- 2x2 Layout, 4 Habitats

# Save to file
ggsave("Plots/4_birdnet_retrained/threshold_05/daily_pattern_of_bird_activity_probability_expert-set.png", plot = p2, width = 8, height = 6, dpi = 300)
