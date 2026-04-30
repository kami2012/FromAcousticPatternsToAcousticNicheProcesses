rm(list=ls(all=TRUE))
Sys.setenv(LANG = "en")

library(dplyr)
library(lme4)
library(multcomp)

## ============================================================================================================ ##
## Test the Acoustic Adaptation Hypothesis (AAH) by linking frequency metrics with succession 


#--------------
## Load data ##
#--------------

dat <- read.csv2("Data/Testing_AAH/mean_peak_frequency_per_plot_mass_corrected.csv")

# Load recovery gradient categories
plot_categories <- read.csv("Data/plot_categories.csv", header=T, check.names = F)
plot_categories <- plot_categories[, 1:7]

dat <- merge(
  dat,
  plot_categories[, c("Plot_ID", "Label")],   
  by.x = "plot",                         
  by.y = "Plot_ID",                      
  all.x = TRUE                           
)

# Set factor levels of 'Label' to reflect the recovery gradient ##
dat$Label <- factor(
  dat$Label,
  levels = c("A_P", "A_C", "F_P", 
             "F_C", "F_P_RegI", "F_C_RegI", "A_Old",
             "F_P_RegII", "F_C_RegII", "F_Old"
  )
)

# Create an ordered factor variable
dat$Label_ord <- ordered(dat$Label,c("A_P" , "A_C" , "F_P" , "F_C" , "F_P_RegI", "F_C_RegI", "A_Old" , "F_P_RegII", "F_C_RegII","F_Old"))

# Load the numerical succession time variable
succession_time <- read.csv2("Data/Testing_AAH/Environ87.csv")

dat <- merge(
  dat,
  succession_time[, c("Plot_ID", "Succession")],
  by.x = "plot",
  by.y = "Plot_ID",
  all.x = TRUE
)


write.csv2(dat, "Data/Testing_AAH/mean_peak_frequency_per_plot_mass_corrected_gradient_succession.csv", row.names = FALSE) 

# Filter Reassembly plots
dat_reassembly <- dat %>%
  dplyr::filter(grepl("^F_", Label))



model_mean_freq_succ <- lm (mean_freq_UNWEIGHTED ~ Succession, data = dat_reassembly)
summary(model_mean_freq_succ)

model_sd_freq_succ <- lm (sd_freq_UNWEIGHTED ~ Succession, data = dat_reassembly)
summary(model_sd_freq_succ)

plot(model_sd_freq_succ, which = 1) # Residuals vs Fitted
plot(model_sd_freq_succ, which = 4) # Cook's Distance

model_check <- lm(sd_freq_UNWEIGHTED ~ Succession + num_unique_species, data = dat_reassembly)
summary(model_check)

anova(model_sd_freq_succ, model_check)


model_mean_freq_gradient <- lm(mean_freq_UNWEIGHTED ~ Label_ord, data = dat_reassembly)
summary(model_mean_freq_gradient)

model_sd_freq_gradient <- lm(sd_freq_UNWEIGHTED ~ Label_ord, data = dat)
summary(model_sd_freq_gradient)

