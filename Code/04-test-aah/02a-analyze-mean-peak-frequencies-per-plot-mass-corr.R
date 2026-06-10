rm(list=ls(all=TRUE))
Sys.setenv(LANG = "en")

library(dplyr)
library(lme4)
library(multcomp)
library(corrplot)

## ============================================================================================================ ##
## Test the Acoustic Adaptation Hypothesis (AAH) by linking frequency metrics with succession 


#--------------
## Load data ##
#--------------

freq <- read.csv2("Data/Testing_AAH/mean_peak_frequency_per_plot_mass_corrected.csv")

tls <- read.csv2("Data/Testing_AAH/reassembly_tls plotdata_Ehbrecht.csv")

# Correct plot name
tls$plot[tls$plot == "OG28"] <- "PR28"


tls$veg_age[tls$veg_type == "old growth forest"] <- 50


# Test successional age as a surrogate for forest structural metrics 
cor(tls$veg_age, tls$ssci, use = "complete.obs")
# [1] 0.7467907

cor(tls$veg_age, tls$enl, use = "complete.obs")
#[1] 0.689193

cor(tls$veg_age, tls$canopy_height, use = "complete.obs")
#[1] 0.7972484


dat <- merge(
  freq[, c("plot", "mean_freq_UNWEIGHTED", "sd_freq_UNWEIGHTED")],
  tls,
  by = "plot"
)


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

write.csv2(dat, "Data/Testing_AAH/mean_peak_frequency_per_plot_mass_corrected_tls.csv", row.names = FALSE) 

# Test the relation between freq and different forest structural metrics
summary(lm(mean_freq_UNWEIGHTED ~ ssci, data = dat))

summary(lm(sd_freq_UNWEIGHTED ~ ssci, data = dat))


summary(lm(mean_freq_UNWEIGHTED ~ enl, data = dat))

summary(lm(sd_freq_UNWEIGHTED ~ enl, data = dat))


summary(lm(mean_freq_UNWEIGHTED ~ canopy_height, data = dat))

summary(lm(sd_freq_UNWEIGHTED ~ canopy_height, data = dat))


summary(lm(mean_freq_UNWEIGHTED ~ veg_age, data = dat))

summary(lm(sd_freq_UNWEIGHTED ~ veg_age, data = dat))



# Test for non-linear relationships between SSCI and frequency dispersion
m_lin <- lm(sd_freq_UNWEIGHTED ~ ssci, data = dat)

m_quad <- lm(sd_freq_UNWEIGHTED ~ ssci + I(ssci^2), data = dat)

summary(m_quad)

anova(m_lin, m_quad)



