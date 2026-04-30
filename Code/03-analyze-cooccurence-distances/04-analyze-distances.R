rm(list=ls(all=TRUE))
Sys.setenv(LANG = "en")

library(dplyr)
library(lme4)
library(multcomp)

## ============================================================================================================ ##
## Analyze pairwise distances between co-occurring species based on phylogeny and trait data 


#--------------
## Load data ##
#--------------

dat <- read.csv2("Data/Co-occurrence/cooccurrence_pairs_distances.csv")


# Recovery gradient categories
plot_categories <- read.csv("Data/plot_categories.csv", header=T, check.names = F)
plot_categories <- plot_categories[, 1:7]

dat <- merge(
  dat ,
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

dat$Label_ord<-ordered(dat$Label,c("A_P" , "A_C" , "F_P" , "F_C" , "F_P_RegI", "F_C_RegI", "A_Old" , "F_P_RegII", "F_C_RegII","F_Old"))


#--------------
## Analyze distances ##
#--------------

# Residuals
peak_phylo_mod<-lm(peak_distance ~ phylo_distance, dat)
dat$peak_phylo_res<-residuals(peak_phylo_mod)

peak_mass_mod<-lm(peak_distance ~ mass_distance, dat)
dat$peak_mass_res<-residuals(peak_mass_mod)

# Combined Residuals (The "fully corrected" acoustic niche)
peak_full_mod <- lm(peak_distance ~ phylo_distance + mass_distance, data = dat)
dat$peak_full_res <- residuals(peak_full_mod)

# 
model_peaks_phylo_res <- lmer(peak_phylo_res ~ type + (1 | plot), data = dat)
cftest(model_peaks_phylo_res)

model_peaks_mass_res <- lmer(peak_mass_res ~ type + (1 | plot), data = dat)
cftest(model_peaks_mass_res)

model_peaks_full_res <- lmer(peak_full_res ~ type + (1 | plot), data = dat)
cftest(model_peaks_full_res)

model_traits <- lmer(traits_distance ~ type + (1 | plot), data = dat)
cftest(model_traits)

model_peaks <- lmer(peak_distance ~ type + (1 | plot), data = dat)
cftest(model_peaks)

# Gradient
model_peaks_phylo_res_grad <- lmer(peak_phylo_res ~ Label_ord*type + (1 | plot), data = dat)
cftest(model_peaks_phylo_res_grad)

model_peaks_mass_res_grad <- lmer(peak_mass_res ~ Label_ord*type + (1 | plot), data = dat)
cftest(model_peaks_mass_res_grad)

model_peaks_full_res_grad <- lmer(peak_full_res ~ Label_ord * type + (1 | plot), data = dat)
cftest(model_peaks_full_res_grad)

model_traits_grad <- lmer(traits_distance ~ Label_ord*type + (1 | plot), data = dat)
cftest(model_traits_grad)



library(ggeffects)
library(ggplot2)

# Calculate predicted values
plot_data <- ggpredict(model_peaks_full_res_grad, terms = c("Label_ord", "type"))

# Plot it
plot(plot_data) + 
  labs(title = "Acoustic Divergence across Forest Recovery",
       x = "Recovery Gradient (Ordered)",
       y = "Phylo- & Mass-corrected Acoustic Distance")







