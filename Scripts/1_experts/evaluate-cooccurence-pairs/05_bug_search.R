rm(list=ls(all=TRUE))
Sys.setenv(LANG = "en")

# Working directory
setwd("E:/Manuscripts/4_co-occurence") 

library(lme4)


## ============================================================================================================ ##
## Analyze pairwise distances between co-occurring species based on phylogeny and trait data 


#--------------
## Load data ##
#--------------

cooccurence_pairs_categories <- read.csv2("Data/1_experts/cooccurence_pairs.csv")

#--------------
## Analyze distances ##
#--------------

model_traits <- lmer(traits_distance ~ type + (1 | plot), data = cooccurence_pairs_categories)
summary(model_traits)

model_peaks <- lmer(peak_distance ~ type + (1 | plot), data = cooccurence_pairs_categories)
summary(model_peaks)

model_mass <- lmer(mass_distance ~ type + (1 | plot), data = cooccurence_pairs_categories)
summary(model_mass)


model_peaks_re <- lmer(
  peak_distance ~ type + (1 | plot) + (1 | Label),
  data = cooccurence_pairs_categories
)

summary(model_peaks_re)


# Subset only labels starting with F
data_F <- subset(cooccurence_pairs_categories, grepl("^F", Label))

# Fit the model
model_peaks_F <- lmer(peak_distance ~ type + (1 | plot), data = data_F)
summary(model_peaks_F)