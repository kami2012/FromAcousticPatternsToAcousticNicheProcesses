rm(list=ls(all=TRUE))
Sys.setenv(LANG = "en")

# Working directory
setwd("E:/Manuscripts/4_co-occurence") 

library(lme4)
library(multcomp)
library(dplyr)
library(picante)
library(tibble)
library(tidyr)

## ============================================================================================================ ##
## Analyze pairwise distances between co-occurring species based on phylogeny and trait data 


#--------------
## Load data ##
#--------------

dat <- read.csv2("Data/1_experts/co-occurrences/cooccurrence_pairs.csv")




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

model_traits <- lmer(traits_distance ~ type + (1 | plot), data = dat)
summary(model_traits)

model_peaks <- lmer(peak_distance ~ type + (1 | plot), data = dat)
summary(model_peaks)
cftest(model_peaks)

model_mass <- lmer(mass_distance ~ type + (1 | plot), data = dat)
summary(model_mass)


model_peaks_re <- lmer(
  peak_distance ~ type + (1 | plot) + (1 | Label),
  data = dat
)
summary(model_peaks_re)


model_peaks_res <- lmer(peak_distance_phylo_corr ~ Label_ord*type + (1 | plot), data = dat)
cftest(model_peaks_res)

model_traits_res <- lmer(trait_distance_phylo_corr ~ Label_ord*type + (1 | plot), data = dat)
cftest(model_traits_res)

