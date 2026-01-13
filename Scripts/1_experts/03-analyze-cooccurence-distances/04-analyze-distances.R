rm(list=ls(all=TRUE))
Sys.setenv(LANG = "en")

# Working directory
setwd("E:/Manuscripts/4_co-occurence") 

library(dplyr)
library(lme4)
library(multcomp)

## ============================================================================================================ ##
## Analyze pairwise distances between co-occurring species based on phylogeny and trait data 


#--------------
## Load data ##
#--------------

dat_kuemmet <- read.csv2("Data/1_experts/co-occurrence/cooccurrence_pairs_distances_kuemmet.csv")
dat_sagar <- read.csv2("Data/1_experts/co-occurrence/cooccurrence_pairs_distances_sagar.csv")
dat <- read.csv2("Data/1_experts/co-occurrence/cooccurrence_pairs_distances.csv")


# Recovery gradient categories
plot_categories <- read.csv("Data/plot_categories.csv", header=T, check.names = F)
plot_categories <- plot_categories[, 1:7]

dat_kuemmet <- merge(
  dat_kuemmet ,
  plot_categories[, c("Plot_ID", "Label")],   
  by.x = "plot",                         
  by.y = "Plot_ID",                      
  all.x = TRUE                           
)

# Set factor levels of 'Label' to reflect the recovery gradient ##
dat_kuemmet$Label <- factor(
  dat_kuemmet$Label,
  levels = c("A_P", "A_C", "F_P", 
             "F_C", "F_P_RegI", "F_C_RegI", "A_Old",
             "F_P_RegII", "F_C_RegII", "F_Old"
  )
)

dat_kuemmet$Label_ord<-ordered(dat_kuemmet$Label,c("A_P" , "A_C" , "F_P" , "F_C" , "F_P_RegI", "F_C_RegI", "A_Old" , "F_P_RegII", "F_C_RegII","F_Old"))


dat_sagar <- merge(
  dat_sagar ,
  plot_categories[, c("Plot_ID", "Label")],   
  by.x = "plot",                         
  by.y = "Plot_ID",                      
  all.x = TRUE                           
)

# Set factor levels of 'Label' to reflect the recovery gradient ##
dat_sagar$Label <- factor(
  dat_sagar$Label,
  levels = c("A_P", "A_C", "F_P", 
             "F_C", "F_P_RegI", "F_C_RegI", "A_Old",
             "F_P_RegII", "F_C_RegII", "F_Old"
  )
)

dat_sagar$Label_ord<-ordered(dat_sagar$Label,c("A_P" , "A_C" , "F_P" , "F_C" , "F_P_RegI", "F_C_RegI", "A_Old" , "F_P_RegII", "F_C_RegII","F_Old"))


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

# Only Reassembly plots
dat_sagar_reassembly <- dat_sagar %>%
  filter(startsWith(as.character(Label), "F_"))

#--------------
## Analyze distances ##
#--------------

peak_mod<-lm(peak_distance ~ phylo_distance, dat)
dat$peak_res<-residuals(peak_mod)

traits_mod<-lm(traits_distance ~ phylo_distance, dat)
dat$traits_res<-residuals(traits_mod)

#modelle der residuen
model_peaks_res <- lmer(peak_res ~ Label_ord*type + (1 | plot), data = dat)
cftest(model_peaks_res)

model_traits_res <- lmer(traits_res ~ Label_ord*type + (1 | plot), data = dat)
cftest(model_traits_res)

# Peak and Trait distance corrected by phylogeny
model_peak_phylo_corr <- lmer(peak_distance_phylo_corr ~ Label_ord*type + (1 | plot), data = dat)
cftest(model_peak_phylo_corr)

model_peak_mass_corr <- lmer(peak_distance_mass_corr ~ Label_ord*type + (1 | plot), data = dat)
cftest(model_peak_mass_corr)

model_traits_phylo_corr <- lmer(trait_distance_phylo_corr ~ Label_ord*type + (1 | plot), data = dat)
cftest(model_traits_phylo_corr)



#alternative im modell gleich phylogeny als predictor
model_peaks_phylo <- lmer(peak_distance ~ Label_ord*type + phylo_distance + (1 | plot), data = dat_kuemmet)
cftest(model_peaks_phylo)

model_peaks_mass <- lmer(peak_distance ~ Label_ord*type + mass_distance + (1 | plot), data = dat)
cftest(model_peaks_mass)

model_traits_phylo <- lmer(traits_distance ~ Label_ord*type + phylo_distance + (1 | plot), data = dat_kuemmet)
cftest(model_traits_phylo)



model_peaks_phylo <- lmer(peak_distance ~ Label_ord*type + phylo_distance + (1 | plot), data = dat_sagar)
cftest(model_peaks_phylo)

model_traits_phylo <- lmer(traits_distance ~ Label_ord*type + phylo_distance + (1 | plot), data = dat_sagar)
cftest(model_traits_phylo)



model_peaks_phylo <- lmer(peak_distance ~ Label_ord*type + phylo_distance + (1 | plot), data = dat_sagar_reassembly)
cftest(model_peaks_phylo)

model_traits_phylo <- lmer(traits_distance ~ Label_ord*type + phylo_distance + (1 | plot), data = dat_reassembly)
cftest(model_traits_phylo)


# 1. Paket laden
library(emmeans)
library(ggplot2)

# 2. Berechne die geschätzten Mittelwerte für die Interaktion
# Dies zieht die "reinen" Werte aus deinem lmer-Modell
ems <- emmeans(model_peak_mass_corr, ~ Label_ord | type)

# 3. In einen Dataframe umwandeln
ems_df <- as.data.frame(ems)

# 4. Visualisierung der Kurven
ggplot(ems_df, aes(x = Label_ord, y = emmean, color = type, group = type)) +
  geom_line(size = 1) + 
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) +
  theme_minimal() +
  labs(title = "Vorhergesagte Frequenzabstände (Mass-corrected)",
       y = "Predicted Residual Distance",
       x = "Sukzessionsstadium")
