rm(list=ls(all=TRUE))
Sys.setenv(LANG = "en")

library(dplyr)
library(lme4)
library(multcomp)

## ========================================================================== ##
## H2 – Spectral differentiation
## Compare significantly positive versus significantly negative temporal
## associations. Non-significant pairs are retained in the master dataset
## but excluded from H2.
## ========================================================================== ##

#--------------
## Load pairwise data ##
#--------------

dat <- read.csv2("Data/Co-occurrence/cooccurrence_pairs_distances.csv")

# Restrict H2 to significantly positive and negative associations
dat_h2 <- dat %>%
  filter(type %in% c("negative", "positive")) %>%
  mutate(
    type = factor(type, levels = c("negative", "positive"))
  ) %>%
  droplevels()

# Check sample sizes
table(dat_h2$type)
nrow(dat_h2)

## -------------------------------------------------------------------------- ##
## Adjust peak-frequency distance for phylogenetic and body-mass distance
## -------------------------------------------------------------------------- ##

peak_full_mod <- lm(
  peak_distance ~ phylo_distance + mass_distance,
  data = dat_h2
)

dat_h2$peak_full_res <- residuals(peak_full_mod)



## -------------------------------------------------------------------------- ##
## H2 models
## -------------------------------------------------------------------------- ##

# Unadjusted peak-frequency distance
model_peaks <- lmer(
  peak_distance ~ type + (1 | plot),
  data = dat_h2
)

cftest(model_peaks)



# Phylogeny- and body-mass-adjusted peak-frequency distance
model_peaks_adjusted <- lmer(
  peak_full_res ~ type + (1 | plot),
  data = dat_h2
)

cftest(model_peaks_adjusted)

# not published
model_peaks_directly_adjusted <- lmer(
  peak_distance ~ type + phylo_distance + mass_distance + (1 | plot),
  data = dat_h2
)

cftest(model_peaks_directly_adjusted)



# Ecological–morphological trait distance
model_traits <- lmer(
  traits_wo_mass_distance ~ type + (1 | plot),
  data = dat_h2
)

cftest(model_traits)
