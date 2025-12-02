rm(list=ls(all=TRUE))
Sys.setenv(LANG = "en")

# Working directory
setwd("E:/Manuscripts/4_co-occurence") 

library(dplyr)


## ============================================================================================================ ##
## Set up a list of peak frequencies, based on Mikula 


#--------------
## Load data ##
#--------------

cooccurence_pairs <- read.csv2("Data/1_experts/cooccurence_pairs.csv")
expert_detections <- read.csv2("Data/1_experts/detections_freile_gelis_2021_2022_birds_dummy_pivot_reordered.csv")
mapping <- read.csv2("Data/1_experts/bird_name_mapping_experts_xeno-canto.csv")
mikula_freq <- read.csv("Data/1_experts/peak_frequency_detections/data_from_mikula/median_song_freq_w_pred.csv", skip = 44) # doi: 10.1111/ele.13662

#--------------
## Filter pairs that have both positive and negative co-occurences ##
#--------------

pairs_both <- cooccurence_pairs %>%
  group_by(sp1_name, sp2_name) %>%
  dplyr::filter(all(c("positive", "negative") %in% unique(type))) %>%
  ungroup() %>%
  arrange(sp1_name, sp2_name)

write.csv2(pairs_both, "Data/1_experts/both_positive_and_negative_cooccurence_pairs.csv")


#--------------
## Select all co-occurence birds
#--------------

cooccurence_species <- union(cooccurence_pairs$sp1_name, cooccurence_pairs$sp2_name)
# 144 species

cooccurence_species <- gsub("_", " ", cooccurence_species)

cooccurence_species_xc <- ifelse(cooccurence_species %in% mapping$ExpertName,
                          mapping$XenoCantoName[match(cooccurence_species, mapping$ExpertName)],
                          cooccurence_species)

#--------------
## Select all birds
#--------------

all_species <- names(expert_detections)[3:ncol(expert_detections)]
# 334 species

all_species <- gsub("_", " ", all_species)

all_species_xc <- ifelse(all_species %in% mapping$ExpertName,
                         mapping$XenoCantoName[match(all_species, mapping$ExpertName)],
                         all_species)

#--------------
## Align the co-occurence birds with Mikula`s species (Ecological Letters)
#--------------

cooccurence_species_mikula <- tolower(gsub(" ", "_", cooccurence_species_xc))
length(cooccurence_species_mikula) # 144

freq_combined <- tibble(scinam = cooccurence_species_mikula) %>% # create a tibble (data frame) with a column 'scinam' containing all cooccurence species names
  left_join(
    mikula_freq %>%
      select(scinam, peak_frequency) %>%              # keep only columns 'scinam' and 'peak_frequency' from mikula_freq
      rename(peak_frequency_mikula = peak_frequency), # rename 'peak_frequency' to 'peak_frequency_mikula'
    by = "scinam"                                     # join both tables by matching values in the 'scinam' column
  ) # perform a left join so all species from cooccurence_species_xc are kept, even if no match is found in mikula_freq

write.csv2(freq_combined, "Data/1_experts/peak_frequency_detections/xeno-canto/peak_frequencies_co-occurence.csv", row.names = FALSE)

#--------------
## Align all detected birds with Mikula`s species (Ecological Letters)
#--------------

all_species_mikula <- tolower(gsub(" ", "_", all_species_xc))

freq_combined <- tibble(scinam = all_species_mikula) %>%  # create a tibble (data frame) with a column 'scinam' containing all species names
  left_join(
    mikula_freq %>%
      select(scinam, peak_frequency) %>%              # keep only columns 'scinam' and 'peak_frequency' from mikula_freq
      rename(peak_frequency_mikula = peak_frequency), # rename 'peak_frequency' to 'peak_frequency_mikula'
    by = "scinam"                                     # join both tables by matching values in the 'scinam' column
  )  # perform a left join so all species from all_species_mikula are kept, even if no match is found in mikula_freq


write.csv2(freq_combined, "Data/1_experts/peak_frequency_detections/xeno-canto/peak_frequencies_all_species.csv", row.names = FALSE)


