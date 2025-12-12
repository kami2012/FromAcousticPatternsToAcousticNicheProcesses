rm(list=ls(all=TRUE))
Sys.setenv(LANG = "en")

# Working directory
setwd("E:/Manuscripts/4_co-occurence") 

library(dplyr)
library(stringr)

## ============================================================================================================ ##
## Set up for detecting peak frequencies


#--------------
## Load data ##
#--------------

expert_detections <- read.csv2("Data/1_experts/bird_presence_absence_per_file.csv")
mapping <- read.csv2("Data/1_experts/bird_name_mapping_experts_xeno-canto.csv")
mikula_freq <- read.csv("Data/1_experts/peak_frequency_detections/data_from_mikula/median_song_freq_w_pred.csv", skip = 44) # doi: 10.1111/ele.13662
sagar_freq <- read.csv("Data/1_experts/peak_frequency_detections/data_from_sagar/Bird_acoustics_dataframe.csv") # doi: 10.1098/rspb.2024.1908

#--------------
## Select and align all detected birds names (from experts) to Xeno-Canto names
#--------------

species <- names(expert_detections)[3:ncol(expert_detections)]
# 334 species

species <- gsub("_", " ", species)

species_xc <- ifelse(species %in% mapping$ExpertName,
                         mapping$XenoCantoName[match(species, mapping$ExpertName)],
                         species)

#--------------
## Align all detected birds with Mikula`s species (Ecological Letters)
#--------------

# Standardize Mikula's scientific names to sentence case for consistent matching
mikula_freq$scinam <- str_to_sentence(gsub("_", " ", mikula_freq$scinam))

freq_combined <- tibble(species = species_xc) %>%  # create a tibble (data frame) with a column 'species' containing all species names
  rename(scinam = species) %>%
  left_join(
    mikula_freq %>%
      select(scinam, peak_frequency) %>%              # keep only columns 'scinam' and 'peak_frequency' from mikula_freq
      rename(peak_freq_mikula = peak_frequency), # rename 'peak_frequency' to 'peak_frequency_mikula'
    by = "scinam"                                     # join both tables by matching values in the 'scinam' column
  )  # perform a left join so all species from species_xc are kept, even if no match is found in mikula_freq


#--------------
## Align all detected birds with Sagar`s species (Proceedings of the Royal Society B)
#--------------

# *** STEP 1: CLEAN AND CONVERT SAGAR'S DATA ***

# Realms relevant for Ecuador's Chocó forest: 'neotropical' and 'panamanian'
relevant_realms <- c("neotropical", "panamanian")

sagar_freq_final <- sagar_freq %>%
  filter(realm %in% relevant_realms) %>%
  mutate(
    # 1. Replace the comma (,) decimal separator with a period (.) in the dom.freq string
    dom.freq_clean = gsub(",", ".", dom.freq),
    
    # 2. Convert the cleaned string to a number and multiply by 1000 
    #    to convert from Kilohertz (kHz) to Hertz (Hz).
    dom_freq_hz = as.numeric(dom.freq_clean) * 1000,
    dom_freq_hz = round(dom_freq_hz, 2)
  ) %>%
  # Remove duplicates
  group_by(species) %>%
  summarise(
    dom_freq_sagar = median(dom_freq_hz, na.rm = TRUE),
    .groups = 'drop'
  )

# *** STEP 2: PERFORM THE JOIN ***
freq_combined <- freq_combined %>%
  left_join(
    # Use the cleaned sagar_freq dataset
    sagar_freq_final,
    
    # Join on the matching scientific name columns
    by = c("scinam" = "species")
  )

write.csv2(freq_combined, "Data/1_experts/peak_frequency_detections/peak_frequencies_mikula_sagar.csv", row.names = FALSE)


