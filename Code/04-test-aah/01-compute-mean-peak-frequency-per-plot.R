rm(list=ls(all=TRUE))
Sys.setenv(LANG = "en")

library(dplyr)

## ============================================================================================================ ##
## Test the Acoustic Adaptation Hypothesis 


#--------------
## Load data ##
#--------------

species_pool <- read.csv2("Data/Experts_detections/bird_presence_absence_per_file.csv")
peak_frequencies <- read.csv2("Data/Peak_frequencies/peak_frequencies_mikula_sagar_kuemmet.csv")
mapping <- read.csv2("Data/Experts_detections/bird_name_mapping_experts_xeno-canto.csv")
traits <- read.csv2("Data/Traits_phylo/CommunityAllplots334BirdSpecies_TraitsAvonet.csv")


#--------------
## Prepare peak frequency  ##
#--------------

peak_frequencies$peak_freq_sagar_kuemmet <- ifelse(
  !is.na(peak_frequencies$dom_freq_sagar),  # condition: If Sagar's data is available
  peak_frequencies$dom_freq_sagar,          # if TRUE: Use Sagar's data
  peak_frequencies$peak_freq_kuemmet        # else: Use Kuemmet's data
)

# Remove all species that lack a frequency value (Ara ambiguus) 
peak_frequencies <- peak_frequencies[!is.na(peak_frequencies$peak_freq_sagar_kuemmet), ]

# Convert peak_freq_sagar_kuemmet to numeric
peak_frequencies$peak_freq_sagar_kuemmet <- as.numeric(gsub(",", ".", peak_frequencies$peak_freq_sagar_kuemmet))

# Replace spaces with underscores in mapping
mapping$ExpertName <- gsub(" ", "_", mapping$ExpertName)
mapping$XenoCantoName <- gsub(" ", "_", mapping$XenoCantoName)

peak_frequencies$scinam <- gsub(" ", "_", peak_frequencies$scinam)

# Remap scinam using mapping: XenoCantoName → ExpertName
idx <- match(peak_frequencies$scinam, mapping$XenoCantoName)
valid <- !is.na(idx)
peak_frequencies$scinam[valid] <- mapping$ExpertName[idx[valid]]


#--------------
## Prepare peak frequency lookup table ##
#--------------

peak_frequencies_map <- peak_frequencies %>%
  dplyr::select(scinam, peak_freq_sagar_kuemmet) %>%
  distinct()

#--------------
## Correct peak frequency for mass ##
#--------------

peak_frequencies_map <- peak_frequencies_map %>%
  left_join(
    traits %>% dplyr::select(Name.Phylogeny.Latin, Mass), 
    by = c("scinam" = "Name.Phylogeny.Latin")
  )

model <- lm(log10(peak_freq_sagar_kuemmet) ~ log10(Mass), data = peak_frequencies_map)
peak_frequencies_map$freq_resid <- resid(model)

#--------------
## Lookup peak frequencies ##
#--------------

long_freq_data <- species_pool %>%
  dplyr::select(-filename) %>%
  tidyr::pivot_longer(
    cols = -plot,
    names_to = "scinam",
    values_to = "presence"
  ) %>%
  dplyr::filter(presence == 1) %>%
  left_join(
    peak_frequencies_map,
    by = c("scinam" = "scinam") 
  ) %>%
  filter(!is.na(freq_resid))


# --- (Unique) species per plot (unweighted mean) ---

mean_unweighted <- long_freq_data %>%
  dplyr::distinct(plot, scinam, .keep_all = TRUE) %>% 
  group_by(plot) %>%
  summarise(
    mean_freq_UNWEIGHTED = mean(freq_resid),
    sd_freq_UNWEIGHTED = sd(freq_resid),
    num_unique_species = n() 
  )


# --- (All) detections per plot (weighted mean) ---

mean_weighted <- long_freq_data %>%
  group_by(plot) %>%
  summarise(
    mean_freq_WEIGHTED = mean(freq_resid),
    sd_freq_WEIGHTED = sd(freq_resid),
    num_total_detections = n() 
  )


# --- Aggregate results ---

plot_mean_peak_freq_combined <- mean_unweighted %>%
  left_join(mean_weighted, by = "plot")
  
  
write.csv2(plot_mean_peak_freq_combined, "Data/Testing_AAH/mean_peak_frequency_per_plot_mass_corrected.csv", row.names = FALSE) 
  
  
  
  
  
  
  
  
