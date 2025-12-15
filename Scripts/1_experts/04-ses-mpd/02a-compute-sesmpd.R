rm(list=ls(all=TRUE))
Sys.setenv(LANG = "en")

# Working directory
setwd("E:/Manuscripts/4_co-occurence") 

library(dplyr)
library(tidyr)
library(picante)

## ============================================================================================================ ##
## Compute Standardized Effect Size of Mean Pairwise Distance (SES-MPD) based on trait data and peak frequencies


#--------------
## Load data ##
#--------------

dat <- read.csv2("Data/1_experts/co-occurrences/cooccurrence_pairs.csv") # co-occurence pairs and distances
species_pool <- read.csv2("Data/1_experts/bird_presence_absence_per_file.csv")

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
## Prepare community matrix ##
#--------------

# rows = plots
# columns = species/birds
# cells = presence(1)/absence(0)

# Filter only positive co-occurrence pairs
dat_pos <- dat %>% filter(type == "positive")

# Filter only negative co-occurrence pairs
# dat_pos <- dat %>% filter(type == "negative")

# Extract species per plot:
# - Take both species columns (sp1_name and sp2_name)
# - Pivot to long format to get one row per species per plot
# - Keep only distinct plot-species combinations
plot_species_144 <- dat_pos %>%
  dplyr::select(plot, sp1_name, sp2_name) %>%
  pivot_longer(cols = c(sp1_name, sp2_name), values_to = "species") %>%
  distinct(plot, species) %>%
  mutate(presence = 1)

# Convert to wide format to create the community matrix
# - Rows = plots
# - Columns = species
# - Missing species entries filled with 0 (absence)
comm <- plot_species_144 %>%
  pivot_wider(names_from = species, values_from = presence, values_fill = 0)

# Sort species alphabetically
comm <- comm %>%
  dplyr::select(plot, sort(names(.)[-1]))  # keep plot column first

# Save plot names separately
plot_names <- comm$plot

# Convert to matrix for SES-MPD calculations
comm_matrix <- as.matrix(comm[,-1])
rownames(comm_matrix) <- plot_names

# Define uniform plot order from the observed matrix
plot_order <- sort(rownames(comm_matrix))

# Reorder observed community matrix
comm_matrix <- comm_matrix[plot_order, , drop = FALSE]

#--------------
## Prepare species pool ##
#--------------

# Aggregate by plot: for each species, check if it occurs in any file in that plot
plot_species_334 <- species_pool %>%
  dplyr::select(-filename) %>%  # remove filename column
  group_by(plot) %>%
  summarise(across(everything(), ~ as.integer(any(. == 1)))) %>%  # each species: 1 if present at least once, else 0
  ungroup()

exclude_species <- c("Ara_ambiguus")

cols_to_keep <- names(plot_species_334)[!names(plot_species_334) %in% exclude_species]
plot_species_334 <- plot_species_334 %>% 
  dplyr::select(all_of(cols_to_keep))

# Convert to a list of species per plot
plot_species_334_list <- plot_species_334 %>%
  rowwise() %>%
  mutate(
    species = list(names(pick(-plot)[which(c_across(-plot) == 1)]))
  ) %>%
  dplyr::select(plot, species)

# Synchronize the plot list with the observed community matrix (comm)
plot_species_334_list <- plot_species_334_list %>%
  filter(plot %in% plot_order) # Use 'plot_order' or 'comm$plot'

#--------------
## Prepare randomized community matrices (= null-model) ##
#--------------

set.seed(123)  

n_rand <- 999 

# Generate a list of randomized communities
# Each random community preserves the number of co-occurrence species observed per plot
random_comms <- lapply(1:n_rand, function(i) {
  plot_species_334_list %>%   # use the list of species per plot
    rowwise() %>%
    mutate(
      # The value of 'plot' (from the current row) is compared to the 'comm$plot' column.
      # The 'which' function returns the row index in 'comm' that matches the current plot.
      plot_row_index = which(comm$plot == plot),
      
      # Number of species to sample = row sum of the matched row in comm, excluding the first column
      n_species_obs = sum(comm[plot_row_index, -1]),
      species_rand = list(
        if(n_species_obs > 0){
          sample(
            species,    
            size = n_species_obs,
            replace = FALSE
          )
        } else {
          character(0)
        }
      )
    ) %>%
    dplyr::select(plot, species_rand)
})
# Each element of random_comms is a tibble with 'plot' and 'species_rand' (randomized species list)

## Convert random species lists to full community matrices

# Get the full species pool (all species names, sorted)
all_species <- sort(names(plot_species_334)[-1])  # exclude 'plot' column

# Function to turn a single randomized species list into a matrix
random_comm_to_matrix <- function(rand_list, species_pool = all_species) {
  rand_matrix <- lapply(rand_list$species_rand, function(sp) {
    # Create a 0/1 vector for all species in the pool
    vec <- integer(length(species_pool))
    names(vec) <- species_pool
    if(length(sp) > 0){
      vec[sp] <- 1
    }
    return(vec)
  })
  # Combine into a matrix
  rand_matrix <- do.call(rbind, rand_matrix)
  rownames(rand_matrix) <- rand_list$plot
  return(rand_matrix)
}

# Apply to all randomized communities
random_comm_matrices <- lapply(random_comms, random_comm_to_matrix)

# Reorder every randomized community matrix to same plot order as comm_matrix
random_comm_matrices <- lapply(random_comm_matrices, function(m){
  m[plot_order, , drop = FALSE]
})

#--------------
## Load distance matrices (Full Species Pool, ca. N=334) ##
#--------------

dist_peak_full <- read.csv2("Data/1_experts/dist_matrices/dist_peak_matrix_334.csv", row.names = 1) 
load("Data/1_experts/dist_matrices/distM.rda")
dist_trait_full <- distM

# Convert to matrix
dist_peak_full <- as.matrix(dist_peak_full)
dist_trait_full <- as.matrix(dist_trait_full)


#--------------
## Compute ses-mpd for peak frequency distances ##
#--------------

obs_mpd_peak <- mpd(comm_matrix, dist_peak_full, abundance.weighted = FALSE)
rand_mpd_peak <- sapply(random_comm_matrices, mpd, dis = dist_peak_full, abundance.weighted = FALSE)
ses_mpd_peak <- (obs_mpd_peak - rowMeans(rand_mpd_peak)) / apply(rand_mpd_peak, 1, sd)

# p-value
p_obs_gt_null_peak <- (rowSums(rand_mpd_peak >= obs_mpd_peak) + 1) / (n_rand + 1)
p_obs_lt_null_peak <- (rowSums(rand_mpd_peak <= obs_mpd_peak) + 1) / (n_rand + 1)
p_value_peak <- 2 * pmin(p_obs_lt_null_peak, p_obs_gt_null_peak)


#--------------
## Compute ses-mpd for trait distances ##
#--------------

obs_mpd_trait <- mpd(comm_matrix, dist_trait_full, abundance.weighted = FALSE)
rand_mpd_trait <- sapply(random_comm_matrices, mpd, dis = dist_trait_full, abundance.weighted = FALSE)
ses_mpd_trait <- (obs_mpd_trait - rowMeans(rand_mpd_trait)) / apply(rand_mpd_trait, 1, sd)

# p-value
p_obs_gt_null_trait <- (rowSums(rand_mpd_trait >= obs_mpd_trait) + 1) / (n_rand + 1)
p_obs_lt_null_trait <- (rowSums(rand_mpd_trait <= obs_mpd_trait) + 1) / (n_rand + 1)
p_value_trait <- 2 * pmin(p_obs_lt_null_trait, p_obs_gt_null_trait)


#--------------
## Combine results ##
#--------------

ses_results_df <- data.frame(
  plot = plot_order, 
  SES_MPD_Peak = ses_mpd_peak,
  P_Value_Peak = p_value_peak,
  SES_MPD_Trait = ses_mpd_trait,
  P_Value_Trait = p_value_trait
)

write.csv2(ses_results_df, "Data/1_experts/ses-mpd/ses-mpd_results_positive.csv", row.names = TRUE)
#write.csv2(ses_results_df, "Data/1_experts/ses-mpd/ses-mpd_results_negative.csv", row.names = TRUE)










plot_labels <- dat %>%
  dplyr::select(plot, Label_ord) %>%
  distinct(plot, .keep_all = TRUE) 

# 2. SES-Ergebnisse mit den Kategorien zusammenführen
ses_results_with_labels <- merge(
  ses_results_df,
  plot_labels,
  by = "plot",
  all.x = TRUE
)

# Aggregation, z.B. Berechnung des Mittelwerts und des Standardfehlers pro Kategorie
ses_summary <- ses_results_with_labels %>%
  group_by(Label_ord) %>%
  summarise(
    N_Plots = n(),
    Mean_SES_Peak = mean(SES_MPD_Peak, na.rm = TRUE),
    SE_SES_Peak = sd(SES_MPD_Peak, na.rm = TRUE) / sqrt(N_Plots),
    Mean_SES_Trait = mean(SES_MPD_Trait, na.rm = TRUE),
    SE_SES_Trait = sd(SES_MPD_Trait, na.rm = TRUE) / sqrt(N_Plots)
  ) %>%
  ungroup()

## ============================================================================================================ ##
## Compute Significance of Aggregated Mean SES (T-Tests)

# Function to perform one-sample t-test against mu=0 and extract P-value and 95% CI
run_t_test_and_extract <- function(x) {
  # Ensure data is numeric and remove NAs
  x <- as.numeric(na.omit(x))
  
  # If not enough data points, return NA
  if (length(x) < 2) {
    return(list(P_Value = NA, CI_Lower = NA, CI_Upper = NA))
  }
  
  # Perform the one-sample t-test against the null hypothesis mu = 0
  t_res <- t.test(x, mu = 0)
  
  return(list(
    P_Value = t_res$p.value,
    CI_Lower = t_res$conf.int[1],
    CI_Upper = t_res$conf.int[2]
  ))
}


# Aggregation, Berechnung des Mittelwerts, des Standardfehlers und der Signifikanz pro Kategorie
ses_summary_final <- ses_results_with_labels %>%
  group_by(Label_ord) %>%
  summarise(
    N_Plots = n(),
    
    # ----------------- Peak Frequency (Akustik) -----------------
    Mean_SES_Peak = mean(SES_MPD_Peak, na.rm = TRUE),
    SE_SES_Peak = sd(SES_MPD_Peak, na.rm = TRUE) / sqrt(N_Plots),
    
    # Apply T-Test for Peak Frequency
    Peak_t_test_results = list(run_t_test_and_extract(SES_MPD_Peak)),
    
    # ----------------- Non-Vocal Traits (Ökologie/Morphologie) -----------------
    Mean_SES_Trait = mean(SES_MPD_Trait, na.rm = TRUE),
    SE_SES_Trait = sd(SES_MPD_Trait, na.rm = TRUE) / sqrt(N_Plots),
    
    # Apply T-Test for Non-Vocal Traits
    Trait_t_test_results = list(run_t_test_and_extract(SES_MPD_Trait))
  ) %>%
  ungroup() %>%
  
  # Entpacken der T-Test-Ergebnisse in separate Spalten
  rowwise() %>%
  mutate(
    P_Value_Peak = Peak_t_test_results$P_Value,
    CI_Lower_Peak = Peak_t_test_results$CI_Lower,
    CI_Upper_Peak = Peak_t_test_results$CI_Upper,
    
    P_Value_Trait = Trait_t_test_results$P_Value,
    CI_Lower_Trait = Trait_t_test_results$CI_Lower,
    CI_Upper_Trait = Trait_t_test_results$CI_Upper
  ) %>%
  ungroup() %>%
  dplyr::select(-Peak_t_test_results, -Trait_t_test_results) # Bereinigtes DataFrame

## ============================================================================================================ ##
## Final Output and Saving

# Ausgabe zur Kontrolle
print(ses_summary_final)

# Speichern der finalen aggregierten Ergebnisse
#write.csv2(ses_summary_final, "Data/1_experts/ses-mpd/ses_mpd_agg_phylo-corrected_positive.csv", row.names = FALSE)
write.csv2(ses_results_df, "Data/1_experts/ses-mpd/ses-mpd_agg_phylo-corrected_negative.csv", row.names = TRUE)

