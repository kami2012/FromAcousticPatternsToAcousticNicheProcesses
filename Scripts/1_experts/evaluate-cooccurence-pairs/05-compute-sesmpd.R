rm(list=ls(all=TRUE))
Sys.setenv(LANG = "en")

# Working directory
setwd("E:/Manuscripts/4_co-occurence") 

library(dplyr)
library(tidyr)

## ============================================================================================================ ##
## Compute Standardized Effect Size of Mean Pairwise Distance (SES-MPD) based on trait data and peak frequencies


#--------------
## Load data ##
#--------------

dat <- read.csv2("Data/1_experts/cooccurence_pairs.csv") # co-occurence pairs and distances
species_pool <- read.csv("Data/1_experts/bird_presence_absence_per_file.csv")


# Convert the 'Label' column to an ordered factor
dat$Label_ord<-ordered(dat$Label,c("A_P" , "A_C" , "F_P" , "F_C" , "F_P_RegI", "F_C_RegI", "A_Old" , "F_P_RegII", "F_C_RegII","F_Old"))


#--------------
## Prepare community matrix ##
#--------------

# rows = plots
# columns = species/birds
# cells = presence(1)/absence(0)

# Filter only positive co-occurrence pairs
dat_pos <- dat %>% filter(type == "positive")

# Extract species per plot:
# - Take both species columns (sp1_name and sp2_name)
# - Pivot to long format to get one row per species per plot
# - Keep only distinct plot-species combinations
plots_species <- dat_pos %>%
  select(plot, sp1_name, sp2_name) %>%
  pivot_longer(cols = c(sp1_name, sp2_name), values_to = "species") %>%
  distinct(plot, species) %>%
  mutate(presence = 1)

# Convert to wide format to create the community matrix
# - Rows = plots
# - Columns = species
# - Missing species entries filled with 0 (absence)
comm <- plots_species %>%
  pivot_wider(names_from = species, values_from = presence, values_fill = 0)

# Sort species alphabetically
comm <- comm %>%
  select(plot, sort(names(.)[-1]))  # keep plot column first

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
plot_species <- species_pool %>%
  select(-filename) %>%  # remove filename column
  group_by(plot) %>%
  summarise(across(everything(), ~ as.integer(any(. == 1)))) %>%  # each species: 1 if present at least once, else 0
  ungroup()

# Optional: convert to a list of species per plot
plot_species_list <- plot_species %>%
  rowwise() %>%
  mutate(
    species = list(names(pick(-plot)[which(c_across(-plot) == 1)]))
  ) %>%
  select(plot, species)



#--------------
## Prepare randomized community matrices (= null-model) ##
#--------------

set.seed(123)  

n_rand <- 999 

# Generate a list of randomized communities
# Each random community preserves the number of co-occurrence species observed per plot
random_comms <- lapply(1:n_rand, function(i) {
  plot_species_list %>%   # use the list of species per plot
    rowwise() %>%
    mutate(
      # Number of species to sample = number of observed species in the co-occurrence matrix
      n_species_obs = sum(comm[comm$plot == plot, -1]),  
      
      # Sample randomly from all species present on this plot in the species pool
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
    select(plot, species_rand)
})
# Each element of random_comms is a tibble with 'plot' and 'species_rand' (randomized species list)

## Convert random species lists to full community matrices

# Get the full species pool (all species names, sorted)
all_species <- sort(names(plot_species)[-1])  # exclude 'plot' column

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
## Prepare distance matrix ##
#--------------






# Prepare square plot x species presence/absence matrix
comm <- dat %>%
  dplyr::select(plot, sp1_name, sp2_name) %>%
  # long species list
  tidyr::pivot_longer(cols = c(sp1_name, sp2_name), names_to = "role", values_to = "species") %>%
  distinct(plot, species) %>% 
  mutate(value = 1) %>%
  tidyr::pivot_wider(
    names_from = species,
    values_from = value,
    values_fill = 0
  ) %>%
  tibble::column_to_rownames("plot") %>%
  as.matrix()

comm <- comm[, species, drop = FALSE]


#--------------
## SESMPD (Standardized Effect Size of Mean Pairwise Distance) ##
#--------------

# Prepare square species × species distance matrix
dat_traits <- dat %>%
  dplyr::select(sp1_name, sp2_name, traits_distance)

dat_traits_clean <- dat_traits %>%
  group_by(sp1_name, sp2_name) %>%
  summarise(
    traits_distance = dplyr::first(traits_distance),
    .groups = "drop"
  )

species <- sort(unique(c(dat_traits_clean$sp1_name, dat_traits_clean$sp2_name)))

dist_matrix <- dat_traits_clean %>%
  tidyr::pivot_wider(
    names_from = sp2_name,
    values_from = traits_distance
  ) %>%
  tibble::column_to_rownames("sp1_name") %>%
  as.matrix()

# add missing rows
missing_rows <- setdiff(species, rownames(dist_matrix))
if (length(missing_rows) > 0) {
  dist_matrix <- rbind(
    dist_matrix,
    matrix(NA, nrow = length(missing_rows), ncol = ncol(dist_matrix),
           dimnames = list(missing_rows, colnames(dist_matrix)))
  )
}

# add missing columns
missing_cols <- setdiff(species, colnames(dist_matrix))
if (length(missing_cols) > 0) {
  dist_matrix <- cbind(
    dist_matrix,
    matrix(NA, nrow = nrow(dist_matrix), ncol = length(missing_cols),
           dimnames = list(rownames(dist_matrix), missing_cols))
  )
}

dist_matrix <- dist_matrix[species, species]
dist_matrix[is.na(dist_matrix)] <- NA  # don’t force zeros
diag(dist_matrix) <- 0

# Prepare square plot x species presence/absence matrix
comm <- dat %>%
  dplyr::select(plot, sp1_name, sp2_name) %>%
  # long species list
  tidyr::pivot_longer(cols = c(sp1_name, sp2_name), names_to = "role", values_to = "species") %>%
  distinct(plot, species) %>% 
  mutate(value = 1) %>%
  tidyr::pivot_wider(
    names_from = species,
    values_from = value,
    values_fill = 0
  ) %>%
  tibble::column_to_rownames("plot") %>%
  as.matrix()

comm <- comm[, species, drop = FALSE]

ses.mpd(comm, dist_matrix, null.model = "taxa.labels", runs = 999)