rm(list=ls(all=TRUE))
Sys.setenv(LANG = "en")

library(ape)

## ============================================================================================================ ##
## Compute distances based on peak frequencies, body mass, phylogeny, traits, ...


#--------------
## Load data ##
#--------------

peak_frequencies <- read.csv2("Data/Peak_frequencies/peak_frequencies_mikula_sagar_kuemmet.csv")
mapping <- read.csv2("Data/Experts_detections/bird_name_mapping_experts_xeno-canto.csv")
avonet <- read.csv2("Data/Traits_phylo/CommunityAllplots334BirdSpecies_TraitsAvonet.csv")
load("Data/Traits_phylo/bird_tree.rda") # Phylogenetic Tree

#--------------
## Peak frequency  ##
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

# Set species names as row identifiers
row.names(peak_frequencies) <- peak_frequencies$scinam

# Log-transform and scale
peak_frequencies_scaled <- scale(log(peak_frequencies$peak_freq_sagar_kuemmet))
row.names(peak_frequencies_scaled) <- row.names(peak_frequencies)

# Compute Euclidean distance matrix
dist_peak <- dist(peak_frequencies_scaled, method = "euclidean")
dist_peak <- as.matrix(dist_peak)

# Sort columns and rows 
ord <- sort(row.names(dist_peak))
dist_peak <- dist_peak[ord, ord]

write.csv2(dist_peak, "Data/Dist_matrices/dist_peak_matrix.csv", row.names = TRUE)


#--------------
## Body mass  ##
#--------------

bodyMass_avonet <- avonet

# Replace commas with dots and convert Mass to numeric
bodyMass_avonet$Mass <- as.numeric(gsub(",", ".", bodyMass_avonet$Mass))

# Check for zero or negative values
bodyMass_avonet$Mass[bodyMass_avonet$Mass <= 0]

# Use species names as row names
row.names(bodyMass_avonet) <- bodyMass_avonet$Name.Phylogeny.Latin

# Alphabetically sort species
avonet_mass <- bodyMass_avonet[order(row.names(bodyMass_avonet)), "Mass", drop = FALSE]

# Log-transform and scale
avonet_mass_scaled <- scale(log(avonet_mass$Mass))

# Keep row names consistent
rownames(avonet_mass_scaled) <- row.names(avonet_mass) 

# Compute Euclidean distances
dist_mass <- dist(avonet_mass_scaled, method = "euclidean")
dist_mass <- as.matrix(dist_mass)

write.csv2(dist_mass, "Data/Dist_matrices/dist_mass_matrix.csv", row.names = TRUE)


#--------------
## Traits without body mass  ##
#--------------

traits_avonet <- avonet

# Use species names as row names
row.names(traits_avonet) <- traits_avonet$Name.Phylogeny.Latin

# Alphabetically sort species
traits_avonet <- traits_avonet[order(row.names(traits_avonet)), ]

# List of columns that should be numeric
numeric_cols <- c("BeakLength_Culmen", "BeakLength_Nares", "BeakWidth", "BeakDepth", 
                  "TarsusLength", "WingLength", "KippsDistance", "Secondary1", 
                  "Hand.WingIndex", "TailLength", "Mass", "MinLatitude", 
                  "MaxLatitude", "CentroidLatitude", "CentroidLongitude", "RangeSize")

# Function to convert columns to numeric
convert_to_numeric <- function(column) {
  # Replace commas with dots
  column <- gsub(",", ".", column)
  # Convert to numeric
  as.numeric(column)
}

# Apply conversion to specified columns
traits_avonet[numeric_cols] <- lapply(traits_avonet[numeric_cols], convert_to_numeric)

####### model body size dependent traits and take residuals 
####### body mass in log(bodymass) 

# Log-transform Mass for residuals
traits <- traits_avonet
traits$log_mass <- log(traits$Mass)

# Compute residuals for body-size dependent traits
mod.beak.l <- lm(log(BeakLength_Culmen) ~ log(Mass), data = traits)
traits$beak_length_res <- mod.beak.l$residuals

mod.beak.w <- lm(log(BeakWidth) ~ log(Mass), data = traits)
traits$beak_width_res <- mod.beak.w$residuals

mod.tarsus <- lm(log(TarsusLength) ~ log(Mass), data = traits)
traits$tarsus_res <- mod.tarsus$residuals

mod.wing <- lm(log(WingLength) ~ log(Mass), data = traits)
traits$wing_res <- mod.wing$residuals

mod.tail <- lm(log(TailLength) ~ log(Mass), data = traits)
traits$tail_res <- mod.tail$residuals

# Keep only relevant traits
keep <- c("Hand.WingIndex","HabitatDensity","Migration","TrophicNiche","PrimaryLifestyle",
          "beak_length_res","beak_width_res","tarsus_res","wing_res","tail_res")
traits <- traits[,names(traits) %in% keep]

# Convert character columns to factors
for (i in 1:ncol(traits)) {
  if (is.character(traits[, i])) traits[, i] <- factor(traits[, i], levels = unique(traits[, i]))
}

# Compute Gower distance matrix
dist_traits_wo_mass <- cluster::daisy(x = traits, metric = "gower") %>% as.matrix()

write.csv2(dist_traits_wo_mass, "Data/Dist_matrices/dist_traits_wo_mass_matrix.csv", row.names = TRUE)


#--------------
## Phylogeny  ##
#--------------

# Create phylogenetic distances matrix
dist_phylo <- cophenetic(tr)

# Alphabetically sort species
ord <- sort(rownames(dist_phylo))
dist_phylo <- dist_phylo[ord, ord]

write.csv2(dist_phylo, "Data/Dist_matrices/dist_phylogeny.csv", row.names = TRUE)


#--------------
## Compute peak distances corrected for phylogenetic relatedness ##
#--------------

dist_peak <- read.csv2("Data/Dist_matrices/dist_peak_matrix.csv", row.names = 1)
dist_phylo <- read.csv2("Data/Dist_matrices/dist_phylogeny.csv", row.names = 1)

# Exclude species that have no peak frequencies
exclude_species <- c("Ara_ambiguus")

# Remove from phylogenetic distance matrix
dist_phylo <- dist_phylo[!rownames(dist_phylo) %in% exclude_species, 
                         !colnames(dist_phylo) %in% exclude_species]

# Check, if rows match
all(rownames(dist_phylo) == rownames(dist_peak))

# Check, if columns match
all(colnames(dist_phylo) == colnames(dist_peak))

species <- colnames(dist_phylo)

# Flatten only upper triangle (excluding diagonal)
y <- dist_peak[upper.tri(dist_peak)]
x <- dist_phylo[upper.tri(dist_phylo)]

# Fit linear model
mod <- lm(y ~ x)
residuals_vec <- residuals(mod)

# Number of elements in upper triangle
n_upper <- length(species) * (length(species) - 1) / 2
length(residuals_vec)  # should equal n_upper

# Initialize empty matrix
res_peak <- matrix(NA, nrow = length(species), ncol = length(species))
rownames(res_peak) <- colnames(res_peak) <- species

# Assign residuals to upper triangle
res_peak[upper.tri(res_peak)] <- residuals_vec

# Mirror to lower triangle to make symmetric
res_peak[lower.tri(res_peak)] <- t(res_peak)[lower.tri(res_peak)]

write.csv2(res_peak, "Data/Dist_matrices/dist_peak_matrix_phylo_corrected.csv", row.names = TRUE)

#--------------
## Compute peak distances corrected for body mass ##
#--------------

dist_peak <- read.csv2("Data/Dist_matrices/dist_peak_matrix.csv", row.names = 1)
dist_mass <- read.csv2("Data/Dist_matrices/dist_mass_matrix.csv", row.names = 1)

# Exclude species that have no peak frequencies
exclude_species <- c("Ara_ambiguus")

# Remove from mass distance matrix
dist_mass <- dist_mass[!rownames(dist_mass) %in% exclude_species, 
                       !colnames(dist_mass) %in% exclude_species]

# Check, if rows match
all(rownames(dist_mass) == rownames(dist_peak))

# Check, if columns match
all(colnames(dist_mass) == colnames(dist_peak))

species <- colnames(dist_mass)

# Flatten only upper triangle (excluding diagonal)
y <- dist_peak[upper.tri(dist_peak)]
x <- dist_mass[upper.tri(dist_mass)]

# Fit linear model
mod <- lm(y ~ x)
residuals_vec <- residuals(mod)

# Number of elements in upper triangle
n_upper <- length(species) * (length(species) - 1) / 2
length(residuals_vec)  # should equal n_upper

# Initialize empty matrix
res_peak <- matrix(NA, nrow = length(species), ncol = length(species))
rownames(res_peak) <- colnames(res_peak) <- species

# Assign residuals to upper triangle
res_peak[upper.tri(res_peak)] <- residuals_vec

# Mirror to lower triangle to make symmetric
res_peak[lower.tri(res_peak)] <- t(res_peak)[lower.tri(res_peak)]

write.csv2(res_peak, "Data/Dist_matrices/dist_peak_matrix_mass_corrected.csv", row.names = TRUE)

#--------------
## Compute peak distances corrected for BOTH Phylogeny and Body Mass ##
#--------------

dist_peak  <- read.csv2("Data/Dist_matrices/dist_peak_matrix.csv", row.names = 1)
dist_phylo <- read.csv2("Data/Dist_matrices/dist_phylogeny.csv", row.names = 1)
dist_mass  <- read.csv2("Data/Dist_matrices/dist_mass_matrix.csv", row.names = 1)

# Exclude specific species (e.g., Ara ambiguus) from all matrices
exclude <- c("Ara_ambiguus")

dist_phylo <- dist_phylo[!rownames(dist_phylo) %in% exclude, !colnames(dist_phylo) %in% exclude]
dist_mass  <- dist_mass[!rownames(dist_mass) %in% exclude, !colnames(dist_mass) %in% exclude]
dist_peak  <- dist_peak[!rownames(dist_peak) %in% exclude, !colnames(dist_peak) %in% exclude]

# Extract the upper triangles (excluding diagonals) to flatten the data
y_peak  <- dist_peak[upper.tri(dist_peak)]
x_phylo <- dist_phylo[upper.tri(dist_phylo)]
x_mass  <- dist_mass[upper.tri(dist_mass)]

# Fit a MULTIPLE linear regression model
# This isolates the variation in peak frequency distance that is NOT 
# explained by phylogeny OR body mass.
mod_both <- lm(y_peak ~ x_phylo + x_mass)

# Extract the residuals
residuals_both <- residuals(mod_both)

# Reconstruct the distance matrix
species <- colnames(dist_peak)
res_matrix <- matrix(NA, nrow = length(species), ncol = length(species))
rownames(res_matrix) <- colnames(res_matrix) <- species

# Assign residuals to the upper triangle
res_matrix[upper.tri(res_matrix)] <- residuals_both

# Mirror to the lower triangle to maintain symmetry
res_matrix[lower.tri(res_matrix)] <- t(res_matrix)[lower.tri(res_matrix)]

write.csv2(res_matrix, "Data/Dist_matrices/dist_peak_matrix_phylo_mass_corrected.csv", row.names = TRUE)

#--------------
## Compute trait distances corrected for phylogenetic relatedness ##
#--------------

load("Data/Dist_matrices/distM.rda") # Gower distance trait matrix (ecological and morphological traits) 
dist_phylo <- read.csv2("Data/Dist_matrices/dist_phylogeny.csv", row.names = 1)

dist_trait <- distM

dist_trait <- dist_trait[order(rownames(dist_trait)), order(colnames(dist_trait))]

# Check, if rows match
all(rownames(dist_trait) == rownames(dist_phylo))

# Check, if columns match
all(colnames(dist_trait) == colnames(dist_phylo))

species <- rownames(dist_trait)

# Flatten upper triangle
y <- dist_trait[upper.tri(dist_trait)]
x <- dist_phylo[upper.tri(dist_phylo)]

# Fit linear model to correct trait distances for phylogeny
mod <- lm(y ~ x)
residuals_vec <- residuals(mod)

# Initialize empty symmetric matrix
res_mat <- matrix(NA, nrow = length(species), ncol = length(species))
rownames(res_mat) <- colnames(res_mat) <- rownames(dist_trait)

# Fill upper triangle with residuals
res_mat[upper.tri(res_mat)] <- residuals_vec

# Mirror to lower triangle
res_mat[lower.tri(res_mat)] <- t(res_mat)[lower.tri(res_mat)]

write.csv2(res_mat, "Data/Dist_matrices/dist_trait_matrix_phylo_corrected.csv", row.names = TRUE)

