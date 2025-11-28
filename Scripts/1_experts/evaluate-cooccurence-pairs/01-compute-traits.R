rm(list=ls(all=TRUE))
Sys.setenv(LANG = "en")

# Working directory
setwd("E:/Manuscripts/4_co-occurence") 


## ============================================================================================================ ##
## Compute different traits based on peak frequencies, body mass,...


#--------------
## Load data ##
#--------------

peak_frequencies <- read.csv2("Data/1_experts/peak_frequency_detections/peak_frequencies.csv")
mapping <- read.csv2("Data/1_experts/bird_name_mapping_experts_xeno-canto.csv")
avonet <- read.csv2("Data/1_experts/traits/CommunityAllplots334BirdSpecies_TraitsAvonet.csv")

#--------------
## Peak frequencies  ##
#--------------

# Replace spaces with underscores in mapping
mapping$ExpertName <- gsub(" ", "_", mapping$ExpertName)
mapping$XenoCantoName <- gsub(" ", "_", mapping$XenoCantoName)

# Capitalize first letter of genus, keep species lowercase
peak_frequencies$scinam <- sapply(strsplit(peak_frequencies$scinam, "_"), function(x) {
  paste0(toupper(substr(x[1], 1, 1)), substr(x[1], 2, nchar(x[1])), "_", x[2])
})

# Remap scinam using mapping: XenoCantoName → ExpertName
peak_frequencies$scinam <- ifelse(
  peak_frequencies$scinam %in% mapping$XenoCantoName,
  mapping$ExpertName[match(peak_frequencies$scinam, mapping$XenoCantoName)],
  peak_frequencies$scinam
)

# Set species names as row identifiers
row.names(peak_frequencies) <- peak_frequencies$scinam

# Log-transform and scale
peak_frequencies_scaled <- scale(log(peak_frequencies$peak_frequency_xc))
row.names(peak_frequencies_scaled) <- row.names(peak_frequencies)

# Compute Euclidean distances
dist_peak <- dist(peak_frequencies_scaled, method = "euclidean")
dist_peak <- as.matrix(dist_peak)

write.csv2(dist_peak, "Data/1_experts/traits/dist_peak_matrix.csv", row.names = TRUE)


#--------------
## Body mass  ##
#--------------

# Replace commas with dots and convert Mass to numeric
avonet$Mass <- as.numeric(gsub(",", ".", avonet$Mass))

# Use species names as row names
row.names(avonet) <- avonet$Name.Phylogeny.Latin

# Align avonet with peak_frequencies$scinam
avonet_mass <- avonet[match(peak_frequencies$scinam, row.names(avonet)), "Mass", drop = FALSE]

# Log-transform and scale
avonet_mass_scaled <- scale(log(avonet_mass$Mass))
rownames(avonet_mass_scaled) <- peak_frequencies$scinam  # same row names as peak_frequencies

# Compute Euclidean distances
dist_mass <- dist(avonet_mass_scaled, method = "euclidean")
dist_mass <- as.matrix(dist_mass)

write.csv2(dist_mass, "Data/1_experts/traits/dist_mass_matrix.csv", row.names = TRUE)


#--------------
## Traits without body mass  ##
#--------------

# Use species names as row names
row.names(avonet) <- avonet$Name.Phylogeny.Latin

# Align avonet with peak_frequencies$scinam
idx <- match(peak_frequencies$scinam, row.names(avonet))
idx <- idx[!is.na(idx)]  # remove species not found
avonet <- avonet[idx, ]

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
avonet[numeric_cols] <- lapply(avonet[numeric_cols], convert_to_numeric)

####### model body size dependent traits and take residuals 
####### body mass in log(bodymass) 

traits <- avonet

traits$log_mass <- log(traits$Mass)

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


keep <- c("Hand-WingIndex","HabitatDensity","Migration","TrophicNiche","PrimaryLifestyle",
          "beak_length_res","beak_width_res","tarsus_res","wing_res","tail_res")


traits <- traits[,names(traits) %in% keep]



for (i in 1:ncol(traits)) {
  if (is.character(traits[, i])) traits[, i] <- factor(traits[, i], levels = unique(traits[, i]))
}

dist_traits_wo_mass <- cluster::daisy(x = traits, metric = "gower") %>% as.matrix()

write.csv2(dist_traits_wo_mass, "Data/1_experts/traits/traits_wo_mass_matrix.csv", row.names = TRUE)

