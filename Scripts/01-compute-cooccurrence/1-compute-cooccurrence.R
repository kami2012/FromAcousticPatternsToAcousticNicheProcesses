rm(list=ls(all=TRUE))
Sys.setenv(LANG = "en")

# Working directory
setwd("E:/Manuscripts/4_co-occurence") 

library("cooccur")
library("dplyr")

## ============================================================================================================ ##
## Run probabilistic co-occurrence analysis 


#--------------
## Load data ##
#--------------

data_complete <- read.csv2("Data/Experts_detections/detections_freile_gelis_2021_2022_birds_dummy_pivot_reordered.csv", header=T, check.names = F)

# Remove plots with all zero values (ACN1) and plots without context information (CR16)
data_filtered <- data_complete[!data_complete$plot %in% c("ACN1", "CR16"), ] # => 85 plots

write.csv2(data_filtered, "Data/Experts_detections/bird_presence_absence_per_file.csv", row.names = FALSE)


#---------------------------------------------
## Preprocessing ##
#---------------------------------------------

# Split by plot
data_per_plot <- split(data_filtered, data_filtered$plot)


# Prepare numeric matrices
matrices <- lapply(data_per_plot, function(df) {
  # Select only species columns
  species_cols <- names(df)[!names(df) %in% c("plot", "filename")]
  matrices_numeric <- as.matrix(df[, species_cols])
  rownames(matrices_numeric) <- df$filename           # recordings
  colnames(matrices_numeric) <- make.names(colnames(matrices_numeric))  # species
  t(matrices_numeric)                                 # transpose: rows = species, columns = sites
})


#Remove singletons and records without detections
matrices_filtered <- lapply(matrices, function(mat) {
  # Remove singletons (species occurring only once across all sites)
  mat <- mat[rowSums(mat) > 1, , drop = FALSE]  # keep species with >1 occurrence
  
  # Remove records without detections
  mat <- mat[, colSums(mat) > 0, drop = FALSE]
  mat
})

#------------------------
# Run cooccurence analysis
#------------------------

results_cooccurrence <- lapply(names(matrices_filtered), function(plot_name) {
  mat_site_spp <- matrices_filtered[[plot_name]]
  
  tryCatch({
    cooccur(
      mat = mat_site_spp,
      type = "spp_site",
      thresh = TRUE,
      spp_names = TRUE
    )
  }, error = function(e) {
    message("Error in plot: ", plot_name, " -> ", e$message)
    return(NULL)
  })
})
names(results_cooccurrence) <- names(matrices_filtered)

save(results_cooccurrence, file = "Data/Co-occurrence/cooccurrence_results.RData")




