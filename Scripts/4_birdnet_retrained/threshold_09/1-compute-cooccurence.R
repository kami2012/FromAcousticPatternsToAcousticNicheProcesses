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

data_complete <- read.table("Data/4_birdnet_retrained/threshold_09/birdNET_retrained_threshold09.csv", header=T, check.names = F, sep=";")

# Remove plots with all zero values (ACN1) and plots without context information (CR16)
data_filtered <- data_complete[!data_complete$plot %in% c("ACN1", "CR16"), ] # => 85 plots

# Replace spaces with underscores
colnames(data_filtered)[3:ncol(data_filtered)] <- gsub(" ", "_", colnames(data_filtered)[3:ncol(data_filtered)])

write.csv(data_filtered, "Data/4_birdnet_retrained/threshold_09/bird_presence_absence_per_file.csv", row.names = FALSE)

#split by plot
data_per_plot <- split(data_filtered, data_filtered$plot)



#---------------------------------------------
## Preprocessing ##
#---------------------------------------------

# Prepare numeric matrices
matrices <- lapply(data_per_plot, function(df) {
  mat_recording <- df[, -2]                                # remove column "plot"
  mat_recording_numeric <- as.matrix(mat_recording[, -1])  # remove column "filename" and convert to numeric matrix
  rownames(mat_recording_numeric) <- df$filename           # recordings
  colnames(mat_recording_numeric) <- make.names(colnames(mat_recording_numeric))  # species
  t(mat_recording_numeric)                                 # transpose: rows = species, columns = sites
})


#Remove singletons and records without detections
matrices_filtered <- lapply(matrices, function(mat) {
  # Remove singletons (species occurring only once across all sites)
  mat <- mat[rowSums(mat) > 1, , drop = FALSE]  # keep species with >1 occurrence
  
  # Remove Records without detections
  mat <- mat[, colSums(mat) > 0, drop = FALSE]
  mat
})

#------------------------
# Run Cooccurrence analysis
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

save(results_cooccurrence, file = "Data/4_birdnet_retrained/threshold_09/results_cooccurrence.RData")

#------------------------
# Save Summary as text
#------------------------

file_summary <- "Data/4_birdnet_retrained/threshold_09/all_cooccur_summaries.txt"
cat("", file = file_summary)

for (plot_name in names(results_cooccurrence)) {
  res <- results_cooccurrence[[plot_name]]
  if (is.null(res)) next
  
  cat("\n==============================\n", file = file_summary, append = TRUE)
  cat("Summary for plot:", plot_name, "\n", file = file_summary, append = TRUE)
  cat("==============================\n", file = file_summary, append = TRUE)
  
  summary_text <- capture.output(summary(res))
  cat(summary_text, sep = "\n", file = file_summary, append = TRUE)
}

#------------------------
# Save Summary as dataframe
#------------------------

summary_df <- do.call(rbind, lapply(names(results_cooccurrence), function(plot_name) {
  res <- results_cooccurrence[[plot_name]]
  if (is.null(res)) return(NULL)
  
  s_text <- capture.output(summary(res))
  line_summary <- tail(grep("^[[:space:]]*[0-9]", s_text, value = TRUE), 1)
  
  vals <- as.numeric(unlist(strsplit(trimws(line_summary), "\\s+")))
  names(vals) <- c("Species", "Sites", "Positive", "Negative", "Random", "Unclassifiable", "NonRandomPercent")
  
  data.frame(
    Plot = plot_name,
    Species = vals["Species"],
    Sites = vals["Sites"],
    Positive = vals["Positive"],
    Negative = vals["Negative"],
    Random = vals["Random"],
    Unclassifiable = vals["Unclassifiable"],
    NonRandomPercent = vals["NonRandomPercent"],
    stringsAsFactors = FALSE
  )
}))

row.names(summary_df) <- NULL

summary_df

write.table(summary_df, "Data/4_birdnet_retrained/threshold_09/results_cooccurrence_summary.csv", sep = ";", row.names = FALSE)



