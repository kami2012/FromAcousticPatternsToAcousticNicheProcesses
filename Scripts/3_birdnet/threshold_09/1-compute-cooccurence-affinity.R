rm(list=ls(all=TRUE))
Sys.setenv(LANG = "en")

# Working directory
setwd("E:/Manuscripts/4_co-occurence") 

library("CooccurrenceAffinity")
library("dplyr")

## ============================================================================================================ ##
## Run probabilistic co-occurrence analysis 


#--------------
## Load data ##
#--------------

data_complete <- read.table("Data/3_birdnet/threshold_09/birdNET_threshold09.csv", header=T, check.names = F,sep=";")

# Remove plots with all zero values (ACN1) and plots without context information (CR16)
data_filtered <- data_complete[!data_complete$plot %in% c("ACN1", "CR16"), ] # => 85 plots

# Replace spaces with underscores
colnames(data_filtered)[3:ncol(data_filtered)] <- gsub(" ", "_", colnames(data_filtered)[3:ncol(data_filtered)])

# Drop the column "Asemospiza_obscura"
data_filtered <- data_filtered[, !names(data_filtered) %in% "Asemospiza_obscura"]

# Merge Hafferia_immaculata and Hafferia_zeledoni
data_filtered$Hafferia_immaculata <- pmax(data_filtered$Hafferia_immaculata, data_filtered$Hafferia_zeledoni, na.rm = TRUE)

# Delete the Hafferia_zeledoni column
data_filtered$Hafferia_zeledoni <- NULL

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



#-------------------------------
# Run CooccurrenceAffinity analysis
#-------------------------------

results_affinity <- lapply(names(matrices_filtered), function(plot_name) {
  mat_site_spp <- matrices_filtered[[plot_name]]
  tryCatch({
    affinity(mat_site_spp, row.or.col = "row")
  }, error = function(e) {
    message("Error in plot ", plot_name, ": ", e$message)
    return(NULL)
  })
})
names(results_affinity) <- names(matrices_filtered)

save(results_affinity, file="Data/3_birdnet/threshold_09/results_affinity.RData")


#-------------------------------
# Define classify_pair() using AlphInts() and confidence intervals (Blaker preferred)
#-------------------------------

classify_pair <- function(row) {
  marg <- c(row["mA"], row["mB"], row["N"])
  x <- as.integer(row["x"])
  
  ai <- tryCatch(AlphInts(x = x, marg = marg, lev = 0.95), error = function(e) NULL)
  if(is.null(ai)) return("Unclassifiable")
  
  # CI-Blaker priorisieren, sonst CI-midP
  #ci <- if(!is.null(ai$CI.Blaker)) ai$CI.Blaker else if(!is.null(ai$CI.midP)) ai$CI.midP else return("Unclassifiable")
  ci <- if(!is.null(ai$CI.midP)) ai$CI.midP else return("Unclassifiable")
  lower <- ci[1]; upper <- ci[2]
  
  if(lower > 0) return("Positive")
  if(upper < 0) return("Negative")
  return("Random")
}


results_summary <- lapply(names(results_affinity), function(plot_name) {
  af_list <- results_affinity[[plot_name]]
  
  # Check list exists and has non-empty 'all' data frame
  if(is.null(af_list) || is.null(af_list$all) || nrow(af_list$all) == 0) {
    return(data.frame(
      Plot = plot_name,
      Species = nrow(matrices_filtered[[plot_name]]),
      Sites = ncol(matrices_filtered[[plot_name]]),
      Positive = NA_integer_,
      Negative = NA_integer_,
      Random = NA_integer_,
      Unclassifiable = NA_integer_,
      NonRandomPercent = NA_real_,
      stringsAsFactors = FALSE
    ))
  }
  
  af <- af_list$all  # extract the data frame of pairwise results
  classes <- apply(af, 1, classify_pair)
  
  data.frame(
    Plot = plot_name,
    Species = nrow(matrices_filtered[[plot_name]]),
    Sites = ncol(matrices_filtered[[plot_name]]),
    Positive = sum(classes == "Positive", na.rm = TRUE),
    Negative = sum(classes == "Negative", na.rm = TRUE),
    Random = sum(classes == "Random", na.rm = TRUE),
    Unclassifiable = sum(classes == "Unclassifiable", na.rm = TRUE),
    NonRandomPercent = 100 * sum(classes %in% c("Positive", "Negative")) / sum(classes %in% c("Positive","Negative","Random")),
    stringsAsFactors = FALSE
  )
})

summary_df <- do.call(rbind, results_summary)
write.table(summary_df, "Data/1_experts/results_affinity_summary.csv", sep=";", row.names=FALSE)








#------------------------
# Save Summary as text
#------------------------

# Datei, in die alle Summaries gespeichert werden
file_summary <- "Data/1_experts/all_cooccur_summaries.txt"

# Leere Datei erstellen / alte Datei überschreiben
cat("", file = file_summary)

# Loop durch alle Plots
for (plot_name in names(results_per_plot)) {
  res <- results_per_plot[[plot_name]]
  
  cat("\n==============================\n", file = file_summary, append = TRUE)
  cat("Summary for plot:", plot_name, "\n", file = file_summary, append = TRUE)
  cat("==============================\n", file = file_summary, append = TRUE)
  
  # Ausgabe der Summary in Textform abfangen
  summary_text <- capture.output(summary(res))
  
  # In die Datei schreiben
  cat(summary_text, sep = "\n", file = file_summary, append = TRUE)
}

#------------------------
# Save Summary as dataframe
#------------------------

summary_df <- do.call(rbind, lapply(names(results_per_plot), function(plot_name) {
  res <- results_per_plot[[plot_name]]
  if (is.null(res)) return(NULL)
  
  s_text <- capture.output(summary(res))
  
  # Find the last line that starts with numbers (the summary line)
  line_summary <- tail(grep("^[[:space:]]*[0-9]", s_text, value = TRUE), 1)
  
  # Extract numeric values
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

# Inspect final dataframe
summary_df

write.table(summary_df, "Data/1_experts/results_cooccurrence_summary.csv", sep = ";", row.names = FALSE)



