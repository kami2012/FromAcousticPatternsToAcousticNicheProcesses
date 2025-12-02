rm(list=ls(all=TRUE))
Sys.setenv(LANG = "en")

# Working directory
setwd("E:/Manuscripts/4_co-occurence") 

library(dplyr)
library(httr)
library(jsonlite)
library(methods)
library(seewave)
library(stringr)
library(tuneR)

## ============================================================================================================ ##
## Prepare bird song snippets based on expert labelled data


#--------------
## Load data ##
#--------------

freile_labels <- read.csv2("Data/1_experts/peak_frequency_detections/Ecuador_2022_labels_Freile.csv")
gelis_labels <- read.csv2("Data/1_experts/peak_frequency_detections/Ecuador_Gelis_labels_2022.csv")

peak_frequencies <- read.csv2("Data/1_experts/peak_frequency_detections/peak_frequencies.csv")
target_species <- peak_frequencies$scinam

#--------------
## Pick 4-5 songs from our expert annotated data (Freile)
#--------------

# Keep only songs
freile_songs <- freile_labels %>%
  filter(vocalization_type == "standard song")

# Remove overlapping segments per file
# Sort by filename and start time
freile_songs <- freile_songs %>%
  arrange(filename, start)

# For each file, keep only songs with no overlaps
freile_songs_no_overlap <- freile_songs %>%
  arrange(filename, start) %>%
  group_by(filename) %>%
  mutate(overlap = {
    n <- n()
    # single-row group
    if(n == 1) {
      rep(FALSE, 1)
    } else {
      # first row never overlaps
      ov <- logical(n)
      ov[1] <- FALSE
      for(i in 2:n){
        ov[i] <- any(start[i] < end[1:(i-1)])
      }
      ov
    }
  }) %>%
  filter(!overlap) %>%
  ungroup()

# Pick one recording per plot per species
one_per_plot <- freile_songs_no_overlap %>%
  group_by(species_latin, plot) %>%
  slice(1) %>%   # first occurrence in each plot
  ungroup()

# Take up to 5 recordings per species
five_per_species <- one_per_plot %>%
  group_by(species_latin) %>%
  slice_head(n = 5) %>%
  ungroup()

target_species <- combined_peak_frequencies$scinam  # or a subset of 144 target species

five_per_species_filtered <- five_per_species %>%
  filter(tolower(gsub(" ", "_", species_latin)) %in% target_species)

five_per_species_filtered <- five_per_species_filtered %>%
  filter(as.numeric(end) > as.numeric(start))

write.csv2(five_per_species_filtered, "Data/bird_song_recordings_from_freile.csv", row.names = FALSE)

five_per_species_filtered <- read.csv2("Data/bird_song_recordings_from_freile.csv")

# Select the files
filenames <- five_per_species_filtered$filename
# Save as plain text, one filename per line
writeLines(filenames, "Data/files_to_select.txt")

# Create the song snippets
input_dir <- "Data/freile_target_files"
output_dir <- "Data/bird_song_recordings_freile"
dir.create(output_dir, showWarnings = FALSE)

target_sr <- 44100 

five_per_species_filtered <- five_per_species_filtered %>%
  mutate(
    start = as.numeric(start),
    end = as.numeric(end)
  )

for (i in seq_len(nrow(five_per_species_filtered))) {
  row <- five_per_species_filtered[i, ]
  f_in <- file.path(input_dir, paste0(row$filename, ".wav"))
  
  if (!file.exists(f_in)) next
  
  wave <- readWave(f_in)
  if (wave@samp.rate != target_sr) {
    wave <- resamp(wave, f = wave@samp.rate, g = target_sr, output = "Wave")
  }
  
  sr <- wave@samp.rate
  start_idx <- as.integer(row$start * sr)
  end_idx <- as.integer(row$end * sr)
  if (start_idx < 1) start_idx <- 1
  if (end_idx > length(wave@left)) end_idx <- length(wave@left)
  
  seg <- extractWave(wave, from = start_idx, to = end_idx, xunit = "samples")
  
  species_dir <- file.path(output_dir, row$species_latin)
  dir.create(species_dir, showWarnings = FALSE)
  
  # filename: species_plot_freile_index.wav
  out_file <- file.path(
    species_dir,
    paste0(row$filename, "_", row$species_latin, "_freile.wav")
  )
  
  writeWave(seg, out_file)
}




#--------------
## Pick 4-5 songs from our expert annotated data (Gelis)
#--------------


# Keep only songs
gelis_songs <- gelis_labels %>%
  filter(vocalization_type == "standard song")

# Remove overlapping segments per file
# Sort by filename and start time
gelis_songs <- gelis_songs %>%
  arrange(filename, start)

# For each file, keep only songs with no overlaps
gelis_songs_no_overlap <- gelis_songs %>%
  arrange(filename, start) %>%
  group_by(filename) %>%
  mutate(overlap = {
    n <- n()
    # single-row group
    if(n == 1) {
      rep(FALSE, 1)
    } else {
      # first row never overlaps
      ov <- logical(n)
      ov[1] <- FALSE
      for(i in 2:n){
        ov[i] <- any(start[i] < end[1:(i-1)])
      }
      ov
    }
  }) %>%
  filter(!overlap) %>%
  ungroup()

# Pick one recording per plot per species
one_per_plot <- gelis_songs_no_overlap %>%
  group_by(species_latin, plot) %>%
  slice(1) %>%   # first occurrence in each plot
  ungroup()

# Take up to 5 recordings per species
five_per_species <- one_per_plot %>%
  group_by(species_latin) %>%
  slice_head(n = 5) %>%
  ungroup()

target_species <- combined_peak_frequencies$scinam  # or a subset of 144 target species

five_per_species_filtered <- five_per_species %>%
  filter(tolower(gsub(" ", "_", species_latin)) %in% target_species)

five_per_species_filtered <- five_per_species_filtered %>%
  filter(as.numeric(end) > as.numeric(start))

write.csv2(five_per_species_filtered, "Data/bird_song_recordings_from_gelis.csv", row.names = FALSE)


# Select the files
filenames <- five_per_species_filtered$filename
# Save as plain text, one filename per line
writeLines(filenames, "Data/files_to_select_gelis.txt")

# Create the song snippets
input_dir <- "Data/gelis_target_files"
output_dir <- "Data/bird_song_recordings_gelis"
dir.create(output_dir, showWarnings = FALSE)

target_sr <- 22050  # optional downsampling

five_per_species_filtered <- five_per_species_filtered %>%
  mutate(
    start = as.numeric(start),
    end = as.numeric(end)
  )

for (i in seq_len(nrow(five_per_species_filtered))) {
  row <- five_per_species_filtered[i, ]
  f_in <- file.path(input_dir, paste0(row$filename, ".wav"))
  
  if (!file.exists(f_in)) next
  
  wave <- readWave(f_in)
  if (wave@samp.rate != target_sr) {
    wave <- resamp(wave, f = wave@samp.rate, g = target_sr, output = "Wave")
  }
  
  sr <- wave@samp.rate
  start_idx <- as.integer(row$start * sr)
  end_idx <- as.integer(row$end * sr)
  if (start_idx < 1) start_idx <- 1
  if (end_idx > length(wave@left)) end_idx <- length(wave@left)
  
  seg <- extractWave(wave, from = start_idx, to = end_idx, xunit = "samples")
  
  species_dir <- file.path(output_dir, row$species_latin)
  dir.create(species_dir, showWarnings = FALSE)
  
  # filename: species_plot_gelis_index.wav
  out_file <- file.path(
    species_dir,
    paste0(row$filename, "_", row$species_latin, "_gelis.wav")
  )
  
  writeWave(seg, out_file)
}


#--------------
## Calculate peak frequency for each species
#--------------

base_dir <- "Data/bird_song_recordings_gelis"
files <- list.files(base_dir, pattern = "\\.wav$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)

target_sr <- 22050   # downsample to speed up
segment_sec <- 2     # segment length in seconds
low_cut <- 500       # Hz, ignore below this
high_cut <- 10000    # Hz, ignore above this

peak_frequencies_gelis <- data.frame()

hann_window <- function(n){
  0.5 - 0.5 * cos(2 * pi * (0:(n-1)) / (n-1))
}

for(f in files){
  species <- gsub("_", " ", basename(dirname(f)))
  
  wave <- readWave(f)
  
  # downsample if needed
  if(wave@samp.rate != target_sr){
    wave <- resamp(wave, f = wave@samp.rate, g = target_sr, output = "Wave")
  }
  
  s <- if (wave@stereo) rowMeans(cbind(wave@left, wave@right)) else wave@left
  sr <- wave@samp.rate
  n <- length(s)
  
  seg_len <- sr * segment_sec
  n_segs <- ceiling(n / seg_len)
  peak_freqs <- numeric(n_segs)
  
  for(i in 1:n_segs){
    start <- (i-1)*seg_len + 1
    end <- min(i*seg_len, n)
    seg <- s[start:end]
    
    # apply Hann window
    seg <- seg * hann_window(length(seg))
    
    # FFT
    fft_res <- fft(seg)
    amp <- Mod(fft_res)[1:floor(length(seg)/2)]
    freq_axis <- seq(0, sr/2, length.out = length(amp))
    
    # limit to frequency range
    valid_idx <- which(freq_axis >= low_cut & freq_axis <= high_cut)
    peak_freqs[i] <- freq_axis[valid_idx[which.max(amp[valid_idx])]]
  }
  
  median_peak <- median(peak_freqs, na.rm = TRUE)
  
  peak_frequencies_gelis <- bind_rows(peak_frequencies_gelis,
                                       data.frame(species = species,
                                                  file = f,
                                                  median_peak = median_peak))
  
  rm(wave, s, fft_res, amp, freq_axis, seg, peak_freqs)
  gc()
}

# Median peak per species
peak_frequencies_gelis_median <- peak_frequencies_gelis %>%
  group_by(species) %>%
  summarise(median_peak = median(median_peak, na.rm = TRUE))


write.csv2(peak_frequencies_gelis, "Data/peak_frequencies_gelis.csv", row.names = FALSE)


combined_peak_frequencies <- read.csv2("Data/peak_frequencies.csv")

# Normalize names in median table: lowercase + underscores
median_gelis <- peak_frequencies_gelis_median %>%
  mutate(scinam_norm = gsub(" ", "_", tolower(species)))

# Merge by matching scinam
combined_peak_frequencies <- combined_peak_frequencies %>%
  left_join(median_gelis %>% select(scinam_norm, median_peak),
            by = c("scinam" = "scinam_norm")) %>%
  rename(peak_frequency_gelis = median_peak)

combined_peak_frequencies <- combined_peak_frequencies %>%
  mutate(peak_frequency_gelis = round(peak_frequency_gelis, 1))

write.csv2(combined_peak_frequencies, "Data/peak_frequencies.csv", row.names = FALSE)


#--------------
## Check correlation of different peak frequency sources
#--------------


# compute correlation between Mikula and Xeno-Canto, excluding NAs
cor(combined_peak_frequencies$peak_frequency_mikula, combined_peak_frequencies$peak_frequency_xc, use = "complete.obs", method = "spearman")

cor(combined_peak_frequencies$peak_frequency_freile, combined_peak_frequencies$peak_frequency_gelis, use = "complete.obs", method = "spearman")

plot(combined_peak_frequencies$peak_frequency_mikula, combined_peak_frequencies$peak_frequency_xc,
     xlab = "Mikula", ylab = "Xeno-Canto",
     main = "Peak Frequency Correlation")
abline(lm(peak_frequency_xc ~ peak_frequency_mikula, data = combined_peak_frequencies), col = "red")


library(ggplot2)
library(ggrepel)

ggplot(combined_peak_frequencies, aes(x = peak_frequency_mikula, y = peak_frequency_xc, label = scinam)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  geom_text_repel(
    data = subset(combined_peak_frequencies, abs(peak_frequency_mikula - peak_frequency_xc) > 2000)
  ) +
  labs(x = "Mikula", y = "Xeno-Canto")
