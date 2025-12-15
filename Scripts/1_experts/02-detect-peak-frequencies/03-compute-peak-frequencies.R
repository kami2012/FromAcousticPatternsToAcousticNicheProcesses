rm(list=ls(all=TRUE))
Sys.setenv(LANG = "en")

# Working directory
setwd("E:/Manuscripts/4_co-occurence") 

library(dplyr)
library(seewave)
library(stringr)
library(tuneR)

## ============================================================================================================ ##
## Compute peak frequencies


#--------------
## Load data ##
#--------------

songs_xc <- read.csv2("Data/1_experts/peak_frequency_detections/data_from_xeno-canto/song_recordings_from_xeno-canto.csv")

#--------------
## Calculate peak frequency for each recording
#--------------

get_peak_freq <- function(file, threshold_db = -80, min_freq_hz = 400, max_freq_hz = 10000) {
  # Read WAV file
  wave <- readWave(file)
  
  # Convert to mono if stereo
  if (wave@stereo) wave <- mono(wave, "left")
  
  # Convert signal to numeric vector
  sig <- wave@left / (2^(wave@bit - 1))  # normalize to -1..1
  
  # Compute spectrum using Hann window over fft_len points
  spec_res <- spec.pgram(sig,
                         spans = NULL,
                         taper = 0.1,       # small Hann taper
                         fast = TRUE,
                         detrend = TRUE,
                         plot = FALSE)
  
  # Spectrum in dB
  amps_db <- 10 * log10(spec_res$spec)
  
  # Frequencies
  freqs <- spec_res$freq * wave@samp.rate
  
  # Ignore low amplitude frequencies and frequencies outside range
  amps_db[amps_db < threshold_db | freqs < min_freq_hz | freqs > max_freq_hz] <- NA
  
  # Peak frequency
  peak_freq <- freqs[which.max(amps_db)]
  return(peak_freq)
}

base_dir <- "Data/1_experts/peak_frequency_detections/ata_from_xeno-canto/song_recordings_xc"

files <- list.files(base_dir, pattern = "\\.wav$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)

peak_data <- data.frame(file = files,
                        peak_freq_hz = sapply(files, get_peak_freq))

# Extract XC ID from file path (e.g., 803507)
peak_data <- peak_data %>%
  mutate(id = str_extract(file, "(?<=XC_)\\d+"))


songs_xc_shortened <- songs_xc[, c("id", "full_name", "cnt", "type", "sex", "stage")]

# Ensure XC_ID exists in songs_xc and same type
songs_xc_shortened <- songs_xc_shortened %>%
  mutate(id = as.character(id))

# Merge by XC_ID
songs_xc_shortened <- songs_xc_shortened %>%
  left_join(peak_data %>% select(id, peak_freq_hz), by = "id")

songs_xc_shortened <- songs_xc_shortened %>%
  mutate(peak_freq_hz = round(peak_freq_hz , 1))

write.csv2(songs_xc_shortened, "Data/1_experts/peak_frequency_detections/data_from_xeno-canto/peak_frequencies.csv", row.names = FALSE)


#--------------
## Calculate median peak frequency per species
#--------------

peak_data <- read.csv2("Data/1_experts/peak_frequency_detections/data_from_xeno-canto/peak_frequencies.csv")

peak_frequencies_xc_median <- peak_data %>%
  #dplyr::filter(is.na(comment) | comment != "skipped") %>% # skip files with dominant noise
  group_by(full_name) %>%
  summarise(
    median_peak = median(peak_freq_hz, na.rm = TRUE),
    xc_record_count = sum(!is.na(peak_freq_hz))
  )

write.csv2(peak_frequencies_xc_median, "Data/1_experts/peak_frequency_detections/data_from_xeno-canto/median_peak_frequency.csv", row.names = FALSE)


