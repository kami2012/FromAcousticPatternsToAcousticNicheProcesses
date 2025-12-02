rm(list=ls(all=TRUE))
Sys.setenv(LANG = "en")

# Working directory
setwd("E:/Manuscripts/4_co-occurence") 

library(dplyr)
library(httr)
library(jsonlite)

## ============================================================================================================ ##
## Download recordings from Xeno-Canto


#--------------
## Load data ##
#--------------

peak_frequencies <- read.csv2("Data/1_experts/peak_frequency_detections/xeno-canto/peak_frequencies_all_species.csv")

#--------------
## Pick 4-5 songs (calls) for each bird from Xeno-Canto
#--------------

all_species_xc <- gsub("_", " ", peak_frequencies$scinam)
all_species_xc <- paste0(toupper(substr(all_species_xc, 1, 1)), substr(all_species_xc, 2, nchar(all_species_xc)))

api_key <- "d288f045af62a2b7550055735f7fabeb28781263"

get_xc_songs_v3 <- function(species, n = 5, api_key, country = "ecuador") {
  url <- "https://xeno-canto.org/api/3/recordings"
  per_page <- min(max(n, 50), 500)
  
  # --- 1. Query country (Ecuador) first ---
  query_country <- paste0('sp:"', species, '" type:song q:">C"')
  if(!is.null(country)) query_country <- paste0(query_country, ' cnt:', country)
  
  res <- GET(url, query = list(query = query_country, key = api_key, per_page = per_page))
  stop_for_status(res)
  data <- fromJSON(content(res, as = "text", encoding = "UTF-8"), flatten = TRUE)
  
  df_country <- if(length(data$recordings) == 0) {
    data.frame()
  } else {
    as.data.frame(data$recordings, stringsAsFactors = FALSE) %>%
      dplyr::filter(lengths(also) == 0, sex != "female", stage != "juvenile") %>%
      distinct(id, .keep_all = TRUE)
  }
  
  if(nrow(df_country) >= n) return(head(df_country, n))
  
  # --- 2. If too few, query globally to fill ---
  query_global <- paste0('sp:"', species, '" type:song q:">C"')
  res <- GET(url, query = list(query = query_global, key = api_key, per_page = per_page))
  stop_for_status(res)
  data <- fromJSON(content(res, as = "text", encoding = "UTF-8"), flatten = TRUE)
  
  df_global <- if(length(data$recordings) == 0) {
    data.frame()
  } else {
    as.data.frame(data$recordings, stringsAsFactors = FALSE) %>%
      dplyr::filter(lengths(also) == 0, sex != "female", stage != "juvenile") %>%
      distinct(id, .keep_all = TRUE)
  }
  
  df_global <- if(nrow(df_country) > 0) {
    df_global %>% dplyr::filter(!id %in% df_country$id)
  } else df_global
  
  # --- 2b. Combine country + global songs ---
  df_final <- bind_rows(df_country, df_global) %>% head(n)
  
  # --- 3. Fallback to calls, but ONLY if there are zero recordings total ---
  if(nrow(df_final) == 0) {
    query_call <- paste0('sp:"', species, '" type:call q:">C"')
    res <- GET(url, query = list(query = query_call, key = api_key, per_page = per_page))
    stop_for_status(res)
    data <- fromJSON(content(res, as = "text", encoding = "UTF-8"), flatten = TRUE)
    
    df_call <- if(length(data$recordings) == 0) {
      data.frame()
    } else {
      as.data.frame(data$recordings, stringsAsFactors = FALSE) %>%
        dplyr::filter(lengths(also) == 0, sex != "female", stage != "juvenile") %>%
        distinct(id, .keep_all = TRUE)
    }
    
    df_final <- head(df_call, n)
  }
  
  if(nrow(df_final) == 0) return(NULL)
  return(df_final)
}


# Test with one species
songs <- get_xc_songs_v3(
  species = "Leptotrygon veraguensis",
  n = 5,
  api_key = api_key
)

# Run for all species
#songs_list <- lapply(all_species_xc, get_xc_songs_v3, api_key = api_key)
songs_list <- lapply(all_species_xc, function(sp) {
  Sys.sleep(1)  # pause 1 second (increase to 2–3 if needed)
  get_xc_songs_v3(sp, api_key = api_key)
})

songs_list <- songs_list[!sapply(songs_list, is.null)]
songs <- bind_rows(songs_list)


# Create full species name in results
songs$full_name <- paste(songs$gen, songs$sp, sep = " ")

# Check which species are missing
found_species <- unique(songs$full_name)
missing_species <- setdiff(all_species_xc, found_species)

length(missing_species) #1
missing_species
# [1] "Leptotrygon veraguensis"

# Only calls:
# Capito squamatus; Cyanerpes caeruleus; Dacnis lineata; Heliothryx barroti; Penelope ortoni; Piculus litae; Tangara florida;
# Tangara icterocephala; Tangara johannae; Thalurania colombica; Cryptopipo holochlora; Polioptila schistaceigula

# Only female:
# Leptotrygon veraguensis

# Count recordings per species
recordings_per_species <- songs %>%
  group_by(full_name) %>%
  summarise(n_recordings = n()) %>%
  arrange(desc(n_recordings))

songs_metadata <- songs %>%
  select(id, full_name, en, rec, cnt, loc, lat, type, sex, stage, method, file) %>%
  mutate(across(everything(), as.character))

write.csv2(songs_metadata, "Data/1_experts/peak_frequency_detections/xeno-canto/all_bird_song_recordings_from_xeno-canto.csv", row.names = FALSE)


#--------------
## Download the songs
#--------------

songs <- read.csv2("Data/1_experts/peak_frequency_detections/xeno-canto/all_bird_song_recordings_from_xeno-canto.csv", check.names = FALSE)
base_dir <- "Data/1_experts/peak_frequency_detections/xeno-canto/all_bird_song_recordings_xc"
dir.create(base_dir, showWarnings = FALSE, recursive = TRUE)

for(i in seq_len(nrow(songs))) {
  species <- songs$full_name[i]
  rec_id <- songs$id[i]
  file_url <- songs$file[i]
  
  # One folder per species
  safe_name <- gsub("[^A-Za-z0-9_]", "_", species)
  species_dir <- file.path(base_dir, safe_name)
  dir.create(species_dir, showWarnings = FALSE)
  
  # target
  file_path <- file.path(species_dir, paste0("XC_", rec_id, "_", safe_name, ".mp3"))
  
  if(!file.exists(file_path)) {
    tryCatch({
      GET(file_url, write_disk(file_path, overwrite = TRUE), timeout(60))
    }, error = function(e) {
      message("Fehler beim Download: ", species, ": ", rec_id, " -> ", e$message)
    })
  }
}

# Ara ambiguus: 872144 restricted access (https://xeno-canto.org/help/FAQ#restricted)
# Ara ambiguus: 871826 restricted access (https://xeno-canto.org/help/FAQ#restricted)
# Ara ambiguus: 871825 restricted access (https://xeno-canto.org/help/FAQ#restricted)

# Manually added calls from the one missing species ("Leptotrygon veraguensis") > FEMALE!!!


#--------------
## Transform mp3 to wav
#--------------

# ... has been done with ffmpeg in the cl




