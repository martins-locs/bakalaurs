# ============================================
# BIRDNET - OPTIMIZĒTĀ VERSIJA
# ============================================

# 0. BIBLIOTĒKAS ----
library(reticulate)
use_python(Sys.which("python3"), required = TRUE)

library(birdnetR)
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(purrr)

# 1. BIRDNET MODEĻI ----
model_meta <- birdnet_model_meta(
  version = "v2.4",
  language = "en_uk",
  tflite_num_threads = NULL
)

model_proto <- birdnet_model_protobuf(
  version = "v2.4",
  language = "en_uk",
  custom_device = NULL
)

# 2. METADATU IELĀDE (REIZI) ----

## 2.1. Vietas ----
vietas <- read_csv("./metadati/sites.0001.csv") %>% 
  mutate(sitename = paste0("ML_", name)) %>% 
  select(site_id, sitename)

## 2.2. Ieraksti ----
ierakstu_faili <- list.files("./metadati/", 
                              pattern = "recordings.*\\.csv$", 
                              full.names = TRUE)

ieraksti <- ierakstu_faili %>%
  map_df(~read_csv(.x, show_col_types = FALSE)) %>%
  mutate(
    original_filename = str_extract(meta, '(?<="filename":")[^"]+'),
    audio_file = str_replace(original_filename, "\\.WAV$", ".flac")
  ) %>%
  left_join(vietas, by = "site_id")

# 3. FUNKCIJA ANALĪZEI ----

analyze_folder <- function(audio_folder, 
                           overlap_values = c(0, 0.5, 1, 1.5),
                           batch_size = 100) {
  
  # Mapes nosaukums un parametri
  folder_name <- basename(normalizePath(audio_folder))
  parts <- strsplit(folder_name, "_")[[1]]
  quadrant_value <- parts[2]
  mic_type_value <- parts[3]
  
  cat("\n===========================================\n")
  cat("Sāk analīzi:", folder_name, "\n")
  cat("===========================================\n\n")
  
  # Audio failu saraksts
  audio_files <- list.files(
    audio_folder,
    pattern = "\\.flac$",
    full.names = TRUE,
    recursive = TRUE
  )
  
  if (length(audio_files) == 0) {
    cat("BRĪDINĀJUMS: Nav atrasti audio faili mapē:", audio_folder, "\n")
    return(NULL)
  }
  
  # file_info tabula
  file_info <- tibble(
    audio_path = audio_files,
    audio_file = basename(audio_files),
    quadrant = quadrant_value,
    mic_type = mic_type_value,
    sitename = basename(dirname(audio_path))
  ) %>%
    left_join(vietas, by = "sitename") %>%
    select(audio_path, audio_file, quadrant, mic_type, sitename, site_id)
  
  # Batch sadale
  batches <- split(audio_files, ceiling(seq_along(audio_files) / batch_size))
  
  # Cikls pa overlap vērtībām
  for (overlap in overlap_values) {
    
    overlap_str <- str_replace(as.character(overlap), "\\.", "")
    csv_file <- paste0("all_results_", 
                       tolower(folder_name), 
                       "_overlap_", 
                       overlap_str, 
                       ".csv")
    
    cat("\nApstrādā overlap =", overlap, "(fails:", csv_file, ")\n")
    
    # Cikls pa batchiem
    for (i in seq_along(batches)) {
      
      cat("  Batch", i, "no", length(batches), "\n")
      
      results_list <- lapply(batches[[i]], function(f) {
        
        res <- predict_species_from_audio_file(
          model_proto,
          f,
          min_confidence = 0.1,
          chunk_overlap_s = overlap,
          keep_empty = FALSE
        )
        
        if (nrow(res) == 0) return(NULL)
        
        res %>%
          mutate(
            audio_path = f,
            audio_file = basename(f),
            quadrant = quadrant_value,
            mic_type = mic_type_value
          )
      })
      
      # Apvieno batch rezultātus
      batch_df <- bind_rows(results_list)
      
      # Pievieno metadatus
      batch_df <- batch_df %>%
        left_join(
          file_info %>% select(audio_file, site_id),
          by = "audio_file"
        ) %>%
        left_join(
          ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
          by = "audio_file"
        )
      
      # Saglabā CSV
      if (!file.exists(csv_file)) {
        write_csv(batch_df, csv_file)
      } else {
        write_csv(batch_df, csv_file, append = TRUE)
      }
      
      # Atbrīvo atmiņu
      rm(results_list, batch_df)
      gc()
    }
    
    cat("  ✓ Overlap", overlap, "pabeigts!\n")
  }
  
  cat("\n✓✓✓ Mape", folder_name, "PABEIGTA! ✓✓✓\n\n")
}

# 4. ANALĪZE VISĀM MAPĒM ----

# Definē visas audio mapes
audio_folders <- c(
  "./ieraksti/ML_Q1_M1",
  "./ieraksti/ML_Q1_L",
  "./ieraksti/ML_Q1_M2",
  "./ieraksti/ML_Q1_M3",
  "./ieraksti/ML_Q1_M4",
  "./ieraksti/ML_Q1_M5",
  "./ieraksti/ML_Q2_L",
  "./ieraksti/ML_Q2_M1",
  "./ieraksti/ML_Q2_M2",
  "./ieraksti/ML_Q2_M3",
  "./ieraksti/ML_Q2_M4",
  "./ieraksti/ML_Q2_M5",
  "./ieraksti/ML_Q3_L",
  "./ieraksti/ML_Q3_M1",
  "./ieraksti/ML_Q3_M3",
  "./ieraksti/ML_Q3_M4",
  "./ieraksti/ML_Q3_M5",
  "./ieraksti/ML_Q4_L",
  "./ieraksti/ML_Q4_M1",
  "./ieraksti/ML_Q4_M2",
  "./ieraksti/ML_Q4_M3",
  "./ieraksti/ML_Q4_M4"
)

# Palaiž analīzi visām mapēm
walk(audio_folders, analyze_folder)

cat("\n")
cat("VISAS BIRDNET ANALĪZES PABEIGTAS!\n")

# 5. META MODEĻA ANALĪZE ----

cat("\nSāk Meta modeļa analīzi...\n")

# Ielādē sites info
sites_info <- read_csv("./metadati/sites.0001.csv") %>%
  select(-elevation, -hidden, -created_at)

# Lookup tabula
lookup_sites <- ieraksti %>%
  select(site_id, quadrant = name, mic_type) %>%
  distinct() %>%
  separate(quadrant, into = c("quadrant", "mic_type"), sep = "_", remove = FALSE)

# Process meta helper function
process_meta_result <- function(res) {
  if (is.null(res) || length(res$label) == 0) return(NULL)
  tibble(
    label = res$label,
    confidence = res$confidence
  )
}

## 5.1. Meta (site) ----
cat("  Meta analīze pa vietām...\n")

results_meta_sites <- map_df(seq_len(nrow(sites_info)), function(i) {
  
  res <- predict_species_at_location_and_time(
    model = model_meta,
    latitude = sites_info$latitude[i],
    longitude = sites_info$longitude[i],
    week = NULL,
    min_confidence = 0.01
  )
  
  process_meta_result(res) %>%
    mutate(site_id = sites_info$site_id[i])
  
}) %>%
  left_join(lookup_sites, by = "site_id")

write_csv(results_meta_sites, "BirdnetR_meta_sites.csv")

## 5.2. Meta (site/week) ----
cat("  Meta analīze pa vietām/nedēļām...\n")

results_meta_sites_week <- map_df(seq_len(nrow(sites_info)), function(i) {
  map_df(1:48, function(week_i) {
    
    res <- predict_species_at_location_and_time(
      model = model_meta,
      latitude = sites_info$latitude[i],
      longitude = sites_info$longitude[i],
      week = week_i,
      min_confidence = 0.01
    )
    
    process_meta_result(res) %>%
      mutate(
        site_id = sites_info$site_id[i],
        week = week_i
      )
  })
}) %>%
  left_join(lookup_sites, by = "site_id")

write_csv(results_meta_sites_week, "BirdnetR_meta_sites_week.csv")

cat("\n✓ Meta analīze pabeigta!\n")
