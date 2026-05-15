# 0. BIBLIOTĒKAS ----

Sys.which("python3")

library(reticulate)
use_python(Sys.which("python3"), required = TRUE)
py_config()

library(birdnetR)
library(dplyr)
library(readr)
library(tidyr)
library(stringr)



# 1. BIRDNET MODEĻI ----

# model_proto <- birdnet_model_tflite(
#   version = "v2.4",
#   language = "en_uk",
# )

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























# 2. AUDIO FAILU IELĀDE Q1 M1 ----

audio_folder <- "./ieraksti/ML_Q1_M1"

audio_files <- list.files(
  audio_folder,
  pattern = "\\.flac$",
  full.names = TRUE,
  recursive = TRUE
)

# Izvelkam informāciju no mapes nosaukuma
folder_name <- basename(normalizePath(audio_folder))
parts <- strsplit(folder_name, "_")[[1]]

quadrant_value <- parts[2]
mic_type_value <- parts[3]

file_info <- tibble(
  audio_path = audio_files,
  audio_file = basename(audio_files),
  quadrant   = quadrant_value,
  mic_type   = mic_type_value
)

file_info <- file_info %>%
  mutate(
    sitename = basename(dirname(audio_path))
  )




# 3. PAPILDINFORMĀCIJAS PIEVIENOŠANA ----

## 3.1.Vietas ----

vietas=read_csv("./metadati/sites.0001.csv")
vietas=vietas %>% 
  mutate(sitename=paste0("ML_",name)) %>% 
  dplyr::select(site_id,sitename)


## 3.2.Ieraksti ----

ierakstu_faili=data.frame(celi=list.files("./metadati/",full.names = TRUE),
                          fails=list.files("./metadati/",full.names = FALSE))
ierakstu_faili=ierakstu_faili %>% 
  filter(stringr::str_detect(celi,"recordings"))

pirmais=read_csv(ierakstu_faili$celi[1])
for (i in 2:length(ierakstu_faili$celi)){
  nakosais=read_csv(ierakstu_faili$celi[i])
  pirmais=bind_rows(pirmais,nakosais)
}
ieraksti=pirmais

ieraksti <- ieraksti %>%
  mutate(
    # Izvelkam filename no meta JSON
    original_filename = str_extract(meta, '(?<="filename":")[^"]+'),
    # Nomainām .WAV uz .flac
    audio_file = str_replace(original_filename, "\\.WAV$", ".flac")
  )


ieraksti <- ieraksti %>%
  left_join(vietas, by = "site_id")


file_info <- file_info %>%
  left_join(vietas, by = "sitename") %>%
  select(audio_path, audio_file, quadrant, mic_type, sitename, site_id)


# 4. ANALIZE ----
# Batch izmērs
batch_size <- 100

# Sadalam audio failus batchos
batches <- split(audio_files, ceiling(seq_along(audio_files) / batch_size))




# ============================
## 4.1. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 0,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q1_m1_overlap_0.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("1. Analīze pabeigta. Visi rezultāti saglabāti.\n")



# ============================
## 4.2. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 0.5,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q1_m1_overlap_05.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("2. Analīze pabeigta. Visi rezultāti saglabāti.\n")




# ====================
## 4.3. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 1,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q1_m1_overlap_1.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("Analīze pabeigta. Visi rezultāti saglabāti.\n")
# =================================================





# ============================
## 4.4. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 1.5,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q1_m1_overlap_15.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("Analīze pabeigta. Visi rezultāti saglabāti.\n")
# =================================================
































# 2. AUDIO FAILU IELĀDE Q1 L ----

audio_folder <- "./ieraksti/ML_Q1_L"

audio_files <- list.files(
  audio_folder,
  pattern = "\\.flac$",
  full.names = TRUE,
  recursive = TRUE
)

# Izvelkam informāciju no mapes nosaukuma
folder_name <- basename(normalizePath(audio_folder))
parts <- strsplit(folder_name, "_")[[1]]

quadrant_value <- parts[2]
mic_type_value <- parts[3]

file_info <- tibble(
  audio_path = audio_files,
  audio_file = basename(audio_files),
  quadrant   = quadrant_value,
  mic_type   = mic_type_value
)

file_info <- file_info %>%
  mutate(
    sitename = basename(dirname(audio_path))
  )




# 3. PAPILDINFORMĀCIJAS PIEVIENOŠANA ----

## 3.1.Vietas ----

vietas=read_csv("./metadati/sites.0001.csv")
vietas=vietas %>% 
  mutate(sitename=paste0("ML_",name)) %>% 
  dplyr::select(site_id,sitename)


## 3.2.Ieraksti ----

ierakstu_faili=data.frame(celi=list.files("./metadati/",full.names = TRUE),
                          fails=list.files("./metadati/",full.names = FALSE))
ierakstu_faili=ierakstu_faili %>% 
  filter(stringr::str_detect(celi,"recordings"))

pirmais=read_csv(ierakstu_faili$celi[1])
for (i in 2:length(ierakstu_faili$celi)){
  nakosais=read_csv(ierakstu_faili$celi[i])
  pirmais=bind_rows(pirmais,nakosais)
}
ieraksti=pirmais


ieraksti <- ieraksti %>%
  mutate(
    # Izvelkam filename no meta JSON
    original_filename = str_extract(meta, '(?<="filename":")[^"]+'),
    # Nomainām .WAV uz .flac
    audio_file = str_replace(original_filename, "\\.WAV$", ".flac")
  )


ieraksti <- ieraksti %>%
  left_join(vietas, by = "site_id")


file_info <- file_info %>%
  left_join(vietas, by = "sitename") %>%
  select(audio_path, audio_file, quadrant, mic_type, sitename, site_id)


# 4. ANALIZE ----
# Batch izmērs
batch_size <- 100

# Sadalam audio failus batchos
batches <- split(audio_files, ceiling(seq_along(audio_files) / batch_size))




# ============================
## 4.1. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 0,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q1_l_overlap_0.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("1. Analīze pabeigta. Visi rezultāti saglabāti.\n")



# ============================
## 4.2. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 0.5,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q1_l_overlap_05.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("2. Analīze pabeigta. Visi rezultāti saglabāti.\n")




# ====================
## 4.3. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 1,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q1_l_overlap_1.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("3. Analīze pabeigta. Visi rezultāti saglabāti.\n")
# =================================================





# ============================
## 4.4. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 1.5,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q1_l_overlap_15.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("4. un pedeja analīze pabeigta. Visi rezultāti saglabāti.\n")
# =================================================

































# 2. AUDIO FAILU IELĀDE Q1 M2 ----

audio_folder <- "./ieraksti/ML_Q1_M2"

audio_files <- list.files(
  audio_folder,
  pattern = "\\.flac$",
  full.names = TRUE,
  recursive = TRUE
)

# Izvelkam informāciju no mapes nosaukuma
folder_name <- basename(normalizePath(audio_folder))
parts <- strsplit(folder_name, "_")[[1]]

quadrant_value <- parts[2]
mic_type_value <- parts[3]

file_info <- tibble(
  audio_path = audio_files,
  audio_file = basename(audio_files),
  quadrant   = quadrant_value,
  mic_type   = mic_type_value
)

file_info <- file_info %>%
  mutate(
    sitename = basename(dirname(audio_path))
  )




# 3. PAPILDINFORMĀCIJAS PIEVIENOŠANA ----

## 3.1.Vietas ----

vietas=read_csv("./metadati/sites.0001.csv")
vietas=vietas %>% 
  mutate(sitename=paste0("ML_",name)) %>% 
  dplyr::select(site_id,sitename)


## 3.2.Ieraksti ----

ierakstu_faili=data.frame(celi=list.files("./metadati/",full.names = TRUE),
                          fails=list.files("./metadati/",full.names = FALSE))
ierakstu_faili=ierakstu_faili %>% 
  filter(stringr::str_detect(celi,"recordings"))

pirmais=read_csv(ierakstu_faili$celi[1])
for (i in 2:length(ierakstu_faili$celi)){
  nakosais=read_csv(ierakstu_faili$celi[i])
  pirmais=bind_rows(pirmais,nakosais)
}
ieraksti=pirmais


ieraksti <- ieraksti %>%
  mutate(
    # Izvelkam filename no meta JSON
    original_filename = str_extract(meta, '(?<="filename":")[^"]+'),
    # Nomainām .WAV uz .flac
    audio_file = str_replace(original_filename, "\\.WAV$", ".flac")
  )


ieraksti <- ieraksti %>%
  left_join(vietas, by = "site_id")


file_info <- file_info %>%
  left_join(vietas, by = "sitename") %>%
  select(audio_path, audio_file, quadrant, mic_type, sitename, site_id)


# 4. ANALIZE ----
# Batch izmērs
batch_size <- 100

# Sadalam audio failus batchos
batches <- split(audio_files, ceiling(seq_along(audio_files) / batch_size))




# ============================
## 4.1. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 0,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q1_m2_overlap_0.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("1. Analīze pabeigta. Visi rezultāti saglabāti.\n")



# ============================
## 4.2. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 0.5,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q1_m2_overlap_05.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("2. Analīze pabeigta. Visi rezultāti saglabāti.\n")




# ====================
## 4.3. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 1,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q1_m2_overlap_1.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("3. Analīze pabeigta. Visi rezultāti saglabāti.\n")
# =================================================





# ============================
## 4.4. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 1.5,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q1_m2_overlap_15.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("4. un pedeja analīze pabeigta. Visi rezultāti saglabāti.\n")
# =================================================
































# 2. AUDIO FAILU IELĀDE Q1 M3 ----

audio_folder <- "./ieraksti/ML_Q1_M3"

audio_files <- list.files(
  audio_folder,
  pattern = "\\.flac$",
  full.names = TRUE,
  recursive = TRUE
)

# Izvelkam informāciju no mapes nosaukuma
folder_name <- basename(normalizePath(audio_folder))
parts <- strsplit(folder_name, "_")[[1]]

quadrant_value <- parts[2]
mic_type_value <- parts[3]

file_info <- tibble(
  audio_path = audio_files,
  audio_file = basename(audio_files),
  quadrant   = quadrant_value,
  mic_type   = mic_type_value
)

file_info <- file_info %>%
  mutate(
    sitename = basename(dirname(audio_path))
  )




# 3. PAPILDINFORMĀCIJAS PIEVIENOŠANA ----

## 3.1.Vietas ----

vietas=read_csv("./metadati/sites.0001.csv")
vietas=vietas %>% 
  mutate(sitename=paste0("ML_",name)) %>% 
  dplyr::select(site_id,sitename)


## 3.2.Ieraksti ----

ierakstu_faili=data.frame(celi=list.files("./metadati/",full.names = TRUE),
                          fails=list.files("./metadati/",full.names = FALSE))
ierakstu_faili=ierakstu_faili %>% 
  filter(stringr::str_detect(celi,"recordings"))

pirmais=read_csv(ierakstu_faili$celi[1])
for (i in 2:length(ierakstu_faili$celi)){
  nakosais=read_csv(ierakstu_faili$celi[i])
  pirmais=bind_rows(pirmais,nakosais)
}
ieraksti=pirmais


ieraksti <- ieraksti %>%
  mutate(
    # Izvelkam filename no meta JSON
    original_filename = str_extract(meta, '(?<="filename":")[^"]+'),
    # Nomainām .WAV uz .flac
    audio_file = str_replace(original_filename, "\\.WAV$", ".flac")
  )


ieraksti <- ieraksti %>%
  left_join(vietas, by = "site_id")


file_info <- file_info %>%
  left_join(vietas, by = "sitename") %>%
  select(audio_path, audio_file, quadrant, mic_type, sitename, site_id)


# 4. ANALIZE ----
# Batch izmērs
batch_size <- 100

# Sadalam audio failus batchos
batches <- split(audio_files, ceiling(seq_along(audio_files) / batch_size))




# ============================
## 4.1. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 0,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q1_m3_overlap_0.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("1. Analīze pabeigta. Visi rezultāti saglabāti.\n")



# ============================
## 4.2. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 0.5,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q1_m3_overlap_05.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("2. Analīze pabeigta. Visi rezultāti saglabāti.\n")




# ====================
## 4.3. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 1,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q1_m3_overlap_1.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("3. Analīze pabeigta. Visi rezultāti saglabāti.\n")
# =================================================





# ============================
## 4.4. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 1.5,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q1_m3_overlap_15.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("4. un pedeja analīze pabeigta. Visi rezultāti saglabāti.\n")
# =================================================
































# 2. AUDIO FAILU IELĀDE Q1 M4 ----

audio_folder <- "./ieraksti/ML_Q1_M4"

audio_files <- list.files(
  audio_folder,
  pattern = "\\.flac$",
  full.names = TRUE,
  recursive = TRUE
)

# Izvelkam informāciju no mapes nosaukuma
folder_name <- basename(normalizePath(audio_folder))
parts <- strsplit(folder_name, "_")[[1]]

quadrant_value <- parts[2]
mic_type_value <- parts[3]

file_info <- tibble(
  audio_path = audio_files,
  audio_file = basename(audio_files),
  quadrant   = quadrant_value,
  mic_type   = mic_type_value
)

file_info <- file_info %>%
  mutate(
    sitename = basename(dirname(audio_path))
  )




# 3. PAPILDINFORMĀCIJAS PIEVIENOŠANA ----

## 3.1.Vietas ----

vietas=read_csv("./metadati/sites.0001.csv")
vietas=vietas %>% 
  mutate(sitename=paste0("ML_",name)) %>% 
  dplyr::select(site_id,sitename)


## 3.2.Ieraksti ----

ierakstu_faili=data.frame(celi=list.files("./metadati/",full.names = TRUE),
                          fails=list.files("./metadati/",full.names = FALSE))
ierakstu_faili=ierakstu_faili %>% 
  filter(stringr::str_detect(celi,"recordings"))

pirmais=read_csv(ierakstu_faili$celi[1])
for (i in 2:length(ierakstu_faili$celi)){
  nakosais=read_csv(ierakstu_faili$celi[i])
  pirmais=bind_rows(pirmais,nakosais)
}
ieraksti=pirmais


ieraksti <- ieraksti %>%
  mutate(
    # Izvelkam filename no meta JSON
    original_filename = str_extract(meta, '(?<="filename":")[^"]+'),
    # Nomainām .WAV uz .flac
    audio_file = str_replace(original_filename, "\\.WAV$", ".flac")
  )


ieraksti <- ieraksti %>%
  left_join(vietas, by = "site_id")


file_info <- file_info %>%
  left_join(vietas, by = "sitename") %>%
  select(audio_path, audio_file, quadrant, mic_type, sitename, site_id)


# 4. ANALIZE ----
# Batch izmērs
batch_size <- 100

# Sadalam audio failus batchos
batches <- split(audio_files, ceiling(seq_along(audio_files) / batch_size))




# ============================
## 4.1. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 0,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q1_m4_overlap_0.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("1. Analīze pabeigta. Visi rezultāti saglabāti.\n")



# ============================
## 4.2. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 0.5,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q1_m4_overlap_05.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("2. Analīze pabeigta. Visi rezultāti saglabāti.\n")




# ====================
## 4.3. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 1,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q1_m4_overlap_1.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("3. Analīze pabeigta. Visi rezultāti saglabāti.\n")
# =================================================





# ============================
## 4.4. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 1.5,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q1_m4_overlap_15.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("4. un pedeja analīze pabeigta. Visi rezultāti saglabāti.\n")
# =================================================
































# 2. AUDIO FAILU IELĀDE Q1 M5 ----

audio_folder <- "./ieraksti/ML_Q1_M5"

audio_files <- list.files(
  audio_folder,
  pattern = "\\.flac$",
  full.names = TRUE,
  recursive = TRUE
)

# Izvelkam informāciju no mapes nosaukuma
folder_name <- basename(normalizePath(audio_folder))
parts <- strsplit(folder_name, "_")[[1]]

quadrant_value <- parts[2]
mic_type_value <- parts[3]

file_info <- tibble(
  audio_path = audio_files,
  audio_file = basename(audio_files),
  quadrant   = quadrant_value,
  mic_type   = mic_type_value
)

file_info <- file_info %>%
  mutate(
    sitename = basename(dirname(audio_path))
  )




# 3. PAPILDINFORMĀCIJAS PIEVIENOŠANA ----

## 3.1.Vietas ----

vietas=read_csv("./metadati/sites.0001.csv")
vietas=vietas %>% 
  mutate(sitename=paste0("ML_",name)) %>% 
  dplyr::select(site_id,sitename)


## 3.2.Ieraksti ----

ierakstu_faili=data.frame(celi=list.files("./metadati/",full.names = TRUE),
                          fails=list.files("./metadati/",full.names = FALSE))
ierakstu_faili=ierakstu_faili %>% 
  filter(stringr::str_detect(celi,"recordings"))

pirmais=read_csv(ierakstu_faili$celi[1])
for (i in 2:length(ierakstu_faili$celi)){
  nakosais=read_csv(ierakstu_faili$celi[i])
  pirmais=bind_rows(pirmais,nakosais)
}
ieraksti=pirmais


ieraksti <- ieraksti %>%
  mutate(
    # Izvelkam filename no meta JSON
    original_filename = str_extract(meta, '(?<="filename":")[^"]+'),
    # Nomainām .WAV uz .flac
    audio_file = str_replace(original_filename, "\\.WAV$", ".flac")
  )


ieraksti <- ieraksti %>%
  left_join(vietas, by = "site_id")


file_info <- file_info %>%
  left_join(vietas, by = "sitename") %>%
  select(audio_path, audio_file, quadrant, mic_type, sitename, site_id)


# 4. ANALIZE ----
# Batch izmērs
batch_size <- 100

# Sadalam audio failus batchos
batches <- split(audio_files, ceiling(seq_along(audio_files) / batch_size))




# ============================
## 4.1. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 0,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q1_m5_overlap_0.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("1. Analīze pabeigta. Visi rezultāti saglabāti.\n")



# ============================
## 4.2. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 0.5,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q1_m5_overlap_05.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("2. Analīze pabeigta. Visi rezultāti saglabāti.\n")




# ====================
## 4.3. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 1,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q1_m5_overlap_1.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("3. Analīze pabeigta. Visi rezultāti saglabāti.\n")
# =================================================





# ============================
## 4.4. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 1.5,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q1_m5_overlap_15.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("4. un pedeja analīze pabeigta. Visi rezultāti saglabāti.\n")
# =================================================
































# 2. AUDIO FAILU IELĀDE Q2 L ----

audio_folder <- "./ieraksti/ML_Q2_L"

audio_files <- list.files(
  audio_folder,
  pattern = "\\.flac$",
  full.names = TRUE,
  recursive = TRUE
)

# Izvelkam informāciju no mapes nosaukuma
folder_name <- basename(normalizePath(audio_folder))
parts <- strsplit(folder_name, "_")[[1]]

quadrant_value <- parts[2]
mic_type_value <- parts[3]

file_info <- tibble(
  audio_path = audio_files,
  audio_file = basename(audio_files),
  quadrant   = quadrant_value,
  mic_type   = mic_type_value
)

file_info <- file_info %>%
  mutate(
    sitename = basename(dirname(audio_path))
  )




# 3. PAPILDINFORMĀCIJAS PIEVIENOŠANA ----

## 3.1.Vietas ----

vietas=read_csv("./metadati/sites.0001.csv")
vietas=vietas %>% 
  mutate(sitename=paste0("ML_",name)) %>% 
  dplyr::select(site_id,sitename)


## 3.2.Ieraksti ----

ierakstu_faili=data.frame(celi=list.files("./metadati/",full.names = TRUE),
                          fails=list.files("./metadati/",full.names = FALSE))
ierakstu_faili=ierakstu_faili %>% 
  filter(stringr::str_detect(celi,"recordings"))

pirmais=read_csv(ierakstu_faili$celi[1])
for (i in 2:length(ierakstu_faili$celi)){
  nakosais=read_csv(ierakstu_faili$celi[i])
  pirmais=bind_rows(pirmais,nakosais)
}
ieraksti=pirmais


ieraksti <- ieraksti %>%
  mutate(
    # Izvelkam filename no meta JSON
    original_filename = str_extract(meta, '(?<="filename":")[^"]+'),
    # Nomainām .WAV uz .flac
    audio_file = str_replace(original_filename, "\\.WAV$", ".flac")
  )


ieraksti <- ieraksti %>%
  left_join(vietas, by = "site_id")


file_info <- file_info %>%
  left_join(vietas, by = "sitename") %>%
  select(audio_path, audio_file, quadrant, mic_type, sitename, site_id)


# 4. ANALIZE ----
# Batch izmērs
batch_size <- 100

# Sadalam audio failus batchos
batches <- split(audio_files, ceiling(seq_along(audio_files) / batch_size))




# ============================
## 4.1. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 0,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q2_l_overlap_0.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("1. Analīze pabeigta. Visi rezultāti saglabāti.\n")



# ============================
## 4.2. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 0.5,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q2_l_overlap_05.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("2. Analīze pabeigta. Visi rezultāti saglabāti.\n")




# ====================
## 4.3. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 1,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q2_l_overlap_1.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("3. Analīze pabeigta. Visi rezultāti saglabāti.\n")
# =================================================





# ============================
## 4.4. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 1.5,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q2_l_overlap_15.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("4. un pedeja analīze pabeigta. Visi rezultāti saglabāti.\n")
# =================================================































# 2. AUDIO FAILU IELĀDE Q2 M1 ----

audio_folder <- "./ieraksti/ML_Q2_M1"

audio_files <- list.files(
  audio_folder,
  pattern = "\\.flac$",
  full.names = TRUE,
  recursive = TRUE
)

# Izvelkam informāciju no mapes nosaukuma
folder_name <- basename(normalizePath(audio_folder))
parts <- strsplit(folder_name, "_")[[1]]

quadrant_value <- parts[2]
mic_type_value <- parts[3]

file_info <- tibble(
  audio_path = audio_files,
  audio_file = basename(audio_files),
  quadrant   = quadrant_value,
  mic_type   = mic_type_value
)

file_info <- file_info %>%
  mutate(
    sitename = basename(dirname(audio_path))
  )




# 3. PAPILDINFORMĀCIJAS PIEVIENOŠANA ----

## 3.1.Vietas ----

vietas=read_csv("./metadati/sites.0001.csv")
vietas=vietas %>% 
  mutate(sitename=paste0("ML_",name)) %>% 
  dplyr::select(site_id,sitename)


## 3.2.Ieraksti ----

ierakstu_faili=data.frame(celi=list.files("./metadati/",full.names = TRUE),
                          fails=list.files("./metadati/",full.names = FALSE))
ierakstu_faili=ierakstu_faili %>% 
  filter(stringr::str_detect(celi,"recordings"))

pirmais=read_csv(ierakstu_faili$celi[1])
for (i in 2:length(ierakstu_faili$celi)){
  nakosais=read_csv(ierakstu_faili$celi[i])
  pirmais=bind_rows(pirmais,nakosais)
}
ieraksti=pirmais


ieraksti <- ieraksti %>%
  mutate(
    # Izvelkam filename no meta JSON
    original_filename = str_extract(meta, '(?<="filename":")[^"]+'),
    # Nomainām .WAV uz .flac
    audio_file = str_replace(original_filename, "\\.WAV$", ".flac")
  )


ieraksti <- ieraksti %>%
  left_join(vietas, by = "site_id")


file_info <- file_info %>%
  left_join(vietas, by = "sitename") %>%
  select(audio_path, audio_file, quadrant, mic_type, sitename, site_id)


# 4. ANALIZE ----
# Batch izmērs
batch_size <- 100

# Sadalam audio failus batchos
batches <- split(audio_files, ceiling(seq_along(audio_files) / batch_size))




# ============================
## 4.1. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 0,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q2_m1_overlap_0.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("1. Analīze pabeigta. Visi rezultāti saglabāti.\n")



# ============================
## 4.2. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 0.5,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q2_m1_overlap_05.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("2. Analīze pabeigta. Visi rezultāti saglabāti.\n")




# ====================
## 4.3. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 1,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q2_m1_overlap_1.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("3. Analīze pabeigta. Visi rezultāti saglabāti.\n")
# =================================================





# ============================
## 4.4. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 1.5,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q2_m1_overlap_15.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("4. un pedeja analīze pabeigta. Visi rezultāti saglabāti.\n")
# =================================================
































# 2. AUDIO FAILU IELĀDE Q2 M2 ----

audio_folder <- "./ieraksti/ML_Q2_M2"

audio_files <- list.files(
  audio_folder,
  pattern = "\\.flac$",
  full.names = TRUE,
  recursive = TRUE
)

# Izvelkam informāciju no mapes nosaukuma
folder_name <- basename(normalizePath(audio_folder))
parts <- strsplit(folder_name, "_")[[1]]

quadrant_value <- parts[2]
mic_type_value <- parts[3]

file_info <- tibble(
  audio_path = audio_files,
  audio_file = basename(audio_files),
  quadrant   = quadrant_value,
  mic_type   = mic_type_value
)

file_info <- file_info %>%
  mutate(
    sitename = basename(dirname(audio_path))
  )




# 3. PAPILDINFORMĀCIJAS PIEVIENOŠANA ----

## 3.1.Vietas ----

vietas=read_csv("./metadati/sites.0001.csv")
vietas=vietas %>% 
  mutate(sitename=paste0("ML_",name)) %>% 
  dplyr::select(site_id,sitename)


## 3.2.Ieraksti ----

ierakstu_faili=data.frame(celi=list.files("./metadati/",full.names = TRUE),
                          fails=list.files("./metadati/",full.names = FALSE))
ierakstu_faili=ierakstu_faili %>% 
  filter(stringr::str_detect(celi,"recordings"))

pirmais=read_csv(ierakstu_faili$celi[1])
for (i in 2:length(ierakstu_faili$celi)){
  nakosais=read_csv(ierakstu_faili$celi[i])
  pirmais=bind_rows(pirmais,nakosais)
}
ieraksti=pirmais


ieraksti <- ieraksti %>%
  mutate(
    # Izvelkam filename no meta JSON
    original_filename = str_extract(meta, '(?<="filename":")[^"]+'),
    # Nomainām .WAV uz .flac
    audio_file = str_replace(original_filename, "\\.WAV$", ".flac")
  )


ieraksti <- ieraksti %>%
  left_join(vietas, by = "site_id")


file_info <- file_info %>%
  left_join(vietas, by = "sitename") %>%
  select(audio_path, audio_file, quadrant, mic_type, sitename, site_id)


# 4. ANALIZE ----
# Batch izmērs
batch_size <- 100

# Sadalam audio failus batchos
batches <- split(audio_files, ceiling(seq_along(audio_files) / batch_size))




# ============================
## 4.1. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 0,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q2_m2_overlap_0.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("1. Analīze pabeigta. Visi rezultāti saglabāti.\n")



# ============================
## 4.2. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 0.5,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q2_m2_overlap_05.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("2. Analīze pabeigta. Visi rezultāti saglabāti.\n")




# ====================
## 4.3. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 1,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q2_m2_overlap_1.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("3. Analīze pabeigta. Visi rezultāti saglabāti.\n")
# =================================================





# ============================
## 4.4. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 1.5,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q2_m2_overlap_15.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("4. un pedeja analīze pabeigta. Visi rezultāti saglabāti.\n")
# =================================================
































# 2. AUDIO FAILU IELĀDE Q2 M3 ----

audio_folder <- "./ieraksti/ML_Q2_M3"

audio_files <- list.files(
  audio_folder,
  pattern = "\\.flac$",
  full.names = TRUE,
  recursive = TRUE
)

# Izvelkam informāciju no mapes nosaukuma
folder_name <- basename(normalizePath(audio_folder))
parts <- strsplit(folder_name, "_")[[1]]

quadrant_value <- parts[2]
mic_type_value <- parts[3]

file_info <- tibble(
  audio_path = audio_files,
  audio_file = basename(audio_files),
  quadrant   = quadrant_value,
  mic_type   = mic_type_value
)

file_info <- file_info %>%
  mutate(
    sitename = basename(dirname(audio_path))
  )




# 3. PAPILDINFORMĀCIJAS PIEVIENOŠANA ----

## 3.1.Vietas ----

vietas=read_csv("./metadati/sites.0001.csv")
vietas=vietas %>% 
  mutate(sitename=paste0("ML_",name)) %>% 
  dplyr::select(site_id,sitename)


## 3.2.Ieraksti ----

ierakstu_faili=data.frame(celi=list.files("./metadati/",full.names = TRUE),
                          fails=list.files("./metadati/",full.names = FALSE))
ierakstu_faili=ierakstu_faili %>% 
  filter(stringr::str_detect(celi,"recordings"))

pirmais=read_csv(ierakstu_faili$celi[1])
for (i in 2:length(ierakstu_faili$celi)){
  nakosais=read_csv(ierakstu_faili$celi[i])
  pirmais=bind_rows(pirmais,nakosais)
}
ieraksti=pirmais


ieraksti <- ieraksti %>%
  mutate(
    # Izvelkam filename no meta JSON
    original_filename = str_extract(meta, '(?<="filename":")[^"]+'),
    # Nomainām .WAV uz .flac
    audio_file = str_replace(original_filename, "\\.WAV$", ".flac")
  )


ieraksti <- ieraksti %>%
  left_join(vietas, by = "site_id")


file_info <- file_info %>%
  left_join(vietas, by = "sitename") %>%
  select(audio_path, audio_file, quadrant, mic_type, sitename, site_id)


# 4. ANALIZE ----
# Batch izmērs
batch_size <- 100

# Sadalam audio failus batchos
batches <- split(audio_files, ceiling(seq_along(audio_files) / batch_size))




# ============================
## 4.1. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 0,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q2_m3_overlap_0.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("1. Analīze pabeigta. Visi rezultāti saglabāti.\n")



# ============================
## 4.2. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 0.5,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q2_m3_overlap_05.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("2. Analīze pabeigta. Visi rezultāti saglabāti.\n")




# ====================
## 4.3. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 1,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q2_m3_overlap_1.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("3. Analīze pabeigta. Visi rezultāti saglabāti.\n")
# =================================================





# ============================
## 4.4. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 1.5,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q2_m3_overlap_15.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("4. un pedeja analīze pabeigta. Visi rezultāti saglabāti.\n")
# =================================================
































# 2. AUDIO FAILU IELĀDE Q2 M4 ----

audio_folder <- "./ieraksti/ML_Q2_M4"

audio_files <- list.files(
  audio_folder,
  pattern = "\\.flac$",
  full.names = TRUE,
  recursive = TRUE
)

# Izvelkam informāciju no mapes nosaukuma
folder_name <- basename(normalizePath(audio_folder))
parts <- strsplit(folder_name, "_")[[1]]

quadrant_value <- parts[2]
mic_type_value <- parts[3]

file_info <- tibble(
  audio_path = audio_files,
  audio_file = basename(audio_files),
  quadrant   = quadrant_value,
  mic_type   = mic_type_value
)

file_info <- file_info %>%
  mutate(
    sitename = basename(dirname(audio_path))
  )




# 3. PAPILDINFORMĀCIJAS PIEVIENOŠANA ----

## 3.1.Vietas ----

vietas=read_csv("./metadati/sites.0001.csv")
vietas=vietas %>% 
  mutate(sitename=paste0("ML_",name)) %>% 
  dplyr::select(site_id,sitename)


## 3.2.Ieraksti ----

ierakstu_faili=data.frame(celi=list.files("./metadati/",full.names = TRUE),
                          fails=list.files("./metadati/",full.names = FALSE))
ierakstu_faili=ierakstu_faili %>% 
  filter(stringr::str_detect(celi,"recordings"))

pirmais=read_csv(ierakstu_faili$celi[1])
for (i in 2:length(ierakstu_faili$celi)){
  nakosais=read_csv(ierakstu_faili$celi[i])
  pirmais=bind_rows(pirmais,nakosais)
}
ieraksti=pirmais


ieraksti <- ieraksti %>%
  mutate(
    # Izvelkam filename no meta JSON
    original_filename = str_extract(meta, '(?<="filename":")[^"]+'),
    # Nomainām .WAV uz .flac
    audio_file = str_replace(original_filename, "\\.WAV$", ".flac")
  )


ieraksti <- ieraksti %>%
  left_join(vietas, by = "site_id")


file_info <- file_info %>%
  left_join(vietas, by = "sitename") %>%
  select(audio_path, audio_file, quadrant, mic_type, sitename, site_id)


# 4. ANALIZE ----
# Batch izmērs
batch_size <- 100

# Sadalam audio failus batchos
batches <- split(audio_files, ceiling(seq_along(audio_files) / batch_size))




# ============================
## 4.1. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 0,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q2_m4_overlap_0.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("1. Analīze pabeigta. Visi rezultāti saglabāti.\n")



# ============================
## 4.2. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 0.5,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q2_m4_overlap_05.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("2. Analīze pabeigta. Visi rezultāti saglabāti.\n")




# ====================
## 4.3. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 1,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q2_m4_overlap_1.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("3. Analīze pabeigta. Visi rezultāti saglabāti.\n")
# =================================================





# ============================
## 4.4. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 1.5,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q2_m4_overlap_15.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("4. un pedeja analīze pabeigta. Visi rezultāti saglabāti.\n")
# =================================================
































# 2. AUDIO FAILU IELĀDE Q2 M5 ----

audio_folder <- "./ieraksti/ML_Q2_M5"

audio_files <- list.files(
  audio_folder,
  pattern = "\\.flac$",
  full.names = TRUE,
  recursive = TRUE
)

# Izvelkam informāciju no mapes nosaukuma
folder_name <- basename(normalizePath(audio_folder))
parts <- strsplit(folder_name, "_")[[1]]

quadrant_value <- parts[2]
mic_type_value <- parts[3]

file_info <- tibble(
  audio_path = audio_files,
  audio_file = basename(audio_files),
  quadrant   = quadrant_value,
  mic_type   = mic_type_value
)

file_info <- file_info %>%
  mutate(
    sitename = basename(dirname(audio_path))
  )




# 3. PAPILDINFORMĀCIJAS PIEVIENOŠANA ----

## 3.1.Vietas ----

vietas=read_csv("./metadati/sites.0001.csv")
vietas=vietas %>% 
  mutate(sitename=paste0("ML_",name)) %>% 
  dplyr::select(site_id,sitename)


## 3.2.Ieraksti ----

ierakstu_faili=data.frame(celi=list.files("./metadati/",full.names = TRUE),
                          fails=list.files("./metadati/",full.names = FALSE))
ierakstu_faili=ierakstu_faili %>% 
  filter(stringr::str_detect(celi,"recordings"))

pirmais=read_csv(ierakstu_faili$celi[1])
for (i in 2:length(ierakstu_faili$celi)){
  nakosais=read_csv(ierakstu_faili$celi[i])
  pirmais=bind_rows(pirmais,nakosais)
}
ieraksti=pirmais


ieraksti <- ieraksti %>%
  mutate(
    # Izvelkam filename no meta JSON
    original_filename = str_extract(meta, '(?<="filename":")[^"]+'),
    # Nomainām .WAV uz .flac
    audio_file = str_replace(original_filename, "\\.WAV$", ".flac")
  )


ieraksti <- ieraksti %>%
  left_join(vietas, by = "site_id")


file_info <- file_info %>%
  left_join(vietas, by = "sitename") %>%
  select(audio_path, audio_file, quadrant, mic_type, sitename, site_id)


# 4. ANALIZE ----
# Batch izmērs
batch_size <- 100

# Sadalam audio failus batchos
batches <- split(audio_files, ceiling(seq_along(audio_files) / batch_size))




# ============================
## 4.1. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 0,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q2_m5_overlap_0.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("1. Analīze pabeigta. Visi rezultāti saglabāti.\n")



# ============================
## 4.2. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 0.5,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q2_m5_overlap_05.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("2. Analīze pabeigta. Visi rezultāti saglabāti.\n")




# ====================
## 4.3. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 1,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q2_m5_overlap_1.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("3. Analīze pabeigta. Visi rezultāti saglabāti.\n")
# =================================================





# ============================
## 4.4. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 1.5,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q2_m5_overlap_15.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("4. un pedeja analīze pabeigta. Visi rezultāti saglabāti.\n")
# =================================================

































# 2. AUDIO FAILU IELĀDE Q3 L ----

audio_folder <- "./ieraksti/ML_Q3_L"

audio_files <- list.files(
  audio_folder,
  pattern = "\\.flac$",
  full.names = TRUE,
  recursive = TRUE
)

# Izvelkam informāciju no mapes nosaukuma
folder_name <- basename(normalizePath(audio_folder))
parts <- strsplit(folder_name, "_")[[1]]

quadrant_value <- parts[2]
mic_type_value <- parts[3]

file_info <- tibble(
  audio_path = audio_files,
  audio_file = basename(audio_files),
  quadrant   = quadrant_value,
  mic_type   = mic_type_value
)

file_info <- file_info %>%
  mutate(
    sitename = basename(dirname(audio_path))
  )




# 3. PAPILDINFORMĀCIJAS PIEVIENOŠANA ----

## 3.1.Vietas ----

vietas=read_csv("./metadati/sites.0001.csv")
vietas=vietas %>% 
  mutate(sitename=paste0("ML_",name)) %>% 
  dplyr::select(site_id,sitename)


## 3.2.Ieraksti ----

ierakstu_faili=data.frame(celi=list.files("./metadati/",full.names = TRUE),
                          fails=list.files("./metadati/",full.names = FALSE))
ierakstu_faili=ierakstu_faili %>% 
  filter(stringr::str_detect(celi,"recordings"))

pirmais=read_csv(ierakstu_faili$celi[1])
for (i in 2:length(ierakstu_faili$celi)){
  nakosais=read_csv(ierakstu_faili$celi[i])
  pirmais=bind_rows(pirmais,nakosais)
}
ieraksti=pirmais


ieraksti <- ieraksti %>%
  mutate(
    # Izvelkam filename no meta JSON
    original_filename = str_extract(meta, '(?<="filename":")[^"]+'),
    # Nomainām .WAV uz .flac
    audio_file = str_replace(original_filename, "\\.WAV$", ".flac")
  )


ieraksti <- ieraksti %>%
  left_join(vietas, by = "site_id")


file_info <- file_info %>%
  left_join(vietas, by = "sitename") %>%
  select(audio_path, audio_file, quadrant, mic_type, sitename, site_id)


# 4. ANALIZE ----
# Batch izmērs
batch_size <- 100

# Sadalam audio failus batchos
batches <- split(audio_files, ceiling(seq_along(audio_files) / batch_size))




# ============================
## 4.1. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 0,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q3_l_overlap_0.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("1. Analīze pabeigta. Visi rezultāti saglabāti.\n")



# ============================
## 4.2. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 0.5,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q3_l_overlap_05.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("2. Analīze pabeigta. Visi rezultāti saglabāti.\n")




# ====================
## 4.3. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 1,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q3_l_overlap_1.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("3. Analīze pabeigta. Visi rezultāti saglabāti.\n")
# =================================================





# ============================
## 4.4. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 1.5,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q3_l_overlap_15.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("4. un pedeja analīze pabeigta. Visi rezultāti saglabāti.\n")
# =================================================































# 2. AUDIO FAILU IELĀDE Q3 M1 ----

audio_folder <- "./ieraksti/ML_Q3_M1"

audio_files <- list.files(
  audio_folder,
  pattern = "\\.flac$",
  full.names = TRUE,
  recursive = TRUE
)

# Izvelkam informāciju no mapes nosaukuma
folder_name <- basename(normalizePath(audio_folder))
parts <- strsplit(folder_name, "_")[[1]]

quadrant_value <- parts[2]
mic_type_value <- parts[3]

file_info <- tibble(
  audio_path = audio_files,
  audio_file = basename(audio_files),
  quadrant   = quadrant_value,
  mic_type   = mic_type_value
)

file_info <- file_info %>%
  mutate(
    sitename = basename(dirname(audio_path))
  )




# 3. PAPILDINFORMĀCIJAS PIEVIENOŠANA ----

## 3.1.Vietas ----

vietas=read_csv("./metadati/sites.0001.csv")
vietas=vietas %>% 
  mutate(sitename=paste0("ML_",name)) %>% 
  dplyr::select(site_id,sitename)


## 3.2.Ieraksti ----

ierakstu_faili=data.frame(celi=list.files("./metadati/",full.names = TRUE),
                          fails=list.files("./metadati/",full.names = FALSE))
ierakstu_faili=ierakstu_faili %>% 
  filter(stringr::str_detect(celi,"recordings"))

pirmais=read_csv(ierakstu_faili$celi[1])
for (i in 2:length(ierakstu_faili$celi)){
  nakosais=read_csv(ierakstu_faili$celi[i])
  pirmais=bind_rows(pirmais,nakosais)
}
ieraksti=pirmais


ieraksti <- ieraksti %>%
  mutate(
    # Izvelkam filename no meta JSON
    original_filename = str_extract(meta, '(?<="filename":")[^"]+'),
    # Nomainām .WAV uz .flac
    audio_file = str_replace(original_filename, "\\.WAV$", ".flac")
  )


ieraksti <- ieraksti %>%
  left_join(vietas, by = "site_id")


file_info <- file_info %>%
  left_join(vietas, by = "sitename") %>%
  select(audio_path, audio_file, quadrant, mic_type, sitename, site_id)


# 4. ANALIZE ----
# Batch izmērs
batch_size <- 100

# Sadalam audio failus batchos
batches <- split(audio_files, ceiling(seq_along(audio_files) / batch_size))




# ============================
## 4.1. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 0,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q3_m1_overlap_0.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("1. Analīze pabeigta. Visi rezultāti saglabāti.\n")



# ============================
## 4.2. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 0.5,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q3_m1_overlap_05.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("2. Analīze pabeigta. Visi rezultāti saglabāti.\n")




# ====================
## 4.3. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 1,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q3_m1_overlap_1.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("3. Analīze pabeigta. Visi rezultāti saglabāti.\n")
# =================================================





# ============================
## 4.4. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 1.5,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q3_m1_overlap_15.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("4. un pedeja analīze pabeigta. Visi rezultāti saglabāti.\n")
# =================================================
































# 2. AUDIO FAILU IELĀDE Q3 M3 ----

audio_folder <- "./ieraksti/ML_Q3_M3"

audio_files <- list.files(
  audio_folder,
  pattern = "\\.flac$",
  full.names = TRUE,
  recursive = TRUE
)

# Izvelkam informāciju no mapes nosaukuma
folder_name <- basename(normalizePath(audio_folder))
parts <- strsplit(folder_name, "_")[[1]]

quadrant_value <- parts[2]
mic_type_value <- parts[3]

file_info <- tibble(
  audio_path = audio_files,
  audio_file = basename(audio_files),
  quadrant   = quadrant_value,
  mic_type   = mic_type_value
)

file_info <- file_info %>%
  mutate(
    sitename = basename(dirname(audio_path))
  )




# 3. PAPILDINFORMĀCIJAS PIEVIENOŠANA ----

## 3.1.Vietas ----

vietas=read_csv("./metadati/sites.0001.csv")
vietas=vietas %>% 
  mutate(sitename=paste0("ML_",name)) %>% 
  dplyr::select(site_id,sitename)


## 3.2.Ieraksti ----

ierakstu_faili=data.frame(celi=list.files("./metadati/",full.names = TRUE),
                          fails=list.files("./metadati/",full.names = FALSE))
ierakstu_faili=ierakstu_faili %>% 
  filter(stringr::str_detect(celi,"recordings"))

pirmais=read_csv(ierakstu_faili$celi[1])
for (i in 2:length(ierakstu_faili$celi)){
  nakosais=read_csv(ierakstu_faili$celi[i])
  pirmais=bind_rows(pirmais,nakosais)
}
ieraksti=pirmais


ieraksti <- ieraksti %>%
  mutate(
    # Izvelkam filename no meta JSON
    original_filename = str_extract(meta, '(?<="filename":")[^"]+'),
    # Nomainām .WAV uz .flac
    audio_file = str_replace(original_filename, "\\.WAV$", ".flac")
  )


ieraksti <- ieraksti %>%
  left_join(vietas, by = "site_id")


file_info <- file_info %>%
  left_join(vietas, by = "sitename") %>%
  select(audio_path, audio_file, quadrant, mic_type, sitename, site_id)


# 4. ANALIZE ----
# Batch izmērs
batch_size <- 100

# Sadalam audio failus batchos
batches <- split(audio_files, ceiling(seq_along(audio_files) / batch_size))




# ============================
## 4.1. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 0,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q3_m3_overlap_0.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("1. Analīze pabeigta. Visi rezultāti saglabāti.\n")



# ============================
## 4.2. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 0.5,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q3_m3_overlap_05.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("2. Analīze pabeigta. Visi rezultāti saglabāti.\n")




# ====================
## 4.3. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 1,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q3_m3_overlap_1.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("3. Analīze pabeigta. Visi rezultāti saglabāti.\n")
# =================================================





# ============================
## 4.4. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 1.5,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q3_m3_overlap_15.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("4. un pedeja analīze pabeigta. Visi rezultāti saglabāti.\n")
# =================================================
































# 2. AUDIO FAILU IELĀDE Q3 M4 ----

audio_folder <- "./ieraksti/ML_Q3_M4"

audio_files <- list.files(
  audio_folder,
  pattern = "\\.flac$",
  full.names = TRUE,
  recursive = TRUE
)

# Izvelkam informāciju no mapes nosaukuma
folder_name <- basename(normalizePath(audio_folder))
parts <- strsplit(folder_name, "_")[[1]]

quadrant_value <- parts[2]
mic_type_value <- parts[3]

file_info <- tibble(
  audio_path = audio_files,
  audio_file = basename(audio_files),
  quadrant   = quadrant_value,
  mic_type   = mic_type_value
)

file_info <- file_info %>%
  mutate(
    sitename = basename(dirname(audio_path))
  )




# 3. PAPILDINFORMĀCIJAS PIEVIENOŠANA ----

## 3.1.Vietas ----

vietas=read_csv("./metadati/sites.0001.csv")
vietas=vietas %>% 
  mutate(sitename=paste0("ML_",name)) %>% 
  dplyr::select(site_id,sitename)


## 3.2.Ieraksti ----

ierakstu_faili=data.frame(celi=list.files("./metadati/",full.names = TRUE),
                          fails=list.files("./metadati/",full.names = FALSE))
ierakstu_faili=ierakstu_faili %>% 
  filter(stringr::str_detect(celi,"recordings"))

pirmais=read_csv(ierakstu_faili$celi[1])
for (i in 2:length(ierakstu_faili$celi)){
  nakosais=read_csv(ierakstu_faili$celi[i])
  pirmais=bind_rows(pirmais,nakosais)
}
ieraksti=pirmais


ieraksti <- ieraksti %>%
  mutate(
    # Izvelkam filename no meta JSON
    original_filename = str_extract(meta, '(?<="filename":")[^"]+'),
    # Nomainām .WAV uz .flac
    audio_file = str_replace(original_filename, "\\.WAV$", ".flac")
  )


ieraksti <- ieraksti %>%
  left_join(vietas, by = "site_id")


file_info <- file_info %>%
  left_join(vietas, by = "sitename") %>%
  select(audio_path, audio_file, quadrant, mic_type, sitename, site_id)


# 4. ANALIZE ----
# Batch izmērs
batch_size <- 100

# Sadalam audio failus batchos
batches <- split(audio_files, ceiling(seq_along(audio_files) / batch_size))




# ============================
## 4.1. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 0,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q3_m4_overlap_0.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("1. Analīze pabeigta. Visi rezultāti saglabāti.\n")



# ============================
## 4.2. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 0.5,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q3_m4_overlap_05.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("2. Analīze pabeigta. Visi rezultāti saglabāti.\n")




# ====================
## 4.3. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 1,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q3_m4_overlap_1.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("3. Analīze pabeigta. Visi rezultāti saglabāti.\n")
# =================================================





# ============================
## 4.4. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 1.5,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q3_m4_overlap_15.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("4. un pedeja analīze pabeigta. Visi rezultāti saglabāti.\n")
# =================================================
































# 2. AUDIO FAILU IELĀDE Q3 M5 ----

audio_folder <- "./ieraksti/ML_Q3_M5"

audio_files <- list.files(
  audio_folder,
  pattern = "\\.flac$",
  full.names = TRUE,
  recursive = TRUE
)

# Izvelkam informāciju no mapes nosaukuma
folder_name <- basename(normalizePath(audio_folder))
parts <- strsplit(folder_name, "_")[[1]]

quadrant_value <- parts[2]
mic_type_value <- parts[3]

file_info <- tibble(
  audio_path = audio_files,
  audio_file = basename(audio_files),
  quadrant   = quadrant_value,
  mic_type   = mic_type_value
)

file_info <- file_info %>%
  mutate(
    sitename = basename(dirname(audio_path))
  )




# 3. PAPILDINFORMĀCIJAS PIEVIENOŠANA ----

## 3.1.Vietas ----

vietas=read_csv("./metadati/sites.0001.csv")
vietas=vietas %>% 
  mutate(sitename=paste0("ML_",name)) %>% 
  dplyr::select(site_id,sitename)


## 3.2.Ieraksti ----

ierakstu_faili=data.frame(celi=list.files("./metadati/",full.names = TRUE),
                          fails=list.files("./metadati/",full.names = FALSE))
ierakstu_faili=ierakstu_faili %>% 
  filter(stringr::str_detect(celi,"recordings"))

pirmais=read_csv(ierakstu_faili$celi[1])
for (i in 2:length(ierakstu_faili$celi)){
  nakosais=read_csv(ierakstu_faili$celi[i])
  pirmais=bind_rows(pirmais,nakosais)
}
ieraksti=pirmais


ieraksti <- ieraksti %>%
  mutate(
    # Izvelkam filename no meta JSON
    original_filename = str_extract(meta, '(?<="filename":")[^"]+'),
    # Nomainām .WAV uz .flac
    audio_file = str_replace(original_filename, "\\.WAV$", ".flac")
  )


ieraksti <- ieraksti %>%
  left_join(vietas, by = "site_id")


file_info <- file_info %>%
  left_join(vietas, by = "sitename") %>%
  select(audio_path, audio_file, quadrant, mic_type, sitename, site_id)


# 4. ANALIZE ----
# Batch izmērs
batch_size <- 100

# Sadalam audio failus batchos
batches <- split(audio_files, ceiling(seq_along(audio_files) / batch_size))




# ============================
## 4.1. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 0,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q3_m5_overlap_0.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("1. Analīze pabeigta. Visi rezultāti saglabāti.\n")



# ============================
## 4.2. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 0.5,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q3_m5_overlap_05.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("2. Analīze pabeigta. Visi rezultāti saglabāti.\n")




# ====================
## 4.3. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 1,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q3_m5_overlap_1.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("3. Analīze pabeigta. Visi rezultāti saglabāti.\n")
# =================================================





# ============================
## 4.4. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 1.5,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q3_m5_overlap_15.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("4. un pedeja analīze pabeigta. Visi rezultāti saglabāti.\n")
# =================================================

































# 2. AUDIO FAILU IELĀDE Q4 L ----

audio_folder <- "./ieraksti/ML_Q4_L"

audio_files <- list.files(
  audio_folder,
  pattern = "\\.flac$",
  full.names = TRUE,
  recursive = TRUE
)

# Izvelkam informāciju no mapes nosaukuma
folder_name <- basename(normalizePath(audio_folder))
parts <- strsplit(folder_name, "_")[[1]]

quadrant_value <- parts[2]
mic_type_value <- parts[3]

file_info <- tibble(
  audio_path = audio_files,
  audio_file = basename(audio_files),
  quadrant   = quadrant_value,
  mic_type   = mic_type_value
)

file_info <- file_info %>%
  mutate(
    sitename = basename(dirname(audio_path))
  )




# 3. PAPILDINFORMĀCIJAS PIEVIENOŠANA ----

## 3.1.Vietas ----

vietas=read_csv("./metadati/sites.0001.csv")
vietas=vietas %>% 
  mutate(sitename=paste0("ML_",name)) %>% 
  dplyr::select(site_id,sitename)


## 3.2.Ieraksti ----

ierakstu_faili=data.frame(celi=list.files("./metadati/",full.names = TRUE),
                          fails=list.files("./metadati/",full.names = FALSE))
ierakstu_faili=ierakstu_faili %>% 
  filter(stringr::str_detect(celi,"recordings"))

pirmais=read_csv(ierakstu_faili$celi[1])
for (i in 2:length(ierakstu_faili$celi)){
  nakosais=read_csv(ierakstu_faili$celi[i])
  pirmais=bind_rows(pirmais,nakosais)
}
ieraksti=pirmais


ieraksti <- ieraksti %>%
  mutate(
    # Izvelkam filename no meta JSON
    original_filename = str_extract(meta, '(?<="filename":")[^"]+'),
    # Nomainām .WAV uz .flac
    audio_file = str_replace(original_filename, "\\.WAV$", ".flac")
  )


ieraksti <- ieraksti %>%
  left_join(vietas, by = "site_id")


file_info <- file_info %>%
  left_join(vietas, by = "sitename") %>%
  select(audio_path, audio_file, quadrant, mic_type, sitename, site_id)


# 4. ANALIZE ----
# Batch izmērs
batch_size <- 100

# Sadalam audio failus batchos
batches <- split(audio_files, ceiling(seq_along(audio_files) / batch_size))




# ============================
## 4.1. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 0,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q4_l_overlap_0.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("1. Analīze pabeigta. Visi rezultāti saglabāti.\n")



# ============================
## 4.2. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 0.5,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q4_l_overlap_05.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("2. Analīze pabeigta. Visi rezultāti saglabāti.\n")




# ====================
## 4.3. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 1,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q4_l_overlap_1.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("3. Analīze pabeigta. Visi rezultāti saglabāti.\n")
# =================================================





# ============================
## 4.4. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 1.5,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q4_l_overlap_15.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("4. un pedeja analīze pabeigta. Visi rezultāti saglabāti.\n")
# =================================================































# 2. AUDIO FAILU IELĀDE Q4 M1 ----

audio_folder <- "./ieraksti/ML_Q4_M1"

audio_files <- list.files(
  audio_folder,
  pattern = "\\.flac$",
  full.names = TRUE,
  recursive = TRUE
)

# Izvelkam informāciju no mapes nosaukuma
folder_name <- basename(normalizePath(audio_folder))
parts <- strsplit(folder_name, "_")[[1]]

quadrant_value <- parts[2]
mic_type_value <- parts[3]

file_info <- tibble(
  audio_path = audio_files,
  audio_file = basename(audio_files),
  quadrant   = quadrant_value,
  mic_type   = mic_type_value
)

file_info <- file_info %>%
  mutate(
    sitename = basename(dirname(audio_path))
  )




# 3. PAPILDINFORMĀCIJAS PIEVIENOŠANA ----

## 3.1.Vietas ----

vietas=read_csv("./metadati/sites.0001.csv")
vietas=vietas %>% 
  mutate(sitename=paste0("ML_",name)) %>% 
  dplyr::select(site_id,sitename)


## 3.2.Ieraksti ----

ierakstu_faili=data.frame(celi=list.files("./metadati/",full.names = TRUE),
                          fails=list.files("./metadati/",full.names = FALSE))
ierakstu_faili=ierakstu_faili %>% 
  filter(stringr::str_detect(celi,"recordings"))

pirmais=read_csv(ierakstu_faili$celi[1])
for (i in 2:length(ierakstu_faili$celi)){
  nakosais=read_csv(ierakstu_faili$celi[i])
  pirmais=bind_rows(pirmais,nakosais)
}
ieraksti=pirmais


ieraksti <- ieraksti %>%
  mutate(
    # Izvelkam filename no meta JSON
    original_filename = str_extract(meta, '(?<="filename":")[^"]+'),
    # Nomainām .WAV uz .flac
    audio_file = str_replace(original_filename, "\\.WAV$", ".flac")
  )


ieraksti <- ieraksti %>%
  left_join(vietas, by = "site_id")


file_info <- file_info %>%
  left_join(vietas, by = "sitename") %>%
  select(audio_path, audio_file, quadrant, mic_type, sitename, site_id)


# 4. ANALIZE ----
# Batch izmērs
batch_size <- 100

# Sadalam audio failus batchos
batches <- split(audio_files, ceiling(seq_along(audio_files) / batch_size))




# ============================
## 4.1. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 0,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q4_m1_overlap_0.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("1. Analīze pabeigta. Visi rezultāti saglabāti.\n")



# ============================
## 4.2. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 0.5,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q4_m1_overlap_05.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("2. Analīze pabeigta. Visi rezultāti saglabāti.\n")




# ====================
## 4.3. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 1,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q4_m1_overlap_1.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("3. Analīze pabeigta. Visi rezultāti saglabāti.\n")
# =================================================





# ============================
## 4.4. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 1.5,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q4_m1_overlap_15.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("4. un pedeja analīze pabeigta. Visi rezultāti saglabāti.\n")
# =================================================
































# 2. AUDIO FAILU IELĀDE Q4 M2 ----

audio_folder <- "./ieraksti/ML_Q4_M2"

audio_files <- list.files(
  audio_folder,
  pattern = "\\.flac$",
  full.names = TRUE,
  recursive = TRUE
)

# Izvelkam informāciju no mapes nosaukuma
folder_name <- basename(normalizePath(audio_folder))
parts <- strsplit(folder_name, "_")[[1]]

quadrant_value <- parts[2]
mic_type_value <- parts[3]

file_info <- tibble(
  audio_path = audio_files,
  audio_file = basename(audio_files),
  quadrant   = quadrant_value,
  mic_type   = mic_type_value
)

file_info <- file_info %>%
  mutate(
    sitename = basename(dirname(audio_path))
  )




# 3. PAPILDINFORMĀCIJAS PIEVIENOŠANA ----

## 3.1.Vietas ----

vietas=read_csv("./metadati/sites.0001.csv")
vietas=vietas %>% 
  mutate(sitename=paste0("ML_",name)) %>% 
  dplyr::select(site_id,sitename)


## 3.2.Ieraksti ----

ierakstu_faili=data.frame(celi=list.files("./metadati/",full.names = TRUE),
                          fails=list.files("./metadati/",full.names = FALSE))
ierakstu_faili=ierakstu_faili %>% 
  filter(stringr::str_detect(celi,"recordings"))

pirmais=read_csv(ierakstu_faili$celi[1])
for (i in 2:length(ierakstu_faili$celi)){
  nakosais=read_csv(ierakstu_faili$celi[i])
  pirmais=bind_rows(pirmais,nakosais)
}
ieraksti=pirmais


ieraksti <- ieraksti %>%
  mutate(
    # Izvelkam filename no meta JSON
    original_filename = str_extract(meta, '(?<="filename":")[^"]+'),
    # Nomainām .WAV uz .flac
    audio_file = str_replace(original_filename, "\\.WAV$", ".flac")
  )


ieraksti <- ieraksti %>%
  left_join(vietas, by = "site_id")


file_info <- file_info %>%
  left_join(vietas, by = "sitename") %>%
  select(audio_path, audio_file, quadrant, mic_type, sitename, site_id)


# 4. ANALIZE ----
# Batch izmērs
batch_size <- 100

# Sadalam audio failus batchos
batches <- split(audio_files, ceiling(seq_along(audio_files) / batch_size))




# ============================
## 4.1. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 0,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q4_m2_overlap_0.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("1. Analīze pabeigta. Visi rezultāti saglabāti.\n")



# ============================
## 4.2. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 0.5,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q4_m2_overlap_05.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("2. Analīze pabeigta. Visi rezultāti saglabāti.\n")




# ====================
## 4.3. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 1,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q4_m2_overlap_1.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("3. Analīze pabeigta. Visi rezultāti saglabāti.\n")
# =================================================





# ============================
## 4.4. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 1.5,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q4_m2_overlap_15.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("4. un pedeja analīze pabeigta. Visi rezultāti saglabāti.\n")
# =================================================
































# 2. AUDIO FAILU IELĀDE Q4 M3 ----

audio_folder <- "./ieraksti/ML_Q4_M3"

audio_files <- list.files(
  audio_folder,
  pattern = "\\.flac$",
  full.names = TRUE,
  recursive = TRUE
)

# Izvelkam informāciju no mapes nosaukuma
folder_name <- basename(normalizePath(audio_folder))
parts <- strsplit(folder_name, "_")[[1]]

quadrant_value <- parts[2]
mic_type_value <- parts[3]

file_info <- tibble(
  audio_path = audio_files,
  audio_file = basename(audio_files),
  quadrant   = quadrant_value,
  mic_type   = mic_type_value
)

file_info <- file_info %>%
  mutate(
    sitename = basename(dirname(audio_path))
  )




# 3. PAPILDINFORMĀCIJAS PIEVIENOŠANA ----

## 3.1.Vietas ----

vietas=read_csv("./metadati/sites.0001.csv")
vietas=vietas %>% 
  mutate(sitename=paste0("ML_",name)) %>% 
  dplyr::select(site_id,sitename)


## 3.2.Ieraksti ----

ierakstu_faili=data.frame(celi=list.files("./metadati/",full.names = TRUE),
                          fails=list.files("./metadati/",full.names = FALSE))
ierakstu_faili=ierakstu_faili %>% 
  filter(stringr::str_detect(celi,"recordings"))

pirmais=read_csv(ierakstu_faili$celi[1])
for (i in 2:length(ierakstu_faili$celi)){
  nakosais=read_csv(ierakstu_faili$celi[i])
  pirmais=bind_rows(pirmais,nakosais)
}
ieraksti=pirmais


ieraksti <- ieraksti %>%
  mutate(
    # Izvelkam filename no meta JSON
    original_filename = str_extract(meta, '(?<="filename":")[^"]+'),
    # Nomainām .WAV uz .flac
    audio_file = str_replace(original_filename, "\\.WAV$", ".flac")
  )


ieraksti <- ieraksti %>%
  left_join(vietas, by = "site_id")


file_info <- file_info %>%
  left_join(vietas, by = "sitename") %>%
  select(audio_path, audio_file, quadrant, mic_type, sitename, site_id)


# 4. ANALIZE ----
# Batch izmērs
batch_size <- 100

# Sadalam audio failus batchos
batches <- split(audio_files, ceiling(seq_along(audio_files) / batch_size))




# ============================
## 4.1. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 0,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q4_m3_overlap_0.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("1. Analīze pabeigta. Visi rezultāti saglabāti.\n")



# ============================
## 4.2. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 0.5,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q4_m3_overlap_05.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("2. Analīze pabeigta. Visi rezultāti saglabāti.\n")




# ====================
## 4.3. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 1,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q4_m3_overlap_1.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("3. Analīze pabeigta. Visi rezultāti saglabāti.\n")
# =================================================





# ============================
## 4.4. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 1.5,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q4_m3_overlap_15.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("4. un pedeja analīze pabeigta. Visi rezultāti saglabāti.\n")
# =================================================
































# 2. AUDIO FAILU IELĀDE Q4 M4 ----

audio_folder <- "./ieraksti/ML_Q4_M4"

audio_files <- list.files(
  audio_folder,
  pattern = "\\.flac$",
  full.names = TRUE,
  recursive = TRUE
)

# Izvelkam informāciju no mapes nosaukuma
folder_name <- basename(normalizePath(audio_folder))
parts <- strsplit(folder_name, "_")[[1]]

quadrant_value <- parts[2]
mic_type_value <- parts[3]

file_info <- tibble(
  audio_path = audio_files,
  audio_file = basename(audio_files),
  quadrant   = quadrant_value,
  mic_type   = mic_type_value
)

file_info <- file_info %>%
  mutate(
    sitename = basename(dirname(audio_path))
  )




# 3. PAPILDINFORMĀCIJAS PIEVIENOŠANA ----

## 3.1.Vietas ----

vietas=read_csv("./metadati/sites.0001.csv")
vietas=vietas %>% 
  mutate(sitename=paste0("ML_",name)) %>% 
  dplyr::select(site_id,sitename)


## 3.2.Ieraksti ----

ierakstu_faili=data.frame(celi=list.files("./metadati/",full.names = TRUE),
                          fails=list.files("./metadati/",full.names = FALSE))
ierakstu_faili=ierakstu_faili %>% 
  filter(stringr::str_detect(celi,"recordings"))

pirmais=read_csv(ierakstu_faili$celi[1])
for (i in 2:length(ierakstu_faili$celi)){
  nakosais=read_csv(ierakstu_faili$celi[i])
  pirmais=bind_rows(pirmais,nakosais)
}
ieraksti=pirmais


ieraksti <- ieraksti %>%
  mutate(
    # Izvelkam filename no meta JSON
    original_filename = str_extract(meta, '(?<="filename":")[^"]+'),
    # Nomainām .WAV uz .flac
    audio_file = str_replace(original_filename, "\\.WAV$", ".flac")
  )


ieraksti <- ieraksti %>%
  left_join(vietas, by = "site_id")


file_info <- file_info %>%
  left_join(vietas, by = "sitename") %>%
  select(audio_path, audio_file, quadrant, mic_type, sitename, site_id)


# 4. ANALIZE ----
# Batch izmērs
batch_size <- 100

# Sadalam audio failus batchos
batches <- split(audio_files, ceiling(seq_along(audio_files) / batch_size))




# ============================
## 4.1. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 0,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q4_m4_overlap_0.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("1. Analīze pabeigta. Visi rezultāti saglabāti.\n")



# ============================
## 4.2. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 0.5,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q4_m4_overlap_05.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("2. Analīze pabeigta. Visi rezultāti saglabāti.\n")




# ====================
## 4.3. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 1,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q4_m4_overlap_1.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("3. Analīze pabeigta. Visi rezultāti saglabāti.\n")
# =================================================





# ============================
## 4.4. Cikls pa batchiem ----
for (i in seq_along(batches)) {
  
  cat("Apstrādājam batch", i, "no", length(batches), "\n")
  
  results_list <- lapply(batches[[i]], function(f) {
    cat("Analizē:", f, "\n")
    
    res <- predict_species_from_audio_file(
      model_proto,
      f,
      min_confidence = 0.1,
      chunk_overlap_s = 1.5,
      keep_empty = FALSE
    )
    
    # ja nav rezultātu
    if (nrow(res) == 0) return(NULL)
    
    res <- res %>%
      mutate(
        audio_path = f,
        audio_file = basename(f),
        quadrant   = quadrant_value,
        mic_type   = mic_type_value
      )
    
    res
  })
  
  # Apvienojam batch rezultātus
  batch_df <- bind_rows(results_list)
  
  # Pievienojam site_id no file_info
  batch_df <- batch_df %>%
    left_join(
      file_info %>% select(audio_file, site_id),
      by = "audio_file"
    ) %>%
    left_join(
      ieraksti %>% select(audio_file, recording_id, datetime, datetime_utc),
      by = "audio_file"
    )
  
  # Saglabājam CSV – ja CSV vēl nav, tad izveidojam ar header, ja jau ir, tad papildinām
  csv_file <- "all_results_q4_m4_overlap_15.csv"
  if (!file.exists(csv_file)) {
    write_csv(batch_df, csv_file)
  } else {
    write_csv(batch_df, csv_file, append = TRUE)
  }
  
  # Atbrīvojam atmiņu
  rm(results_list, batch_df)
  gc()
  
  cat("Batch", i, "ir saglabāts.\n")
}

cat("4. un pedeja analīze pabeigta. Visi rezultāti saglabāti.\n")
# =================================================


































































































































































# 4. PAPILDINFORMĀCIJAS PIEVIENOŠANA ----


library(tidyverse)

# 1. Atrodam visus CSV failus Metadati mapē
metadata_files <- list.files(
  path = "./Metadati",
  pattern = "recordings\\..*\\.csv$", # Atrod visus recordings.XXXX.csv
  full.names = TRUE
)

# Ielādējam visus metadatu failus bez liekām darbībām
recordings_info <- metadata_files %>%
  map_df(~read_csv(.x, show_col_types = FALSE))




library(tidyverse)
library(lubridate)

recordings_info <- recordings_info %>%
  # 1. Sadalām tekstu pie atstarpes divās kolonnās
  separate(datetime_utc, into = c("date_raw", "time"), sep = " ", remove = FALSE) %>%
  
  # 2. Pārvēršam "date_raw" par īstu datumu un aprēķinām nedēļu
  mutate(
    date = mdy(date_raw),
    birdnet_week = birdnet_week(date)
  ) %>%
  
  # 3. Izdzēšam lieko "date_raw" kolonnu
  select(-date_raw)



# Vietu koordinātas
sites_info <- read_csv("./Metadati/sites.0001.csv") %>%
  select(-elevation, -hidden, -created_at)




# 5. BIRDNET NEDEĻU APRĒĶINS ----

birdnet_week <- function(date) {
  
  date <- as.Date(date)
  
  month <- as.integer(format(date, "%m"))
  day   <- as.integer(format(date, "%d"))
  
  week_in_month <- case_when(
    day <= 7  ~ 1L,
    day <= 14 ~ 2L,
    day <= 21 ~ 3L,
    TRUE      ~ 4L
  )
  
  (month - 1) * 4 + week_in_month
}

recordings_info <- recordings_info %>%
  mutate(
    birdnet_week = birdnet_week(date)
  )




recordings_info <- recordings_info %>%
  left_join(sites_info, by = "site_id")


recordings_info <- recordings_info %>%
  separate(name, into = c("quadrant", "mic_type"), sep = "_", remove = FALSE)


# 6. META MODELIS ----


file_info <- recordings_info
  

## 6.1. META (VIETA) ----

results_list_meta_sites <- lapply(
  seq_len(nrow(sites_info)),
  function(i) {
    
    res <- predict_species_at_location_and_time(
      model = model_meta,
      latitude = sites_info$latitude[i],
      longitude = sites_info$longitude[i],
      week = NULL,
      min_confidence = 0.01
    )
    
    if (is.null(res) || length(res$label) == 0) {
      return(NULL)
    }
    
    tibble(
      label      = res$label,
      confidence = res$confidence,
      site_id    = sites_info$site_id[i]
    )
  }
)

results_list_meta_sites_clean <- lapply(
  results_list_meta_sites,
  process_meta_result
)

lookup_sites <- file_info %>%
  select(site_id, quadrant, mic_type) %>%
  distinct()

results_meta_sites <- bind_rows(
  results_list_meta_sites_clean
) %>%
  left_join(
    lookup_sites,
    by = "site_id"
  )




## 6.2. META (VIETA / NEDĒĻA) ----

results_list_meta_sites_week <- lapply(seq_len(nrow(sites_info)), function(i) {
  lapply(1:48, function(week_i) {
    predict_species_at_location_and_time(
      model = model_meta,
      latitude = sites_info$latitude[i],
      longitude = sites_info$longitude[i],
      week = week_i,
      min_confidence = 0.01
    ) %>%
      bind_cols(
        tibble(
          site_id = sites_info$site_id[i],
          week = week_i
        )
      )
  })
})

results_list_meta_sites_week_clean <- lapply(results_list_meta_sites_week, function(site_list) {
  lapply(site_list, process_meta_result)
})

results_meta_sites_week <- bind_rows(
  results_list_meta_sites_week_clean
) %>%
  left_join(
    lookup_sites,
    by = "site_id"
  )






# 7. REZULTĀTU SAGLABĀŠANA ----

# Meta (site)
write.csv(
  results_meta_sites,
  "BirdnetR_meta_sites.csv",
  row.names = FALSE
)

# Meta (site/week)
write.csv(
  results_meta_sites_week,
  "BirdnetR_meta_sites_week.csv",
  row.names = FALSE
)


# Atlasa tikai Latvijā sastopamās sugas
# results_meta_latvia <- read.csv("Metadati/BirdnetR_meta_latvia.csv")
# 
# 
# # Izveido vidējo confidence
# results_meta_sites_average <- results_meta_sites %>%
#   select(scientific_name, confidence) %>%
#   group_by(scientific_name) %>%
#   summarise(
#     mean_confidence = mean(confidence, na.rm = TRUE),
#     n = n(),
#     .groups = "drop"
#   ) %>%
#   arrange(desc(mean_confidence))
# 
# 
# # Izveido vidējo confidence ņemot vērā nedēļu
# results_meta_sites_week_average <- results_meta_sites_week %>%
#   select(scientific_name, confidence) %>%
#   group_by(scientific_name) %>%
#   summarise(
#     mean_confidence = mean(confidence, na.rm = TRUE),
#     n = n(),
#     .groups = "drop"
#   ) %>%
#   arrange(desc(mean_confidence))
# 
# 
# # Salīdzina tos savā starpā
# comparison_confidence <- results_meta_sites_week_average %>%
#   select(scientific_name, mean_confidence_week = mean_confidence) %>%
#   inner_join(
#     results_meta_sites_average %>%
#       select(scientific_name, mean_confidence_site = mean_confidence),
#     by = "scientific_name"
#   )
# 
# comparison_confidence <- comparison_confidence %>%
#   mutate(
#     diff_confidence = abs(mean_confidence_week - mean_confidence_site)
#   )
# 
# 
# 
# 
# library(ggplot2)
# 
# ggplot(comparison_confidence,
#        aes(x = mean_confidence_site, y = mean_confidence_week)) +
#   geom_point(alpha = 0.6) +
#   geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
#   labs(
#     x = "Mean confidence (site)",
#     y = "Mean confidence (site × week)",
#     title = "BirdNET confidence: site vs site–week"
#   ) +
#   theme_minimal()
# 
# 
# 
# 
# 8. CITU REZULTĀTU SAGLABĀŠANA ----
# 
# 
# 
# Meta (Latvija)
# write.csv(
#   results_meta_latvia,
#   "BirdnetR_meta_latvia.csv",
#   row.names = FALSE
# )
# 
# Meta (Average)
# write.csv(
#   results_meta_sites_average,
#   "BirdnetR_meta_sites_average.csv",
#   row.names = FALSE
# )
# 
# Meta (Average/week)
# write.csv(
#   results_meta_sites_week_average,
#   "BirdnetR_meta_sites_week_average.csv",
#   row.names = FALSE
# )
# 
# 
# 
# 7. LATVIJAS SUGU FILTRĒŠANA ----
# 
# 
# # Atrast visus failus kas sākas ar "all_results_"
# csv_files <- list.files(pattern = "^all_results_.*\\.csv$")
# 
# # Ielasīt katru failu un izveidot datu kopu ar "_meta_latvia" galā
# for (file in csv_files) {
#   # Iegūt nosaukumu bez .csv paplašinājuma
#   name <- tools::file_path_sans_ext(file)
#   
#   # Izveidot jauno datu kopas nosaukumu
#   new_name <- paste0(name, "_meta_latvia")
#   
#   # Ielasīt failu un piešķirt jaunam nosaukumam
#   assign(new_name, read.csv(file))
# }
# 
# 
# 
# 
# # Iegūt unikālās vērtības no results_meta_latvia
# valid_names <- unique(results_meta_latvia$scientific_name)
# 
# # Filtrēt visas all_results_ datu kopas
# filtered_objects <- ls(pattern = "^all_results_")
# 
# for (obj_name in filtered_objects) {
#   df <- get(obj_name)
#   df_filtered <- df %>% filter(scientific_name %in% valid_names)
#   assign(obj_name, df_filtered)
# }
# 
# 
# # Izveidot mapi ja tā neeksistē
# dir.create("meta_latvia", showWarnings = FALSE)
# 
# # Saglabāt visas all_results_ datu kopas
# for (obj_name in ls(pattern = "^all_results_")) {
#   write.csv(
#     get(obj_name),
#     file.path("meta_latvia", paste0(obj_name, ".csv")),
#     row.names = FALSE
#   )
# }
