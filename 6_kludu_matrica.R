# ============================================================================
# TP/FP/FN ANALĪZE PA OVERLAP
# ============================================================================

library(tidyverse)
library(readxl)

# ----------------------------------------------------------------------------
# 1. IELĀDĒ DATUS
# ----------------------------------------------------------------------------

manual_files <- list.files(pattern = "^manual_.*\\.xlsx$", full.names = TRUE)
manual <- map_df(manual_files, read_excel) %>%
  mutate(
    manual_id = row_number(),
    scientific_name = str_to_lower(str_trim(scientific_name))
  )

cat("Ielādēti", nrow(manual), "manuālie novērojumi\n")
cat("Unikālie manuāli klausītie ieraksti:", length(unique(manual$audio_file)), "\n")
cat("Unikālās sugas:", length(unique(manual$scientific_name)), "\n\n")

birdnet_files <- list.files("proto", pattern = "\\.csv$", full.names = TRUE)
birdnet_all <- map_df(birdnet_files, read_csv, show_col_types = FALSE) %>%
  filter(latvia == 1) %>%
  mutate(scientific_name = str_to_lower(str_trim(scientific_name)))

cat("BirdNET novērojumi PIRMS filtrēšanas:", nrow(birdnet_all), "\n\n")

# Filtrē pa ierakstiem UN sugām UN confidence
manual_audio_files <- unique(manual$audio_file)
manual_species <- unique(manual$scientific_name)

birdnet_filtered <- birdnet_all %>%
  filter(
    audio_file %in% manual_audio_files,
    scientific_name %in% manual_species,
    # confidence >= 0.3
  )

cat("BirdNET PĒC filtrēšanas:", nrow(birdnet_filtered), "\n")
cat("Unikālās sugas:", length(unique(birdnet_filtered$scientific_name)), "\n\n")


# ----------------------------------------------------------------------------
# 2. FUNKCIJA: SUGU-SPECIFISKA ANALĪZE AR 0.5s PĀRKLĀŠANOS
# ----------------------------------------------------------------------------

analyze_overlap <- function(birdnet_data, manual_data, overlap_value) {
  
  cat("ANALIZĒ OVERLAP =", overlap_value, "\n")
  
  birdnet <- birdnet_data %>%
    filter(overlap == overlap_value) %>%
    mutate(birdnet_id = row_number())
  
  cat("BirdNET novērojumi:", nrow(birdnet), "\n")
  
  if (nrow(birdnet) == 0) {
    return(NULL)
  }
  
  # Sugu saraksts
  species_in_manual <- unique(manual_data$scientific_name)
  species_in_birdnet <- unique(birdnet$scientific_name)
  species_list <- intersect(species_in_manual, species_in_birdnet)
  
  cat("Sugas analīzei:", length(species_list), "\n\n")
  
  all_birdnet_results <- list()
  all_manual_results <- list()
  
  # KATRAI SUGAI
  for (species in species_list) {
    
    birdnet_sp <- birdnet %>% filter(scientific_name == species)
    manual_sp <- manual_data %>% filter(scientific_name == species)
    
    # Join visas kombinācijas
    candidates <- birdnet_sp %>%
      inner_join(
        manual_sp,
        by = c("audio_file", "site_id"),
        suffix = c("_birdnet", "_manual"),
        relationship = "many-to-many"
      )
    
    # Aprēķina pārklāšanās ilgumu un filtrē >= 0.5s
    matches <- candidates %>%
      mutate(
        overlap_start = pmax(start_birdnet, start_manual),
        overlap_end = pmin(end_birdnet, end_manual),
        overlap_duration = overlap_end - overlap_start
      ) %>%
      filter(
        overlap_duration >= 0.5
      )
    
    # TP/FP
    tp_birdnet_ids <- unique(matches$birdnet_id)
    
    birdnet_sp_results <- birdnet_sp %>%
      mutate(
        TP = if_else(birdnet_id %in% tp_birdnet_ids, 1, 0),
        FP = if_else(birdnet_id %in% tp_birdnet_ids, 0, 1),
        FN = 0
      )
    
    # FN
    fn_manual_ids <- unique(matches$manual_id)
    
    manual_sp_results <- manual_sp %>%
      filter(!manual_id %in% fn_manual_ids) %>%
      mutate(TP = 0, FP = 0, FN = 1)
    
    all_birdnet_results[[species]] <- birdnet_sp_results
    all_manual_results[[species]] <- manual_sp_results
  }
  
  birdnet_results <- bind_rows(all_birdnet_results) %>%
    mutate(overlap_analyzed = overlap_value, source = "birdnet")
  
  manual_results <- bind_rows(all_manual_results) %>%
    mutate(overlap_analyzed = overlap_value, source = "manual_only")
  
  results <- bind_rows(birdnet_results, manual_results)
  
  # Statistika
  tp <- sum(results$TP)
  fp <- sum(results$FP)
  fn <- sum(results$FN)
  
  precision <- tp / (tp + fp)
  recall <- tp / (tp + fn)
  f1 <- 2 * (precision * recall) / (precision + recall)
  
  cat("TP:", tp, " FP:", fp, " FN:", fn, "\n")
  cat("Precision:", round(precision*100,2), "% Recall:", round(recall*100,2), "%\n")
  
  list(
    results = results,
    summary = tibble(
      overlap = overlap_value,
      TP = tp, FP = fp, FN = fn,
      precision = precision,
      recall = recall,
      f1_score = f1
    )
  )
}

# ----------------------------------------------------------------------------
# 3. ANALIZĒ VISUS OVERLAP
# ----------------------------------------------------------------------------

overlap_values <- c(0, 0.5, 1, 1.5)
all_results <- list()
all_summaries <- list()

for (ov in overlap_values) {
  result <- analyze_overlap(birdnet_filtered, manual, ov)
  if (!is.null(result)) {
    all_results[[as.character(ov)]] <- result$results
    all_summaries[[as.character(ov)]] <- result$summary
  }
}


# ----------------------------------------------------------------------------
# 5. APRĒĶINA METRIKU PA SUGĀM
# ----------------------------------------------------------------------------



combined_results <- bind_rows(all_results, .id = "overlap_group")

species_metrics <- combined_results %>%
  group_by(overlap_analyzed, scientific_name) %>%
  summarise(
    TP = sum(TP, na.rm = TRUE),
    FP = sum(FP, na.rm = TRUE),
    FN = sum(FN, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    # Pamatmetriku
    precision = TP / (TP + FP),
    recall = TP / (TP + FN),
    f1_score = 2 * (precision * recall) / (precision + recall),
    
    # Papildu metriku
    accuracy = TP / (TP + FP + FN),  # Vienkāršota versija bez TN
    
    # Absolūtie skaitļi
    total_detections = TP + FP,  # Cik reizes BirdNET detektēja šo sugu
    total_manual = TP + FN,      # Cik reizes manuāli novērota
    
    # Kļūdu līmenis
    false_positive_rate = FP / (TP + FP),  # % no BirdNET detekcijām, kas ir FP
    false_negative_rate = FN / (TP + FN)   # % no manuālajām, ko BirdNET nedetektēja
  ) %>%
  # Nomaina NaN un Inf ar NA (gadījumos, kad dalīšana ar 0)
  mutate(across(where(is.numeric), ~ifelse(is.nan(.) | is.infinite(.), NA, .)))

# Saglabā
write_csv(species_metrics, "./rezultati/metrika_pa_sugam.csv")

# Parāda top/bottom sugas pēc F1
cat("\nLABĀKĀS SUGAS (pēc F1, overlap = 0):\n")
species_metrics %>%
  filter(overlap_analyzed == 0) %>%
  arrange(desc(f1_score)) %>%
  select(scientific_name, TP, FP, FN, precision, recall, f1_score) %>%
  head(10) %>%
  print()

cat("\nSLIKTĀKĀS SUGAS (pēc F1, overlap = 0):\n")
species_metrics %>%
  filter(overlap_analyzed == 0) %>%
  arrange(f1_score) %>%
  select(scientific_name, TP, FP, FN, precision, recall, f1_score) %>%
  head(10) %>%
  print()




# ----------------------------------------------------------------------------
# 4. SAGLABĀ
# ----------------------------------------------------------------------------

combined_results <- bind_rows(all_results, .id = "overlap_group") %>%
  select(-overlap_group, -TempIN, -sure, -overlap_analyzed, -birdnet_id, -source, -manual_id)  # Noņem nevajadzīgās kolonnas

write_csv(combined_results, "./rezultati/TP_FP_FN_rezultati.csv")

summary_by_overlap <- bind_rows(all_summaries)
write_csv(summary_by_overlap, "./rezultati/TP_FP_FN_salidzinajums.csv")

cat("SALĪDZINĀJUMS\n")
print(summary_by_overlap, n = Inf)
