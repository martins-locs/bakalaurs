# 0. Bibliotēkas ----
library(tidyverse)
library(readxl)




# 1. Ielādē datus ----

manual_files <- file.path("manuali_klausits", "manual.xlsx")

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




# 2. Funkcija: Sugu-specifiska analīze ar 0.5s pārklāšanos ----

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
  
  # Katrai sugai
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
  sensitivity <- tp / (tp + fn)
  f1 <- 2 * (precision * sensitivity) / (precision + sensitivity)
  
  cat("TP:", tp, " FP:", fp, " FN:", fn, "\n")
  cat("Precision:", round(precision*100,2), "% sensitivity:", round(sensitivity*100,2), "%\n")
  
  list(
    results = results,
    summary = tibble(
      overlap = overlap_value,
      TP = tp, FP = fp, FN = fn,
      precision = precision,
      sensitivity = sensitivity,
      f1_score = f1
    )
  )
}




# 3. Overlap analīze ----
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




# 4. Aprēķina metriku pa sugām ----
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
    sensitivity = TP / (TP + FN),
    f1_score = 2 * (precision * sensitivity) / (precision + sensitivity),
    
    # Papildu metriku
    accuracy = TP / (TP + FP + FN),
    
    # Absolūtie skaitļi
    total_detections = TP + FP,
    total_manual = TP + FN,
    
    # Kļūdu līmenis
    false_positive_rate = FP / (TP + FP), 
    false_negative_rate = FN / (TP + FN)
  ) %>%
  # Nomaina NaN un Inf ar NA (gadījumos, kad dalīšana ar 0)
  mutate(across(where(is.numeric), ~ifelse(is.nan(.) | is.infinite(.), NA, .)))

# Saglabā
write_csv(species_metrics, "./rezultati/metrika_pa_sugam.csv")

# Parāda top/bottom sugas pēc F1
cat("\nSugas ar labākajiem rādītājiem (pēc F1, overlap = 0):\n")
species_metrics %>%
  filter(overlap_analyzed == 0) %>%
  arrange(desc(f1_score)) %>%
  select(scientific_name, TP, FP, FN, precision, sensitivity, f1_score) %>%
  head(10) %>%
  print()

cat("\nSugas ar sliktākajiem rādītājiem (pēc F1, overlap = 0):\n")
species_metrics %>%
  filter(overlap_analyzed == 0) %>%
  arrange(f1_score) %>%
  select(scientific_name, TP, FP, FN, precision, sensitivity, f1_score) %>%
  head(10) %>%
  print()




# 5. Saglabā ----

combined_results <- bind_rows(all_results, .id = "overlap_group") %>%
  select(-overlap_group, -overlap_analyzed, -birdnet_id, -source, -manual_id) %>%
  mutate(response = if_else(TP == 1, 1, 0))


write_csv(combined_results, "./rezultati/TP_FP_FN_rezultati.csv")

summary_by_overlap <- bind_rows(all_summaries)
write_csv(summary_by_overlap, "./rezultati/TP_FP_FN_salidzinajums.csv")

cat("Salīdzinājums\n")
print(summary_by_overlap, n = Inf)


# Ielādē datus
species_metrics <- read_csv("./rezultati/metrika_pa_sugam.csv")

# Sagatavo datus violin grafikam
metrics_long <- species_metrics %>%
  select(scientific_name, overlap_analyzed, precision, sensitivity, f1_score) %>%
  pivot_longer(
    cols = c(precision, sensitivity, f1_score),
    names_to = "metric",
    values_to = "value"
  ) %>%
  filter(!is.na(value)) %>%  # Noņem NA vērtības
  mutate(
    overlap_analyzed = factor(overlap_analyzed),
    metric = factor(metric, 
                    levels = c("precision", "sensitivity", "f1_score"),
                    labels = c("Precizitāte / Precision", "Jūtība / Sensitivity", "F1"))
  )

n_species <- length(unique(metrics_long$scientific_name))


# Izveido violin grafiku
p <- ggplot(metrics_long, aes(x = overlap_analyzed, y = value, fill = overlap_analyzed)) +
  geom_violin(alpha = 0.7, trim = FALSE) +
  geom_boxplot(width = 0.15, alpha = 0.5, outlier.shape = NA) +
  facet_wrap(~metric, ncol = 1, scales = "free_y") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    x = "Pārklāšanās vērtība / Overlap value",
    y = NULL,
    title = "BirdNET modeļu salīdzinājums / BirdNET model comparison",
    subtitle = paste0("n = ", n_species, " sugas / species"),
    fill = NULL
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 13, hjust = 0.5),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14, face = "bold"),
    strip.background = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 13, face = "bold")
  )

print(p)

ggsave("./rezultati/violin_plot_metriku.png", plot = p, width = 10, height = 12, dpi = 300,bg = "white")


# Papildus: aprēķina statistiku katram modelim
summary_stats <- metrics_long %>%
  group_by(overlap_analyzed, metric) %>%
  summarise(
    mean = mean(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    min = min(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

write_csv(summary_stats, "./rezultati/metriku_statistika.csv")

cat("\nSTATISTIKA PA MODEĻIEM:\n")
print(summary_stats, n = Inf)
