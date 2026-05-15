library(tidyverse)

# Ielasa BirdnetR_meta_latvia.csv no darba mapes
meta_latvia <- read_csv("BirdnetR_meta_latvia.csv")

# Latvijas sugu saraksts
latvia_species <- meta_latvia$scientific_name

# Ielasa visus .csv failus no mapes "proto"
proto_files <- list.files("proto", pattern = "\\.csv$", full.names = TRUE)

# Apstrādā katru failu
walk(proto_files, function(file_path) {
  df <- read_csv(file_path)
  
  df <- df %>%
    mutate(latvia = if_else(scientific_name %in% latvia_species, 1, 0))
  
  write_csv(df, file_path)
})

message("Gatavs! Apstrādāti ", length(proto_files), " faili.")





# Ielasa meta sites failu
meta_sites <- read_csv("BirdnetR_meta_sites.csv")

# Izveido lookup tabulu: scientific_name + site_id -> confidence
site_confidence_lookup <- meta_sites %>%
  select(scientific_name, site_id, confidence)

# Apstrādā katru proto failu
walk(proto_files, function(file_path) {
  df <- read_csv(file_path)
  
  df <- df %>%
    left_join(
      site_confidence_lookup %>% rename(site_confidence = confidence),
      by = c("scientific_name", "site_id")
    )
  
  write_csv(df, file_path)
})

message("Gatavs! site_confidence pievienota visiem failiem.")






# Definē birdnet_week funkciju
birdnet_week <- function(date) {
  date <- as.Date(date, format = "%m/%d/%Y %H:%M:%S")
  
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

# Pievieno week kolonnu visiem proto failiem
walk(proto_files, function(file_path) {
  df <- read_csv(file_path)
  
  df <- df %>%
    mutate(week = birdnet_week(datetime))
  
  write_csv(df, file_path)
})

# Pārbaude ar pirmo failu
first_df <- read_csv(proto_files[[1]])
cat("Pirmie 5 ieraksti ar week kolonnu:\n")
first_df %>%
  select(audio_file, datetime, week) %>%
  head(5) %>%
  print()
cat(sprintf("\nNA vērtības week kolonnā: %d\n", sum(is.na(first_df$week))))






# Ielasa meta sites week failu
meta_sites_week <- read_csv("BirdnetR_meta_sites_week.csv")

# Izveido lookup tabulu: scientific_name + site_id + week -> confidence
site_week_confidence_lookup <- meta_sites_week %>%
  select(scientific_name, site_id, week, confidence)

# Apstrādā katru proto failu
walk(proto_files, function(file_path) {
  df <- read_csv(file_path)
  
  df <- df %>%
    left_join(
      site_week_confidence_lookup %>% rename(site_week_confidence = confidence),
      by = c("scientific_name", "site_id", "week")
    )
  
  write_csv(df, file_path)
})

message("Gatavs! site_week_confidence pievienota visiem failiem.")






walk(proto_files, function(file_path) {
  df <- read_csv(file_path)
  
  df <- df %>%
    select(-audio_path)
  
  write_csv(df, file_path)
})

message("Gatavs! audio_path kolonna noņemta no visiem failiem.")










library(tidyverse)

# ── 1. Ielasa un apvieno visus proto failus ───────────────────────────────────
proto_files <- list.files("proto", pattern = "\\.csv$", full.names = TRUE)

proto_all <- proto_files %>%
  map_dfr(read_csv) %>%
  filter(latvia == 1)

# ── 2. Sugu daudzveidība pa vietām ───────────────────────────────────────────
species_per_site <- proto_all %>%
  group_by(site_id) %>%
  summarise(n_species = n_distinct(scientific_name)) %>%
  arrange(desc(n_species))

ggplot(species_per_site, aes(x = reorder(site_id, -n_species), y = n_species)) +
  geom_col(fill = "#2E8B57") +
  labs(
    title = "Sugu daudzveidība pa monitoringa vietām",
    x = "Vieta (site_id)",
    y = "Unikālo sugu skaits"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("grafiki/1_sugas_pa_vietam.png", width = 10, height = 6, dpi = 300)

# ── 3. Sugu aktivitāte laika gaitā (nedēļas) ─────────────────────────────────
detections_per_week <- proto_all %>%
  group_by(week) %>%
  summarise(n_detections = n()) %>%
  filter(!is.na(week))

ggplot(detections_per_week, aes(x = week, y = n_detections)) +
  geom_line(color = "#2E8B57", linewidth = 1) +
  geom_point(color = "#2E8B57", size = 2) +
  labs(
    title = "Putnu aktivitāte laika gaitā",
    x = "Nedēļa",
    y = "Konstatējumu skaits"
  ) +
  scale_x_continuous(breaks = seq(1, 48, by = 4)) +
  theme_classic()

ggsave("grafiki/2_aktivitate_pa_nedalam.png", width = 10, height = 6, dpi = 300)

# ── 4. Confidence salīdzinājums: proto vs meta (site) ────────────────────────
confidence_compare <- proto_all %>%
  filter(!is.na(site_confidence)) %>%
  group_by(scientific_name) %>%
  summarise(
    proto_confidence = max(confidence),
    meta_confidence  = max(site_confidence)
  )

ggplot(confidence_compare, aes(x = meta_confidence, y = proto_confidence)) +
  geom_point(alpha = 0.5, color = "#2E8B57") +
  geom_abline(linetype = "dashed", color = "gray50") +
  labs(
    title = "Proto vs Meta confidence salīdzinājums (pēc sugas)",
    x = "Meta confidence (site)",
    y = "Proto confidence"
  ) +
  theme_classic()

ggsave("grafiki/3_confidence_proto_vs_meta.png", width = 8, height = 8, dpi = 300)

# ── 5. Top 20 biežākās sugas ──────────────────────────────────────────────────
top_species <- proto_all %>%
  count(scientific_name, sort = TRUE) %>%
  slice_head(n = 20)

ggplot(top_species, aes(x = reorder(scientific_name, n), y = n)) +
  geom_col(fill = "#2E8B57") +
  coord_flip() +
  labs(
    title = "Top 20 biežāk konstatētās sugas",
    x = NULL,
    y = "Konstatējumu skaits"
  ) +
  theme_classic()

ggsave("grafiki/4_top20_sugas.png", width = 10, height = 8, dpi = 300)

message("Visi grafiki saglabāti mapē 'grafiki'!")






# ── 1. Ielasa visus proto failus ar metadata no nosaukuma ────────────────────
proto_files <- list.files("proto", pattern = "\\.csv$", full.names = TRUE)

proto_all <- proto_files %>%
  map_dfr(function(file_path) {
    # Izvelk lokāciju un overlap no faila nosaukuma
    fname <- basename(file_path)
    
    location <- str_extract(fname, "(?<=all_results_)[^_]+_[^_]+")
    
    overlap_raw <- str_extract(fname, "(?<=overlap_)\\d+(?=_proto)")
    overlap_val <- case_when(
      overlap_raw == "0"  ~ 0.0,
      overlap_raw == "05" ~ 0.5,
      overlap_raw == "1"  ~ 1.0,
      overlap_raw == "15" ~ 1.5
    )
    
    read_csv(file_path) %>%
      mutate(
        location = location,
        overlap  = overlap_val
      )
  })

# ── 2. Saglabā katru lokāciju kā atsevišķu failu ─────────────────────────────
dir.create("merged", showWarnings = FALSE)

proto_all %>%
  group_by(location) %>%
  group_walk(~ write_csv(.x, file.path("proto", paste0(.y$location, ".csv"))))

message("Gatavs! Apvienotie faili saglabāti mapē 'merged'.")
