library(tidyverse)

# ── 1. Pārbauda vai Meta faili eksistē ────────────────────────────────────────
if (!file.exists("BirdnetR_meta_sites.csv")) {
  stop("KĻŪDA: Nav atrasts 'BirdnetR_meta_sites.csv'!")
}

if (!file.exists("BirdnetR_meta_sites_week.csv")) {
  stop("KĻŪDA: Nav atrasts 'BirdnetR_meta_sites_week.csv'!")
}

if (!file.exists("BirdnetR_meta_latvia.csv")) {
  stop("KĻŪDA: Nav atrasts 'BirdnetR_meta_latvia.csv'!")
}

# ── 2. Ielādē Meta failus ─────────────────────────────────────────────────────
cat("Ielādē Meta modeļa datus...\n")

meta_sites <- read_csv("BirdnetR_meta_sites.csv", show_col_types = FALSE)
meta_sites_week <- read_csv("BirdnetR_meta_sites_week.csv", show_col_types = FALSE)
meta_latvia <- read_csv("BirdnetR_meta_latvia.csv", show_col_types = FALSE)

cat(sprintf("  ✓ Meta sites: %d prognozes (%d sugas × %d vietas)\n",
            nrow(meta_sites),
            n_distinct(meta_sites$scientific_name),
            n_distinct(meta_sites$site_id)))

cat(sprintf("  ✓ Meta sites+week: %d prognozes (%d sugas × %d vietas × %d nedēļas)\n",
            nrow(meta_sites_week),
            n_distinct(meta_sites_week$scientific_name),
            n_distinct(meta_sites_week$site_id),
            n_distinct(meta_sites_week$week)))

cat(sprintf("  ✓ Latvijas sugu saraksts: %d sugas\n\n",
            nrow(meta_latvia)))

# Izveido Latvijas sugu vektoru
latvia_species <- meta_latvia$scientific_name

# ── 3. Definē birdnet_week funkciju ───────────────────────────────────────────
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

# ── 4. Ielādē all_results failus ──────────────────────────────────────────────
all_files <- list.files(path = ".", pattern = "^all_results_.*\\.csv$",
                        full.names = TRUE)

if (length(all_files) == 0) {
  stop("KĻŪDA: Nav atrasti all_results_*.csv faili!\n",
       "Pārliecinies, ka esi palaidis BirdNET analīzi.")
}

cat(sprintf("Atrasti %d all_results faili\n\n", length(all_files)))

all_results_list <- all_files %>%
  set_names(basename(.)) %>%
  map(~ read_csv(.x, show_col_types = FALSE))

cat("Faili ielādēti!\n\n")

# ── 5. Pievieno VISAS Meta kolonnas ───────────────────────────────────────────

all_results_list <- all_results_list %>%
  imap(~ {
    cat(sprintf("Apstrādā: %s\n", .y))
    
    df <- .x %>%
      # 1. Pievieno latvia kolonnu
      mutate(latvia = if_else(scientific_name %in% latvia_species, 1, 0)) %>%
      
      # 2. Pievieno week kolonnu
      mutate(week = birdnet_week(datetime)) %>%
      
      # 3. Pievieno site_confidence (bez nedēļas)
      left_join(
        meta_sites %>% select(scientific_name, site_id, 
                             site_confidence = confidence),
        by = c("scientific_name", "site_id")
      ) %>%
      
      # 4. Pievieno site_week_confidence (ar nedēļu)
      left_join(
        meta_sites_week %>% select(scientific_name, site_id, week,
                                   site_week_confidence = confidence),
        by = c("scientific_name", "site_id", "week")
      )
    
    # Statistika
    cat(sprintf("  ✓ latvia: %d Latvijas sugas / %d visas sugas\n",
                sum(df$latvia == 1), n_distinct(df$scientific_name)))
    cat(sprintf("  ✓ week: %d/%d rindas\n", 
                sum(!is.na(df$week)), nrow(df)))
    cat(sprintf("  ✓ site_confidence: %d/%d rindas\n", 
                sum(!is.na(df$site_confidence)), nrow(df)))
    cat(sprintf("  ✓ site_week_confidence: %d/%d rindas\n\n", 
                sum(!is.na(df$site_week_confidence)), nrow(df)))
    
    df
  })

# ── 6. Kopsavilkuma pārbaude ──────────────────────────────────────────────────

summary_df <- map_dfr(all_results_list, ~ {
  tibble(
    rindas_kopā              = nrow(.x),
    latvia_sugas             = sum(.x$latvia == 1),
    week_aizpildītas         = sum(!is.na(.x$week)),
    site_conf_aizpildītas    = sum(!is.na(.x$site_confidence)),
    site_week_conf_aizpildītas = sum(!is.na(.x$site_week_confidence))
  )
}, .id = "fails")

print(summary_df, n = Inf)

# ── 7. Saglabā rezultātus ─────────────────────────────────────────────────────
if (!dir.exists("final")) dir.create("final")

walk2(all_results_list, names(all_results_list), ~ {
  # Izvelk tikai q1_m1 daļu no nosaukuma
  jaunais_nosaukums <- .y %>%
    str_remove("^all_results_") %>%
    str_remove("\\.csv$") %>%
    paste0(".csv")
  
  write_csv(.x, file.path("final", jaunais_nosaukums))
  cat(sprintf("✓ Saglabāts: final/%s\n", jaunais_nosaukums))
})
