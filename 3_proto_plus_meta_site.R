library(tidyverse)

# ── 1. Ielādē Proto failus ────────────────────────────────────────────────────
birdnet_files <- list.files(path = "../AudioMoth", pattern = "all_results_q1_l_overlap_.*\\.csv$",
                            full.names = TRUE)

proto <- birdnet_files %>%
  map_dfr(~ read_csv(.x, show_col_types = FALSE) %>%
            mutate(overlap_param = basename(.x) %>%
                     str_extract("(?<=overlap_)[0-9]+") %>%
                     recode("0" = 0, "05" = 0.5, "1" = 1.0, "15" = 1.5)))

cat(sprintf("Unikālās sugas Proto datos: %d\n", n_distinct(proto$scientific_name)))

# ── 2. Ielādē Meta sites failu ────────────────────────────────────────────────
meta_sites <- read_csv("BirdnetR_meta_sites.csv", show_col_types = FALSE)

# ── 3. Pievieno site_confidence Proto datiem ──────────────────────────────────
proto <- proto %>%
  left_join(
    meta_sites %>% select(scientific_name, site_id, site_confidence = confidence),
    by = c("scientific_name", "site_id")
  )

# ── 4. Pārbaude ───────────────────────────────────────────────────────────────
cat(sprintf("Rindas ar site_confidence: %d\n", sum(!is.na(proto$site_confidence))))
cat(sprintf("Rindas bez site_confidence (suga nav Meta sarakstā): %d\n", sum(is.na(proto$site_confidence))))

glimpse(proto)










library(tidyverse)

# ── 1. Ielādē Meta sites failu ────────────────────────────────────────────────
meta_sites <- read_csv("BirdnetR_meta_sites.csv", show_col_types = FALSE)

# ── 2. Izveido mapi "meta_site" ja tā neeksistē ───────────────────────────────
if (!dir.exists("meta_site")) dir.create("meta_site")

# ── 3. Nolasa visus all_results_ failus, pievieno site_confidence, saglabā ────
all_files <- list.files(path = ".", pattern = "^all_results_.*\\.csv$",
                        full.names = TRUE)

cat(sprintf("Atrasti %d faili\n\n", length(all_files)))

walk(all_files, ~ {
  faila_nosaukums <- basename(.x)
  cat(sprintf("Apstrādā: %s\n", faila_nosaukums))
  
  # Nolasa failu
  df <- read_csv(.x, show_col_types = FALSE)
  
  # Pievieno site_confidence
  df <- df %>%
    left_join(
      meta_sites %>% select(scientific_name, site_id, site_confidence = confidence),
      by = c("scientific_name", "site_id")
    )
  
  # Saglabā jaunais nosaukums: noņem .csv, pievieno _meta_site.csv
  jaunais_nosaukums <- str_replace(faila_nosaukums, "\\.csv$", "_meta_site.csv")
  write_csv(df, file.path("meta_site", jaunais_nosaukums))
  
  cat(sprintf("  Saglabāts: meta_site/%s\n", jaunais_nosaukums))
})

cat("\nVisi faili apstrādāti!")
