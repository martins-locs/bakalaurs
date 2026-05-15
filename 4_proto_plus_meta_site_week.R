library(tidyverse)

# ── 1. Pārbaudi recordings failu struktūru ────────────────────────────────────
recordings_files <- list.files(path = "Metadati", pattern = "^recordings\\..*\\.csv$",
                               full.names = TRUE)

cat(sprintf("Atrasti %d recordings faili\n", length(recordings_files)))

# Apskatī pirmo failu
test <- read_csv(recordings_files[1], show_col_types = FALSE)
glimpse(test)

# Apskatī kolonnas "meta" pirmos piemērus
cat("\nPirmie 5 meta kolonnas ieraksti:\n")
test %>% pull(meta) %>% head(5) %>% cat(sep = "\n")



# ── 1. Ielādē un apvieno visus recordings failus ──────────────────────────────
recordings_files <- list.files(path = "Metadati", pattern = "^recordings\\..*\\.csv$",
                               full.names = TRUE)

cat(sprintf("Atrasti %d recordings faili\n", length(recordings_files)))

recordings <- recordings_files %>%
  map_dfr(~ read_csv(.x, show_col_types = FALSE))

cat(sprintf("Kopā ieraksti recordings datos: %d\n", nrow(recordings)))

# Izvelk VIETAS_KODS no meta kolonnas un noņem galotni
recordings_clean <- recordings %>%
  mutate(
    vietas_kods = str_extract(meta, "(?<=Original Filename:)[^;\"]+") %>%
      str_remove("\\.[A-Za-z]+$")  # noņem .WAV vai citu galotni
  ) %>%
  select(recording_id, site_id, vietas_kods)

# Pārbaude
cat("\nPirmie 5 VIETAS_KODS piemēri:\n")
recordings_clean %>% head(5) %>% print()

# ── 2. Ielādē visus all_results_ failus atsevišķi ────────────────────────────
all_files <- list.files(path = ".", pattern = "^all_results_.*\\.csv$",
                        full.names = TRUE)

cat(sprintf("\nAtrasti %d all_results_ faili\n", length(all_files)))

# Ielādē katru failu atsevišķi un pievieno recording_id
all_results_list <- all_files %>%
  set_names(basename(.)) %>%
  map(~ {
    df <- read_csv(.x, show_col_types = FALSE)
    
    df %>%
      mutate(audio_file_clean = str_remove(audio_file, "\\.[A-Za-z]+$")) %>%
      left_join(
        recordings_clean,
        by = c("site_id" = "site_id", "audio_file_clean" = "vietas_kods")
      ) %>%
      select(-audio_file_clean)
  })

# ── 3. Pārbaude ───────────────────────────────────────────────────────────────
cat("\nPārbaude pirmajam failam:\n")
all_results_list[[1]] %>%
  select(audio_file, site_id, recording_id) %>%
  head(10) %>%
  print()

cat(sprintf("\nRindas ar recording_id: %d\n", sum(!is.na(all_results_list[[1]]$recording_id))))
cat(sprintf("Rindas bez recording_id: %d\n", sum(is.na(all_results_list[[1]]$recording_id))))





# Kādi audio_file nav atrasti recordings datos?
all_results_list[[1]] %>%
  filter(is.na(recording_id)) %>%
  distinct(audio_file, site_id) %>%
  print()


# Vai šie audio_file eksistē recordings datos (ar citu site_id vai vienkārši nav)?
missing_files <- all_results_list[[1]] %>%
  filter(is.na(recording_id)) %>%
  distinct(audio_file) %>%
  mutate(audio_file_clean = str_remove(audio_file, "\\.[A-Za-z]+$"))

# Meklē recordings datos neatkarīgi no site_id
recordings_clean %>%
  filter(vietas_kods %in% missing_files$audio_file_clean) %>%
  print()




# Atjauno recordings_clean ar datetime kolonnām
recordings_clean <- recordings %>%
  mutate(
    vietas_kods = str_extract(meta, "(?<=Original Filename:)[^;\"]+") %>%
      str_remove("\\.[A-Za-z]+$")
  ) %>%
  select(recording_id, site_id, vietas_kods, datetime, datetime_utc)

# Ielādē no jauna visus all_results_ failus ar recording_id + datetime
all_results_list <- all_files %>%
  set_names(basename(.)) %>%
  map(~ {
    df <- read_csv(.x, show_col_types = FALSE)
    
    df %>%
      mutate(audio_file_clean = str_remove(audio_file, "\\.[A-Za-z]+$")) %>%
      left_join(
        recordings_clean,
        by = c("site_id" = "site_id", "audio_file_clean" = "vietas_kods")
      ) %>%
      select(-audio_file_clean)
  })

# Pārbaude
cat("Pirmie 5 ieraksti ar jaunajām kolonnām:\n")
all_results_list[[1]] %>%
  select(audio_file, site_id, recording_id, datetime, datetime_utc) %>%
  head(5) %>%
  print()



# Pārbaude visām trijām kolonnām visos failos
map_dfr(all_results_list, ~ {
  tibble(
    rindas_kopā     = nrow(.x),
    recording_id_NA = sum(is.na(.x$recording_id)),
    datetime_NA     = sum(is.na(.x$datetime)),
    datetime_utc_NA = sum(is.na(.x$datetime_utc))
  )
}, .id = "fails") %>%
  print(n = Inf)


# Izveido mapi "Proto" ja tā neeksistē
if (!dir.exists("Proto")) dir.create("Proto")

# Saglabā katru failu
walk2(all_results_list, names(all_results_list), ~ {
  jaunais_nosaukums <- str_replace(.y, "\\.csv$", "_proto.csv")
  write_csv(.x, file.path("Proto", jaunais_nosaukums))
  cat(sprintf("Saglabāts: Proto/%s\n", jaunais_nosaukums))
})

cat("\nVisi faili saglabāti!")



df <- read_csv("Proto/all_results_q1_l_overlap_0_proto.csv")




# Pārbaudi vai q1_l failos visi site_id ir 69989
all_results_list[["all_results_q1_l_overlap_0.csv"]] %>%
  distinct(site_id) %>%
  print()



# Pārbaudi vai vietas_kods ir unikāls recordings_clean datos (bez site_id)
recordings_clean %>%
  count(vietas_kods) %>%
  filter(n > 1) %>%
  nrow() %>%
  cat("Vietas_kods ar vairāk nekā vienu ierakstu:", ., "\n")








# Definē pareizos site_id katram failam
site_id_lookup <- tibble(
  faila_pattern = c("q1_l", "q1_m2", "q4_m2"),
  pareizais_site_id = c(69989, 70854, 71823)
)

# Funkcija kas aizpilda NA vērtības
fill_na_recording <- function(df, faila_nosaukums) {
  # Nosaka pareizo site_id no faila nosaukuma
  pareizais_site_id <- site_id_lookup %>%
    filter(str_detect(faila_nosaukums, faila_pattern)) %>%
    pull(pareizais_site_id)
  
  if (length(pareizais_site_id) == 0) return(df)
  
  # Aizpilda NA rindas
  df %>%
    mutate(audio_file_clean = str_remove(audio_file, "\\.[A-Za-z]+$")) %>%
    left_join(
      recordings_clean %>%
        filter(site_id == pareizais_site_id) %>%
        select(vietas_kods, recording_id_fix = recording_id,
               datetime_fix = datetime, datetime_utc_fix = datetime_utc),
      by = c("audio_file_clean" = "vietas_kods")
    ) %>%
    mutate(
      recording_id  = coalesce(recording_id, recording_id_fix),
      datetime      = coalesce(datetime, datetime_fix),
      datetime_utc  = coalesce(datetime_utc, datetime_utc_fix)
    ) %>%
    select(-audio_file_clean, -recording_id_fix, -datetime_fix, -datetime_utc_fix)
}

# Pielieto visiem failiem
all_results_list <- all_results_list %>%
  imap(~ fill_na_recording(.x, .y))

# Pārbaude
map_dfr(all_results_list, ~ {
  tibble(
    recording_id_NA = sum(is.na(.x$recording_id)),
    datetime_NA     = sum(is.na(.x$datetime)),
    datetime_utc_NA = sum(is.na(.x$datetime_utc))
  )
}, .id = "fails") %>%
  filter(recording_id_NA > 0) %>%
  print()



# Pārbaudi vai pareizais site_id eksistē recordings_clean datos
cat("site_id 69989 ierakstu skaits:", recordings_clean %>% filter(site_id == 69989) %>% nrow(), "\n")
cat("site_id 70854 ierakstu skaits:", recordings_clean %>% filter(site_id == 70854) %>% nrow(), "\n")
cat("site_id 71823 ierakstu skaits:", recordings_clean %>% filter(site_id == 71823) %>% nrow(), "\n")

# Pārbaudi vai NA audio_file eksistē recordings_clean ar pareizo site_id
all_results_list[["all_results_q1_l_overlap_0.csv"]] %>%
  filter(is.na(recording_id)) %>%
  distinct(audio_file) %>%
  mutate(audio_file_clean = str_remove(audio_file, "\\.flac$")) %>%
  left_join(
    recordings_clean %>% filter(site_id == 69989),
    by = c("audio_file_clean" = "vietas_kods")
  ) %>%
  print()


# Pārbaudi ar jebkādu site_id
all_results_list[["all_results_q1_l_overlap_0.csv"]] %>%
  filter(is.na(recording_id)) %>%
  distinct(audio_file) %>%
  mutate(audio_file_clean = str_remove(audio_file, "\\.flac$")) %>%
  left_join(recordings_clean, by = c("audio_file_clean" = "vietas_kods")) %>%
  select(audio_file, recording_id, site_id) %>%
  print(n = Inf)
