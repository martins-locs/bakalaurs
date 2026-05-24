# 0. Bibliotēkas + iestatījumi ----

library(tidyverse)
library(lubridate)
library(purrr)
library(fs)
library(ggplot2)
library(dplyr)
library(suncalc)

set.seed(4)




# 1. VietasS ----

vietas <- read_csv("./metadati/sites.0001.csv") %>% 
  mutate(sitename = paste0("ML_", name)) %>% 
  select(site_id, sitename)




# 2. Ieraksti ----

ierakstu_faili <- tibble(
  celi  = list.files("./metadati/", full.names = TRUE),
  fails = list.files("./metadati/", full.names = FALSE)
) %>% 
  filter(
    str_detect(celi, "recordings") &   
      !str_detect(celi, "playlist")
  )

ieraksti <- map_dfr(ierakstu_faili$celi, read_csv) %>%
  select(-upload_time, -file_size)



# 3. Apvienošana ----

vadiba <- ieraksti %>% 
  left_join(vietas, by = "site_id") %>% 
  mutate(
    # datetime (mm/dd/yyyy hh:mm:ss),
    datetime = mdy_hms(datetime),
    date = as_date(datetime),
    time = format(datetime, "%H:%M")
  )


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


vadiba <- vadiba %>%
  mutate(
    birdnet_week = birdnet_week(date)
  )



# 4. Kvadrants + mikrofona tips ----
vadiba <- vadiba %>% 
  mutate(
    quadrant = str_extract(sitename, "Q[1-4]"),
    mic_type = case_when(
      str_detect(sitename, "_L")  ~ "L",
      str_detect(sitename, "_M1") ~ "M1",
      str_detect(sitename, "_M2") ~ "M2",
      str_detect(sitename, "_M3") ~ "M3",
      str_detect(sitename, "_M4") ~ "M4",
      str_detect(sitename, "_M5") ~ "M5",
      TRUE ~ NA_character_
    )
  )


vadiba <- vadiba %>%
  mutate(
    hour = as.integer(str_sub(time, 1, 2))
  )




# 5. Saullēkts / saulriets ----

sites_info <- read_csv("./metadati/sites.0001.csv") %>%
  select(-elevation, -hidden, -created_at)

vadiba <- vadiba %>%
  left_join(
    sites_info %>% select(site_id, latitude, longitude),
    by = "site_id"
  )


# Unikālas kombinācijas
unique_sites <- vadiba %>%
  distinct(site_id, date, latitude, longitude)

# Aprēķina saules laikus tikai vienreiz katrai kombinācijai
sun_times_unique <- unique_sites %>%
  rowwise() %>%
  mutate(
    sunrise = getSunlightTimes(date = date, lat = latitude, lon = longitude, keep = c("sunrise"))$sunrise,
    sunset  = getSunlightTimes(date = date, lat = latitude, lon = longitude, keep = c("sunset"))$sunset
  ) %>%
  ungroup()

vadiba <- vadiba %>%
  left_join(sun_times_unique %>% select(site_id, date, sunrise, sunset),
            by = c("site_id", "date"))


get_sun_hours <- function(sunrise, sunset) {
  # Noapaļo saullēktu un saulrietu uz tuvāko stundu
  sunrise_hour <- ifelse(minute(sunrise) >= 30, hour(sunrise) + 1, hour(sunrise))
  sunset_hour  <- ifelse(minute(sunset) >= 30, hour(sunset) + 1, hour(sunset))
  
  # Rīta stundas: 1 stunda pirms saullēkta, saullēkta stunda, +3 stundas pēc
  morning_hours <- (sunrise_hour - 1):(sunrise_hour + 3)
  
  # Vakara stundas: 3 stundas pirms saulrieta, saulrieta stunda
  evening_hours <- (sunset_hour - 3):(sunset_hour)
  
  # Apvieno un filtrē stundas 0–23
  sun_hours <- unique(c(morning_hours, evening_hours))
  sun_hours <- sun_hours[sun_hours >= 0 & sun_hours <= 23]
  
  return(sun_hours)
}


vadiba <- vadiba %>%
  mutate(
    bird_week = ceiling(birdnet_week / 4),
    hour = hour(datetime)
  )


atlase_standard <- vadiba %>%
  filter(!is.na(quadrant), !is.na(mic_type)) %>%
  group_by(bird_week, hour, quadrant, mic_type) %>%
  slice_sample(n = 1) %>%
  ungroup()


sun_hours_df <- vadiba %>%
  filter(!is.na(quadrant), !is.na(mic_type)) %>%
  rowwise() %>%
  mutate(
    sun_hours = list(get_sun_hours(sunrise, sunset))
  ) %>%
  ungroup() %>%
  unnest(cols = sun_hours)

atlase_sun <- sun_hours_df %>%
  anti_join(atlase_standard, by = c("datetime", "quadrant", "mic_type")) %>%
  filter(hour == sun_hours) %>%
  group_by(bird_week, hour, quadrant, mic_type) %>%
  slice_sample(n = 1) %>%
  ungroup()


atlase_final <- bind_rows(atlase_standard, atlase_sun) %>%
  filter(!month(date) %in% c(9, 10)) %>%
  arrange(date, hour, quadrant, mic_type)




# 6. Vizualizācija ----

atlase_hm <- atlase_final %>%
  mutate(
    month = month(datetime, label = TRUE, abbr = TRUE),
    hour  = hour(datetime)
  )

heatmap_data <- atlase_hm %>%
  count(month, hour)


# Pirmais grafiks - vispārējais heatmap
p1 <- ggplot(heatmap_data, aes(x = hour, y = month, fill = n)) +
  geom_tile(color = "white") +
  scale_x_continuous(breaks = 0:23) +
  scale_fill_viridis_c(name = "Ierakstu skaits /\nNumber of recordings") +
  labs(
    x = "Stunda / Hour",
    y = "Mēnesis / Month",
    title = "Atlasīto ierakstu sadalījums pa stundām un mēnešiem /\nDistribution of selected records by hours and months"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 13, face = "bold"),
    legend.text = element_text(size = 12),
    panel.grid = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank()
  )

print(p1)

# Saglabā pirmo grafiku
ggsave(
  filename = "./rezultati/heatmap_overall.png",
  plot = p1,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)

# Otrais grafiks - pa kvadrantiem un mikrofoniem
plot_df <- atlase_hm %>%
  count(
    quadrant,
    mic_type,
    month,
    hour,
    name = "n"
  )


p2 <- ggplot(plot_df, aes(x = hour, y = month, fill = n)) +
  geom_tile(color = "white", linewidth = 0.2) +
  
  facet_grid(
    rows = vars(quadrant),
    cols = vars(mic_type)
  ) +
  
  scale_fill_viridis_c(
    name = "Ierakstu skaits /\nNumber of recordings",
    option = "C"
  ) +
  
  scale_x_continuous(
    breaks = seq(0, 23, 3)
  ) +
  
  labs(
    x = "Stunda / Hour",
    y = "Mēnesis / Month",
    title = "Atlasīto ierakstu sadalījums pa stundām un mēnešiem /\nDistribution of selected records by hours and months"
  ) +
  
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 15, face = "bold"),
    legend.text = element_text(size = 13),
    strip.text = element_text(size = 16, face = "bold"),
    strip.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank()
  )

print(p2)

# Saglabā otro grafiku
ggsave(
  filename = "./rezultati/heatmap_faceted.png", plot = p2, width = 20, height = 10, dpi = 300, bg = "white")




# 7. Audio faili ----

# Faila nosaukums
atlase_files <- atlase_final %>%
  mutate(
    audio_file_base = str_extract(meta, "(?<=Original Filename:)\\d{8}_\\d{6}"),
    audio_file = paste0(audio_file_base, ".flac")
  ) %>%
  filter(!is.na(audio_file_base))

# Faila meklēšana sitename mapē
atlase_files <- atlase_files %>%
  rowwise() %>%
  mutate(
    mic_map = file.path("../AudioMoth", sitename),
    file_path = list.files(
      path = mic_map,
      pattern = audio_file,
      full.names = TRUE
    ) %>% first()
  ) %>%
  ungroup() %>%
  filter(!is.na(file_path)) %>%
  select(-mic_map)

atlase_files <- atlase_files %>%
  mutate(
    month = month(date, label = TRUE)
  )


#Mērķa mapes struktūra
target_root <- "../AudioMoth_2026/"

atlase_files <- atlase_files %>%
  mutate(
    audio_directory = file.path(target_root, quadrant, mic_type, as.character(month))
  )

atlase_files <- atlase_files %>%
  mutate(
    audio_path = file.path(audio_directory, basename(file_path))
  )

# Mapju izveide + failu kopēšana
unique(atlase_files$audio_directory) %>% walk(dir_create)

atlase_files %>%
  select(file_path, audio_path) %>%
  pmap(~ file_copy(..1, ..2, overwrite = TRUE))

sum(file.exists(atlase_files$audio_path))




# 8. Rezultātu saglabāšana ----

write_csv(
  atlase_files,
  "./rezultati/ierakstu_atlase.csv"
)
