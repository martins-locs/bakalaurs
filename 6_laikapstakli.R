library(tidyverse)
library(jsonlite)
library(lubridate)
library(httr2)
library(cli)

# ── 1. Ielasa metadatus (koordinātes) ────────────────────────────────────────
sites <- read_csv("metadati/sites.0001.csv") %>%
  select(site_id, latitude, longitude)

# ── 2. Palīgfunkcijas ────────────────────────────────────────────────────────
fetch_weather <- function(lat, lon, start_date, end_date) {
  base_url <- "https://archive-api.open-meteo.com/v1/archive"
  
  params <- list(
    latitude   = lat,
    longitude  = lon,
    start_date = format(start_date, "%Y-%m-%d"),
    end_date   = format(end_date,   "%Y-%m-%d"),
    hourly     = paste(
      "temperature_2m",
      "relative_humidity_2m",
      "wind_speed_10m",
      "wind_direction_10m",
      "precipitation",
      sep = ","
    ),
    timezone   = "Europe/Riga"
  )
  
  resp <- request(base_url) |>
    req_url_query(!!!params) |>
    req_perform()
  
  raw  <- resp_body_string(resp)
  data <- fromJSON(raw)
  
  data.frame(
    datetime       = as.POSIXct(data$hourly$time,
                                format = "%Y-%m-%dT%H:%M",
                                tz = "Europe/Riga"),
    temperature    = data$hourly$temperature_2m,
    humidity       = data$hourly$relative_humidity_2m,
    wind_speed     = data$hourly$wind_speed_10m,
    wind_direction = data$hourly$wind_direction_10m,
    precipitation  = data$hourly$precipitation,
    stringsAsFactors = FALSE
  )
}

# ── 3. Attīra vecās kolonnas ──────────────────────────────────────────────────
proto_files <- list.files("proto", pattern = "\\.csv$", full.names = TRUE)

cli_alert_info("Attīra vecās kolonnas...")
walk(proto_files, function(file_path) {
  df <- read_csv(file_path, show_col_types = FALSE) %>%
    select(-matches("^TempOUT|^wind|^humidity|^precipitation"))
  write_csv(df, file_path)
})
cli_alert_success("Kolonnas attīrītas!")

# ── 4. Pievieno jaunās kolonnas ───────────────────────────────────────────────
cli_h1("Laika apstākļu pievienošana proto failiem")
cli_alert_info("Atrasti {length(proto_files)} faili apstrādei")

pb_files <- cli_progress_bar(
  name   = "Faili",
  total  = length(proto_files),
  format = "{cli::pb_bar} {cli::pb_current}/{cli::pb_total} faili | ETA: {cli::pb_eta_str}"
)

walk(proto_files, function(file_path) {
  fname <- basename(file_path)
  
  df <- read_csv(file_path, show_col_types = FALSE) %>%
    left_join(sites, by = "site_id") %>%
    mutate(
      datetime_utc_parsed = as.POSIXct(datetime_utc,
                                       format = "%m/%d/%Y %H:%M:%S",
                                       tz = "UTC"),
      datetime_riga = with_tz(datetime_utc_parsed, "Europe/Riga"),
      date_only     = as.Date(datetime_riga),
      hour_floor    = as.POSIXct(floor_date(datetime_riga, "hour"), tz = "Europe/Riga")
    )
  
  weather_cache <- df %>%
    distinct(site_id, date_only, latitude, longitude) %>%
    filter(!is.na(latitude), !is.na(longitude), !is.na(date_only))
  
  n_api <- nrow(weather_cache)
  
  pb_api <- cli_progress_bar(
    name   = paste0("  API: ", fname),
    total  = n_api,
    format = "{cli::pb_bar} {cli::pb_current}/{cli::pb_total} API pieprasījumi"
  )
  
  weather_hourly <- weather_cache %>%
    pmap_dfr(function(site_id, date_only, latitude, longitude) {
      Sys.sleep(0.05)
      
      weather_df <- tryCatch(
        fetch_weather(latitude, longitude, date_only, date_only),
        error = function(e) {
          cli_alert_warning("API kļūda: site {site_id}, {date_only}")
          NULL
        }
      )
      
      cli_progress_update(id = pb_api)
      
      if (is.null(weather_df)) return(NULL)
      
      weather_df %>%
        mutate(
          site_id    = site_id,
          hour_floor = as.POSIXct(floor_date(datetime, "hour"), tz = "Europe/Riga"),
          wind_sin   = sin(wind_direction * pi / 180),
          wind_cos   = cos(wind_direction * pi / 180)
        ) %>%
        select(site_id, hour_floor, temperature, wind_speed,
               humidity, precipitation, wind_sin, wind_cos)
    })
  
  cli_progress_done(id = pb_api)
  
  df <- df %>%
    left_join(
      weather_hourly %>%
        rename(
          hour_before = hour_floor,
          temp_before = temperature,
          wind_before = wind_speed,
          hum_before  = humidity,
          prec_before = precipitation,
          wsin_before = wind_sin,
          wcos_before = wind_cos
        ),
      by = c("site_id", "hour_floor" = "hour_before")
    ) %>%
    left_join(
      weather_hourly %>%
        mutate(hour_ceil = hour_floor + 3600) %>%
        rename(
          temp_after  = temperature,
          wind_after  = wind_speed,
          hum_after   = humidity,
          prec_after  = precipitation,
          wsin_after  = wind_sin,
          wcos_after  = wind_cos
        ) %>%
        select(site_id, hour_ceil, temp_after, wind_after,
               hum_after, prec_after, wsin_after, wcos_after),
      by = c("site_id", "hour_floor" = "hour_ceil")
    ) %>%
    mutate(
      frac          = as.numeric(difftime(datetime_riga, hour_floor, units = "secs")) / 3600,
      TempOUT       = temp_before + frac * (temp_after  - temp_before),
      wind          = wind_before + frac * (wind_after  - wind_before),
      humidity      = hum_before  + frac * (hum_after   - hum_before),
      precipitation = prec_before + frac * (prec_after  - prec_before),
      wind_sin      = wsin_before + frac * (wsin_after  - wsin_before),
      wind_cos      = wcos_before + frac * (wcos_after  - wcos_before)
    ) %>%
    select(-latitude, -longitude, -datetime_utc_parsed, -datetime_riga,
           -date_only, -hour_floor, -temp_before, -wind_before, -hum_before,
           -prec_before, -wsin_before, -wcos_before, -temp_after, -wind_after,
           -hum_after, -prec_after, -wsin_after, -wcos_after, -frac)
  
  write_csv(df, file_path)
  cli_progress_update(id = pb_files)
})

cli_progress_done(id = pb_files)
cli_alert_success("Visi {length(proto_files)} faili veiksmīgi atjaunoti!")
