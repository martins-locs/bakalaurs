# ====================================
# 1. Bibliotēku ielāde ----
# ====================================
library(tidyverse)
library(terra)
library(sf)
library(dplyr)
library(purrr)




# ====================================
# 2. Ainavas rastra un atlases teritorijas sagatavošana ----
# ====================================
ainava = terra::rast("../IevadesDati/Ainava_vienk_mask.tif")
rastrs = raster::raster(ainava)

# Pārbaude par pieejamajiem slāņiem
st_layers("./IevadesDati/km35_radius_riga.gpkg")

# Teritorijas sagatavošana un pārprojekcija
atlases_telpa = st_read("./IevadesDati/km35_radius_riga.gpkg", layer = "km35_radius")
atlases_telpa = atlases_telpa %>% 
  mutate(yes = 1) %>% 
  dplyr::select(yes)
atlases_telpa = st_transform(atlases_telpa, crs = st_crs(ainava))

# Teritorijas maskas izveide
references_teritorija = fasterize::fasterize(atlases_telpa, rastrs)
references_teritorija = terra::rast(references_teritorija)
terra::plot(references_teritorija)

# Maskēšana un apgriešana
atlasits = terra::mask(ainava, references_teritorija)
atlasits2 = terra::crop(atlasits, atlases_telpa)
plot(atlasits2)

# Tīrīšana
rm(ainava, rastrs, atlases_telpa, references_teritorija, atlasits)




# ====================================
# 3. Pikseļu klasifikācija un pārvēršana par poligoniem ----
# ====================================
klasificets = ifel(atlasits2 >= 720 & atlasits2 <= 730, 1, NA)
terra::plot(klasificets)
gc()
rm(atlasits2)
gc()

# No klasificētā rastra iegūst poligonus
poligoniem = terra::as.polygons(klasificets, aggregate = TRUE, na.rm = TRUE, round = TRUE, digits = 0)
multipoligoni = sf::st_as_sf(poligoniem)
poligoni = st_cast(multipoligoni, "POLYGON")




# ====================================
# 4. Buferēšana, savienošana un filtrēšana pēc platības ----
# ====================================
poligoni_buf = st_buffer(poligoni, dist = 5.5)
prove = st_union(poligoni_buf)
prove_multipoly = st_as_sf(prove)
prove_poly = st_cast(prove_multipoly, "POLYGON")

# Aprēķina platību
prove_poly$platiba = as.numeric(st_area(prove_poly))
prove_poly$platiba_ha = prove_poly$platiba / 10000
prove_poly$ID = rownames(prove_poly)

# Minimālā platība = apļa ar rādiusu 100m laukums (ha)
min_platiba <- (100^2) * pi / 10000
prove_poly <- prove_poly %>%
  filter(platiba_ha >= min_platiba)
print(prove_poly)

#write_sf(prove_poly,"./IevadesDati/Poligoni_720730_v3.gpkg",layer="Poligoni_darbam",append=FALSE)




# ====================================
# 5. Atlases sadalījuma sagatavošana ----
# ====================================
sadalijums <- st_read("./IevadesDati/sadalijums.gpkg")
sadalijums <- st_transform(sadalijums, crs = st_crs(prove_poly))
sadalijums$ID <- seq_len(nrow(sadalijums))
colnames(sadalijums)
print(sadalijums)




# ====================================
# 6. Dažu ID izslēgšana pirms izlases ----
# ====================================
iznemtie_ID_atlase <- c(56, 732, 1260, 1373)
prove_poly <- prove_poly %>% filter(!ID %in% iznemtie_ID_atlase)




# ====================================
# 7. Nejauša atlase katrā sadalījuma vienībā (5 gabali) ----
# ====================================
set.seed(4)
atlase <- sadalijums %>%
  group_split(ID) %>%
  map(~ {
    sub_set <- st_intersection(prove_poly, .x)
    sample_n(sub_set, 5, replace = FALSE)
  }) %>%
  bind_rows()

print(atlase, n = Inf)




# ====================================
# 8. Centroides izvilkšana un secības piešķiršana ----
# ====================================
centroidas <- st_centroid(prove_poly)

# Izņem noteiktus centroidu ID
iznemtie_ID <- c(1322, 1064, 1164, 1040, 1053, 1283, 1358, 1302, 1185, 1763, 623, 273, 791, 10, 186, 184, 704, 1030, 617, 679)
centroidas <- centroidas %>% filter(!ID %in% iznemtie_ID)

# Savieno centroides ar sadalījumu
savienotas_centroidas <- st_join(centroidas, sadalijums)
print(colnames(savienotas_centroidas))

# Piešķir nejaušu secību katrā sadalījuma vienībā
set.seed(4)
savienotas_centroidas <- savienotas_centroidas %>%
  group_by(id) %>% 
  mutate(seciba = sample(1:n(), replace = FALSE)) %>%
  ungroup()

# Sakārto pēc secības
savienotas_centroidas <- savienotas_centroidas %>% arrange(seciba)
print(savienotas_centroidas, n = 80)