
# Materiāli "BIRDNET KLASIFIKĀCIJAS PRECIZITĀTES NOVĒRTĒJUMS NIEDRĀJU PUTNU PASĪVĀ AKUSTISKAJĀ MONITORINGĀ" reproducēšanai

Šis repozitorijs satur komandu rindas un interneta saites ievades datiem mana bakalaura darba gaitas un rezultātu reproducēšanai. Repozitorijs satur visas darbā izmantotās komandu rindas un to rezultātā radītos produktus vai to interneta saites.

## Repozitorija saturs

### R skripti

- **`1_poligonu_atlase.R`**  
  - Niedrāju atlase un izloze.
  - Nepieciešamie ievades dati/faili:
    - LULC klašu klasifikators Latvijas teritorijai (2024. gads, 10x10 m) (Faila izmēra dēļ nepieciešams sazināties ar darba autoru)
    - [IevadesDati/riga_35km_radius.gpkg](./IevadesDati/riga_35km_radius.gpkg)
    - [/IevadesDati/riga_sadalijums.gpkg](./IevadesDati/riga_sadalijums.gpkg)
  
  - Komandu rindā iegūtie faili:
    - [/IevadesDati/poligoni_720730.gpkg](./IevadesDati/poligoni_720730.gpkg)



- **`2_birdnet_proto_meta.R`**  
  - Proto un meta modeļu palaišana.
  - Nepieciešamie ievades dati/faili:
    - [Arbimon projekts un ieraksti](https://arbimon.org/p/niedraju-putni-bakalaurs/insights/) (Failu piekļūšanai nepieciešams sazināties ar darba autoru)
    - Visi `metadati/recordings.XXXX.csv` faili
    - [/metadati/sites.0001.csv](./metadati/sites.0001.csv)
  
  - Komandu rindā iegūtie faili:
    - Visi `all_results_XX_XX_overlap_XX.csv` faili (Iegūtās kolonnas:  start,  end,  scientific_name,  common_name,  confidence,  audio_path,  audio_file,  quadrant,  mic_type,  site_id,  record_id,  datetime,  datetime_utc)
    - [/rezultati/BirdnetR_meta_sites.csv](./rezultati/BirdnetR_meta_sites.csv)
    - [/rezultati/BirdnetR_meta_sites_week.csv](./rezultati/BirdnetR_meta_sites_week.csv)


 
- **`2_birdnet_optimized.R`**  
  - Kompaktāka `2_birdnet_proto_meta.R`. Kā alternatīva proto un meta modeļu palaišanai. Nepieciešamie ievades un komandu rinā iegūto dati/faili identiski neoptimizētajai versijai.
  
 
- **`3_proto_plus_meta_site.R`**  
  - Proto un meta ar lokācijas modeļu apvienošana.
  - Nepieciešamie ievades dati/faili:
    - Visi `all_results_QX_XX_overlap_XX.csv` faili kas iegūti pēc `2_birdnet_proto_meta.R`
    - [/rezultati/BirdnetR_meta_sites.csv](./rezultati/BirdnetR_meta_sites.csv)
   
  - Komandu rindā iegūtie faili:
    - Visi `qX_XX.csv` faili (Papildus iegūtās kolonnas: site_confidence)


- **`4_proto_plus_meta_site_week.R`**  
  - Proto un meta ar lokācijas un laika modeļu apvienošana.
  - Nepieciešamie ievades dati/faili:
    - Visi `qX_XX.csv` faili
    - [/rezultati/BirdnetR_meta_sites_week.csv](./rezultati/BirdnetR_meta_sites_week.csv)
   
  - Komandu rindā iegūtie faili:
    - Visi `qX_XX.csv` faili (Papildus iegūtās kolonnas: week, site_week_confidence)


- **`5_proto_plus_meta_latvia.R`**  
  - Proto modeļa papildināšana ar informāciju ar Latvijā fiksētajām sugām.
  - Nepieciešamie ievades dati/faili:
    - Visi `qX_XX.csv` faili kas iegūti pēc `4_proto_plus_meta_site_week.R`
    - [/rezultati/BirdnetR_meta_sites_week.csv](./rezultati/BirdnetR_meta_sites_week.csv)
   
  - Komandu rindā iegūtie faili:
    - Visi `qX_XX.csv` faili (Papildus iegūtās kolonnas: latvia)
 

- **`6_laikapstakli.R`**  
  - Proto modeļa papildināšana ar informāciju par laikapstākļiem ieraksta veikšanas brīdī.
  - Nepieciešamie ievades dati/faili:
    - Visi `qX_XX.csv` faili kas iegūti pēc `5_proto_plus_meta_latvia.R`
   
  - Komandu rindā iegūtie faili:
    - Visi `qX_XX.csv` faili (Papildus iegūtās kolonnas: TempIN, TempOUT, wind, humidity, precipitation, wind_sin, wind_cos)


### [IevadesDati/](./IevadesDati/)
Satur .gpkg ievades datus, niedrāju poligonu atlasei un izlozei.

### [metadati/](./metadati/)
Satur ievades datus BirdNet modeļa vajadzībām.

### [rezultati/](./rezultati/)
Šī mape satur izlozēto niedrāju datus.

##

Visas komandu rindas šajā repozitorijā ir pārbaudītas 2026-05-15 pieejamajā versijā.
