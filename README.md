
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
  
  Komandu rindā iegūtie faili:
  - poligoni_720730.gpkg



- **`2_birdnet_proto_meta.R`**  
  Proto un meta modeļu palaišana.
  Nepieciešamie ievades dati/faili:
  - [Arbimon projekts un ieraksti](https://arbimon.org/p/niedraju-putni-bakalaurs/insights/) (Failu piekļūšanai nepieciešams sazināties ar darba autoru)
  - Visi `metadati/recordings.XXXX.csv` faili
  - [/metadati/sites.0001.csv](./metadati/sites.0001.csv)
  
  Komandu rindā iegūtie faili:
  - BirdnetR_proto.csv
  - BirdnetR_meta_sites.csv
  - BirdnetR_meta_sites_week.csv

 
- **`2_birdnet_optimized.R`**  
  Kompaktāka `2_birdnet_proto_meta.R`. Kā alternatīva proto un meta modeļu palaišanai. Nepieciešamie ievades un komandu rinā iegūto dati/faili identiski neoptimizētajai versijai.
  
 
- **`3_birdnet_full.R`**  
  Proto un meta modeļu palaišana.
  Nepieciešamie ievades dati/faili:
  - [Arbimon projekts un ieraksti](https://arbimon.org/p/niedraju-putni-bakalaurs/insights/) (Failu piekļūšanai nepieciešams sazināties ar darba autoru)
  
  Komandu rindā iegūtie faili:
  - BirdnetR_meta_sites.csv
  - BirdnetR_meta_sites_week.csv


### [IevadesDati/](./IevadesDati/)
Šī mape satur dažādu veidu ievades datus, kas izmantoti datu apstrādei un analīzei.

### [rezultati/](./rezultati/)
Šī mape satur izlozēto niedrāju datus.

##

Visas komandu rindas šajā repozitorijā ir pārbaudītas 2026-05-12 pieejamajā versijā.
