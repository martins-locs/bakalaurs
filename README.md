
# Materiāli "BIRDNET KLASIFIKĀTORA PRECIZITĀTES NOVĒRTĒJUMS NIEDRĀJU PUTNU PASĪVĀ AKUSTISKAJĀ MONITORINGĀ" reproducēšanai

Šis repozitorijs satur komandu rindas un interneta saites ievades datiem mana bakalaura darba gaitas un rezultātu reproducēšanai. Repozitorijs satur visas darbā izmantotās komandu rindas un to rezultātā radītos produktus vai to interneta saites.

## Repozitorija saturs

### R skripti

- **`1_poligonu_atlase.R`**  
  - Niedrāju atlase un izloze.
  - Nepieciešamie ievades dati/faili:
    - LULC klašu klasifikators Latvijas teritorijai (2024. gads, 10x10 m) (Faila izmēra dēļ nepieciešams sazināties ar darba autoru)
    - [/IevadesDati/riga_35km_radius.gpkg](./IevadesDati/riga_35km_radius.gpkg)
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
    - Visi `proto/all_results_QX_XX.csv` faili (Iegūtās kolonnas:  start,  end,  scientific_name,  common_name,  confidence,  audio_path,  audio_file,  quadrant,  mic_type,  site_id,  record_id,  datetime,  datetime_utc, overlap)
    - [/rezultati/BirdnetR_meta_sites.csv](./rezultati/BirdnetR_meta_sites.csv)
    - [/rezultati/BirdnetR_meta_sites_week.csv](./rezultati/BirdnetR_meta_sites_week.csv)


 
- **`3_pievienot_meta__info.R`**  
  - Proto, meta ar lokācijas un metas ar lokācijas un laika modeļu apvienošana. Papildināšana ar informāciju ar Latvijā fiksētajām sugām.
  - Nepieciešamie ievades dati/faili:
    - Visi `proto/all_results_QX_XX.csv` faili kas iegūti pēc `2_birdnet_proto_meta.R`
    - [/rezultati/BirdnetR_meta_sites.csv](./rezultati/BirdnetR_meta_sites.csv)
    - [/rezultati/BirdnetR_meta_sites_week.csv](./rezultati/BirdnetR_meta_sites_week.csv)
    - [/rezultati/BirdnetR_meta_latvia.csv](./rezultati/BirdnetR_meta_latvia.csv)
   
  - Komandu rindā iegūtie faili:
    - Visi `proto/qX_XX.csv` faili (Papildus iegūtās kolonnas: site_confidence, week, site_week_confidence, latvia)

 



- **`4_ierakstu_atlase.R`**  
  - Stratificēta ierakstu atlase (nedēļa × stunda × kvadrants × mikrofons) ar saullēkta/saulrieta periodu papildatlasi.
  - Nepieciešamie ievades dati/faili:
    - Visi `metadati/recordings.XXXX.csv` faili
    - [/metadati/sites.0001.csv](./metadati/sites.0001.csv)
    - Visi veikto ierakstu audio faili
   
  - Komandu rindā iegūtie faili:
    - [/rezultati/ierakstu_atlase.csv](./rezultati/ierakstu_atlase.csv)
    - [/rezultati/heatmap_overall.png](./rezultati/heatmap_overall.png)
    - [/rezultati/heatmap_faceted.png](./rezultati/heatmap_faceted.png)
   


- **`5_kludu_matrica.R`**  
  - Salīdzina BirdNet un manuāli klausīto ierakstu detekcijas pēc laika pārklāšanās, aprēķina precision/recall/F1 pa overlap vērtībām.
  - Nepieciešamie ievades dati/faili:
    - Visi `proto/qX_XX.csv` faili
    - [/manuali_klausits/manual.xlsx](./manuali_klausits/manual.xlsx)
   
  - Komandu rindā iegūtie faili:
    - [/rezultati/TP_FP_FN_rezultati.csv](./rezultati/TP_FP_FN_rezultati.csv)
    - [/rezultati/TP_FP_FN_salidzinajums.csv](./rezultati/TP_FP_FN_salidzinajums.csv)
    - [/rezultati/metrika_pa_sugam.csv](.[/rezultati/metrika_pa_sugam.csv)
    - [/rezultati/metriku_statistika.csv](.[/rezultati/metriku_statistika.csv)

   



- **`6_GLMM.R`**
- Standartizēti GLMM koeficienti sugu-specifiskiem BirdNET confidence threshold ar meta parametriem.
  - Nepieciešamie ievades dati/faili:
    - [/rezultati/TP_FP_FN_rezultati.csv](./rezultati/TP_FP_FN_rezultati.csv)
   
  - Komandu rindā iegūtie faili:
    - [/GLMM/glmm_scaling_parameters.csv](./GLMM/glmm_scaling_parameters.csv)
    - [/GLMM/glmm_performance.csv](./GLMM/glmm_performance.csv)
    - [/GLMM/glmm_final_coefficients_SCALED.csv](./GLMM/glmm_final_coefficients_SCALED.csv)
    - [/GLMM/glmm_user_guide.csv](./GLMM/glmm_user_guide.csv)
    - [/GLMM/glmm_model_comparison_f1.png](./GLMM/glmm_model_comparison_f1.png)
    - [/GLMM/glmm_species_performance.png](./GLMM/glmm_species_performance.png)




### [IevadesDati/](./IevadesDati/)
Satur .gpkg ievades datus, niedrāju poligonu atlasei un izlozei.

### [metadati/](./metadati/)
Satur ievades datus BirdNet modeļa vajadzībām.

### [manuali_klausits/](./manuali_klausits/)
Satur patstāvīgi noteikto putnu sarakstu.

### [rezultati/](./rezultati/)
Satur datus un vizuālos materiālus par iegūtajiem rezultātiem no komandu rindām.

### [GLMM/](./GLMM/)
Satur iegūtos rezultātus no GLMM analīzes.

### [proto/](./proto/)
Satur iegūtos rezultātus no GLMM analīzes.

##

Visas komandu rindas šajā repozitorijā ir pārbaudītas 2026-05-18 pieejamajā versijā.
