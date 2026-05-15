
# Materiāli "BIRDNET KLASIFIKĀCIJAS PRECIZITĀTES NOVĒRTĒJUMS NIEDRĀJU PUTNU PASĪVĀ AKUSTISKAJĀ MONITORINGĀ" reproducēšanai

Šis repozitorijs satur komandu rindas un interneta saites ievades datiem mana bakalaura darba gaitas un rezultātu reproducēšanai. Repozitorijs satur visas darbā izmantotās komandu rindas un to rezultātā radītos produktus vai to interneta saites.

## Repozitorija saturs

### R skripti

- **`1_poligonu_atlase.R`**  
  Niedrāju atlase un izloze.
  Nepieciešamie ievades dati/faili:
  - [LULC klašu klasifikators Latvijas teritorijai (2024. gads, 10x10 m)](404)
  - /IevadesDati/km35_radius_riga.gpkg
  
  Iegūtie dati/faili:
  - `piemers.jpg`
  - /IevadesDati/km35_radius_riga.gpkg

- **`2_birdnet_full.R`**  
  Proto un meta modeļu palaišana. Nepieciešamie ievades dati: [Arbimon projekts un ieraksti](https://arbimon.org/p/niedraju-putni-bakalaurs/insights/) (Failu piekļūšanai nepieciešams sazināties ar darba autoru)
  Iegūtie faili:
  - `piemers.jpg`

### [IevadesDati/](./IevadesDati/)
Šī mape satur dažādu veidu ievades datus, kas izmantoti datu apstrādei un analīzei.

### [rezultati/](./rezultati/)
Šī mape satur izlozēto niedrāju datus.

##

Visas komandu rindas šajā repozitorijā ir pārbaudītas 2026-05-12 pieejamajā versijā.
