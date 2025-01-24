#### IMPORT DES DONNEES ----


# ELECTIONS 2024 ----

## Législatives 24 à la circo fusionnée avec données Insee ----

Legis22 = read("01_data/")

circos <- read_dta(
  here(
    "01_data/legis24",
    "base_generale_INSEE.dta"
    )
  )

## Européennes 24 à la commune ----

europ24_brut <- read_xlsx(
  here(
    "01_data/europ24",
    "resultats-definitifs-par-commune.xlsx"
    )
  )

# RECENSEMENT 2020 ---- 

## Fichiers activité ----

census20_act2a <- read_xlsx(
  here(
    "01_data/census20",
    "TD_ACT2A_2020.xlsx"), 
  skip = 10
  )

census20_act2b <- read_xlsx(
  here("01_data/census20",
       "TD_ACT2B_2020.xlsx"), 
  skip = 10)

census20_act3 <- read_xlsx(
  here("01_data/census20",
       "TD_ACT3_2020.xlsx"), 
  skip = 10)

census20_act4 <- read_xlsx(
  here("01_data/census20",
       "TD_ACT4_2020.xlsx"), 
  skip = 10)

## Fichiers emploi ----

census20_emp1 <- read_xlsx(
  here("01_data/census20",
       "TD_EMP1_2020.xlsx"), 
  skip = 10)

census20_emp3 <- read_xlsx(
  here("01_data/census20",
       "TD_EMP3_2020.xlsx"), 
  skip = 10)

census20_emp4 <- read_xlsx(
  here("01_data/census20",
       "TD_EMP4_2020.xlsx"), 
  skip = 10)

## Fichiers formation ----

census20_for2 <- read_xlsx(
  here("01_data/census20",
       "TD_FOR2_2020.xlsx"), 
  skip = 10)

## Fichiers logement ----

census20_log1 <- read_xlsx(
  here("01_data/census20",
       "TD_LOG1_2020.xlsx"), 
  skip = 10)

## Fichiers immigration / nationalité ----

census20_img1A <- read_xlsx(
  here("01_data/census20",
       "TD_IMG1A_2020.xlsx"), 
  skip = 10)

census20_nat1 <- read_xlsx(
  here("01_data/census20",
       "TD_NAT1_2020.xlsx"), 
  skip = 10)

## Fichiers population ----

census20_pop1B <- read_xlsx(
  here("01_data/census20",
       "TD_POP1B_2020.xlsx"), 
  skip = 10)

## Fichier densité urbaine ----

densite_communes <- read_xlsx(
  here("01_data/densite_communes",
       "grille_densite_7_niveaux_2024.xlsx"), 
  skip = 4)

# DONNEES GEOLOC ----

shp_path <- here(
  "01_data/geoloc", 
  "fr-esr-referentiel-geographique.shp")

france_sf <- 
  st_read(shp_path) %>% 
  filter(regrgp_nom != "DROM-COM")
