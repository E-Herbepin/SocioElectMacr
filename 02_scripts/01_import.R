#### IMPORT DES DONNEES ----


# ELECTIONS 2017-2022 ----

## Election présidentielle 2017 ----
# Récupérées le 24/01 sur : https://www.data.gouv.fr/fr/datasets/election-presidentielle-des-23-avril-et-7-mai-2017-resultats-definitifs-du-1er-tour-par-bureaux-de-vote/

#/!\ Ne faire tourner qu'une fois pour créer le fichier .CSV, puis ne lancer que le read.csv lorsque nécessaire

Pres17T1 <- read_xls(
  here(
    "01_data",
    "Presidentielle_2017_Resultats_Communes_Tour_1_c.xls"
  ),
  skip = 3
)

for (i in c(19+c(0:10)*7)) {#Pour chaque "panneau" (i= le numéro de la colonne)
  for (x in 1:nrow(Pres17T1)) {#Pour chaque commune
    Pres17T1[x,paste0("17_Voix_",str_to_title(as.character(Pres17T1[x,i+2])))] <- Pres17T1[x,i+4] #la case de la commune dans la colonne "NomDuCandidat_Voix" prend la valeur de la colonne "Voix" qui suit le panneau qu'on regarde
  }}

Pres17T1 %<>% select(-c(19:95, which(grepl("%",names(Pres17T1)) | grepl("[.][.]",names(Pres17T1))))) %>% # On enlève les colonnes que l'on a utilisées pour créer les colonnes "NomDuCandidat_Voix" etc. ET les colonnes recalculables (pourcentages)
  rename( "Libellé_commune" = "Libellé de la commune", # On renomme les colonnes pour plus de clarté
          "Libellé_du_département" = "Libellé du département",
          "Code_département" = "Code du département",
          "Code_commune" = "Code de la commune",
          "17_Inscrits" = "Inscrits",
          "17_Abstentions" = "Abstentions",
          "17_Votants" = "Votants",
          "17_Blancs" = "Blancs",
          "17_Nuls" = "Nuls",
          "17_Exprimés" = "Exprimés",
          "17_Voix_Le_Pen" = "17_Voix_Le Pen",)

write.csv(Pres17T1, "01_data/Pres17T1.csv") # On exporte en CSV (plus pratique pour la suite)

Pres17T1 <- read.csv("01_data/Pres17T1.csv")

# Récupérées le 24/01 sur : https://www.data.gouv.fr/fr/datasets/resultats-du-premier-tour-de-lelection-presidentielle-2022-par-commune-et-par-departement/

Pres22T1 <- read_delim(
  here(
    "01_data",
    "04-resultats-par-commune.csv"
  ),
  delim = ",",
  col_select = -c(1)
)

Pres22T1$cand_nom %<>% str_to_title()

Pres22T1$commune_code %<>% 
  as.numeric() %>%
  as.character()
Pres22T1$dep_code_3 %<>% 
  as.numeric() %>%
  as.character()
Pres22T1$reg_code_3 %<>% 
  as.numeric() %>%
  as.character()

# On enlève les colonnes qui calculent les pourcentages et rapports (calculs que l'on pourra faire de notre côté selon nos besoins) qui aloourdissent le tableau inutilement

Pres22T1 %<>% select(-c(1,5,4,24,26,27,"num_tour", which(grepl("pourc",names(Pres22T1)) | grepl("rapport",names(Pres22T1))))) %>%
  pivot_wider( names_from = cand_nom, values_from = cand_nb_voix) %>%
  rename(
    "Libellé_commune" = "commune_name",
    "Code_commune" = "commune_code",
    "Libellé_du_département" = "dep_name",
    "Code_département" = "dep_code_3",
    "Libellé_région" = "reg_name",
    "Code_région" = "reg_code_3",
    "22_Inscrits" = "inscrits_nb",
    "22_Abstentions" = "abstention_nb",
    "22_Votants" = "votants_nb",
    "22_Blancs" = "blancs_nb",
    "22_Nuls" = "nuls_nb",
    "22_Exprimés" = "exprimes_nb",
    "22_Voix_Le_Pen" = "Le Pen",
    "22_Voix_Macron" = "Macron",
    "22_Voix_Mélenchon" = "Mélenchon",
    "22_Voix_Pécresse" = "Pécresse",
    "22_Voix_Zemmour" = "Zemmour",
    "22_Voix_Lassalle" = "Lassalle",
    "22_Voix_Hidalgo" = "Hidalgo",
    "22_Voix_Jadot" = "Jadot",
    "22_Voix_Poutou" = "Poutou",
    "22_Voix_Dupont-Aignan" = "Dupont-Aignan",
    "22_Voix_Arthaud" = "Arthaud",
    "22_Voix_Roussel" = "Roussel",
  )

write.csv(Pres22T1, "01_data/Pres22T1.csv") # On exporte en CSV (plus pratique pour la suite)

Pres22T1 <- read.csv("01_data/Pres22T1.csv")


# Les communes qui ne sont plus dans le fichier de 2022 (notamment les communes de l'étranger)
setdiff(Pres17T1$Libellé_commune, Pres22T1$Libellé_commune)

# Les communes qui ne sont pas dans le fichier de 2017 (notamment, les arrondissements des grandes villes)
setdiff(Pres22T1$Libellé_commune, Pres17T1$Libellé_commune)



#Communes fusionnées : reprendre les fichiers ici : https://www.insee.fr/fr/information/2549968



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

