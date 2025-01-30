#### IMPORT DES DONNEES ----

if(!dir.exists("01_data")){
  dir.create("01_data", showWarnings = FALSE)}

if(!dir.exists("01_data/Présidentielles 2017-2022")){
  dir.create("01_data/Présidentielles 2017-2022", showWarnings = FALSE)}

if(!file.exists("01_data/Présidentielles 2017-2022/Présidentielles&Recensement.csv")) {

# Téléchargement des données ----

# Présidentielles 2017
# Récupérées le 25/01 sur : https://www.data.gouv.fr/fr/datasets/election-presidentielle-des-23-avril-et-7-mai-2017-resultats-definitifs-du-1er-tour-par-communes/

if(!file.exists("01_data/Présidentielles 2017-2022/Pres17T1_DL.xls")) { #Si le fichier n'est pas trouvé
  download.file( #On le télécharge
    url = paste0("https://www.data.gouv.fr/fr/datasets/r/77ed6b2f-c48f-4037-8479-50af74fa5c7a"),
    destfile = paste0("01_data/Présidentielles 2017-2022/Pres17T1_DL.xls"),
    mode = "wb")}

# Présidentielles 2022
# Récupérées le 25/01 sur : https://www.data.gouv.fr/fr/datasets/resultats-du-premier-tour-de-lelection-presidentielle-2022-par-commune-et-par-departement/

if(!file.exists("01_data/Présidentielles 2017-2022/Pres22T1_DL.csv")) {
  download.file(
    url = paste0("https://www.data.gouv.fr/fr/datasets/r/54782507-e795-4f9d-aa70-ed06feba22e3"),
    destfile = paste0("01_data/Présidentielles 2017-2022/Pres22T1_DL.csv"),
    mode = "wb")}

# Nouvelles communes et fusions
# Récupérées le 25/01 sur : https://www.insee.fr/fr/information/2549968

if(!dir.exists("01_data/Communes_nouvelles")){
  dir.create("01_data/Communes_nouvelles", showWarnings = FALSE)
  for (x in 2017:2022) {
    download.file(
      url = paste0("https://www.insee.fr/fr/statistiques/fichier/2549968/Communes_nouvelles_", x , if_else(x<=2019,".xls",".xlsx")),
      destfile = paste0("01_data/Communes_nouvelles/Communes_nouvelles_",x,if_else(x<=2019,".xls",".xlsx")),
      mode = "wb"
    )
  }
}

## Election présidentielle 2017 ----

#/!\ Ne faire tourner qu'une fois pour créer le fichier .CSV, puis ne lancer que le read.csv lorsque nécessaire

if(!file.exists("01_data/Présidentielles 2017-2022/Pres17T1_recodé.csv")) {
  
  Pres17T1 <- read_xls(
    here(
      "01_data/Présidentielles 2017-2022",
      "Pres17T1_DL.xls"
    ),
    skip = 3
  )
  
  for (i in c(19+c(0:10)*7)) {#Pour chaque "panneau" (i= le numéro de la colonne)
    for (x in 1:nrow(Pres17T1)) {#Pour chaque commune
      Pres17T1[x,paste0("Nb_17_Voix_",str_to_title(as.character(Pres17T1[x,i+2])))] <- Pres17T1[x,i+4] #la case de la commune dans la colonne "NomDuCandidat_Voix" prend la valeur de la colonne "Voix" qui suit le panneau qu'on regarde
    }}
  
  Pres17T1 %<>% select(-c(19:95, which(grepl("%",names(Pres17T1)) | grepl("[.][.]",names(Pres17T1))))) %>% # On enlève les colonnes que l'on a utilisées pour créer les colonnes "NomDuCandidat_Voix" etc. ET les colonnes recalculables (pourcentages)
    rename( "Libellé_commune" = "Libellé de la commune", # On renomme les colonnes pour plus de clarté
            "Code_commune" = "Code de la commune",
            "Libellé_département" = "Libellé du département",
            "Code_département" = "Code du département",
            "Nb_17_Inscrits" = "Inscrits",
            "Nb_17_Abstentions" = "Abstentions",
            "Nb_17_Votants" = "Votants",
            "Nb_17_Blancs" = "Blancs",
            "Nb_17_Nuls" = "Nuls",
            "Nb_17_Exprimés" = "Exprimés",
            "Nb_17_Voix_Le_Pen" = "Nb_17_Voix_Le Pen",)
  
  # On enlève les communes de l'étranger
  Pres17T1 %<>% filter(!str_detect(Pres17T1$Libellé_département, "Français établis hors de France"))
  
  # On attribut les codes départements à la corse et aux DOM
  
  Pres17T1$Code_département <- case_when(
    Pres17T1$Libellé_département == "Corse-du-Sud" ~ "2A",
    Pres17T1$Libellé_département == "Haute-Corse" ~ "2B",
    Pres17T1$Libellé_département == "Guadeloupe" ~ "971",# Or la Corse, ces départements ne sont pas dans les résutats 2022
    Pres17T1$Libellé_département == "Martinique" ~ "972",
    Pres17T1$Libellé_département == "Guyane" ~ "973",
    Pres17T1$Libellé_département == "La Réunion" ~ "974",
    Pres17T1$Libellé_département == "Mayotte" ~ "976",
    Pres17T1$Libellé_département == "Nouvelle-Calédonie" ~ "988",
    Pres17T1$Libellé_département == "Polynésie française" ~ "987",
    Pres17T1$Libellé_commune == "Saint-Barthélémy" ~ "977",
    Pres17T1$Libellé_commune == "Saint-Martin" & Pres17T1$Libellé_département == "Saint-Martin/Saint-Barthélemy" ~ "977",
    Pres17T1$Libellé_département == "Saint-Pierre-et-Miquelon" ~ "975",
    Pres17T1$Libellé_département == "Wallis et Futuna" ~ "986",
    TRUE ~ as.character(Pres17T1$Code_département))
  
  Pres17T1$Code_commune <- case_when(
    Pres17T1$Libellé_département == "Mayotte" ~ as.numeric(Pres17T1$Code_commune) + 100,
    Pres17T1$Libellé_département == "Polynésie française" ~ as.numeric(Pres17T1$Code_commune) + 700,
    TRUE ~ Pres17T1$Code_commune)
  
  write.csv(Pres17T1, "01_data/Présidentielles 2017-2022/Pres17T1_recodé.csv", row.names = FALSE) # On exporte en CSV (plus pratique pour la suite)
} else {Pres17T1 <- read.csv("01_data/Présidentielles 2017-2022/Pres17T1_recodé.csv")}

# On modifie le code commune pour simplifier les manipulations :
Pres17T1$Code_commune %<>% str_pad(width = 3, pad = "0")

## Election présidentielle 2022 ----

if(!file.exists("01_data/Présidentielles 2017-2022/Pres22T1_recodé.csv")) {
  
  Pres22T1 <- read_delim(
    here(
      "01_data/Présidentielles 2017-2022",
      "Pres22T1_DL.csv"
    ),
    delim = ",",
    col_select = -c(1)
  )
  
  Pres22T1$cand_nom %<>% str_to_title()
  
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
      "Libellé_département" = "dep_name",
      "Code_département" = "dep_code_3",
      "Libellé_région" = "reg_name",
      "Code_région" = "reg_code_3",
      "Nb_22_Inscrits" = "inscrits_nb",
      "Nb_22_Abstentions" = "abstention_nb",
      "Nb_22_Votants" = "votants_nb",
      "Nb_22_Blancs" = "blancs_nb",
      "Nb_22_Nuls" = "nuls_nb",
      "Nb_22_Exprimés" = "exprimes_nb",
      "Nb_22_Voix_Le_Pen" = "Le Pen",
      "Nb_22_Voix_Macron" = "Macron",
      "Nb_22_Voix_Mélenchon" = "Mélenchon",
      "Nb_22_Voix_Pécresse" = "Pécresse",
      "Nb_22_Voix_Zemmour" = "Zemmour",
      "Nb_22_Voix_Lassalle" = "Lassalle",
      "Nb_22_Voix_Hidalgo" = "Hidalgo",
      "Nb_22_Voix_Jadot" = "Jadot",
      "Nb_22_Voix_Poutou" = "Poutou",
      "Nb_22_Voix_Dupont-Aignan" = "Dupont-Aignan",
      "Nb_22_Voix_Arthaud" = "Arthaud",
      "Nb_22_Voix_Roussel" = "Roussel",
    )
  
  # On enlève les arrondissement des grandes villes
  Pres22T1 %<>% filter(!str_detect(Pres22T1$Libellé_commune, pattern = "arrondissement")) 
  
  Pres22T1$Code_département <- case_when(
    Pres22T1$Libellé_département == "Corse-du-Sud" ~ "2A",
    Pres22T1$Libellé_département == "Haute-Corse" ~ "2B",
    TRUE ~ as.character(Pres22T1$Code_département))
  
  write.csv(Pres22T1, "01_data/Présidentielles 2017-2022/Pres22T1_recodé.csv", row.names = FALSE) # On exporte en CSV (plus pratique pour la suite)
} else {Pres22T1 <- read.csv("01_data/Présidentielles 2017-2022/Pres22T1_recodé.csv")}

Pres22T1$Code_commune %<>% str_pad(width = 3, pad = "0")

## Communes nouvelles ----

if(!file.exists("01_data/Communes_nouvelles/Communes_nouvelles_fusionnées.csv")) {
  Date_elections_Début <- dmy("23/01/2017", tz = "Europe/Paris") # 3 mois avant le premier tour
  Date_elections_Fin <- dmy("04/03/2022", tz = "Europe/Paris") # date de cloture des inscriptions sur les listes électorales
  
  Communes_nouvelles <- bind_rows(
    read_xls(
      here("01_data/Communes_nouvelles",
           "Communes_nouvelles_2017.xls"),
      sheet = 1)[1:93,] %>%
      mutate(
        DepComN = as.numeric(DepComN),
        DepComA = as.numeric(DepComA),
        Date = if_else(is.na(Date2), 
                       dmy("01/01/2018", tz = "Europe/Paris"), 
                       dmy(Date2, tz = "Europe/Paris")))%>%
      select(-c(5:9)), 
    
    read_xls(
      here("01_data/Communes_nouvelles",
           "Communes_nouvelles_2018.xls"),
      sheet = 1) %>%
      mutate(
        DepComN = as.numeric(DepComN),
        DepComA = as.numeric(DepComA),
        Date = if_else(is.na(Date2), 
                       dmy("01/01/2019", tz = "Europe/Paris"), 
                       ymd(Date2, tz = "Europe/Paris"))) %>%
      select(-c(5:11)), 
    
    read_xls(
      here("01_data/Communes_nouvelles",
           "Communes_nouvelles_2019.xls"),
      sheet = 1) %>%
      mutate(
        DepComN = as.numeric(DepComN),
        DepComA = as.numeric(DepComA),
        Date = if_else(is.na(Date2), 
                       dmy("01/01/2020", tz = "Europe/Paris"), 
                       ymd(Date2, tz = "Europe/Paris"))) %>%
      select(-c(5:11)), 
    
    read_xlsx(
      here("01_data/Communes_nouvelles",
           "Communes_nouvelles_2020.xlsx"),
      sheet = 1) %>%
      mutate(
        DepComN = as.numeric(DepComN),
        DepComA = as.numeric(DepComA),
        Date = if_else(is.na(Date2), 
                       dmy("01/01/2021", tz = "Europe/Paris"), 
                       ymd(Date2, tz = "Europe/Paris"))) %>%
      select(-c(5:10)), 
    
    read_xlsx(
      here("01_data/Communes_nouvelles",
           "Communes_nouvelles_2021.xlsx"),
      sheet = 1) %>%
      mutate(
        DepComN = as.numeric(DepComN),
        DepComA = as.numeric(DepComA),
        Date = if_else(is.na(Date2), 
                       dmy("01/01/2022", tz = "Europe/Paris"), 
                       ymd(Date2, tz = "Europe/Paris"))) %>%
      select(-c(5:9)), 
    
    read_xlsx(
      here("01_data/Communes_nouvelles",
           "Communes_nouvelles_2022.xlsx"),
      sheet = 1) %>%
      mutate(
        DepComN = as.numeric(DepComN),
        DepComA = as.numeric(DepComA),
        Date = if_else(is.na(Date2), 
                       dmy("01/01/2023", tz = "Europe/Paris"), 
                       ymd(Date2, tz = "Europe/Paris"))) %>%
      select(-c(5:9))) %>%
    filter(Date >= Date_elections_Début & Date <= Date_elections_Fin)
  
  Communes_nouvelles <- rbind(Communes_nouvelles,data.frame(
    DepComN = c("79078", "95040"),
    NomCN = c("Plaine-d'Argenson", "Avernes"),
    DepComA = c("79247", "95259"),
    NomCA = c("Saint-Etienne-la-Cigogne", "Gadancourt"),
    Date = c("2018-01-01", "2018-01-01"),
    stringsAsFactors = FALSE))
  
  write.csv(Communes_nouvelles, "01_data/Communes_nouvelles/Communes_nouvelles_fusionnées.csv")} else {Communes_nouvelles <- read.csv("01_data/Communes_nouvelles/Communes_nouvelles_fusionnées.csv")}

# Fusion des bases ----

## Présidentielles ----

Pres17T1$DepCom <- str_pad(paste0(as.character(Pres17T1$Code_département),Pres17T1$Code_commune),width = 3, pad = "0")
Pres22T1$DepCom <- str_pad(paste0(Pres22T1$Code_département,Pres22T1$Code_commune),width = 5, pad = "0")
#On ajoute un identifiant unique pour les communes 

if(!file.exists("01_data/Présidentielles 2017-2022/Présidentielles_fusionnées.csv")) {
  
  Pres17T1_fusion <- Pres17T1
  
  #On nettoie la base nouvelles communes
  
  Communes_nouvelles <- Communes_nouvelles %>%
    merge( #on ajoute la colonne des noms de 2022 en fonction du Code
      Pres22T1[, c("DepCom", "Libellé_commune")],
      by.x = "DepComN",
      by.y = "DepCom",
      sort=FALSE,
      all.x = TRUE
    ) %>%
    mutate(
      
      Libellé_commune_tr = stri_trans_general(Libellé_commune, "Latin-ASCII") %>%
        tolower() %>%
        gsub("-", "", .) %>% #On créé une nouvelle colonne avec les noms 22 "nettoyés"
        gsub("’", "", .) %>%
        gsub(" ", "", .) %>%
        gsub("'", "", .),
      
      NomCN_tr = stri_trans_general(NomCN, "Latin-ASCII") %>%
        tolower() %>%
        gsub("-", "", .) %>% #On créé une nouvelle colonne avec les noms 17 "nettoyés"
        gsub("’", "", .) %>%
        gsub(" ", "", .) %>%
        gsub("'", "", .),
      
      NomCN = if_else(NomCN != Libellé_commune & #Que les noms entre 2022 et communes nouvelles sont différents
                        NomCN_tr == Libellé_commune_tr,
                      Libellé_commune, 
                      NomCN),# On modifie les libellés mais aussi les différents codes
      
      DepComN = str_pad(DepComN, width = 5, pad = "0")
      
    ) %>%
    select(1:6) #On enlève les colonnes que l'on vient de créer
  
  # On remplace celles qu'on a par leurs nouveaux noms
  
  Pres17T1_fusion <- Pres17T1_fusion %>%
    merge( #on ajoute la colonne des noms de 2022 en fonction du Code
      Communes_nouvelles[, c("NomCN", "NomCA", "DepComN", "DepComA")],
      by.x = "DepCom",
      by.y = "DepComA",
      sort=FALSE,
      all.x = TRUE
    ) %>%
    mutate(
      Libellé_commune = if_else(!is.na(NomCN), NomCN, Libellé_commune),# On modifie les libellés mais aussi les différents codes
      Code_commune = if_else(!is.na(NomCN), str_sub(DepComN, -3), Code_commune),
      Code_département = if_else(!is.na(NomCN), str_sub(DepComN, end = 2), Code_département),
      DepCom = str_pad(if_else(!is.na(NomCN), DepComN, DepCom), width = 5, pad = "0")
    )
  
  # Attribut aux communes de 2017 le nom de 2022 lorsqu'il diffère d'un accent ou d'une présence de - alors qu'ils ont le même code
  
  Pres17T1_fusion <- Pres17T1_fusion %>%
    left_join( #on ajoute la colonne des noms de 2022 en fonction du Code
      Pres22T1[, c("DepCom", "Libellé_commune")], 
      suffix = c("", "_22"),
      by = "DepCom"
    ) %>%
    mutate(
      Libellé_commune_22_tr = stri_trans_general(Libellé_commune_22, "Latin-ASCII") %>%
        tolower() %>%
        gsub("-", "", .) %>% #On créé une nouvelle colonne avec les noms 22 "nettoyés"
        gsub("’", "", .) %>%
        gsub(" ", "", .) %>%
        gsub("'", "", .),
      
      Libellé_commune_17_tr = stri_trans_general(Libellé_commune, "Latin-ASCII") %>%
        tolower() %>%
        gsub("-", "", .) %>% #On créé une nouvelle colonne avec les noms 17 "nettoyés"
        gsub("’", "", .) %>%
        gsub(" ", "", .) %>%
        gsub("'", "", .),
      
      Dist = stringdist(Libellé_commune, Libellé_commune_22, method = "lv"),
      
      Libellé_commune = case_when(Libellé_commune_17_tr == Libellé_commune_22_tr #Si les noms nettoyés coincident
                                  & Libellé_commune_22 != Libellé_commune #Et qu'ils sont "différents" à la base
                                  &!is.na(Libellé_commune_22) ~ Libellé_commune_22, #On remplace par le nom 2022
                                  Dist %in% c(1:3,5,7) ~ Libellé_commune_22, #S'il y entre 1 et 3 caractères de différents (plus deux avec 5 et 7)
                                  DepCom %in% c("06128", "08165", "12133", "02333", "22183","88465", "14689", "17377", "24177", "26291", "31479", "31492", "35306", "38206", "38216", "38348", "41110", "41193", "41222", "47176", "63343", "64293", "65024", "67539", "70472", "71401", "73229", "73269", "74014", "74162", "76087", "79066", "84033", "88413", "93070", "95306")~ Libellé_commune_22,
                                  TRUE ~ Libellé_commune), #Sinon, on garde le nom de base
      
      Libellé_département = case_when(Libellé_commune == "Vallons-de-l'Erdre" ~ "Loire-Atlantique",
                                      Libellé_commune == "Tessy-Bocage" ~ "Manche",
                                      TRUE~Libellé_département)
    ) %>%	
    select(-c(23:29)) #On enlève les colonnes que l'on vient de créer
  
  
  
  # On a maintenant des noms de communes fusionnées qui ont plusieurs lignes (une pour chaque commune qui a fusionné)
  # Il nous faut les regrouper en additionnant leurs valeurs
  
  
  Pres17T1_fusion <- Pres17T1_fusion %>%
    group_by(DepCom, Libellé_commune, Code_commune, Code_département, Libellé_département) %>%
    summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)), .groups = "drop")
  
  
  Pres22T1[Pres22T1$DepCom %in% c("14712","14666"), "Libellé_commune"] <- c("Saline","Saline")
  Pres22T1[Pres22T1$DepCom %in% c("14712","14666", "27058"), "Code_commune"] <- c("712","712","676")
  Pres22T1[Pres22T1$DepCom %in% c("14712","14666", "27058"), "Code_département"] <- c("14", "14", "27")
  Pres22T1[Pres22T1$DepCom %in% c("14712","14666", "27058"), "DepCom"] <- c("14712", "14712", "27676")
  
  
  Pres22T1 <- Pres22T1 %>%
    group_by(DepCom, Libellé_commune, Code_commune, Code_département, Libellé_département) %>%
    summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)), .groups = "drop")
  
  # # Les communes de 2017 dont le code est en 2022 mais avec un mauvais libellé
  # 
  # View(
  #   merge(sort=FALSE,
  #     Pres17T1_fusion[Pres17T1_fusion$DepCom %in% Pres22T1$DepCom, c("DepCom", "Libellé_commune")],
  #     Pres22T1[, c("DepCom", "Libellé_commune")],
  #     by = "DepCom",
  #     suffixes = c("_17", "_22")
  #   ) %>%
  #     filter(Libellé_commune_17 != Libellé_commune_22)
  # )
  # 
  # # Les communes de 2017 dont ni le code ni le libellé n'est en 2022
  # 
  # View(
  #   Pres17T1_fusion %>%
  #     filter(
  #       !DepCom %in% Pres22T1$DepCom &
  #         !Libellé_commune %in% Pres22T1$Libellé_commune
  #     )
  # )
  # 
  # 
  # 
  # # Les communes de 2017 dont le code département et le libellé est en 2022 mais avec un mauvais code commune
  # 
  # View(
  #   merge(sort=FALSE,
  #     Pres17T1_fusion[, c("DepCom", "Code_département", "Libellé_commune")],
  #     Pres22T1[, c("DepCom", "Code_département", "Libellé_commune")],
  #     by = c("Code_département", "Libellé_commune"),
  #     suffixes = c("_17", "_22")
  #   ) %>%
  #     filter(DepCom_17 != DepCom_22)
  # )
  # 
  # # Les communes de 2022 dont ni le code ni le libellé n'est en 2017
  # 
  # View(
  #   Pres22T1 %>%
  #     filter(
  #       !DepCom %in% Pres17T1_fusion$DepCom &
  #         !Libellé_commune %in% Pres17T1_fusion$Libellé_commune
  #     )
  # )
  
  #On peut maintenant fusionner les bases :
  
  Pres <- Pres17T1_fusion %>%
    select(c(starts_with("Nb_17"),"DepCom")) %>%
    inner_join(Pres22T1, ., by = "DepCom") #inner_join va enlever les lignes qui n'ont pas de valeurs pour 2022
  
  write.csv(Pres, "01_data/Présidentielles 2017-2022/Présidentielles_fusionnées.csv", row.names = FALSE)} else {Pres <- read.csv("01_data/Présidentielles 2017-2022/Présidentielles_fusionnées.csv")}



# RECENSEMENT 2020 ---- 

  ## Fichiers activité ----
  
  census20_act2a <- read_xlsx(
    here("01_data/census20",
         "TD_ACT2A_2020.xlsx"), 
    skip = 10)%>%
    mutate(
      LIBGEO = if_else(CODGEO %in% c("14712","14666"), "Saline", LIBGEO),# On modifie les libellés mais aussi les différents codes
      CODGEO = case_when(CODGEO %in% c("14712","14666") ~ "14712",
                         CODGEO =="27676" ~ "27058",
                         TRUE ~ CODGEO)) %>%
    group_by(CODGEO, LIBGEO) %>%
    summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)), .groups = "drop")
  
  census20_act2b <- read_xlsx(
    here("01_data/census20",
         "TD_ACT2B_2020.xlsx"), 
    skip = 10)%>%
    mutate(
      LIBGEO = if_else(CODGEO %in% c("14712","14666"), "Saline", LIBGEO),# On modifie les libellés mais aussi les différents codes
      CODGEO = case_when(CODGEO %in% c("14712","14666") ~ "14712",
                         CODGEO =="27676" ~ "27058",
                         TRUE ~ CODGEO)) %>%
    group_by(CODGEO, LIBGEO) %>%
    summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)), .groups = "drop")
  
  census20_act3 <- read_xlsx(
    here("01_data/census20",
         "TD_ACT3_2020.xlsx"), 
    skip = 10)%>%
    mutate(
      LIBGEO = if_else(CODGEO %in% c("14712","14666"), "Saline", LIBGEO),# On modifie les libellés mais aussi les différents codes
      CODGEO = case_when(CODGEO %in% c("14712","14666") ~ "14712",
                         CODGEO =="27676" ~ "27058",
                         TRUE ~ CODGEO)) %>%
    group_by(CODGEO, LIBGEO) %>%
    summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)), .groups = "drop")
  
  census20_act4 <- read_xlsx(
    here("01_data/census20",
         "TD_ACT4_2020.xlsx"), 
    skip = 10)%>%
    mutate(
      LIBGEO = if_else(CODGEO %in% c("14712","14666"), "Saline", LIBGEO),# On modifie les libellés mais aussi les différents codes
      CODGEO = case_when(CODGEO %in% c("14712","14666") ~ "14712",
                         CODGEO =="27676" ~ "27058",
                         TRUE ~ CODGEO)) %>%
    group_by(CODGEO, LIBGEO) %>%
    summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)), .groups = "drop")
  
  ## Fichiers emploi ----
  
  census20_emp1 <- read_xlsx(
    here("01_data/census20",
         "TD_EMP1_2020.xlsx"), 
    skip = 10)%>%
    mutate(
      LIBGEO = if_else(CODGEO %in% c("14712","14666"), "Saline", LIBGEO),# On modifie les libellés mais aussi les différents codes
      CODGEO = case_when(CODGEO %in% c("14712","14666") ~ "14712",
                         CODGEO =="27676" ~ "27058",
                         TRUE ~ CODGEO)) %>%
    group_by(CODGEO, LIBGEO) %>%
    summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)), .groups = "drop")
  
  census20_emp3 <- read_xlsx(
    here("01_data/census20",
         "TD_EMP3_2020.xlsx"), 
    skip = 10)%>%
    mutate(
      LIBGEO = if_else(CODGEO %in% c("14712","14666"), "Saline", LIBGEO),# On modifie les libellés mais aussi les différents codes
      CODGEO = case_when(CODGEO %in% c("14712","14666") ~ "14712",
                         CODGEO =="27676" ~ "27058",
                         TRUE ~ CODGEO)) %>%
    group_by(CODGEO, LIBGEO) %>%
    summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)), .groups = "drop")
  
  census20_emp4 <- read_xlsx(
    here("01_data/census20",
         "TD_EMP4_2020.xlsx"), 
    skip = 10)%>%
    mutate(
      LIBGEO = if_else(CODGEO %in% c("14712","14666"), "Saline", LIBGEO),# On modifie les libellés mais aussi les différents codes
      CODGEO = case_when(CODGEO %in% c("14712","14666") ~ "14712",
                         CODGEO =="27676" ~ "27058",
                         TRUE ~ CODGEO)) %>%
    group_by(CODGEO, LIBGEO) %>%
    summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)), .groups = "drop")
  
  ## Fichiers formation ----
  
  census20_for2 <- read_xlsx(
    here("01_data/census20",
         "TD_FOR2_2020.xlsx"), 
    skip = 10)%>%
    mutate(
      LIBGEO = if_else(CODGEO %in% c("14712","14666"), "Saline", LIBGEO),# On modifie les libellés mais aussi les différents codes
      CODGEO = case_when(CODGEO %in% c("14712","14666") ~ "14712",
                         CODGEO =="27676" ~ "27058",
                         TRUE ~ CODGEO)) %>%
    group_by(CODGEO, LIBGEO) %>%
    summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)), .groups = "drop")
  
  ## Fichiers logement ----
  
  census20_log1 <- read_xlsx(
    here("01_data/census20",
         "TD_LOG1_2020.xlsx"), 
    skip = 10)%>%
    mutate(
      LIBGEO = if_else(CODGEO %in% c("14712","14666"), "Saline", LIBGEO),# On modifie les libellés mais aussi les différents codes
      CODGEO = case_when(CODGEO %in% c("14712","14666") ~ "14712",
                         CODGEO =="27676" ~ "27058",
                         TRUE ~ CODGEO)) %>%
    group_by(CODGEO, LIBGEO) %>%
    summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)), .groups = "drop")
  
  ## Fichiers immigration / nationalité ----
  
  census20_img1A <- read_xlsx(
    here("01_data/census20",
         "TD_IMG1A_2020.xlsx"), 
    skip = 10)%>%
    mutate(
      LIBGEO = if_else(CODGEO %in% c("14712","14666"), "Saline", LIBGEO),# On modifie les libellés mais aussi les différents codes
      CODGEO = case_when(CODGEO %in% c("14712","14666") ~ "14712",
                         CODGEO =="27676" ~ "27058",
                         TRUE ~ CODGEO)) %>%
    group_by(CODGEO, LIBGEO) %>%
    summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)), .groups = "drop")
  
  census20_nat1 <- read_xlsx(
    here("01_data/census20",
         "TD_NAT1_2020.xlsx"), 
    skip = 10)%>%
    mutate(
      LIBGEO = if_else(CODGEO %in% c("14712","14666"), "Saline", LIBGEO),# On modifie les libellés mais aussi les différents codes
      CODGEO = case_when(CODGEO %in% c("14712","14666") ~ "14712",
                         CODGEO =="27676" ~ "27058",
                         TRUE ~ CODGEO)) %>%
    group_by(CODGEO, LIBGEO) %>%
    summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)), .groups = "drop")
  
  ## Fichiers population ----
  
  census20_pop1B <- read_xlsx(
    here("01_data/census20",
         "TD_POP1B_2020.xlsx"), 
    skip = 10)%>%
    mutate(
      LIBGEO = if_else(CODGEO %in% c("14712","14666"), "Saline", LIBGEO),# On modifie les libellés mais aussi les différents codes
      CODGEO = case_when(CODGEO %in% c("14712","14666") ~ "14712",
                         CODGEO =="27676" ~ "27058",
                         TRUE ~ CODGEO)) %>%
    group_by(CODGEO, LIBGEO) %>%
    summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)), .groups = "drop")
  
  ## Fichier densité urbaine ----
  
  densite_communes <- read_xlsx(
    here("01_data/densite_communes",
         "grille_densite_7_niveaux_2024.xlsx"), 
    skip = 4) %>%
    mutate(
      LIBGEO = if_else(CODGEO %in% c("14712","14666"), "Saline", LIBGEO),# On modifie les libellés mais aussi les différents codes
      CODGEO = case_when(CODGEO %in% c("14712","14666") ~ "14712",
                         CODGEO =="27676" ~ "27058",
                         TRUE ~ CODGEO),
      P1 = P1*PMUN21/100,
      P2 = P2*PMUN21/100,
      P3 = P3*PMUN21/100,
      P4 = P4*PMUN21/100,
      P5 = P5*PMUN21/100,
      P6 = P6*PMUN21/100,
      P7 = P7*PMUN21/100) %>%
    group_by(CODGEO, LIBGEO, LIBDENS, DENS) %>%
    summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)), .groups = "drop")
  

  # Fusion Recensement ----
  
  Pres22T1$DepCom <- case_when(
    str_detect(Pres22T1$Code_département, "^97") ~ paste0("97",str_pad(as.numeric(Pres22T1$Code_commune), width = 2, pad = "0")),
    TRUE ~ paste0(Pres22T1$Code_département,Pres22T1$Code_commune))
  
  Pres17T1$DepCom %<>% str_pad(width = 5, pad = "0")
  Pres22T1$DepCom %<>% str_pad(width = 5, pad = "0")
  

  
  source("02_scripts/02_recode_census20.R", echo = FALSE )
  
  # On enlève les communes vides
  
  communes <- communes[!communes$codgeo %in% c("55039","55050","55139","55189","55239","55307"),] #Villes commémoratives sans habitants
  
  communes %<>% mutate(
    DepCom = case_when(
      str_detect(codgeo, "^97") ~ paste0("97", str_sub(codgeo, 3, 3), str_sub(codgeo, 3, 5)),
      TRUE ~ codgeo))
  
  
  PresF<-merge(Pres,
               communes[,!(names(communes) %in% c("libgeo", "codgeo"))],
               by.x="DepCom",
               by.y="DepCom",
               all.x=TRUE)
  

  # Nettoyage ----
  
  rm(list = c(ls(pattern = "^ear"), ls(pattern = "^census"), "densite_communes"), i,x,Date_elections_Début, Date_elections_Fin, Communes_nouvelles, Pres17T1_fusion, communes, Pres, Pres17T1, Pres22T1)
  
  write.csv(PresF, "01_data/Présidentielles 2017-2022/Présidentielles&Recensement.csv", row.names = FALSE)} else {PresF <- read.csv("01_data/Présidentielles 2017-2022/Présidentielles&Recensement.csv")}

# DONNEES GEOLOC ----

# france_sf <- 
#   st_read(here(
#     "01_data/geoloc", 
#     "fr-esr-referentiel-geographique.shp")) %>% 
#   filter(regrgp_nom != "DROM-COM")
