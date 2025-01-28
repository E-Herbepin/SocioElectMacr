#### IMPORT DES DONNEES ----
# Téléchargement des données ----

# Présidentielles 2017
# Récupérées le 25/01 sur : https://www.data.gouv.fr/fr/datasets/election-presidentielle-des-23-avril-et-7-mai-2017-resultats-definitifs-du-1er-tour-par-communes/


if(!file.exists("01_data/Pres17T1.xls")) { #Si le fichier n'est pas trouvé
    download.file( #On le télécharge
      url = paste0("https://www.data.gouv.fr/fr/datasets/r/77ed6b2f-c48f-4037-8479-50af74fa5c7a"),
      destfile = paste0("01_data/Pres17T1.xls"),
      mode = "wb")}

# Présidentielles 2022
# Récupérées le 25/01 sur : https://www.data.gouv.fr/fr/datasets/resultats-du-premier-tour-de-lelection-presidentielle-2022-par-commune-et-par-departement/

if(!file.exists("01_data/Pres22T1.csv")) {
download.file(
      url = paste0("https://www.data.gouv.fr/fr/datasets/r/54782507-e795-4f9d-aa70-ed06feba22e3"),
      destfile = paste0("01_data/Pres22T1.csv"),
      mode = "wb")}

# Nouvelles communes et fusions
# Récupérées le 25/01 sur : https://www.insee.fr/fr/statistiques/fichier/2549968/

if(!dir.exists("01_data/communes_fusionnees")){
  dir.create("01_data/communes_fusionnees", showWarnings = FALSE)
  for (x in 2017:2022) {
    download.file(
      url = paste0("https://www.insee.fr/fr/statistiques/fichier/2549968/Communes_nouvelles_", x , if_else(x<=2019,".xls",".xlsx")),
      destfile = paste0("01_data/communes_fusionnees/Communes_nouvelles_",x,if_else(x<=2019,".xls",".xlsx")),
      mode = "wb"
    )
  }
}

## Election présidentielle 2017 ----

#/!\ Ne faire tourner qu'une fois pour créer le fichier .CSV, puis ne lancer que le read.csv lorsque nécessaire

if(!file.exists("01_data/Pres17T1_recodé.csv")) {
  
Pres17T1 <- read_xls(
  here(
    "01_data",
    "Pres17T1.xls"
  ),
  skip = 3
)

for (i in c(19+c(0:10)*7)) {#Pour chaque "panneau" (i= le numéro de la colonne)
  for (x in 1:nrow(Pres17T1)) {#Pour chaque commune
    Pres17T1[x,paste0("Nb_17_Voix_",str_to_title(as.character(Pres17T1[x,i+2])))] <- Pres17T1[x,i+4] #la case de la commune dans la colonne "NomDuCandidat_Voix" prend la valeur de la colonne "Voix" qui suit le panneau qu'on regarde
  }}
rm(i,x)
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

write.csv(Pres17T1, "01_data/Pres17T1_recodé.csv", row.names = FALSE) # On exporte en CSV (plus pratique pour la suite)
} else {Pres17T1 <- read.csv("01_data/Pres17T1_recodé.csv")}

# On modifie le code commune pour simplifier les manipulations :
Pres17T1$Code_commune %<>% str_pad(width = 3, pad = "0")

## Election présidentielle 2022 ----

if(!file.exists("01_data/Pres22T1_recodé.csv")) {
 
Pres22T1 <- read_delim(
  here(
    "01_data",
    "Pres22T1.csv"
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

write.csv(Pres22T1, "01_data/Pres22T1_recodé.csv", row.names = FALSE) # On exporte en CSV (plus pratique pour la suite)
} else {Pres22T1 <- read.csv("01_data/Pres22T1_recodé.csv")}

Pres22T1$Code_commune %<>% str_pad(width = 3, pad = "0")

## Communes fusionnées ----

if(!file.exists("01_data/nouvelles_communes.csv")) {
Date_elections_Début <- dmy("23/01/2017", tz = "Europe/Paris") # 3 mois avant le premier tour
Date_elections_Fin <- dmy("04/03/2022", tz = "Europe/Paris") # date de cloture des inscriptions sur les listes électorales

nouvelles_communes <- bind_rows(
  read_xls(
    here("01_data/communes_fusionnees",
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
    here("01_data/communes_fusionnees",
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
    here("01_data/communes_fusionnees",
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
    here("01_data/communes_fusionnees",
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
    here("01_data/communes_fusionnees",
         "Communes_nouvelles_2021.xlsx"),
    sheet = 1) %>%
    mutate(
      DepComN = as.numeric(DepComN),
      DepComA = as.numeric(DepComA),
      Date = if_else(is.na(Date2), 
                     dmy("01/01/2022", tz = "Europe/Paris"), 
                     ymd(Date2, tz = "Europe/Paris"))) %>%
    select(-c(5:9)), 
  
  commmune_fusionnees_2022 <- read_xlsx(
    here("01_data/communes_fusionnees",
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

write.csv(nouvelles_communes, "01_data/nouvelles_communes.csv")

rm(Date_elections_Début, Date_elections_Fin)} else {nouvelles_communes <- read.csv("01_data/nouvelles_communes.csv")}

# Fusion des bases ----

## Présidentielles ----

#On ajoute un identifiant unique pour les communes 
Pres17T1$DepCom <- paste0(as.character(Pres17T1$Code_département),Pres17T1$Code_commune)
Pres22T1$DepCom <- paste0(Pres22T1$Code_département,Pres22T1$Code_commune)

# Les communes qui ne sont plus dans le fichier de 2022 (notamment les communes de l'étranger qu'on a enlevé)
#setdiff(Pres17T1$Libellé_commune, Pres22T1$Libellé_commune)

# Les communes qui ne sont pas dans le fichier de 2017 (notamment, les arrondissements des grandes villes qu'on a enlevé)
#setdiff(Pres22T1$Libellé_commune, Pres17T1$Libellé_commune)


#sum(Pres22T1$Code_commune %in% Pres17T1$Code_commune)
# L'ensemble des codes communes de 2022 sont dans ceux de 2017 : On a donc uniquement affaire à des fusions

# Parmis les commmunes qui sont "apparus" entre 2017 et 2022, on regarde celles qui ne sont pas dans le fichier des nouvelles communes
# setdiff(paste0(Pres22T1$Code_département[!(Pres22T1$Libellé_commune %in% Pres17T1$Libellé_commune)],
#                Pres22T1$Code_commune[!(Pres22T1$Libellé_commune %in% Pres17T1$Libellé_commune)]), 
#         nouvelles_communes$DepComN)

# il y a 97 communes qui ont apparu entre 2017 et 2022 et qui ne sont pas dans le fichier des nouvelles communes
# Par exemple :

# Pres22T1$Libellé_commune[Pres17T1$DepCom == "2224"]

# Pres17T1[Pres17T1$Libellé_commune == "Courcelles-sur-Vesle",]
#Cette commune n'existe pas en 2017

# nouvelles_communes[nouvelles_communes$NomCN == "Courcelles-sur-Vesle"| nouvelles_communes$NomCA == "Courcelles-sur-Vesle"| nouvelles_communes$DepComN =="2224"| nouvelles_communes$DepComA =="2224",]
# Elle n'existe pas non plus dans le fichier des nouvelles communes

# Les communes qui ont disparu et qu'on ne retrouve pas dans les nouvelles communes : 
# Pres17T1[!(Pres17T1$DepCom %in% Pres22T1$DepCom) & #Les communes qui ont disparus
           # !(Pres17T1$DepCom %in% nouvelles_communes$DepComA),] #Et qui ne sont pas dans les communes qui ont fusionné
# Il n'y en a que 90
# /!\Ou alors elles ont changé de département
# A vérifier à partir de https://www.insee.fr/fr/information/7671844

# On remplace celles qu'on a par leurs nouveaux noms

Pres17T1_fusion <- Pres17T1

Pres17T1_fusion$Libellé_commune[!(Pres17T1_fusion$DepCom %in% Pres22T1$DepCom) & #Les communes qui ont disparus
                                  Pres17T1_fusion$DepCom %in% nouvelles_communes$DepComA] <- #Et qui sont dans les communes qui ont fusionné
  nouvelles_communes$NomCN[match(Pres17T1_fusion$DepCom[!(Pres17T1_fusion$DepCom %in% Pres22T1$DepCom) & #Prennent le nom associé
                                                          Pres17T1_fusion$DepCom %in% nouvelles_communes$DepComA],
                                 nouvelles_communes$DepComA)]

# Existe il des communes qui ont disparu mais dont le code a été réattribué à une autre commune ?
# Non : d’après l’INSEE les codes des communes qui disparaissent ne sont jamais réutilisés

#On modifie aussi le code des communes 
Pres17T1_fusion$Code_commune[!(Pres17T1_fusion$DepCom %in% Pres22T1$DepCom) &
  Pres17T1_fusion$DepCom %in% nouvelles_communes$DepComA] <- 
  nouvelles_communes$DepComN[match(Pres17T1$DepCom[!(Pres17T1_fusion$DepCom %in% Pres22T1$DepCom) & #Prennent le nom associé
                                                     Pres17T1_fusion$DepCom %in% nouvelles_communes$DepComA],
                                 nouvelles_communes$DepComA)] %>% 
  str_sub(-3)


# Et celui des départements
Pres17T1_fusion$Code_département[!(Pres17T1_fusion$DepCom %in% Pres22T1$DepCom) &
                                   Pres17T1_fusion$DepCom %in% nouvelles_communes$DepComA] <- 
  nouvelles_communes$DepComN[match(Pres17T1$DepCom[!(Pres17T1_fusion$DepCom %in% Pres22T1$DepCom) & #Prennent le nom associé
                                                     Pres17T1_fusion$DepCom %in% nouvelles_communes$DepComA],
                                   nouvelles_communes$DepComA)] %>% 
  str_sub(end=2)


Pres17T1_fusion[Pres17T1_fusion$DepCom == 	
                  27676,"Code_commune"] <- "058"

# Enfin on reproduit l'identifiant
Pres17T1_fusion$DepCom <- paste0(Pres17T1_fusion$Code_département,Pres17T1_fusion$Code_commune)

# Si on regarde à nouveau les communes dont le nom est encore dans la colonne "ancien nom" : ce sont les communes qui ont fusionné avec d'autres mais ont gardé leurs noms
# Pres17T1_fusion$Libellé_commune[paste0(Pres17T1_fusion$Code_département,Pres17T1_fusion$Code_commune) %in% nouvelles_communes$DepComA]

# On a maintenant des noms de communes fusionnées qui ont plusieurs lignes (une pour chaque commune qui a fusionné)
# Il nous faut les regrouper en additionnant leurs valeurs

Pres17T1_fusion <- Pres17T1_fusion %>%
  group_by(DepCom, Libellé_commune, Code_commune, Code_département, Libellé_département) %>%
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)), .groups = "drop")

# 1 ville est apparue sans raison apparente : Sannerville : elle est le résultat de la séparation de la ville de Saline en deux (Troarn et Sannerville) 
# En 2022, Sannerville comptait 1 884 hab, Troarn 3 442. On peut donc attribuer les voix de Saline à Troarn et Sannerville proportionnellement à leur population sauf présumer d'une différence idéologique

# On ajoute les nouvelles communes à la base de 2017
# 
# Pres17T1_fusion <- bind_rows(Pres17T1_fusion, 
#                              data.frame(
#                                DepCom = c("14712", "14666"),
#                                Libellé_commune = c("Troarn", "Sannerville"),
#                                Code_commune = c("712", "666"),
#                                Code_département = c("14", "14"),
#                                Libellé_département =c("Calvados", "Calvados"),
#                                Nb_17_Inscrits = as.integer(round(c(Pres17T1_fusion[Pres17T1_fusion$Libellé_commune == "Saline","Nb_17_Inscrits"]*3442/(1884+3442),Pres17T1_fusion[Pres17T1_fusion$Libellé_commune == "Saline","Nb_17_Inscrits"]*1884/(1884+3442)), digit=0)),
#                                Nb_17_Abstentions = as.integer(round(c(Pres17T1_fusion[Pres17T1_fusion$Libellé_commune == "Saline","Nb_17_Abstentions"]*3442/(1884+3442),Pres17T1_fusion[Pres17T1_fusion$Libellé_commune == "Saline","Nb_17_Abstentions"]*1884/(1884+3442)), digit=0)),
#                                Nb_17_Votants = as.integer(round(c(Pres17T1_fusion[Pres17T1_fusion$Libellé_commune == "Saline","Nb_17_Votants"]*3442/(1884+3442),Pres17T1_fusion[Pres17T1_fusion$Libellé_commune == "Saline","Nb_17_Votants"]*1884/(1884+3442)), digit=0)),
#                                Nb_17_Blancs = as.integer(round(c(Pres17T1_fusion[Pres17T1_fusion$Libellé_commune == "Saline","Nb_17_Blancs"]*3442/(1884+3442),Pres17T1_fusion[Pres17T1_fusion$Libellé_commune == "Saline","Nb_17_Blancs"]*1884/(1884+3442)), digit=0)),
#                                Nb_17_Nuls = as.integer(round(c(Pres17T1_fusion[Pres17T1_fusion$Libellé_commune == "Saline","Nb_17_Nuls"]*3442/(1884+3442),Pres17T1_fusion[Pres17T1_fusion$Libellé_commune == "Saline","Nb_17_Nuls"]*1884/(1884+3442)), digit=0)),
#                                Nb_17_Exprimés = as.integer(round(c(Pres17T1_fusion[Pres17T1_fusion$Libellé_commune == "Saline","Nb_17_Exprimés"]*3442/(1884+3442),Pres17T1_fusion[Pres17T1_fusion$Libellé_commune == "Saline","Nb_17_Exprimés"]*1884/(1884+3442)), digit=0)),
#                                Nb_17_Voix_Le_Pen = as.integer(round(c(Pres17T1_fusion[Pres17T1_fusion$Libellé_commune == "Saline","Nb_17_Voix_Le_Pen"]*3442/(1884+3442),Pres17T1_fusion[Pres17T1_fusion$Libellé_commune == "Saline","Nb_17_Voix_Le_Pen"]*1884/(1884+3442)), digit=0)),
#                                Nb_17_Voix_Macron = as.integer(round(c(Pres17T1_fusion[Pres17T1_fusion$Libellé_commune == "Saline","Nb_17_Voix_Macron"]*3442/(1884+3442),Pres17T1_fusion[Pres17T1_fusion$Libellé_commune == "Saline","Nb_17_Voix_Macron"]*1884/(1884+3442)), digit=0)),
#                                Nb_17_Voix_Mélenchon = as.integer(round(c(Pres17T1_fusion[Pres17T1_fusion$Libellé_commune == "Saline","Nb_17_Voix_Mélenchon"]*3442/(1884+3442),Pres17T1_fusion[Pres17T1_fusion$Libellé_commune == "Saline","Nb_17_Voix_Mélenchon"]*1884/(1884+3442)), digit=0)),
#                                Nb_17_Voix_Fillon = as.integer(round(c(Pres17T1_fusion[Pres17T1_fusion$Libellé_commune == "Saline","Nb_17_Voix_Fillon"]*3442/(1884+3442),Pres17T1_fusion[Pres17T1_fusion$Libellé_commune == "Saline","Nb_17_Voix_Fillon"]*1884/(1884+3442)), digit=0)),
#                                Nb_17_Voix_Lassalle = as.integer(round(c(Pres17T1_fusion[Pres17T1_fusion$Libellé_commune == "Saline","Nb_17_Voix_Lassalle"]*3442/(1884+3442),Pres17T1_fusion[Pres17T1_fusion$Libellé_commune == "Saline","Nb_17_Voix_Lassalle"]*1884/(1884+3442)), digit=0)),
#                                Nb_17_Voix_Hammon = as.integer(round(c(Pres17T1_fusion[Pres17T1_fusion$Libellé_commune == "Saline","Nb_17_Voix_Hamon"]*3442/(1884+3442),Pres17T1_fusion[Pres17T1_fusion$Libellé_commune == "Saline","Nb_17_Voix_Hamon"]*1884/(1884+3442)), digit=0)),
#                                Nb_17_Voix_Asselineau = as.integer(round(c(Pres17T1_fusion[Pres17T1_fusion$Libellé_commune == "Saline","Nb_17_Voix_Asselineau"]*3442/(1884+3442),Pres17T1_fusion[Pres17T1_fusion$Libellé_commune == "Saline","Nb_17_Voix_Asselineau"]*1884/(1884+3442)), digit=0)),
#                                Nb_17_Voix_Poutou = as.integer(round(c(Pres17T1_fusion[Pres17T1_fusion$Libellé_commune == "Saline","Nb_17_Voix_Poutou"]*3442/(1884+3442),Pres17T1_fusion[Pres17T1_fusion$Libellé_commune == "Saline","Nb_17_Voix_Poutou"]*1884/(1884+3442)), digit=0)),
#                                Nb_17_Voix_Dupont.Aignan = as.integer(round(c(Pres17T1_fusion[Pres17T1_fusion$Libellé_commune == "Saline","Nb_17_Voix_Dupont.Aignan"]*3442/(1884+3442),Pres17T1_fusion[Pres17T1_fusion$Libellé_commune == "Saline","Nb_17_Voix_Dupont.Aignan"]*1884/(1884+3442)), digit=0)),
#                                Nb_17_Voix_Arthaud = as.integer(round(c(Pres17T1_fusion[Pres17T1_fusion$Libellé_commune == "Saline","Nb_17_Voix_Arthaud"]*3442/(1884+3442),Pres17T1_fusion[Pres17T1_fusion$Libellé_commune == "Saline","Nb_17_Voix_Arthaud"]*1884/(1884+3442)), digit=0)),
#                                Nb_17_Voix_Cheminade = as.integer(round(c(Pres17T1_fusion[Pres17T1_fusion$Libellé_commune == "Saline","Nb_17_Voix_Cheminade"]*3442/(1884+3442),Pres17T1_fusion[Pres17T1_fusion$Libellé_commune == "Saline","Nb_17_Voix_Cheminade"]*1884/(1884+3442)), digit=0))
#                              ))

# Pres17T1_fusion<-Pres17T1_fusion[Pres17T1_fusion$Libellé_commune != "Saline",]

#On vérifie que toute les communes ont fusionné en comptant les identifiant qui réapparaissent : 
# Pres17T1_fusion$DepCom[duplicated(Pres17T1_fusion$DepCom)] #Il n'y en a pas

# Pres17T1_fusion$Libellé_commune[Pres17T1_fusion$DepCom[!(Pres17T1_fusion$DepCom %in% Pres22T1$DepCom)] %in% nouvelles_communes$NomCA]
# Il n'y a pas de communes manquante en 2022, pourtant dans le fichier des nouvelles communes, qui n'ai pas été renommée

# View(Pres17T1_fusion[Pres17T1_fusion$DepCom %in% setdiff(Pres17T1_fusion$DepCom, Pres22T1$DepCom),])
#Il reste quelques communes (17) qui sont présentes en 2017 mais pas en 2022, Peut être à voir au cas par cas

#On peut maintenant fusionner les bases :

Pres <- Pres17T1_fusion %>%
  select(c(starts_with("Nb_17"),"DepCom")) %>%
  inner_join(Pres22T1, ., by = "DepCom") #inner_join va enlever les lignes qui n'ont pas de valeurs pour 2022

rm(nouvelles_communes, Pres17T1_fusion)
## Recensement ----

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

Pres$DepCom %<>% str_pad(width=5, pad ="0")

t <- merge(Pres, communes, by.x = "DepCom", by.y= "codgeo", all.x=FALSE)

#pour comprendre pourquoi il y a trop de lignes dans t
t2 <- table(t$DepCom)
df <- as.data.frame(t2)
df2 <- filter(df, df$Freq>1)
double <- merge(t, df2, by.x ="DepCom", by.y="Var1",all.x=FALSE)
