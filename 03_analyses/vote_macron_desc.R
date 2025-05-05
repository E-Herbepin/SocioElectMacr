#### Préparation des données.

### On crée une variable pour les voix au premier tour.

## Macron

# Parmi les exprimés en 2017

PresF <- PresF %>%
  mutate(pourc_macron_17 = Nb_17_Voix_Macron * 100 / Nb_17_Exprimés)

# Parmi les inscrits en 2017

PresF <- PresF %>%
  mutate(pourc_macron_17_inscr = Nb_17_Voix_Macron * 100 / Nb_17_Inscrits)

# Parmi les exprimés en 2022

PresF <- PresF %>%
  mutate(pourc_macron_22 = Nb_22_Voix_Macron * 100 / Nb_22_Exprimés)

# Parmi les incrits en 2022

PresF <- PresF %>%
  mutate(pourc_macron_22_inscr = Nb_22_Voix_Macron * 100 / Nb_22_Inscrits)

## Le Pen

# Parmi les exprimés en 2017

PresF <- PresF %>%
  mutate(pourc_lepen_17 = Nb_17_Voix_Le_Pen * 100 / Nb_17_Exprimés)

# Parmi les exprimés en 2022

PresF <- PresF %>%
  mutate(pourc_lepen_22 = Nb_22_Voix_Le_Pen * 100 / Nb_22_Exprimés)

## Républicains (Fillon et Pécresse)

# Parmi les exprimés en 2017

PresF <- PresF %>%
  mutate(pourc_repu_17 = Nb_17_Voix_Fillon * 100 / Nb_17_Exprimés)

# Parmi les exprimés en 2022

PresF <- PresF %>%
  mutate(pourc_repu_22 = Nb_22_Voix_Pécresse * 100 / Nb_22_Exprimés)

## Mélenchon

# Parmi les exprimés en 2017

PresF <- PresF %>%
  mutate(pourc_melenchon_17 = Nb_17_Voix_Mélenchon * 100 / Nb_17_Exprimés)

# Parmi les exprimés en 2022

PresF <- PresF %>%
  mutate(pourc_melenchon_22 = Nb_22_Voix_Mélenchon * 100 / Nb_22_Exprimés)

### On crée une variable pour suivre l'évolution du vote Macron.

## Parmi les exprimés.

PresF <- PresF %>%
  mutate(diff_pourc_macron = pourc_macron_22 - pourc_macron_17)

## Parmi les inscrits.

PresF <- PresF %>% 
  mutate(diff_pourc_macron_inscr = pourc_macron_22_inscr - pourc_macron_17_inscr)

#### Analyses

### Histogramme du vote Macron en 2017.

# On définit l'ordre de libdens.
ordre_libdens <- c(
  "Grands centres urbains",
  "Centres urbains intermédiaires",
  "Ceintures urbaines",
  "Petites villes",
  "Bourgs ruraux",
  "Rural à habitat dispersé",
  "Rural à habitat très dispersé"
)

# Préparation des données
PresF_clean <- PresF %>%
  filter(!is.na(libdens), !is.na(pourc_macron_22)) %>%
  mutate(libdens = factor(libdens, levels = ordre_libdens))

# Moyenne par densité
moyennes <- PresF_clean %>%
  group_by(libdens) %>%
  summarise(moy_pourc_macron = mean(pourc_macron_22, na.rm = TRUE)) %>%
  mutate(vjust_label = ifelse(libdens == "Rural à habitat très dispersé", -0.8, -1.2))

# Moyenne globale
moyenne_globale <- mean(PresF_clean$pourc_macron_22, na.rm = TRUE)

# Valeur max pour ajuster l'échelle
val_max <- max(moyennes$moy_pourc_macron)

# Création du graphique
ggplot(moyennes, aes(x = libdens, y = moy_pourc_macron)) +
  geom_col(width = 0.7, fill = "steelblue") +
  geom_text(aes(label = round(moy_pourc_macron, 1), vjust = vjust_label)) +
  geom_hline(yintercept = moyenne_globale, color = "red", linetype = "dashed", size = 1) +
  annotate("text", x = 7.5, y = moyenne_globale + 2, 
           label = paste0("Moyenne nationale : ", round(moyenne_globale, 1), "%"), 
           color = "red", hjust = 1, size = 3.5) +
  labs(
    title = "Moyenne du vote Macron au premier tour des élections présidentielles de 2017 en fonction du degré de densité",
    x = "Degré de densité",
    y = "Moyenne du vote Macron parmi les suffrages exprimés (en %)",
    caption = "Source : Résultats des élections présidentielles par commune, 2017 et 2022 ; données du recensement 2020.\nChamp : France métropolitaine.\nLecture : Les communes  appartenant au rural à habitat dispersé ont voté en moyenne à 24,7 % pour Emmanuel Macron, soit 0,5 points de pourcentage en-dessous de la moyenne nationale."
  ) +
  coord_cartesian(ylim = c(0, val_max * 1.3)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 25, hjust = 1),
    axis.title.y = element_text(margin = margin(r = 20)),
    plot.caption = element_text(size = 8, hjust = 0, margin = margin(t = 15))
  )

ggsave("vote_macron_2017.png", width = 14, height = 8, dpi = 300, bg = "white")




france_sf <- st_transform(france_sf, crs = 2154)

france_sf <- france_sf %>%
  left_join(PresF, by = c("com_nom" = "Libellé_commune"))

# Préparation des données
france_sf <- france_sf %>%
  filter(!is.na(libdens), !is.na(pourc_macron_22)) %>%
  mutate(libdens = factor(libdens, levels = ordre_libdens))

# Nettoyage et conversion en numérique (si nécessaire)
france_sf$pourc_macron_17 <- gsub(",", ".", france_sf$pourc_macron_17)
france_sf$pourc_macron_17 <- as.numeric(france_sf$pourc_macron_17)

# Suppression des lignes avec NA
france_sf <- france_sf %>%
  filter(!is.na(pourc_macron_17))

# Création des catégories selon les intervalles spécifiés
france_sf$macron_cat <- cut(france_sf$pourc_macron_17,
                            breaks = c(-Inf, 20, 25, 30, Inf),
                            labels = c("< 20%", "20-25%", "25-30%", "> 30%"),
                            include.lowest = TRUE)

# Définition d'une palette de couleurs pour la carte
macron_palette <- c("< 20%" = "#d9d9d9",   # Gris clair pour < 20%
                    "20-25%" = "#fee08b", # Jaune pour 20-25%
                    "25-30%" = "#fc8d59", # Orange pour 25-30%
                    "> 30%" = "#d7301f")  # Rouge pour > 30%

# Création de la carte univariée
mf_map(france_sf,
       var = "macron_cat",
       type = "typo",
       pal = macron_palette,
       border = "grey60",
       lwd = 0.1,
       leg_title = "Pourcentage Macron (%)")

# Ajouter un titre à la carte
mf_title("Carte du vote Macron (2017) par commune")

# Vérification des modalités
unique(france_sf$libdens)

# Définition de la palette de couleurs pour les 7 modalités
libdens_palette <- c(
  "Grands centres urbains" = "#131225",       # Bleu pour "Grands centres urbains"
  "Centres urbains intermédiaires" = "#223157", # Vert pour "Centres urbains intermédiaires"
  "Ceintures urbaines" = "#2d4e6f",             # Rose pour "Ceintures urbaines"
  "Petites villes" = "#167b84",                # Orange pour "Petites villes"
  "Bourgs ruraux" = "#45aba6",                 # Violet pour "Bourgs ruraux"
  "Rural à habitat dispersé" = "#b3cecb",      # Brun pour "Rural à habitat dispersé"
  "Rural à habitat très dispersé" = "#ffffff"   # Bleu clair pour "Rural à habitat très dispersé"
)

# Définition de la palette de couleurs pour les 7 modalités
libdens_palette <- c(
  "Grands centres urbains" = "#990000",       # Bleu pour "Grands centres urbains"
  "Centres urbains intermédiaires" = "#D7301F", # Vert pour "Centres urbains intermédiaires"
  "Ceintures urbaines" = "#F76E11",             # Rose pour "Ceintures urbaines"
  "Petites villes" = "#FEE08B",                # Orange pour "Petites villes"
  "Bourgs ruraux" = "#D9EF8B",                 # Violet pour "Bourgs ruraux"
  "Rural à habitat dispersé" = "#91CF60",      # Brun pour "Rural à habitat dispersé"
  "Rural à habitat très dispersé" = "#1A9850"   # Bleu clair pour "Rural à habitat très dispersé"
)

# Création de la carte univariée pour libdens
mf_map(france_sf,
       var = "libdens",
       type = "typo",
       pal = libdens_palette,
       border = "grey60",
       lwd = 0.1,
       leg_title = "Type de densité de population")

# Ajouter un titre à la carte
mf_title("Carte de la densité de population par commune")

# Export direct en PNG avec mf_export
mf_export(france_sf,
          filename = "Carte.png",  # nom du fichier
          width = 7,               # largeur en pouces
          height = 7,              # hauteur en pouces
          res = 300,               # résolution en dpi
          expr = {
            # Code de génération de la carte
            mf_map(france_sf,
                   var = "libdens",
                   type = "typo",
                   pal = libdens_palette,
                   border = "grey60",
                   lwd = 0.1,
                   leg_title = "Type de densité de population")
            
            mf_title("Carte de la densité de population par commune")
          })
 
png("densité.png", width = 1000, height = 800)
par(mar = c(2, 2, 2, 2))
mf_map(france_sf,
       var = "libdens",
       type = "typo",
       pal = libdens_palette,
       border = "grey60",
       lwd=0,1,
       leg_title = "Catégorie de densité")
dev.off()

mf_map(x=france_sf, border = "white")

# Carte du pourcentage de vote Macron 2017 par commune
mf_map(france_sf,
       var = "pourc_macron_17",
       type = "choro",
       breaks = c(0, 20, 25, 30, 100),  # 4 classes personnalisées
       pal = "Blues",
       border = "grey60",
       leg_title = "Vote Macron 2017 (%)")

mf_title("Résultats de Macron en 2017 par commune")




png("macron_map.png", width = 1000, height = 800)
par(mar = c(2, 2, 2, 2))
mf_map(france_sf,
       var = "pourc_macron_17",
       type = "choro",
       breaks = c(0, 20, 25, 30, 100),
       pal = "Blues",
       border = "grey60",
       leg_title = "Vote Macron 2017 (%)")
dev.off()

