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
