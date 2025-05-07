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

## Parti socialiste

# Parmi les exprimés en 2017

PresF <- PresF %>% 
  mutate(pourc_ps_17 = Nb_17_Voix_Hamon * 100 / Nb_17_Exprimés)

# Parmi les exprimés en 2022

PresF <- PresF %>% 
  mutate(pourc_ps_22 = Nb_22_Voix_Hidalgo * 100 / Nb_22_Exprimés)

### On crée une variable pour suivre les évolutions de vote.

## Macron

# Parmi les exprimés.

PresF <- PresF %>%
  mutate(diff_pourc_macron = pourc_macron_22 - pourc_macron_17)

# Parmi les inscrits.

PresF <- PresF %>% 
  mutate(diff_pourc_macron_inscr = pourc_macron_22_inscr - pourc_macron_17_inscr)

## Républicains

# Parmi les exprimés

PresF <- PresF %>%
  mutate(diff_pourc_repu = pourc_repu_22 - pourc_repu_17)

## Le Pen

# Parmi les exprimés

PresF <- PresF %>%
  mutate(diff_pourc_lepen = pourc_lepen_22 - pourc_lepen_17)

## Parti socialiste

# Parmi les exprimés

PresF <- PresF %>%
  mutate(diff_pourc_ps = pourc_ps_22 - pourc_ps_17)

## Mélenchon

PresF <- PresF %>%
  mutate(diff_pourc_melenchon = pourc_melenchon_22 - pourc_melenchon_17)

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

#### Cartes

### Préparation de la base de données.

france_sf <- st_transform(france_sf, crs = 2154)

france_sf <- france_sf %>%
  left_join(PresF, by = c("com_nom" = "Libellé_commune"))

### Carte densité

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

## Début de l'enregistrement

png("densité.png", width = 12, height = 8, units = "in", res = 300)

# Création de la carte univariée pour libdens
mf_map(france_sf,
       var = "libdens",
       type = "typo",
       pal = libdens_palette,
       border = "grey60",
       lwd = 0.1,
       leg_pos = NA)

mf_legend(type = "typo", val = c("Grands centres urbains", "Centres urbains intermédiaires", "Ceintures urbaines", "Petites villes", "Bourgs ruraux","Rural à habitat dispersé", "Rural à habitat très dispersé"), pal = libdens_palette, pos = "left", title = "Catégories de densité", size = 1.3)

# Ajouter un titre à la carte
mf_title("Carte de la densité de population par commune")

# Ajouter l'échelle
mf_scale(cex = 1)

# Ajouter l'orientation
mf_arrow(cex = 1)

# Ajouter la source et autres détails
mf_credits(txt = "Source : Insee, données du recensement 2020.\nChamp : France métropolitaine.", cex = 1)

dev.off()

### Carte vote Macron

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

# Carte du pourcentage de vote Macron 2017 par commune
png("macron_map.png", width = 12, height = 8, units = "in", res = 300)

mf_map(france_sf,
       var = "pourc_macron_17",
       type = "choro",
       breaks = c(100, 30, 25, 20, 0),  # 4 classes personnalisées
       pal = "Blues",
       border = "grey60",
       leg_title = "Vote Macron 2017 (%)",
       leg_pos = NA
)

mf_legend(type = "typo", val = c("< 20 %", "25 à 30 %", "20 à 25 %", "> 30 %"), pal = rev("Blues"), pos = "left", title = "Pourcentage de vote Macron parmi\nles voix exprimées au premier tour", size = 1.3)

mf_title("Résultats de Macron en 2017 par commune")

# Ajouter l'échelle
mf_scale(cex = 1)

# Ajouter l'orientation
mf_arrow(cex = 1)

# Ajouter la source et autres détails
mf_credits(txt = "Source : Ministère de l'Intérieur, données du premier tour des élections présidentielles 2017.\nChamp : France métropolitaine.", cex = 1)

dev.off()

### Carte densité rural seulement

# Filtrer les données pour ne conserver que les modalités spécifiées
france_sf_filtered <- france_sf[france_sf$libdens %in% c("Bourgs ruraux", "Rural à habitat dispersé", "Rural à habitat très dispersé"), ]

# Définition de la palette de couleurs pour les 3 modalités
libdens_palette3 <- c(
  "Bourgs ruraux" = "#D9EF8B",                 # Violet pour "Bourgs ruraux"
  "Rural à habitat dispersé" = "#91CF60",      # Brun pour "Rural à habitat dispersé"
  "Rural à habitat très dispersé" = "#1A9850"   # Bleu clair pour "Rural à habitat très dispersé"
)

png("macron_map_ruronly.png", width = 12, height = 8, units = "in", res = 300)

# Création de la carte univariée pour libdens, en ne montrant que les catégories filtrées
mf_map(france_sf_filtered,
       var = "libdens",
       type = "typo",
       pal = libdens_palette3,
       border = "grey60",
       lwd = 0.1,
       leg_pos = NA)

# Mise à jour de la légende en fonction des catégories filtrées
mf_legend(type = "typo", val = c("Bourgs ruraux", "Rural à habitat dispersé", "Rural à habitat très dispersé"), 
          pal = libdens_palette3, pos = "left", title = "Catégories de densité", size = 1.3)

# Ajouter un titre à la carte
mf_title("Carte de la densité de population (Catégories rurales)")

# Ajouter l'échelle
mf_scale(cex = 1)

# Ajouter l'orientation
mf_arrow(cex = 1)

# Ajouter la source et autres détails
mf_credits(txt = "Source : Insee, données du recensement 2020.\nChamp : France métropolitaine.", cex = 1)

dev.off()

#### Tests de corrélation.

cor.test(PresF$diff_pourc_repu, PresF$diff_pourc_macron, method = "pearson")

cor(PresF$diff_pourc_repu, PresF$diff_pourc_repu, method = "pearson")

plot(PresF$diff_pourc_macron, PresF$diff_pourc_repu,
     xlab = "Évolution vote Macron (%)",
     ylab = "Évolution vote républicain (%)",
     main = "Corrélation entre évolutions des votes Macron et Les Républicains")
abline(lm(PresF$diff_pourc_macron ~ PresF$diff_pourc_repu), col = "blue")  # ligne de régression

cor.test(PresF$diff_pourc_lepen, PresF$diff_pourc_macron, method = "pearson")

plot(PresF$diff_pourc_macron, PresF$diff_pourc_lepen,
     xlab = "Évolution vote Macron (%)",
     ylab = "Évolution vote Le Pen (%)",
     main = "Corrélation entre évolutions des votes Macron et Le Pen")
abline(lm(PresF$diff_pourc_macron ~ PresF$diff_pourc_lepen), col = "blue")  # ligne de régression

cor.test(PresF$diff_pourc_ps, PresF$diff_pourc_macron, method = "pearson")

plot(PresF$diff_pourc_macron, PresF$diff_pourc_ps,
     xlab = "Évolution vote Macron (%)",
     ylab = "Évolution vote PS (%)",
     main = "Corrélation entre évolutions des votes Macron et PS")
abline(lm(PresF$diff_pourc_macron ~ PresF$diff_pourc_ps), col = "blue")  # ligne de régression

# Créer un graphique
plot(PresF$diff_pourc_ps, PresF$diff_pourc_macron, main = "Différence de pourcentage entre Macron et PS", 
     xlab = "Différence PS", ylab = "Différence Macron")

rlm_model <- rlm(diff_pourc_ps ~ diff_pourc_macron, data = PresF)

# Ajouter la droite du modèle de régression
abline(rlm_model, col = "red")

# Créer un fichier PNG de haute qualité
png("cor_repu_macron.png", width = 12, height = 8, units = "in", res = 300)

# Créer le graphique avec diff_pourc_macron et diff_pourc_repu
plot(PresF$diff_pourc_macron, PresF$diff_pourc_repu, 
     main = "Corrélation entre évolutions des votes Macron et Les Républicains", 
     xlab = "Évolution Macron", 
     ylab = "Évolution Les Républicains")

# Ajuster le modèle de régression robuste
rlm_model <- rlm(diff_pourc_repu ~ diff_pourc_macron, data = PresF)

# Ajouter la droite de régression
abline(rlm_model, col = "red")

dev.off()

abline(lm(PresF$diff_pourc_repu ~ PresF$diff_pourc_macron), col = "blue")  # ligne de régression

library(MASS)

# Créer un fichier PNG avec une haute résolution
png("four_plots.png", width = 14, height = 8, units = "in", res = 300)

# Diviser la fenêtre graphique en 2x2
par(mfrow = c(2, 2))

### Créer les 4 graphiques (identiques à l'exemple précédent)

## Graph 1 Le Pen
plot(PresF$diff_pourc_macron, PresF$diff_pourc_lepen, 
     main = "Corrélation entre évolutions des votes Macron et Le Pen", 
     xlab = "Évolution Macron", 
     ylab = "Évolution Le Pen")

# Ajuster le modèle de régression robuste
rlm_model1 <- rlm(diff_pourc_macron ~ diff_pourc_lepen, data = PresF)

# Ajouter la droite de régression
abline(rlm_model1, col = "red")

## Graph 2 Repu

plot(PresF$diff_pourc_macron, PresF$diff_pourc_repu, 
     main = "Corrélation entre évolutions des votes Macron et Les Républicains", 
     xlab = "Évolution Macron", 
     ylab = "Évolution Les Républicains")

# Ajuster le modèle de régression robuste
rlm_model2 <- rlm(diff_pourc_macron ~ diff_pourc_repu, data = PresF)

# Ajouter la droite de régression
abline(rlm_model2, col = "red")

## Graph 3 Mélenchon

plot(PresF$diff_pourc_macron, PresF$diff_pourc_melenchon, 
     main = "Corrélation entre évolutions des votes Macron et Mélenchon", 
     xlab = "Évolution Macron", 
     ylab = "Évolution Mélenchon")

# Ajuster le modèle de régression robuste
rlm_model3 <- rlm(diff_pourc_macron ~ diff_pourc_melenchon, data = PresF)

# Ajouter la droite de régression
abline(rlm_model3, col = "red")

## Graph 4 PS

plot(PresF$diff_pourc_macron, PresF$diff_pourc_ps, 
     main = "Corrélation entre évolutions des votes Macron et Les PS", 
     xlab = "Évolution Macron", 
     ylab = "Évolution PS")

# Ajuster le modèle de régression robuste
rlm_model4 <- rlm(diff_pourc_macron ~ diff_pourc_ps, data = PresF)

# Ajouter la droite de régression
abline(rlm_model4, col = "red")

dev.off()

#### Corrélation Macron / Le Pen différence rural urbain.


cor.test(PresF$diff_pourc_repu, PresF$diff_pourc_macron, method = "pearson")

plot(PresF$diff_pourc_macron, PresF$diff_pourc_repu,
     xlab = "Évolution vote Macron (%)",
     ylab = "Évolution vote républicain (%)",
     main = "Corrélation entre évolutions des votes Macron et Les Républicains")
abline(lm(PresF$diff_pourc_macron ~ PresF$diff_pourc_repu), col = "blue")  # ligne de régression
