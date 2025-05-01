#### Premières analyses.

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

### On crée une variable pour suivre l'évolution des voix.

## Parmi les exprimés.

PresF <- PresF %>%
  mutate(diff_pourc_macron = pourc_macron_22 - pourc_macron_17)

## Parmi les inscrits.

PresF <- PresF %>% 
  mutate(diff_pourc_macron_inscr = pourc_macron_22_inscr - pourc_macron_17_inscr)



# Moyenne, médiane, quartiles de l'ensemble.


summary(PresF$diff_pourc_macron)
summary(PresF$diff_pourc_macron_inscr)

# Moyenne, médiane, quintiles de communes rurales (dens = 7, 6 ,5)

mutate(diff_pourc_macron = (pourc_macron_22 - pourc_macron_17)/pourc_macron_17)

PresF <- PresF %>% 
  mutate(diff_pourc_pourc_repu = (pourc_repu_22 - pourc_fillon_17)/pourc_fillon_17)

## Pour les inscrits.

PresF <- PresF %>% 
  mutate(diff_pourc_macron_inscr = pourc_macron_22_inscr - pourc_macron_17_inscr)

# Moyenne, médiane, quartiles de l'ensemble.

summary(PresF$diff_pourc_macron)
summary(PresF$diff_pourc_macron_inscr)

# Moyenne, médiane, quintiles de communes rurales (dens = 7, 6 ,5)


PresF %>%
summary(Pres$diff_pourc_macron)

# Moyenne, médiane, quintiles de communes rurales (dens = 7, 6 ,5)

Pres %>%
  filter(dens %in% c(1, 2, 3, 4)) %>%
  summarise(
    moyenne = mean(diff_pourc_macron, na.rm = TRUE),
    mediane = median(diff_pourc_macron, na.rm = TRUE),
    quartiles = list(quantile(diff_pourc_macron, na.rm = TRUE))
  )

Pres %>%
  filter(dens %in% c(7, 6, 5)) %>%
  summarise(
    moyenne = mean(diff_pourc_macron, na.rm = TRUE),
    mediane = median(diff_pourc_macron, na.rm = TRUE),
    quartiles = list(quantile(diff_pourc_macron, na.rm = TRUE))
  )

Pres %>%
  filter(dens %in% c(7)) %>%
  summarise(
    moyenne = mean(diff_pourc_macron, na.rm = TRUE),
    mediane = median(diff_pourc_macron, na.rm = TRUE),
    quartiles = list(quantile(diff_pourc_macron, na.rm = TRUE))
  )

Pres %>%
  filter(dens %in% c(6)) %>%
  summarise(
    moyenne = mean(diff_pourc_macron, na.rm = TRUE),
    mediane = median(diff_pourc_macron, na.rm = TRUE),
    quartiles = list(quantile(diff_pourc_macron, na.rm = TRUE))
  )

Pres %>%
  filter(dens %in% c(5)) %>%
  summarise(
    moyenne = mean(diff_pourc_macron, na.rm = TRUE),
    mediane = median(diff_pourc_macron, na.rm = TRUE),
    quartiles = list(quantile(diff_pourc_macron, na.rm = TRUE))
  )

# Boxplot

Pres %>%
  filter(dens %in% c(7, 6, 5)) %>%
  ggplot(aes(y = diff_pourc_macron)) +
  geom_boxplot()

# Histogramme.

Pres %>%
  filter(dens %in% c(7, 6, 5)) %>%
  ggplot(aes(x = diff_pourc_macron)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black")

tab1 <- PresF %>% tabyl(diff_pourc_macron, libdens)

tableau_moyenne <- PresF %>%
  group_by(libdens) %>%
  summarise(moyenne_pourcentage = round(mean(diff_pourc_macron, na.rm = TRUE),1))

tableau_moyenne <- tableau_moyenne %>% rename(Densité = libdens)
tableau_moyenne <- tableau_moyenne %>% rename("Différence du pourcentage de votes exprimés au premier tour en faveur du candidat Macron, entre 2017 et 2022" = moyenne_pourcentage)

PresF %>% 
  group_by(libdens) %>% 
  table(pourc_macron_17)

table(PresF$pourc_macron_17)

moyennes <- aggregate(pourc_macron_17 ~ libdens, data = PresF, FUN = mean)
moyennes

# Calculer les statistiques pour pourc_macron_17
stats_17 <- aggregate(pourc_macron_17 ~ libdens, data = PresF, FUN = mean)

# Calculer les statistiques pour pourc_macron_22
stats_22 <- aggregate(pourc_macron_22 ~ libdens, data = PresF, FUN = mean)

# Combiner les résultats
stats_combined <- merge(stats_17, stats_22, by = "libdens", suffixes = c("_17", "_22"))

# Transformer stats_combined en format long
long_stats <- stats_combined %>%
  select(libdens, moyenne_17, moyenne_22) %>%
  pivot_longer(cols = c(moyenne_17, moyenne_22),
               names_to = "annee",
               values_to = "pourc_macron") %>%
  mutate(annee = ifelse(annee == "moyenne_17", "2017", "2022"))

# Créer l'histogramme avec espacement ajusté
ggplot(long_stats, aes(x = libdens, y = pourc_macron, fill = annee)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  scale_x_discrete(expand = c(0.1, 0.1)) +
  labs(title = "Pourcentage Macron par Densité (2017 vs 2022)",
       x = "Densité",
       y = "Pourcentage Macron",
       fill = "Année") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





stats_combined <- stats_combined[-8, ]
tableau_moyenne <- PresF %>%
  group_by(libdens) %>%
  summarise(moyenne_pourcentage = round(mean(pourc_macron_17, pourc_macron_22, na.rm = TRUE),1))

tableau_moyenne <- tableau_moyenne %>% rename(Densité = libdens)
tableau_moyenne <- tableau_moyenne %>% rename("Différence du pourcentage de votes exprimés au premier tour en faveur du candidat Macron, entre 2017 et 2022" = moyenne_pourcentage)


tableau_moyenne <- tableau_moyenne[-8,]

flextable(tableau_moyenne) %>% 
  theme_booktabs(bold_header = TRUE) %>% 
  align(align = "center", part = "all") %>% 
  set_table_properties(layout = "autofit", width = 1) %>% 
  border_outer(part="all") %>% 
  border_inner_v(part="all") %>% 
  hline(i = 4, border = fp_border(color="grey")) %>% 
  set_caption("Évolution du vote Macron entre 2017 et 2022 en fonction de la densité") %>% 
  add_footer_lines("Données : Résultats des élections présidentielles par commune, 2017 et 2022.") %>%
  add_footer_lines("Champ : Communes françaises (DROM et COM compris).") %>% 
  add_footer_lines("Lecture : Dans les communes catégorisées comme appartenant au rural à habitat très dispersé, le candidat Macron a gagné en moyenne 3,5% de voix exprimées au premier tour en 2022 par rapport à 2017.") %>% 
  font(fontname = "Times New Roman", part = "all") %>% 
  fontsize(size = 10, part = "all")

ordre_perso <- c("Grands centres urbains", "Centres urbains intermédiaires", "Ceintures urbaines", "Petites villes", "Bourgs ruraux", "Rural à habitat dispersé", "Rural à habitat très dispersé")

# Définir l'ordre des niveaux de libdens
nouvel_ordre <- c("Grands centres urbains", "Centres urbains intermédiaires", "Ceintures urbaines",
                  "Petites villes", "Bourgs ruraux", "Rural à habitat dispersé",
                  "Rural à habitat très dispersé")

# Convertir libdens en facteur avec le nouvel ordre
stats_combined$libdens <- factor(stats_combined$libdens, levels = nouvel_ordre)



# Créer les nouvelles variables
PresF <- PresF %>%
  mutate(
    nouvelle_zone = case_when(
      libdens %in% c("Grands centres urbains", "Centres urbains intermédiaires") ~ "urbain",
      libdens %in% c("Ceintures urbaines", "Petites villes") ~ "périurbain",
      libdens %in% c("Bourgs ruraux", "Rural à habitat dispersé", "Rural à habitat très dispersé") ~ "rural",
      TRUE ~ NA_character_  # Au cas où il y aurait des valeurs non prévues
    )
  )




tableau_moyenne$libdens <- factor(tableau_moyenne$libdens, levels = ordre_perso)
tableau_moyenne <- tableau_moyenne[order(tableau_moyenne$libdens),]

PresF %>%
  group_by(libdens) %>%
  summarise(sum(Nb_22_Voix_Macron, na.rm = TRUE))

PresF %>%
  group_by(libdens) %>%
  summarise(sum(Nb_22_Exprimés, na.rm = TRUE))


PresF %>%
  group_by(libdens) %>%
  summarise(Total_Voix = sum(Nb_22_Voix_Macron, na.rm = TRUE)) %>%
  mutate(Pourcentage = (Total_Voix / sum(Total_Voix)) * 100)

PresF %>%
  group_by(libdens) %>%
  summarise(Total_Voix = sum(Nb_22_Exprimés, na.rm = TRUE)) %>%
  mutate(Pourcentage = (Total_Voix / sum(Total_Voix)) * 100)

##### ----------------

PresF %>%
  group_by(libdens) %>%
  summarise(pourc_macron_17 = round(mean(pourc_macron_17, na.rm = TRUE)))

PresF %>%
  group_by(libdens) %>%
  summarise(pourc_macron_22 = round(mean(pourc_macron_22, na.rm = TRUE)))

PresF %>%
  group_by(libdens) %>%
  summarise(pourc_macron_22 = round(mean(pourc_macron_22_inscr, na.rm = TRUE)))

tableau_moyenne_inscr <- tableau_moyenne_inscr %>% rename(Densité = libdens)
tableau_moyenne_inscr <- tableau_moyenne_inscr %>% rename("Différence du pourcentage de votes exprimés au premier tour en faveur du candidat Macron, entre 2017 et 2022" = moyenne_pourcentage_inscr)


tableau_moyenne_inscr <- tableau_moyenne_inscr[-8,]

flextable(tableau_moyenne_inscr) %>% 
  theme_booktabs(bold_header = TRUE) %>% 
  align(align = "center", part = "all") %>% 
  set_table_properties(layout = "autofit", width = 1) %>% 
  border_outer(part="all") %>% 
  border_inner_v(part="all") %>% 
  hline(i = 4, border = fp_border(color="grey")) %>% 
  set_caption("Évolution du vote Macron entre 2017 et 2022 en fonction de la densité") %>% 
  add_footer_lines("Données : Résultats des élections présidentielles par commune, 2017 et 2022.") %>%
  add_footer_lines("Champ : Communes françaises (DROM et COM compris).") %>% 
  add_footer_lines("Lecture : Dans les communes catégorisées comme appartenant au rural à habitat très dispersé, le candidat Macron a gagné en moyenne 3,5% de voix exprimées au premier tour en 2022 par rapport à 2017.") %>% 
  font(fontname = "Times New Roman", part = "all") %>% 
  fontsize(size = 10, part = "all")

ordre_perso <- c("Grands centres urbains", "Centres urbains intermédiaires", "Ceintures urbaines", "Petites villes", "Bourgs ruraux", "Rural à habitat dispersé", "Rural à habitat très dispersé")

tableau_moyenne_inscr$libdens <- factor(tableau_moyenne_inscr$libdens, levels = ordre_perso)
tableau_moyenne_inscr <- tableau_moyenne_inscr[order(tableau_moyenne_inscr$libdens),]

PresF %>%
  group_by(libdens)

cor.test(PresF$pourc_macron_17, PresF$pourc_lepen_17, use  = "complete.obs")

cor.test(PresF$pourc_macron_17, PresF$pourc_fillon_17, use  = "complete.obs")

cor.test(PresF$diff_pourc_macron, PresF$diff_pourc_pourc_repu, use  = "complete.obs")



# Calculer la corrélation pour chaque groupe

PresF %>%
  group_by(libdens) %>%
  do({
    test_result <- cor.test(.$pourc_macron_17, .$pourc_fillon_17, use = "complete.obs")
    data.frame(
      estimate = test_result$estimate,
      p_value = test_result$p.value,
      stringsAsFactors = FALSE
    )
  })

PresF %>%
  group_by(libdens) %>%
  do({
    test_result <- cor.test(.$pourc_macron_17, .$pourc_lepen_17, use = "complete.obs")
    data.frame(
      estimate = test_result$estimate,
      p_value = test_result$p.value,
      stringsAsFactors = FALSE
    )
  })

PresF %>%
  group_by(libdens) %>%
  do({
    test_result <- cor.test(.$diff_pourc_macron, .$diff_pourc_pourc_repu, use = "complete.obs")
    data.frame(
      estimate = test_result$estimate,
      p_value = test_result$p.value,
      stringsAsFactors = FALSE
    )
  })




mod <- lm(pourc_macron_17 ~ libdens, data = PresF)

summary(mod)


mod %>%
  tbl_regression(intercept = TRUE)


### Mettre les pourcentages de vote côte à côte.

# Calculer les moyennes des pourcentages de vote pour chaque zone
summary_data <- PresF %>%
  group_by(libdens) %>%
  summarise(
    moy_macron = mean(pourc_macron_17, na.rm = TRUE),
    moy_lepen = mean(pourc_lepen_17, na.rm = TRUE),
    moy_fillon = mean(pourc_fillon_17, na.rm = TRUE),
    moy_melenchon = mean(pourc_melenchon_17, na.rm = TRUE)
  )

# Convertir les données en format long pour ggplot2
summary_long <- summary_data %>%
  pivot_longer(
    cols = starts_with("moy_"),
    names_to = "candidat",
    values_to = "pourcentage"
  )

summary_long <- summary_long[-c(29, 30, 31, 32), ]

# Définir l'ordre des niveaux de libdens
nouvel_ordre <- c("Grands centres urbains", "Centres urbains intermédiaires", "Ceintures urbaines",
                  "Petites villes", "Bourgs ruraux", "Rural à habitat dispersé",
                  "Rural à habitat très dispersé")

# Convertir libdens en facteur avec le nouvel ordre
summary_data$libdens <- factor(summary_data$libdens, levels = nouvel_ordre)

# Créer l'histogramme avec espacement ajusté
ggplot(summary_long, aes(x = libdens, y = pourcentage, fill = candidat)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  scale_x_discrete(expand = c(0.1, 0.1)) +
  labs(title = "Pourcentage des votes par zone",
       x = "Densité",
       y = "Pourcentage de vote",
       fill = "Candidat") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

sum(PresF$Nb_22_Inscrits, na.rm = TRUE)


tableau_moyenne <- tableau_moyenne[-8,]

flextable(tableau_moyenne) %>% 
  theme_booktabs(bold_header = TRUE) %>% 
  align(align = "center", part = "all") %>% 
  set_table_properties(layout = "autofit", width = 1) %>% 
  border_outer(part="all") %>% 
  border_inner_v(part="all") %>% 
  hline(i = 4, border = fp_border(color="grey")) %>% 
  set_caption("Évolution du vote Macron entre 2017 et 2022 en fonction de la densité") %>% 
  add_footer_lines("Données : Résultats des élections présidentielles par commune, 2017 et 2022.") %>%
  add_footer_lines("Champ : Communes françaises (DROM et COM compris).") %>% 
  add_footer_lines("Lecture : Dans les communes catégorisées comme appartenant au rural à habitat très dispersé, le candidat Macron a gagné en moyenne 3,5% de voix exprimées au premier tour en 2022 par rapport à 2017.") %>% 
  font(fontname = "Times New Roman", part = "all") %>% 
  fontsize(size = 10, part = "all")

ordre_perso <- c("Grands centres urbains", "Centres urbains intermédiaires", "Ceintures urbaines", "Petites villes", "Bourgs ruraux", "Rural à habitat dispersé", "Rural à habitat très dispersé")

tableau_moyenne$libdens <- factor(tableau_moyenne$libdens, levels = ordre_perso)
tableau_moyenne <- tableau_moyenne[order(tableau_moyenne$libdens),]

PresF %>%
  group_by(libdens) %>%
  summarise(total_votants = sum(Nb_22_Votants, na.rm = TRUE))

##### ----------------

tableau_moyenne_inscr <- PresF %>%
  group_by(libdens) %>%
  summarise(moyenne_pourcentage_inscr = round(mean(diff_pourc_macron_inscr, na.rm = TRUE),1))

tableau_moyenne_inscr <- tableau_moyenne_inscr %>% rename(Densité = libdens)
tableau_moyenne_inscr <- tableau_moyenne_inscr %>% rename("Différence du pourcentage de votes exprimés au premier tour en faveur du candidat Macron, entre 2017 et 2022" = moyenne_pourcentage_inscr)


tableau_moyenne_inscr <- tableau_moyenne_inscr[-8,]

flextable(tableau_moyenne_inscr) %>% 
  theme_booktabs(bold_header = TRUE) %>% 
  align(align = "center", part = "all") %>% 
  set_table_properties(layout = "autofit", width = 1) %>% 
  border_outer(part="all") %>% 
  border_inner_v(part="all") %>% 
  hline(i = 4, border = fp_border(color="grey")) %>% 
  set_caption("Évolution du vote Macron entre 2017 et 2022 en fonction de la densité") %>% 
  add_footer_lines("Données : Résultats des élections présidentielles par commune, 2017 et 2022.") %>%
  add_footer_lines("Champ : Communes françaises (DROM et COM compris).") %>% 
  add_footer_lines("Lecture : Dans les communes catégorisées comme appartenant au rural à habitat très dispersé, le candidat Macron a gagné en moyenne 3,5% de voix exprimées au premier tour en 2022 par rapport à 2017.") %>% 
  font(fontname = "Times New Roman", part = "all") %>% 
  fontsize(size = 10, part = "all")

ordre_perso <- c("Grands centres urbains", "Centres urbains intermédiaires", "Ceintures urbaines", "Petites villes", "Bourgs ruraux", "Rural à habitat dispersé", "Rural à habitat très dispersé")

tableau_moyenne_inscr$libdens <- factor(tableau_moyenne_inscr$libdens, levels = ordre_perso)
tableau_moyenne_inscr <- tableau_moyenne_inscr[order(tableau_moyenne_inscr$libdens),]
