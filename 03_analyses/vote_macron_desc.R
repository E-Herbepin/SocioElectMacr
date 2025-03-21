### Premières analyses.

## On crée une variable pour les voix Macron en pourcentage des votes exprimés.

# Pour 2017 (parmi les exprimés)

PresF <- PresF %>%
  mutate(pourc_macron_17 = Nb_17_Voix_Macron * 100 / Nb_17_Exprimés)

# Pour 2017 (parmi les inscrits)

PresF <- PresF %>%
  mutate(pourc_macron_17_inscr = Nb_17_Voix_Macron * 100 / Nb_17_Inscrits)

# Pour 2022 (parmi les exprimés)

PresF <- PresF %>%
  mutate(pourc_macron_22 = Nb_22_Voix_Macron * 100 / Nb_22_Exprimés)

# Pour 2022 (parmi les inscrits)

PresF <- PresF %>%
  mutate(pourc_macron_22_inscr = Nb_22_Voix_Macron * 100 / Nb_22_Inscrits)

## On crée une variable qui calcule la différence de pourcentage entre les deux.

PresF <- PresF %>% 
  mutate(diff_pourc_macron = pourc_macron_22 - pourc_macron_17)

## Pour les inscrits.

PresF <- PresF %>% 
  mutate(diff_pourc_macron_inscr = pourc_macron_22_inscr - pourc_macron_17_inscr)

# Moyenne, médiane, quartiles de l'ensemble.

summary(PresF$diff_pourc_macron)
summary(PresF$diff_pourc_macron_inscr)

# Moyenne, médiane, quintiles de communes rurales (dens = 7, 6 ,5)

PresF %>%
  filter(dens %in% c(1, 2, 3, 4)) %>%
  summarise(
    moyenne = mean(diff_pourc_macron, na.rm = TRUE),
    mediane = median(diff_pourc_macron, na.rm = TRUE),
    quartiles = list(quantile(diff_pourc_macron, na.rm = TRUE))
  )

PresF %>%
  filter(dens %in% c(7, 6, 5)) %>%
  summarise(
    moyenne = mean(diff_pourc_macron, na.rm = TRUE),
    mediane = median(diff_pourc_macron, na.rm = TRUE),
    quartiles = list(quantile(diff_pourc_macron, na.rm = TRUE))
  )

PresF %>%
  filter(dens %in% c(7)) %>%
  summarise(
    moyenne = mean(diff_pourc_macron, na.rm = TRUE),
    mediane = median(diff_pourc_macron, na.rm = TRUE),
    quartiles = list(quantile(diff_pourc_macron, na.rm = TRUE))
  )

PresF %>%
  filter(dens %in% c(6)) %>%
  summarise(
    moyenne = mean(diff_pourc_macron, na.rm = TRUE),
    mediane = median(diff_pourc_macron, na.rm = TRUE),
    quartiles = list(quantile(diff_pourc_macron, na.rm = TRUE))
  )

PresF %>%
  filter(dens %in% c(5)) %>%
  summarise(
    moyenne = mean(diff_pourc_macron, na.rm = TRUE),
    mediane = median(diff_pourc_macron, na.rm = TRUE),
    quartiles = list(quantile(diff_pourc_macron, na.rm = TRUE))
  )

# Boxplot

PresF %>%
  filter(dens %in% c(7, 6, 5)) %>%
  ggplot(aes(y = diff_pourc_macron)) +
  geom_boxplot()

# Histogramme.

PresF %>%
  filter(dens %in% c(7, 6, 5)) %>%
  ggplot(aes(x = diff_pourc_macron)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black")

tab1 <- PresF %>% tabyl(diff_pourc_macron, libdens)

tableau_moyenne <- PresF %>%
  group_by(libdens) %>%
  summarise(moyenne_pourcentage = round(mean(diff_pourc_macron, na.rm = TRUE),1))

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
