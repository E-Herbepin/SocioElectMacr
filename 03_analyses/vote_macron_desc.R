### Premières analyses.

source("02_scripts/00_setup.R")
source("02_scripts/01_import.R")



PresF <- PresF %>%
  mutate(
    ## On crée une variable pour les voix Macron en pourcentage des votes exprimés.
    # Pour 2017
    pourc_macron_17 = Nb_17_Voix_Macron * 100 / Nb_17_Exprimés,
    # Pour 2022
    pourc_macron_22 = Nb_22_Voix_Macron * 100 / Nb_22_Exprimés,
    ## On crée une variable qui calcule la différence de pourcentage entre les deux.
    diff_pourc_macron = pourc_macron_22 - pourc_macron_17,
    
    cs_pi_pourc = cs_pi_n * 100 / cs_tot,
    cs_agri_pourc = cs_agri_n * 100 / cs_tot,
    cs_acce_pourc = cs_acce_n * 100 / cs_tot,
    
    dens = as.factor(PresF$dens)
  )

# summary(PresF$diff_pourc_macron)
# mean(PresF$diff_pourc_macron, na.rm = TRUE)
#
# barplot(PresF$diff_pourc_macron)


## Répartition des communes en fonction de la différence de pourcentage de vote pour Macron entre 2017 et 2022.


PresF %>%
  filter(diff_pourc_macron <= 50) %>% # On enlève les extrêmes pour une question de lisibilité
  ggplot(aes(
    x = round(diff_pourc_macron, 0),
    group = dens,
    fill = dens
  )) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = seq(-50, 50, 5), limits = c(-50, 50)) +
  geom_vline(xintercept = mean(PresF$diff_pourc_macron, na.rm = T),
             color = "blue") +
  geom_bar() +
  labs(title = "diff_pourc_macron", y = "Différence pourcentage Macron")


PresF %>%
  filter(diff_pourc_macron <= 50 &
           !is.na(diff_pourc_macron) & !is.na(dens)) %>%
  ggplot(aes(
    x = round(diff_pourc_macron, 0),
    group = dens,
    color = dens
  )) +
  geom_boxplot(alpha = .7)

# répartition par densité
PresF %>%
  filter(diff_pourc_macron <= 50 &
           !is.na(diff_pourc_macron) & !is.na(dens)) %>%
  ggplot(aes(
    x = round(diff_pourc_macron, 0),
    group = dens,
    color = dens
  )) +
  geom_density(alpha = 0.7) + scale_color_brewer(palette = "Paired")
# Attention à ne pas interpréter trop vite ! Les courbes de densités sont le reflet des effectifs de chaque catégorie.
# Ces répartitions suivent une loi normale. Donc plus les effectifs sont importants, plus la courbe est haute et resserrée : c'est un résultat mathématique et non sociologique


## Proportion d'emplois stables ----

PresF %>%
  filter(diff_pourc_macron <= 50 &
           !is.na(diff_pourc_macron) &
           !is.na(empl_stable_pourc)) %>%
  ggplot(aes(x = empl_stable_pourc)) +
  geom_density() + # On prefere une densité à un histogramme parce que les valeurs rondes ont de très fort effectifs, ce qui rend difficile la lecture de la tendance
  geom_vline(
    xintercept = quantile(
      PresF$empl_stable_pourc,
      probs = seq(0.1, 0.9, 0.1),
      na.rm = TRUE
    ),
    color = "blue"
  ) +
  labs(
    title = "Répartition des communes en fonction de la proportion d'emploi stable",
    x = "Proportion d'emploi stable",
    y = "Densité",
    caption = "En bleu : les déciles
       Lecture : 10% des communes ont une proportion d'emploi stable inférieure à 50%."
  )

  names(Pres)
names(communes)

match <- Pres |>
  right_join(communes, by = c("Libellé_commune" = "libgeo"))

match <- match %>%
  filter(!is.na(Libellé_région))

match <- match %>%
  filter(!Libellé_région %in% c("Guyane", "La Réunion", "Martinique", "Guadeloupe"))

boxplot(match$diff_pourc_macron)

match %>%
  filter(dens %in% c(6, 5, 4)) %>%
  ggplot(aes(y = diff_pourc_macron)) +
  geom_boxplot() +
  labs(title = "Boxplot de diff_pourc_macron", y = "Différence pourcentage Macron")

match %>%
  filter(dens %in% c(6, 5, 4)) %>%
  summarise(
    moyenne = mean(diff_pourc_macron, na.rm = TRUE),
    mediane = median(diff_pourc_macron, na.rm = TRUE),
    quartiles = list(quantile(diff_pourc_macron, na.rm = TRUE))
  )
unique(match$Libellé_région)

PresF %>%
  filter(!is.na(libdens)) %>%
  mutate(
    libdens = as.factor(libdens),
    libdens = fct_relevel(
      libdens,
      c(
        "Grands centres urbains",
        "Centres urbains intermédiaires",
        "Ceintures urbaines",
        "Petites villes",
        "Bourgs ruraux",
        "Rural à habitat dispersé",
        "Rural à habitat très dispersé"
      )
    ),
    libdens = fct_recode(
      libdens,
      "Grands centres
urbains" = "Grands centres urbains",
      "Centres urbains
intermédiaires" = "Centres urbains intermédiaires",
      "Ceintures urbaines" = "Ceintures urbaines",
      "Petites villes" = "Petites villes",
      "Bourgs ruraux" = "Bourgs ruraux",
      "Rural à habitat
dispersé" = "Rural à habitat dispersé",
      "Rural à habitat
très dispersé" = "Rural à habitat très dispersé"
    )
  ) %>%
  group_by(libdens) %>%
  summarise(moyenne = mean(diff_pourc_macron, na.rm = TRUE)) %>%
  ggplot(aes(x = libdens, y = moyenne, fill = moyenne)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(moyenne, 2)), vjust = -0.5) +
  labs(
    title = "Evolution du vote Macron en fonction de la densité",
    subtitle = "Moyennes des points de pourcentage gagnés entre 2017 et 2022",
    x = "Niveaux de densité",
    y = "Point de pourcentage gagné en moyenne par Macron entre 2017 et 2022",
    caption = "
    Sources : Résultats des premiers tours des élections présidentielles 2017 et 2022, DDCT ; Recensement, INSEE.
    Champ : Communes françaises.
    Lecture : Dans les communes rurales à habitat très dispersé, le candidat Macron a gagné en moyenne 3.45 points de pourcentage de voix exprimées
                   au premier tour en 2022 par rapport à 2017"
  ) +
  theme(
    plot.caption = element_text(hjust = 0),
    text = element_text(family = "Times New Roman"),
    strip.text.y = element_text(angle = 0)
  ) +
  guides(fill = "none")

ggsave(
  "03_analyses/fig13.png",
  width = 8,
  height = 7,
  units = "in"
)

Proportion d'agriculteurrs exploitants dans les bourgs ruraux .'


tmp <- PresF %>%
  mutate(decile = as.factor(ntile(cs_agri_pourc, 10))) %>%
  group_by(decile, libdens) %>%
  summarise(moyenne = mean(cs_agri_pourc, na.rm = TRUE))
