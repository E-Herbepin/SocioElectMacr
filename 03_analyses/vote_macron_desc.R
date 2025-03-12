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
           !is.na(diff_pourc_macron) & !is.na(empl_stable_pourc)) %>%
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