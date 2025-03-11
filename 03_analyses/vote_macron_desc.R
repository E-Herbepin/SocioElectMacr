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


# Moyenne de l'évolution du vote Macron en fonction du décile d'appartenance de la proportion d'emploi stable

PresF %>%
  mutate(decile = as.factor(ntile(empl_stable_pourc, 10))) %>%
  filter(!is.na(diff_pourc_macron) & !is.na(empl_stable_pourc)) %>%
  select(decile, diff_pourc_macron) %>%
  group_by(decile) %>%
  summarise(moyenne = mean(diff_pourc_macron)) %>%
  ggplot(aes(x = decile, y = moyenne)) +
  geom_point() +
  labs(
    title = "Evolution du vote Macron en fonction de la stabilité de l'emploi",
    x = "Décile de la proportion d'emploi stable",
    y = "Point de pourcentage gagné en moyenne par Macron entre 2017 et 2022",
    caption = "Lecture : Les communes dont la proportion d'emploi stable appartient au 7ème décile ont vu leur vote pour Macron augmenter en moyenne de 5 points de pourcentage."
  ) +
  theme(plot.caption = element_text(hjust = 0))

ggsave(
  "03_analyses/fig2.png",
  width = 10,
  height = 6,
  units = "in"
)

## Proportion de chaque CSP ----
{
  PresF_tmp <- PresF %>%
    mutate_at(c("cs_cpis_pourc", "cs_empl_pourc", "cs_ouvr_pourc" ), ~replace(., is.na(.), 0)) %>%
    filter(!is.na(diff_pourc_macron) & !is.na(libdens)) %>%
    mutate(libdens_cat = case_when(
      libdens %in% c("Bourgs ruraux", "Rural à habitat dispersé", "Rural à habitat très dispersé") ~ "Rural",
      libdens %in% c("Ceintures urbaines", "Centres urbains intermédiaires", "Petites villes") ~ "Densité intermédiaire",
      TRUE ~ libdens
    ),
    libdens_cat = fct_relevel(libdens_cat, c("Rural", "Densité intermédiaire", "Grands centres urbains"))
    )
  
  
  tab <- rbind(
    PresF_tmp %>%
      group_by(libdens_cat) %>%
      mutate(decile = as.factor(ntile(cs_cpis_pourc, 10))) %>%
      select(decile, diff_pourc_macron, libdens_cat) %>%
      group_by(libdens_cat, decile) %>%
      summarise(moyenne = mean(diff_pourc_macron),
                nb = n()) %>%
      mutate(CSP = "Cadres et professions intellectuelles supérieures") %>%
      filter(decile %in% c(1:10)),
    
    PresF_tmp %>%
      group_by(libdens_cat) %>%
      mutate(decile = as.factor(ntile(cs_ouvr_pourc, 10))) %>%
      select(decile, diff_pourc_macron, libdens_cat) %>%
      group_by(libdens_cat, decile) %>%
      summarise(moyenne = mean(diff_pourc_macron),
                nb = n()) %>%
      mutate(CSP = "Ouvriers"),
    
    PresF_tmp %>%
      group_by(libdens_cat) %>%
      mutate(decile = as.factor(ntile(cs_empl_pourc, 10))) %>%
      select(decile, diff_pourc_macron, libdens_cat) %>%
      group_by(libdens_cat, decile) %>%
      summarise(moyenne = mean(diff_pourc_macron),
                nb = n()) %>%
      mutate(CSP = "Employés"),
    
    PresF_tmp %>%
      group_by(libdens_cat) %>%
      mutate(
        cs_pi_pourc = cs_pi_n * 100 / cs_tot,
        decile = as.factor(ntile(cs_pi_pourc, 10))
      ) %>%
      select(decile, diff_pourc_macron, libdens_cat) %>%
      group_by(libdens_cat, decile) %>%
      summarise(moyenne = mean(diff_pourc_macron),
                nb = n()) %>%
      mutate(CSP = "Professions Intermédiaires"),
    
    PresF_tmp %>%
      group_by(libdens_cat) %>%
      mutate(
        cs_agri_pourc = cs_agri_n * 100 / cs_tot,
        decile = as.factor(ntile(cs_agri_pourc, 10))
      ) %>%
      select(decile, diff_pourc_macron, libdens_cat) %>%
      group_by(libdens_cat, decile) %>%
      summarise(moyenne = mean(diff_pourc_macron),
                nb = n()) %>%
      mutate(CSP = "Agriculteurs exploitants"),
    
    PresF_tmp %>%
      group_by(libdens_cat) %>%
      mutate(
        cs_acce_pourc = cs_acce_n * 100 / cs_tot,
        decile = as.factor(ntile(cs_acce_pourc, 10))
      ) %>%
      select(decile, diff_pourc_macron, libdens_cat) %>%
      group_by(libdens_cat, decile) %>%
      summarise(moyenne = mean(diff_pourc_macron),
                nb = n()) %>%
      mutate(CSP = "Artisans, commerçants et chefs d'entreprise") ,
    
    PresF_tmp %>%
      group_by(libdens_cat) %>%
      mutate(
        cs_acce_pourc = cs_acce_n * 100 / cs_tot,
        decile = as.factor(ntile(cs_acce_pourc, 10))
      ) %>%
      select(decile, diff_pourc_macron, libdens_cat) %>%
      group_by(libdens_cat, decile) %>%
      summarise(moyenne = mean(diff_pourc_macron),
                nb = n()) %>%
      mutate(CSP = "Artisans, commerçants et chefs d'entreprise")
  ) %>%
    filter(decile %in% c(1:10)) %>%
    mutate(cat = factor(
      case_when(
        CSP %in% c("Employés", "Artisans, commerçants et chefs d'entreprise") ~ "Diminue",
        CSP %in% c(
          "Cadres et professions intellectuelles supérieures",
          "Professions Intermédiaires",
          "Agriculteurs exploitants"
        ) ~ "Stagne",
        CSP %in% c("Ouvriers") ~ "Augmente"
      ),
      ordered = TRUE
    ))
}

ggplot(tab, aes(
  x = decile,
  y = moyenne,
  group = CSP,
  fill = CSP,
  colour = CSP
)) +
  facet_grid(cols = vars(libdens_cat)) +
  geom_point(shape = 21, size = 2) +
  geom_line() +
  labs(
    fill = "Classes socio-professionelles",
    title = "Evolution du vote Macron en fonction des proportions de CSP",
    x = "Décile de la proportion de chaque CSP",
    y = "Point de pourcentage gagné en moyenne par Macron entre 2017 et 2022",
    caption = "Lectures :
       1: Le vote Macron des 10% des communes rurales qui possèdent le plus d'employés a augmenté, en moyenne, de 4 points de pourcentage.
       2: Dans les communes rurales, plus la proportion d'ouvriers augmente, plus l'augmentation du vote Macron entre 2017 et 2022 est forte.
           Dans les grands centres urbains, c'est l'inverse.
       3: La proportion de professions intermédiaires dans les communes rurales semble moins corrélée à l'augmentation du vote pour Macron que dans les communes à densité intermédiaire."
  ) +
  guides(colour = "none") +
  theme(plot.caption = element_text(hjust = 0))

ggsave(
  "03_analyses/fig6.png",
  width = 20,
  height = 10,
  units = "in"
)

# Double facet : 
tab %>% mutate(
  CSP = as.factor(CSP),
  CSP = fct_relevel(CSP, c(
    "Agriculteurs exploitants",
    "Ouvriers",
    "Employés",
    "Professions Intermédiaires",
    "Artisans, commerçants et chefs d'entreprise",
    "Cadres et professions intellectuelles supérieures"
  )),
  CSP = fct_recode(CSP, "Cadres et PIS" = "Cadres et professions intellectuelles supérieures")
) %>%
  ggplot(aes(
    x = decile,
    y = moyenne,
    group = CSP,
    fill = CSP,
    colour = CSP
  )) +
  facet_grid(CSP ~ libdens_cat, labeller = labeller(CSP = label_wrap_gen(width = 13))) +
  geom_point(shape = 21, size = 2) +
  geom_line() +
  labs(
    fill = "Classes socio-professionelles",
    title = "Evolution du vote Macron en fonction des proportions de classe socio-professionelle",
    x = "Décile de la proportion de chaque CSP",
    y = "Point de pourcentage gagné en moyenne par Macron entre 2017 et 2022",
    caption = "Lectures :
       1: Le vote Macron des 10% des communes rurales qui possèdent le plus d'employés a augmenté, en moyenne, de 4 points 
           de pourcentage.
       2: Dans les communes rurales, plus la proportion d'ouvriers augmente, plus l'augmentation du vote Macron entre 2017 et 2022
           est forte. Dans les grands centres urbains, c'est l'inverse.
       3: La proportion de professions intermédiaires dans les communes rurales semble moins corrélée à l'augmentation du vote
           pour Macron que dans les communes à densité intermédiaire."
  ) +
  guides(colour = "none", fill = "none") +
  theme(plot.caption = element_text(hjust = 0))

ggsave(
  "03_analyses/fig5.png",
  width = 8,
  height = 7,
  units = "in"
)
