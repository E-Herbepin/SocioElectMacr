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
