{
  PresF_tmp <- PresF %>%
    mutate_at(c("cs_cpis_pourc", "cs_empl_pourc", "cs_ouvr_pourc" ), ~replace(., is.na(.), 0)) %>%
    mutate(
      cs_pi_pourc = cs_pi_n * 100 / cs_tot,
      cs_agri_pourc = cs_agri_n * 100 / cs_tot,
      cs_acce_pourc = cs_acce_n * 100 / cs_tot,
    )
    filter(!is.na(diff_pourc_macron) & !is.na(libdens) & dens %in% c("5", "6", "7")) %>%
    group_by(libdens)
  
tab <- rbind(
  PresF_tmp %>%
    mutate(decile = as.factor(ntile(cs_cpis_pourc, 10))) %>%
    select(decile, diff_pourc_macron, libdens) %>%
    group_by(libdens, decile) %>%
    summarise(moyenne = mean(diff_pourc_macron),
              nb = n()) %>%
    mutate(CSP = "Cadres et professions intellectuelles supérieures") %>%
    filter(decile %in% c(1:10)),
  
  PresF_tmp %>%
    mutate(decile = as.factor(ntile(cs_ouvr_pourc, 10))) %>%
    select(decile, diff_pourc_macron, libdens) %>%
    group_by(libdens, decile) %>%
    summarise(moyenne = mean(diff_pourc_macron),
              nb = n()) %>%
    mutate(CSP = "Ouvriers"),
  
  PresF_tmp %>%
    mutate(decile = as.factor(ntile(cs_empl_pourc, 10))) %>%
    select(decile, diff_pourc_macron, libdens) %>%
    group_by(libdens, decile) %>%
    summarise(moyenne = mean(diff_pourc_macron),
              nb = n()) %>%
    mutate(CSP = "Employés"),
  
  PresF_tmp %>%
    mutate(
      decile = as.factor(ntile(cs_pi_pourc, 10))
    ) %>%
    select(decile, diff_pourc_macron, libdens) %>%
    group_by(libdens, decile) %>%
    summarise(moyenne = mean(diff_pourc_macron),
              nb = n()) %>%
    mutate(CSP = "Professions Intermédiaires"),
  
  PresF_tmp %>%
    mutate(
      decile = as.factor(ntile(cs_agri_pourc, 10))
    ) %>%
    select(decile, diff_pourc_macron, libdens) %>%
    group_by(libdens, decile) %>%
    summarise(moyenne = mean(diff_pourc_macron),
              nb = n()) %>%
    mutate(CSP = "Agriculteurs exploitants"),
  
  PresF_tmp %>%
    mutate(
      decile = as.factor(ntile(cs_acce_pourc, 10))
    ) %>%
    select(decile, diff_pourc_macron, libdens) %>%
    group_by(libdens, decile) %>%
    summarise(moyenne = mean(diff_pourc_macron),
              nb = n()) %>%
    mutate(CSP = "Artisans, commerçants et chefs d'entreprise") ,
  
  PresF_tmp %>%
    mutate(
      decile = as.factor(ntile(cs_acce_pourc, 10))
    ) %>%
    select(decile, diff_pourc_macron, libdens) %>%
    group_by(libdens, decile) %>%
    summarise(moyenne = mean(diff_pourc_macron),
              nb = n()) %>%
    mutate(CSP = "Artisans, commerçants et chefs d'entreprise")
) %>%
  filter(decile %in% c(1:10)) %>%
  mutate(
    CSP = as.factor(CSP),
    CSP = fct_relevel(CSP, c(
      "Agriculteurs exploitants",
      "Ouvriers",
      "Employés",
      "Professions Intermédiaires",
      "Artisans, commerçants et chefs d'entreprise",
      "Cadres et professions intellectuelles supérieures"
    )))
}



ggplot(tab, aes(
  x = decile,
  y = moyenne,
  group = CSP,
  fill = CSP,
  colour = CSP
)) +
  facet_grid(cols = vars(libdens)) +
  geom_point(shape = 21, size = 2) +
  geom_line() +
  labs(
    fill = "Classes socio-professionelles",
    title = "Evolution du vote Macron en fonction des proportions de CSP",
    x = "Décile de la proportion de chaque CSP",
    y = "Point de pourcentage gagné en moyenne par Macron entre 2017 et 2022",
    caption = "
    Sources : Résultats des premiers tours des élections présidentielles 2017 et 2022, DDCT ; Recensement, INSEE.
    Champs : Communes rurales françaises (d'après la définition en 7 catégories de l'INSEE).
    Lectures :
       1: Le vote pour E.Macron des 10% des bourgs ruraux qui possèdent le plus d'employés a augmenté, en moyenne, d'environ 3,7 points de pourcentage entre 2017 et 2022.
       2: Dans les communes rurales à habitat dispersé, plus la proportion d'ouvriers augmente, plus l'augmentation moyenne du vote Macron entre 2017 et 2022 est forte."
    ) +
  guides(colour = "none") +
  theme_minimal(base_family = "Times New Roman") +
  theme(plot.caption = element_text(hjust = 0))

ggsave(
  "03_analyses/fig8.png",
  width = 20,
  height = 10,
  units = "in"
)


tab %>%
  ggplot(aes(
    x = decile,
    y = moyenne,
    group = CSP,
    fill = CSP,
    colour = CSP
  )) +
  facet_grid(CSP ~ libdens, labeller = labeller(CSP = label_wrap_gen(width = 13))) +
  geom_point(shape = 21, size = 2) +
  geom_line() +
  labs(
    fill = "Classes socio-professionelles",
    title = "Evolution du vote Macron en fonction des proportions de CSP",
    x = "Décile de la proportion de chaque CSP",
    y = "Point de pourcentage gagné en moyenne par Macron entre 2017 et 2022",
    caption = "
    Sources : Résultats des premiers tours des élections présidentielles 2017 et 2022, DDCT ; Recensement, INSEE.
    Champ : Communes rurales françaises (d'après la définition en 7 catégories de l'INSEE).
    Lectures :
       1: Le vote pour E.Macron des 10% des bourgs ruraux qui possèdent le plus d'employés a augmenté, en moyenne, d'environ 3,7 points
          de pourcentage entre 2017 et 2022.
       2: Dans les communes rurales à habitat dispersé, plus la proportion d'ouvriers augmente, plus l'augmentation moyenne du vote Macron 
          entre 2017 et 2022 est forte."
  ) +
  guides(colour = "none", fill= "none") +
  theme(plot.caption = element_text(hjust = 0),
        text = element_text(family = "Times New Roman"),
        strip.text.y = element_text(angle = 0))

ggsave(
  "03_analyses/fig9.png",
  width = 8,
  height = 7,
  units = "in"
)
