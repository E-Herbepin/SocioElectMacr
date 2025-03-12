{
  PresF_tmp <- PresF %>%
    mutate_at(c("cs_cpis_pourc", "cs_empl_pourc", "cs_ouvr_pourc" ), ~replace(., is.na(.), 0)) %>%
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
      cs_pi_pourc = cs_pi_n * 100 / cs_tot,
      decile = as.factor(ntile(cs_pi_pourc, 10))
    ) %>%
    select(decile, diff_pourc_macron, libdens) %>%
    group_by(libdens, decile) %>%
    summarise(moyenne = mean(diff_pourc_macron),
              nb = n()) %>%
    mutate(CSP = "Professions Intermédiaires"),
  
  PresF_tmp %>%
    mutate(
      cs_agri_pourc = cs_agri_n * 100 / cs_tot,
      decile = as.factor(ntile(cs_agri_pourc, 10))
    ) %>%
    select(decile, diff_pourc_macron, libdens) %>%
    group_by(libdens, decile) %>%
    summarise(moyenne = mean(diff_pourc_macron),
              nb = n()) %>%
    mutate(CSP = "Agriculteurs exploitants"),
  
  PresF_tmp %>%
    mutate(
      cs_acce_pourc = cs_acce_n * 100 / cs_tot,
      decile = as.factor(ntile(cs_acce_pourc, 10))
    ) %>%
    select(decile, diff_pourc_macron, libdens) %>%
    group_by(libdens, decile) %>%
    summarise(moyenne = mean(diff_pourc_macron),
              nb = n()) %>%
    mutate(CSP = "Artisans, commerçants et chefs d'entreprise") ,
  
  PresF_tmp %>%
    mutate(
      cs_acce_pourc = cs_acce_n * 100 / cs_tot,
      decile = as.factor(ntile(cs_acce_pourc, 10))
    ) %>%
    select(decile, diff_pourc_macron, libdens) %>%
    group_by(libdens, decile) %>%
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
  facet_grid(cols = vars(libdens)) +
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
  "03_analyses/fig8.png",
  width = 20,
  height = 10,
  units = "in"
)


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
  facet_grid(CSP ~ libdens, labeller = labeller(CSP = label_wrap_gen(width = 13))) +
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
  "03_analyses/fig9.png",
  width = 8,
  height = 7,
  units = "in"
)
