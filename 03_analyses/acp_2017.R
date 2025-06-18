# Importer les packages n?cessaires
library(FactoMineR)
library(Factoshiny)
library(explor)
library(gt)
# Pour la stat descriptive
library(questionr)
## Pour le recodage
library(tidyverse)

source("02_scripts/00_setup.R")
source("02_scripts/01_import.R")
source("02_scripts/05_recode_PresF.R")



# azeazeazeazeazaze

PresFrural <- PresF %>%
  mutate(
    dens = as.numeric(dens),
  ) %>%
  filter(dens >= 5 & dens <= 7) |>
  left_join(
    read.csv("01_data/departement2020.csv") |>
      select(dep, "Region" = reg) |>
      mutate(
        dep = as.character(dep)
      ),
    by = join_by(Code_département == dep)
  ) |>
  mutate(
    Region = factor(
      Region,
      levels = c(
        "11",
        "24",
        "27",
        "28",
        "32",
        "44",
        "52",
        "53",
        "75",
        "76",
        "84",
        "93"
      ),
      labels = c(
        "Île-de-France",
        "Centre-Val de Loire",
        "Bourgogne-Franche-Comté",
        "Normandie",
        "Hauts-de-France",
        "Grand Est",
        "Pays de la Loire",
        "Bretagne",
        "Nouvelle-Aquitaine",
        "Occitanie",
        "Auvergne-Rhône-Alpes",
        "Provence-Alpes-Côte d'Azur"
      )
    ))
var_active <- c("pourc_Macron_17", "pourc_Le_Pen_17", "pourc_Mélenchon_17", 
                "pourc_Fillon_17", "pourc_Dupont.Aignan_17", "pourc_Lassalle_17", 
                "pourc_Hamon_17", "pourc_Asselineau_17", "pourc_Poutou_17", 
                "pourc_Arthaud_17", "pourc_Cheminade_17")

var_sup <- PresFrural |>
  select(c(
    empl_precaire_pourc,
    empl_stable_pourc,
    empl_indep_pourc,
    empl_employeurs_pourc,
    empl_precaire_pourc,
    age_q1,
    age_q2,
    age_q3,
    prop_h,
    cs_empl_pourc,
    cs_ouvr_pourc,
    cs_agri_pourc,
    cs_acce_pourc,
    cs_cpis_pourc,
    cs_pi_pourc,
    log_vacants_pourc,
    log_resprinc_pourc,
    log_ressecond_pourc,
    log_logoccas_pourc,
    immi_pourc,
    Region,
    dens
  )) |>
  names()

# ACP ----
# (base avec uniquement les variables actives)

res_PCA <- PresFrural |>
  select(all_of(var_active), all_of(var_sup)) |>
  PCA(
    quanti.sup = 12:30,
    quali.sup = 31:32,
    graph = F,
    ncp = 15,
    scale.unit = FALSE,
  )

# Visualisation de la variance expliquée ----

res_PCA$eig |>
  as.data.frame() |>
  mutate(Axe = as.factor(row_number())) |>
  select(
    Axe,
    variance = "percentage of variance",
    cumulative = "cumulative percentage of variance"
  ) |>
  ggplot() +
  geom_col(aes(x = Axe, y = variance), fill = "steelblue") +
  geom_text(
    aes(x = Axe, y = variance, label = round(variance, 1)),
    vjust = -0.5
  ) +
  labs(
    x = "Axes",
    y = "Variance expliquée (%)",
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    plot.title = element_text(hjust = 0.5)
  )

ggsave("PCA_rural_variance.png", width = 5, height = 4.5, dpi = 1100)


# Visualisation des contributions des variables actives ----

res_PCA$var$contrib |>
  as.data.frame() |>
  mutate(
    across(everything(), ~ round(.x, 2)),
    sum = rowSums(across(everything()))
  ) |>
  filter(sum > 2) |>
  rownames_to_column("Variable") |>
  mutate(
    Variable = str_remove(Variable, "pourc") |>
      str_replace_all("_", " ") |>
      str_to_title()
  ) |>
  select(1:11) |>
  gt() |>
  gtsave(file = "Contribution des variables actives.docx")


# Visualisation des qualités de représentation des variables actives ----

cos2 <- res_PCA$var$cos2 |>
  as.data.frame() |>
  bind_rows(
    res_PCA$quali.sup$cos2 |>
      as.data.frame()
  ) |>
  bind_rows(
    res_PCA$quanti.sup$cos2 |>
      as.data.frame()
  ) |>
  select(1:10) |>
  mutate(
    across(everything(), ~ round(.x, 2)),
    sum = rowSums(across(everything()))
  ) |>
  filter(sum > 0.2) |>
  rownames_to_column("Variable") |>
  mutate(
    Variable = str_remove(Variable, "pourc_") |>
      str_replace_all("_", " ") |>
      str_to_title(),
    Variable = case_when(
      Variable == "Empl Precaire Pourc" ~ "Emploi précaire (%)",
      Variable == "Empl Stable Pourc" ~ "Emploi stable (%)",
      Variable == "Empl Indep Pourc" ~ "Emploi indépendant (%)",
      Variable == "Empl Employeurs Pourc" ~ "Emploi employeurs (%)",
      Variable == "Age Q1" ~ "Âge Q1",
      Variable == "Age Q2" ~ "Âge Q2",
      Variable == "Age Q3" ~ "Âge Q3",
      Variable == "Prop H" ~ "Proportion d'hommes",
      Variable == "Cs Empl Pourc" ~ "Employés (%)",
      Variable == "Cs Ouvr Pourc" ~ "Ouvriers (%)",
      Variable == "Cs Agri Pourc" ~ "Agriculteurs (%)",
      Variable == "Cs Acce Pourc" ~ "Chefs d'entreprise (%)",
      Variable == "Cs Cpis Pourc" ~ "Cadres et PI supérieures (%)",
      Variable == "Cs Pi Pourc" ~ "Professions intermédiaires (%)",
      Variable == "Log Vacants Pourc" ~ "Logements vacants (%)",
      Variable == "Log Resprinc Pourc" ~ "Logements principaux (%)",
      Variable == "Log Ressecond Pourc" ~ "Logements secondaires (%)",
      Variable == "Log Logoccas Pourc" ~ "Logements occasionnels (%)",
      Variable == "Immi Pourc" ~ "Immigrés (%)",
      Variable == "Dens.7" ~ "Rural à habitat très dispersé",
      Variable == "Dens.6" ~ "Rural à habitat dispersé",
      Variable == "Dens.5" ~ "Bourgs ruraux",
      TRUE ~ Variable
    )
  ) |>
  select(1:11)

cos2 |>
  gt() %>%
  gtsave(file = "Qualité de représentation des variables.docx")

cos2 |>
  select(1, 2, 3) |>
  filter(Dim.1 > 0.05 | Dim.2 > 0.05) |>
  gt()

cos2 |>
  select(1, 4, 5) |>
  filter(Dim.3 > 0.05 | Dim.4 > 0.05) |>
  gt()

cos2 |>
  select(1, 6, 7) |>
  filter(Dim.5 > 0.05 | Dim.6 > 0.05) |>
  gt()

cos2 |>
  select(1, 8, 9) |>
  filter(Dim.7 > 0.03 | Dim.8 > 0.03) |>
  gt()

cos2 |>
  select(1, 10, 11) |>
  filter(Dim.9 > 0.03 | Dim.10 > 0.03) |>
  gt()


# Visualisation des coefficients de correlation ----

res_PCA$var$cor |>
  as.data.frame() |>
  bind_rows(
    res_PCA$quanti.sup$cor |>
      as.data.frame()
  ) |>
  select(1:10) |>
  mutate(
    across(everything(), ~ round(.x, 2)),
    sum = rowSums(across(everything()))
  ) |>
  rownames_to_column("Variable") |>
  mutate(
    Variable = str_remove(Variable, "pourc_") |>
      str_replace_all("_", " ") |>
      str_to_title(),
    Variable = case_when(
      Variable == "Empl Precaire Pourc" ~ "Emploi précaire (%)",
      Variable == "Empl Stable Pourc" ~ "Emploi stable (%)",
      Variable == "Empl Indep Pourc" ~ "Emploi indépendant (%)",
      Variable == "Empl Employeurs Pourc" ~ "Emploi employeurs (%)",
      Variable == "Age Q1" ~ "Âge Q1",
      Variable == "Age Q2" ~ "Âge Q2",
      Variable == "Age Q3" ~ "Âge Q3",
      Variable == "Prop H" ~ "Proportion d'hommes",
      Variable == "Cs Empl Pourc" ~ "Employés (%)",
      Variable == "Cs Ouvr Pourc" ~ "Ouvriers (%)",
      Variable == "Cs Agri Pourc" ~ "Agriculteurs (%)",
      Variable == "Cs Acce Pourc" ~ "Chefs d'entreprise (%)",
      Variable == "Cs Cpis Pourc" ~ "Cadres et PI supérieures (%)",
      Variable == "Cs Pi Pourc" ~ "Professions intermédiaires (%)",
      Variable == "Log Vacants Pourc" ~ "Logements vacants (%)",
      Variable == "Log Resprinc Pourc" ~ "Logements principaux (%)",
      Variable == "Log Ressecond Pourc" ~ "Logements secondaires (%)",
      Variable == "Log Logoccas Pourc" ~ "Logements occasionnels (%)",
      Variable == "Immi Pourc" ~ "Immigrés (%)",
      Variable == "Dens.7" ~ "Rural à habitat très dispersé",
      Variable == "Dens.6" ~ "Rural à habitat dispersé",
      Variable == "Dens.5" ~ "Bourgs ruraux",
      TRUE ~ Variable
    )
  ) |>
  select(1:11) |>
  gt() |>
  gtsave(file = "Coefficients de corrélation des variables.docx")

# Visualisation des axes ----

coord <- res_PCA$var$coord |>
  as.data.frame() |>
  rownames_to_column("Variable") |>
  mutate(type = "Active") |>
  bind_rows(
    res_PCA$quali.sup$coord |>
      as.data.frame() |>
      rownames_to_column("Variable") |>
      mutate(type = "Supplémentaire")
  ) |>
  bind_rows(
    res_PCA$quanti.sup$coord |>
      as.data.frame() |>
      rownames_to_column("Variable") |>
      mutate(type = "Supplémentaire")
  ) |>
  mutate(
    Variable = str_remove(Variable, "pourc_") |>
      str_replace_all("_", " ") |>
      str_to_title(),
    Variable = case_when(
      Variable == "Empl Precaire Pourc" ~ "Emploi précaire (%)",
      Variable == "Empl Stable Pourc" ~ "Emploi stable (%)",
      Variable == "Empl Indep Pourc" ~ "Emploi indépendant (%)",
      Variable == "Empl Employeurs Pourc" ~ "Emploi employeurs (%)",
      Variable == "Age Q1" ~ "Âge Q1",
      Variable == "Age Q2" ~ "Âge Q2",
      Variable == "Age Q3" ~ "Âge Q3",
      Variable == "Prop H" ~ "Proportion d'hommes",
      Variable == "Cs Empl Pourc" ~ "Employés (%)",
      Variable == "Cs Ouvr Pourc" ~ "Ouvriers (%)",
      Variable == "Cs Agri Pourc" ~ "Agriculteurs (%)",
      Variable == "Cs Acce Pourc" ~ "Chefs d'entreprise (%)",
      Variable == "Cs Cpis Pourc" ~ "Cadres et PI supérieures (%)",
      Variable == "Cs Pi Pourc" ~ "Professions intermédiaires (%)",
      Variable == "Log Vacants Pourc" ~ "Logements vacants (%)",
      Variable == "Log Resprinc Pourc" ~ "Logements principaux (%)",
      Variable == "Log Ressecond Pourc" ~ "Logements secondaires (%)",
      Variable == "Log Logoccas Pourc" ~ "Logements occasionnels (%)",
      Variable == "Immi Pourc" ~ "Immigrés (%)",
      Variable == "Dens.7" ~ "Rural à habitat très dispersé",
      Variable == "Dens.6" ~ "Rural à habitat dispersé",
      Variable == "Dens.5" ~ "Bourgs ruraux",
      TRUE ~ Variable
    )
  )

# Axe 1:2 ----
lim = .7

coord |>
  ggplot(aes(x = Dim.1, y = Dim.2, label = Variable, color = type)) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_vline(xintercept = 0, color = "grey") +
  geom_point(size = 2, show.legend = FALSE) +
  geom_segment(
    data = coord |>
      filter(
        (Dim.1 > lim | Dim.1 < -lim | Dim.2 > lim | Dim.2 < -lim) &
          !grepl("^Code Region", Variable)
      ),
    aes(xend = 0, yend = 0),
    arrow = arrow(length = unit(0.30, "cm"), ends = "first", type = "closed"),
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = coord |>
      filter(
        (grepl("^Code Region", Variable) &
           (abs(Dim.1) > 2 * lim | abs(Dim.2) > 2 * lim)) |
          (!grepl("^Code Region", Variable) &
             (abs(Dim.1) > lim | abs(Dim.2) > lim))
      ) |>
      mutate(
        type = ifelse(
          Variable %in%
            c(
              "Bourgs ruraux",
              "Rural à habitat dispersé",
              "Rural à habitat très dispersé"
            ),
          "Densité",
          type
        )
      ),
    aes(
      x = Dim.1,
      y = Dim.2,
      label = Variable,
      colour = type
    ),
    inherit.aes = FALSE,
    max.overlaps = Inf,
    max.iter = 100000,
    max.time = 10,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20,
    force = 10,
  ) +
  labs(
    x = "Axe 1",
    y = "Axe 2",
    color = "Type de variable"
  ) +
  scale_color_manual(
    name = "Type de variable",
    values = c(
      "Densité" = "green",
      "Active" = "red",
      "Supplémentaire" = "blue"
    ),
    labels = c("Densité", "Active", "Supplémentaire")
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    plot.title = element_text(hjust = 0.5)
  )
ggsave("PCA_axes1_2_2017.png", width = 15, height = 10, dpi = 1000)

# Axe 3:4 ----
lim = .3

coord |>
  ggplot(aes(x = Dim.3, y = Dim.4, label = Variable, color = type)) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_vline(xintercept = 0, color = "grey") +
  geom_point(size = 2, show.legend = FALSE) +
  geom_segment(
    data = coord |>
      filter(
        (Dim.3 > lim | Dim.3 < -lim | Dim.4 > lim | Dim.4 < -lim) &
          !grepl("^Code Region", Variable)
      ),
    aes(xend = 0, yend = 0),
    arrow = arrow(length = unit(0.30, "cm"), ends = "first", type = "closed"),
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = subset(
      coord,
      Dim.3 > lim | Dim.3 < -lim | Dim.4 > lim | Dim.4 < -lim
    ) |>
      mutate(
        type = ifelse(
          Variable %in%
            c(
              "Bourgs ruraux",
              "Rural à habitat dispersé",
              "Rural à habitat très dispersé"
            ),
          "Densité",
          type
        )
      ),
    aes(
      x = Dim.3,
      y = Dim.4,
      label = Variable,
      colour = type
    ),
    inherit.aes = FALSE,
    max.overlaps = Inf,
    max.iter = 100000,
    max.time = 10,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20,
    force = 10,
  ) +
  labs(
    x = "Axe 3",
    y = "Axe 4",
    color = "Type de variable"
  ) +
  scale_color_manual(
    name = "Type de variable",
    values = c(
      "Densité" = "green",
      "Active" = "red",
      "Supplémentaire" = "blue"
    ),
    labels = c("Densité", "Active", "Supplémentaire")
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    plot.title = element_text(hjust = 0.5)
  )
ggsave("PCA_axes3_4_2017.png", width = 15, height = 10, dpi = 1000)

# Axe 1:4 ----

lim = .5
coord |>
  ggplot(aes(x = Dim.1, y = Dim.4, label = Variable, color = type)) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_vline(xintercept = 0, color = "grey") +
  geom_point(size = 2, show.legend = FALSE) +
  geom_segment(
    data = coord |>
      filter(
        (Dim.1 > lim | Dim.1 < -lim | Dim.4 > lim | Dim.4 < -lim) &
          !grepl("^Code Region", Variable)
      ),
    aes(xend = 0, yend = 0),
    arrow = arrow(length = unit(0.30, "cm"), ends = "first", type = "closed"),
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = subset(
      coord,
      Dim.1 > lim | Dim.1 < -lim | Dim.4 > lim | Dim.4 < -lim
    ) |>
      mutate(
        type = ifelse(
          Variable %in%
            c(
              "Bourgs ruraux",
              "Rural à habitat dispersé",
              "Rural à habitat très dispersé"
            ),
          "Densité",
          type
        )
      ),
    aes(
      x = Dim.1,
      y = Dim.4,
      label = Variable,
      colour = type
    ),
    inherit.aes = FALSE,
    max.overlaps = Inf,
    max.iter = 100000,
    max.time = 10,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20,
    force = 10,
  ) +
  labs(
    x = "Axe 1",
    y = "Axe 4",
    color = "Type de variable"
  ) +
  scale_color_manual(
    name = "Type de variable",
    values = c(
      "Densité" = "green",
      "Active" = "red",
      "Supplémentaire" = "blue"
    ),
    labels = c("Densité", "Active", "Supplémentaire")
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    plot.title = element_text(hjust = 0.5)
  )

ggsave("PCA_axes1_4.png", width = 15, height = 10, dpi = 1000)

# Axe 2:5 ----

lim = .3

coord |>
  ggplot(aes(x = Dim.2, y = Dim.5, label = Variable, color = type)) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_vline(xintercept = 0, color = "grey") +
  geom_point(size = 2, show.legend = FALSE) +
  geom_segment(
    data = coord |>
      filter(
        (Dim.2 > lim | Dim.2 < -lim | Dim.5 > lim | Dim.5 < -lim) &
          !grepl("^Code Region", Variable)
      ),
    aes(xend = 0, yend = 0),
    arrow = arrow(length = unit(0.30, "cm"), ends = "first", type = "closed"),
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = subset(
      coord,
      Dim.2 > lim | Dim.2 < -lim | Dim.5 > lim | Dim.5 < -lim
    ) |>
      mutate(
        type = ifelse(
          Variable %in%
            c(
              "Bourgs ruraux",
              "Rural à habitat dispersé",
              "Rural à habitat très dispersé"
            ),
          "Densité",
          type
        )
      ),
    aes(
      x = Dim.2,
      y = Dim.5,
      label = Variable,
      colour = type
    ),
    inherit.aes = FALSE,
    max.overlaps = Inf,
    max.iter = 100000,
    max.time = 10,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20,
    force = 10,
  ) +
  labs(
    x = "Axe 2",
    y = "Axe 5",
    color = "Type de variable"
  ) +
  scale_color_manual(
    name = "Type de variable",
    values = c(
      "Densité" = "green",
      "Active" = "red",
      "Supplémentaire" = "blue"
    ),
    labels = c("Densité", "Active", "Supplémentaire")
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    plot.title = element_text(hjust = 0.5)
  )

ggsave("PCA_axes2_5.png", width = 15, height = 10, dpi = 1000)


# Axe 1:3 ----
lim = .5
coord |>
  ggplot(aes(x = Dim.1, y = Dim.3, label = Variable, color = type)) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_vline(xintercept = 0, color = "grey") +
  geom_point(size = 2, show.legend = FALSE) +
  geom_segment(
    data = coord |>
      filter(
        (Dim.1 > lim | Dim.1 < -lim | Dim.3 > lim | Dim.3 < -lim) &
          !grepl("^Code Region", Variable)
      ),
    aes(xend = 0, yend = 0),
    arrow = arrow(length = unit(0.30, "cm"), ends = "first", type = "closed"),
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = subset(
      coord,
      Dim.1 > lim | Dim.1 < -lim | Dim.3 > lim | Dim.3 < -lim
    ) |>
      mutate(
        type = ifelse(
          Variable %in%
            c(
              "Bourgs ruraux",
              "Rural à habitat dispersé",
              "Rural à habitat très dispersé"
            ),
          "Densité",
          type
        )
      ),
    aes(
      x = Dim.1,
      y = Dim.3,
      label = Variable,
      colour = type
    ),
    inherit.aes = FALSE,
    max.overlaps = Inf,
    max.iter = 100000,
    max.time = 10,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20,
    force = 10,
  ) +
  labs(
    x = "Axe 1",
    y = "Axe 3",
    color = "Type de variable"
  ) +
  scale_color_manual(
    name = "Type de variable",
    values = c(
      "Active" = "red",
      "Densité" = "green",
      "Supplémentaire" = "blue"
    ),
    labels = c("Active", "Densité", "Supplémentaire")
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    plot.title = element_text(hjust = 0.5)
  )

ggsave("PCA_axes1_3_2017.png", width = 15, height = 10, dpi = 1000)

# Axe 5:6 ----

lim = .2
coord |>
  ggplot(aes(x = Dim.5, y = Dim.6, label = Variable, color = type)) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_vline(xintercept = 0, color = "grey") +
  geom_point(size = 2, show.legend = FALSE) +
  geom_segment(
    data = coord |>
      filter(
        (Dim.5 > lim | Dim.5 < -lim | Dim.6 > lim | Dim.6 < -lim) &
          !grepl("^Code Region", Variable)
      ),
    aes(xend = 0, yend = 0),
    arrow = arrow(length = unit(0.30, "cm"), ends = "first", type = "closed"),
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = subset(
      coord,
      Dim.5 > lim | Dim.5 < -lim | Dim.6 > lim | Dim.6 < -lim
    ) |>
      mutate(
        type = ifelse(
          Variable %in%
            c(
              "Bourgs ruraux",
              "Rural à habitat dispersé",
              "Rural à habitat très dispersé"
            ),
          "Densité",
          type
        )
      ),
    aes(
      x = Dim.5,
      y = Dim.6,
      label = Variable,
      colour = type
    ),
    inherit.aes = FALSE,
    max.overlaps = Inf,
    max.iter = 100000,
    max.time = 10,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20,
    force = 10,
  ) +
  labs(
    x = "Axe 5",
    y = "Axe 6",
    color = "Type de variable"
  ) +
  scale_color_manual(
    name = "Type de variable",
    values = c(
      "Densité" = "green",
      "Active" = "red",
      "Supplémentaire" = "blue"
    ),
    labels = c("Densité", "Active", "Supplémentaire")
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    plot.title = element_text(hjust = 0.5)
  )
ggsave("PCA_axes5_6.png", width = 15, height = 10, dpi = 1000)

# Axe 7:8 ----
lim = .2

coord |>
  ggplot(aes(x = Dim.7, y = Dim.8, label = Variable, color = type)) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_vline(xintercept = 0, color = "grey") +
  geom_point(size = 2, show.legend = FALSE) +
  geom_segment(
    data = coord |>
      filter(
        (Dim.7 > lim | Dim.7 < -lim | Dim.8 > lim | Dim.8 < -lim) &
          !grepl("^Code Region", Variable)
      ),
    aes(xend = 0, yend = 0),
    arrow = arrow(length = unit(0.30, "cm"), ends = "first", type = "closed"),
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = subset(
      coord,
      (Dim.7 > lim | Dim.7 < -lim | Dim.8 > lim | Dim.8 < -lim) &
        !grepl("^Code Region", Variable)
    ) |>
      mutate(
        type = ifelse(
          Variable %in%
            c(
              "Bourgs ruraux",
              "Rural à habitat dispersé",
              "Rural à habitat très dispersé"
            ),
          "Densité",
          type
        )
      ),
    aes(
      x = Dim.7,
      y = Dim.8,
      label = Variable,
      colour = type
    ),
    inherit.aes = FALSE,
    max.overlaps = Inf,
    max.iter = 100000,
    max.time = 10,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20,
    force = 10,
  ) +
  labs(
    x = "Axe 7",
    y = "Axe 8",
    color = "Type de variable"
  ) +
  scale_color_manual(
    name = "Type de variable",
    values = c(
      "Densité" = "green",
      "Active" = "red",
      "Supplémentaire" = "blue"
    ),
    labels = c("Densité", "Active", "Supplémentaire")
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    plot.title = element_text(hjust = 0.5)
  )

ggsave("PCA_axes7_8.png", width = 15, height = 10, dpi = 1000)

# Axe 9:10 ----
lim = .2

coord |>
  ggplot(aes(x = Dim.9, y = Dim.10, label = Variable, color = type)) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_vline(xintercept = 0, color = "grey") +
  geom_point(size = 2, show.legend = FALSE) +
  geom_segment(
    data = coord |>
      filter(
        (Dim.9 > lim | Dim.9 < -lim | Dim.10 > lim | Dim.10 < -lim) &
          !grepl("^Code Region", Variable)
      ),
    aes(xend = 0, yend = 0),
    arrow = arrow(length = unit(0.30, "cm"), ends = "first", type = "closed"),
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = subset(
      coord,
      Dim.9 > lim | Dim.9 < -lim | Dim.10 > lim | Dim.10 < -lim
    ) |>
      mutate(
        type = ifelse(
          Variable %in%
            c(
              "Bourgs ruraux",
              "Rural à habitat dispersé",
              "Rural à habitat très dispersé"
            ),
          "Densité",
          type
        )
      ),
    aes(
      x = Dim.9,
      y = Dim.10,
      label = Variable,
      colour = type
    ),
    inherit.aes = FALSE,
    max.overlaps = Inf,
    max.iter = 100000,
    max.time = 10,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20,
    force = 10,
  ) +
  labs(
    x = "Axe 9",
    y = "Axe 10",
    color = "Type de variable"
  ) +
  scale_color_manual(
    name = "Type de variable",
    values = c(
      "Densité" = "green",
      "Active" = "red",
      "Supplémentaire" = "blue"
    ),
    labels = c("Densité", "Active", "Supplémentaire")
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    plot.title = element_text(hjust = 0.5)
  )

ggsave("PCA_axes9_10.png", width = 15, height = 10, dpi = 1000)

# 1. Charger les librairies nécessaires
library(FactoMineR)
library(factoextra)
library(dplyr)
library(ade4)
library(questionr)

# 2. Extraire les coordonnées des individus sur les 15 premières dimensions
ind_coord <- res_PCA$ind$coord[, 1:5]

# 3. Calcul de la distance euclidienne
d <- dist(ind_coord)

# 4. Réalisation de la CAH (méthode de Ward)
cah <- hclust(d, method = "ward.D2")

# 5. Affichage du dendrogramme
plot(cah, labels = FALSE, hang = -1, main = "Dendrogramme de la CAH")

# 6. Découpage en k groupes (ex: 4)
groupes <- cutree(cah, k = 4)

# 7. Ajout des groupes à la base de données
PresFrural$Groupe_CAH <- groupes

# 8. Visualisation des groupes sur le plan factoriel
fviz_pca_ind(res_PCA,
             habillage = groupes,
             palette = "jco",
             addEllipses = TRUE,
             ellipse.level = 0.95,
             repel = TRUE)
