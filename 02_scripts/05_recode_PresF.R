### Ce script récupère tous les recodages et variables construites

PresF <- PresF %>%
  mutate(
    ## On crée une variable pour les voix Macron en pourcentage des votes exprimés.
    across(
      matches("^Nb_(17|22)_Voix_"),
      ~ {
        year <- sub("^Nb_(\\d+)_Voix_.+$", "\\1", cur_column())
        .x * 100 / get(paste0("Nb_", year, "_Exprimés"))
      },
      .names = "temp_{.col}"
    ),

    pourc_abstention_22 = Nb_22_Abstentions * 100 / Nb_22_Inscrits,
    pourc_abstention_17 = Nb_17_Abstentions * 100 / Nb_17_Inscrits,

    immi_pourc = immi_oui * 100 / sum(immi_oui, immi_non, na.rm = TRUE),

    dens = as.factor(PresF$dens)
  ) %>%
  # Renommez les colonnes selon le motif souhaité
  rename_with(
    ~ sub('^temp_Nb_(\\d+)_Voix_(.+)$', 'pourc_\\2_\\1', .x),
    .cols = matches("^temp_Nb_")
  ) |>
  mutate(
    diff_pourc_abstentions = pourc_abstention_22 - pourc_abstention_17,
    diff_pourc_Macron = pourc_Macron_22 - pourc_Macron_17,
    diff_pourc_Le_Pen = pourc_Le_Pen_22 - pourc_Le_Pen_17,
    diff_pourc_Mélenchon = pourc_Mélenchon_22 - pourc_Mélenchon_17,
    diff_pourc_Lr = pourc_Pécresse_22 - pourc_Fillon_17,
    diff_pourc_PS = pourc_Hidalgo_22 - pourc_Hamon_17,
    diff_pourc_Dupont_Aignan = pourc_Dupont.Aignan_22 -
      pourc_Dupont.Aignan_17,
    diff_pourc_Lassalle = pourc_Lassalle_22 - pourc_Lassalle_17,
    diff_pourc_Arthaud = pourc_Arthaud_22 - pourc_Arthaud_17,
    diff_pourc_Poutou = pourc_Poutou_22 - pourc_Poutou_17
  )
