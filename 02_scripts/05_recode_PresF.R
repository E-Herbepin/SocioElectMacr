### Ce script récupère tous les recodages et variables construites


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
    
    dens = as.factor(PresF$dens))

