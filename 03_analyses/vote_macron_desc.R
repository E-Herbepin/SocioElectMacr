### Premières analyses.

## On crée une variable pour les voix Macron en pourcentage des votes exprimés.

# Pour 2017

Pres <- Pres %>%
  mutate(pourc_macron_17 = Nb_17_Voix_Macron * 100 / Nb_17_Exprimés)

# Pour 2022

Pres <- Pres %>%
  mutate(pourc_macron_22 = Nb_22_Voix_Macron * 100 / Nb_22_Exprimés)

## On crée une variable qui calcule la différence de pourcentage entre les deux.

Pres <- Pres %>% 
  mutate(diff_pourc_macron = pourc_macron_22 - pourc_macron_17)

summary(Pres$diff_pourc_macron)

boxplot(Pres$diff_pourc_macron)

Pres %>%
  filter(dens %in% c(6, 5, 4)) %>%
  ggplot(aes(y = diff_pourc_macron)) +
  geom_boxplot() +
  labs(title = "Boxplot de diff_pourc_macron", y = "Différence pourcentage Macron")

Pres %>%
  filter(dens %in% c(6, 5, 4)) %>%
  summarise(
    moyenne = mean(diff_pourc_macron, na.rm = TRUE),
    mediane = median(diff_pourc_macron, na.rm = TRUE),
    quartiles = list(quantile(diff_pourc_macron, na.rm = TRUE))
  )