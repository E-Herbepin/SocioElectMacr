### Premières analyses.

## On crée une variable pour les voix Macron en pourcentage des votes exprimés.

# Pour 2017

match <- match %>%
PresF <- PresF %>%
  mutate(pourc_macron_17 = Nb_17_Voix_Macron * 100 / Nb_17_Exprimés)

# Pour 2022

PresF <- PresF %>%
  mutate(pourc_macron_22 = Nb_22_Voix_Macron * 100 / Nb_22_Exprimés)

## On crée une variable qui calcule la différence de pourcentage entre les deux.

PresF <- PresF %>% 
  mutate(diff_pourc_macron = pourc_macron_22 - pourc_macron_17)

# Moyenne, médiane, quartiles de l'ensemble.

summary(PresF$diff_pourc_macron)

# Moyenne, médiane, quintiles de communes rurales (dens = 7, 6 ,5)

PresF %>%
  filter(dens %in% c(1, 2, 3, 4)) %>%
  summarise(
    moyenne = mean(diff_pourc_macron, na.rm = TRUE),
    mediane = median(diff_pourc_macron, na.rm = TRUE),
    quartiles = list(quantile(diff_pourc_macron, na.rm = TRUE))
  )

PresF %>%
  filter(dens %in% c(7, 6, 5)) %>%
  summarise(
    moyenne = mean(diff_pourc_macron, na.rm = TRUE),
    mediane = median(diff_pourc_macron, na.rm = TRUE),
    quartiles = list(quantile(diff_pourc_macron, na.rm = TRUE))
  )

PresF %>%
  filter(dens %in% c(7)) %>%
  summarise(
    moyenne = mean(diff_pourc_macron, na.rm = TRUE),
    mediane = median(diff_pourc_macron, na.rm = TRUE),
    quartiles = list(quantile(diff_pourc_macron, na.rm = TRUE))
  )

PresF %>%
  filter(dens %in% c(6)) %>%
  summarise(
    moyenne = mean(diff_pourc_macron, na.rm = TRUE),
    mediane = median(diff_pourc_macron, na.rm = TRUE),
    quartiles = list(quantile(diff_pourc_macron, na.rm = TRUE))
  )

PresF %>%
  filter(dens %in% c(5)) %>%
  summarise(
    moyenne = mean(diff_pourc_macron, na.rm = TRUE),
    mediane = median(diff_pourc_macron, na.rm = TRUE),
    quartiles = list(quantile(diff_pourc_macron, na.rm = TRUE))
  )

# Boxplot

PresF %>%
  filter(dens %in% c(7, 6, 5)) %>%
  ggplot(aes(y = diff_pourc_macron)) +
  geom_boxplot()

# Histogramme.

PresF %>%
  filter(dens %in% c(7, 6, 5)) %>%
  ggplot(aes(x = diff_pourc_macron)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black")