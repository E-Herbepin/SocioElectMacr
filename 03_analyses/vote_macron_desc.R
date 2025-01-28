### Premières analyses.

## On crée une variable pour les voix Macron en pourcentage des votes exprimés.

# Pour 2017

t <- t %>%
  mutate(pourc_macron_17 = Nb_17_Voix_Macron * 100 / Nb_17_Exprimés)

# Pour 2022

t <- t %>%
  mutate(pourc_macron_22 = Nb_22_Voix_Macron * 100 / Nb_22_Exprimés)

## On crée une variable qui calcule la différence de pourcentage entre les deux.

t <- t %>% 
  mutate(diff_pourc_macron = pourc_macron_22 - pourc_macron_17)

t <- t %>%
  mutate(pourc_LP_17 = Nb_17_Voix_Le_Pen * 100 / Nb_17_Exprimés)

# Pour 2022

t <- t %>%
  mutate(pourc_LP_22 = Nb_22_Voix_Le_Pen * 100 / Nb_22_Exprimés)

## On crée une variable qui calcule la différence de pourcentage entre les deux.

t <- t %>% 
  mutate(diff_pourc_LP = pourc_LP_22 - pourc_LP_17)


Densite_vote <- t %>%group_by(dens)%>% 
  summarise(diffEM=mean(diff_pourc_macron, na.rm=TRUE),diffLP=mean(diff_pourc_LP, na.rm=TRUE), hab=sum(pmun21), votants22=sum(Nb_22_Votants), votants17=sum(Nb_17_Votants), abst17=mean(Nb_17_Abstentions/Nb_17_Inscrits),abst22=mean(Nb_22_Abstentions/Nb_22_Inscrits))

summary(Pres$diff_pourc_macron)

names(Pres)
names(communes)

match <- t

match <- match %>%
  filter(!is.na(Libellé_région))

match <- match %>%
  filter(!Libellé_région %in% c("Guyane", "La Réunion", "Martinique", "Guadeloupe"))

boxplot(match$diff_pourc_macron)

match %>%
  filter(dens %in% c(6, 5, 4)) %>%
  ggplot(aes(y = diff_pourc_macron)) +
  geom_boxplot() +
  labs(title = "Boxplot de diff_pourc_macron", y = "Différence pourcentage Macron")

match %>%
  filter(dens %in% c(6, 5, 4)) %>%
  summarise(
    moyenne = mean(diff_pourc_macron, na.rm = TRUE),
    mediane = median(diff_pourc_macron, na.rm = TRUE),
    quartiles = list(quantile(diff_pourc_macron, na.rm = TRUE))
  )

unique(match$Libellé_région)


