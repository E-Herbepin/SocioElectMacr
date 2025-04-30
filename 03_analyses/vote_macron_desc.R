### Premières analyses.

source("02_scripts/00_setup.R")
source("02_scripts/01_import.R")

## On crée une variable pour les voix Macron en pourcentage des votes exprimés.

# Pour 2017

PresF <- PresF %>%
  mutate(pourc_macron_17 = Nb_17_Voix_Macron * 100 / Nb_17_Exprimés)

# Pour 2022

PresF <- PresF %>%
  mutate(pourc_macron_22 = Nb_22_Voix_Macron * 100 / Nb_22_Exprimés)

## On crée une variable qui calcule la différence de pourcentage entre les deux.

PresF <- PresF %>% 
  mutate(diff_pourc_macron = pourc_macron_22 - pourc_macron_17)

summary(PresF$diff_pourc_macron)
mean(PresF$diff_pourc_macron, na.rm = TRUE)

barplot(PresF$diff_pourc_macron)

PresF$dens<-as.factor(PresF$dens)

PresF %>%
  filter(diff_pourc_macron <= 50) %>%
  ggplot(aes(x = round(diff_pourc_macron,0),  group = dens, fill = dens)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = summary(PresF$diff_pourc_macron, na.rm = T)[c(1:3,5,6)], color="red") +
  geom_vline(xintercept = mean(PresF$diff_pourc_macron, na.rm = T), color="blue") +
  scale_x_continuous(breaks = seq(-50, 50, 5), limits = c(-50, 50)) +
  geom_bar() +
labs(title = "diff_pourc_macron", y = "Différence pourcentage Macron")


PresF %>%
  filter(diff_pourc_macron <= 50 & !is.na(diff_pourc_macron) & !is.na(dens)) %>%
  ggplot(aes(x = round(diff_pourc_macron,0),  group = dens, color = dens)) +
  geom_boxplot(alpha=.7) 




PresF %>%
  filter(diff_pourc_macron <= 50 & !is.na(diff_pourc_macron) & !is.na(empl_stable_pourc)) %>%
  ggplot(aes(x = empl_stable_pourc))+
  geom_bar(width = 5)

summary(PresF$empl_stable_pourc)
quantile(PresF$empl_stable_pourc, probs = seq(0,1,0.1), na.rm = TRUE)


PresF[PresF$empl_stable_pourc < 50,"empl_stable_pourc"] %>%
length()


PresF %>%
  mutate(decile = as.factor(ntile(empl_stable_pourc, 10))) %>%
  filter(diff_pourc_macron <= 50 & !is.na(diff_pourc_macron) & !is.na(empl_stable_pourc)) %>%
  ggplot(aes(x = round(diff_pourc_macron,0), group = decile, color = decile)) +
  geom_density()

  

# en fonction de la stabilité de l'emploi
PresF %>%
  mutate(decile = as.factor(ntile(empl_stable_pourc, 10))) %>%
  filter(diff_pourc_macron <= 50 & !is.na(diff_pourc_macron) & !is.na(empl_stable_pourc)) %>%
  select(decile, diff_pourc_macron) %>%
  group_by(decile) %>%
  summarise(moyenne = mean(diff_pourc_macron))

#Plus la proportion d'emploi stable est élevée, plus l'augmentation du vote macron est forte. 



# en fonction de la proportion de cadres
PresF %>%
  mutate(decile = as.factor(ntile(cs_cpis_pourc, 10))) %>%
  filter(diff_pourc_macron <= 50 & !is.na(diff_pourc_macron) & !is.na(cs_cpis_pourc)) %>%
  select(decile, diff_pourc_macron) %>%
  group_by(decile) %>%
  summarise(moyenne = mean(diff_pourc_macron)) %>%
  ggplot(aes(x = decile, y = moyenne)) +
  geom_point(shape=21, color="black", fill="#69b3a2", size=2) +
  labs(title = "Evolution du vote Macron en fonction de la proportion de cadre",
       x = "Décile de la proportion de cadre",
       y = "Point de pourcentage gagné en moyenne par Macron entre 2017 et 2022")

tab<-rbind(
PresF %>%
  mutate(decile = as.factor(ntile(cs_cpis_pourc, 10))) %>%
  filter(diff_pourc_macron <= 50 & !is.na(diff_pourc_macron)) %>%
  select(decile, diff_pourc_macron) %>%
  group_by(decile) %>%
  summarise(moyenne = mean(diff_pourc_macron)) %>%
  mutate(CSP = "Cadres et professions intellectuelles supérieures") %>%
  filter(decile %in% c(1:10)),

PresF %>%
  mutate(decile = as.factor(ntile(cs_ouvr_pourc, 10))) %>%
  filter(diff_pourc_macron <= 50 & !is.na(diff_pourc_macron)) %>%
  select(decile, diff_pourc_macron) %>%
  group_by(decile) %>%
  summarise(moyenne = mean(diff_pourc_macron)) %>%
  mutate(CSP = "Ouvriers"),

PresF %>%
  mutate(decile = as.factor(ntile(cs_empl_pourc, 10))) %>%
  filter(diff_pourc_macron <= 50 & !is.na(diff_pourc_macron)) %>%
  select(decile, diff_pourc_macron) %>%
  group_by(decile) %>%
  summarise(moyenne = mean(diff_pourc_macron)) %>%
  mutate(CSP = "Employés"),

PresF %>%
  mutate(cs_pi_pourc = cs_pi_n*100/cs_tot,
    decile = as.factor(ntile(cs_pi_pourc, 10))) %>%
  filter(diff_pourc_macron <= 50 & !is.na(diff_pourc_macron)) %>%
  select(decile, diff_pourc_macron) %>%
  group_by(decile) %>%
  summarise(moyenne = mean(diff_pourc_macron)) %>%
  mutate(CSP = "Professions Intermédiaires"),

PresF %>%
  mutate(cs_agri_pourc = cs_agri_n*100/cs_tot,
         decile = as.factor(ntile(cs_agri_pourc, 10))) %>%
  filter(diff_pourc_macron <= 50 & !is.na(diff_pourc_macron)) %>%
  select(decile, diff_pourc_macron) %>%
  group_by(decile) %>%
  summarise(moyenne = mean(diff_pourc_macron)) %>%
  mutate(CSP = "Agriculteurs exploitants"),

PresF %>%
  mutate(cs_acce_pourc = cs_acce_n*100/cs_tot,
         decile = as.factor(ntile(cs_acce_pourc, 10))) %>%
  filter(diff_pourc_macron <= 50 & !is.na(diff_pourc_macron)) %>%
  select(decile, diff_pourc_macron) %>%
  group_by(decile) %>%
  summarise(moyenne = mean(diff_pourc_macron)) %>%
  mutate(CSP = "Artisans, commerçants et chefs d'entreprise") ,

PresF %>%
  mutate(cs_acce_pourc = cs_acce_n*100/cs_tot,
         decile = as.factor(ntile(cs_acce_pourc, 10))) %>%
  filter(diff_pourc_macron <= 50 & !is.na(diff_pourc_macron)) %>%
  select(decile, diff_pourc_macron) %>%
  group_by(decile) %>%
  summarise(moyenne = mean(diff_pourc_macron)) %>%
  mutate(CSP = "Artisans, commerçants et chefs d'entreprise") 
) %>%
  filter(decile %in% c(1:10)) %>%
  mutate(cat = factor(case_when(
    CSP %in% c("Employés","Artisans, commerçants et chefs d'entreprise") ~ "Diminue",
    CSP %in% c("Cadres et professions intellectuelles supérieures","Professions Intermédiaires", "Agriculteurs exploitants") ~ "Stagne",
    CSP %in% c("Ouvriers") ~ "Augmente"
    ), ordered = TRUE
  ))
  



ggplot(tab, aes(x = decile, y = moyenne, group = CSP, fill = CSP, colour = CSP)) +
  facet_grid(cols = vars(cat)) +
  geom_point(shape=21, size=2) +
  geom_line() +
  labs(title = "Evolution du vote Macron en fonction des proportions de CSP",
       x = "Décile de la proportion de chaque CSP",
       y = "Point de pourcentage gagné en moyenne par Macron entre 2017 et 2022",
       caption = "Lectures : 
       1: Le vote Macron des 10% des villes qui possèdent le plus d'employés a augmenté, en moyenne, de 3.8 points de pourcentage.
       2: Plus la proportion d'ouvrier augmente dans une ville, plus l'augmentation du vote Macron entre 2017 et 2022 est forte.
       3: La proportion de professions intermédiaires dans une commune semble peu impacter l'augmentation du vote pour Macron.") + 
  guides(colour = "none")+ 
  theme(plot.caption = element_text(hjust = 0))
