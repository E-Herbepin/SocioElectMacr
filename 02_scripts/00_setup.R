#### PACKAGES ET OPTIONS #####

# PACKAGES ----

library(stringi)
library(stringdist)
library(tidyverse) # pour la manipulation des données
library(magrittr) # pour les pipes
library(questionr) # pour la description des variables
library(janitor) # pour des outils de description
library(here)
library(knitr)
library(rmarkdown)
library(printr)

library(haven)
library(readxl)

library(labelled)
library(scales)
library(Hmisc)
# library(kableExtra)

library(FactoMineR) # pour l'analyse géométrique des données
library(factoextra)
library(cluster)
library(RColorBrewer) # pour les palettes de couleurs
library(ggrepel) # pour que les libellés des points ne se chevauchent pas (crucial !)
library(flextable) # pour mettre en forme les tableaux
library(gtsummary) # pour les tableaux


library(mapsf)
library(sf)
library(mapview)
library(biscale)
library(cowplot)

# OPTIONS ----

set_flextable_defaults(decimal.mark = ",", big.mark = " ", na_st = "-")

options(OutDec= ",")

options(scipen=999) # pour désactiver l'écriture scientifique des nombres

# on indique à gtsummary un affichage en français
theme_gtsummary_language(
  "fr",
  decimal.mark = ",", # séparateur de décimales
  big.mark = " " # séparateur de milliers
)

# pour empêcher knitr d'afficher les messages et les warnings dans les .Rmd et .qmd

knitr::opts_chunk$set(
  warning = FALSE, 
  message = FALSE
) 

# PALETTES ----

pal_cereq <- c("#008B99", "#EF5350", "#256299", "#F8AC00", "#7B9A62", "#B88DC5")
