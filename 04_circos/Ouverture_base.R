
# library(haven)
# setwd("~/Dropbox/enseignements/Enseignements_2024_2025/Ens_Sociologie_electorale/Circos/")
# 
# d <- read_dta("base_generale_INSEE.dta")

str(d, list.len=ncol(d))
View(d)

## Et c'est parti ... 
# Graphique lien chaudière au fioul / vote RN au 1er tour législative 2022 
hist (d$voix_FN_SurInscrits_Leg_1_2022)
# mfule : " Part des maisons chauffés au fuel (en % des résidences principales) " 
d$mfuel [d$mfuel %in% c("nd") ] <- "." 
hist(d$mfuel)
d$mfuel <- as.numeric(d$mfuel)
hist(d$mfuel)

plot (d$mfuel , d$voix_FN_SurInscrits_Leg_1_2022 ) # Serait à habiller ... 






