#### ANALYSE GEOMETRIQUE DES RESULTATS ELECTORAUX ----

# SELECTION DES VARIABLES ----

names(europ24)

set.seed(80)

europ24_sample <- europ24 %>% 
  sample_n(size = 20000)

acp_e24 <- europ24_sample %>% 
  select(inscrits,
         abstention_pourc,
         starts_with("res") & ends_with("pourc")) %>%
  select(-res_lo_pourc, -res_npar_pourc, -res_pt_pourc,
         -res_autres_pourc) %>%
  mutate(inscrits = as.numeric(inscrits))

acp_e24 %>% 
  lapply(summary)

# ACP SANS PONDERATION ---- 

## Réalisation de l'ACP ----

res_acp <- PCA(acp_e24, quanti.sup = 1)

fviz_screeplot(res_acp, addlabels = TRUE, ylim = c(0, 30)) # avec factoextra


comp1 <- cbind(res_acp$var$cor[,1], res_acp$var$contrib[,1], res_acp$var$cos2[,1])
colnames(comp1) <- c("Correlation", "Contribution", "Cosinus carre")
comp1 <- round(comp1, 2)
comp1

comp2 <- cbind(res_acp$var$cor[,2], res_acp$var$contrib[,2], res_acp$var$cos2[,2])
colnames(comp2) <- c("Correlation", "Contribution", "Cosinus carre")
comp2 <- round(comp2, 2)
comp2

comp3 <- cbind(res_acp$var$cor[,3], res_acp$var$contrib[,3], res_acp$var$cos2[,3])
colnames(comp3) <- c("Correlation", "Contribution", "Cosinus carre")
comp3 <- round(comp3, 2)
comp3

comp4 <- cbind(res_acp$var$cor[,4], res_acp$var$contrib[,4], res_acp$var$cos2[,4])
colnames(comp4) <- c("Correlation", "Contribution", "Cosinus carre")
comp4 <- round(comp4, 2)
comp4

res.variables <- cbind(comp1, comp2, comp3, comp4)
res.variables

# Seuil de contribution
100/nrow(res.variables)

fviz_pca_ind(res_acp, axes = c(1, 2), geom = c("point", "text"),
             label = "none", labelsize = 3,
             pointsize = 2, col.ind = "red", alpha.ind = "contrib")

fviz_pca_var(res_acp, col.var="contrib")

fviz_pca_var(res_acp, col.var="contrib", 
             geom = c("point", "text"))


## CAH à partir des coordonnées des individus sur les axes -----

md <- daisy(res_acp$ind$coord, metric = "gower") # matrice de distances

arbre <- hclust(md, method = "ward.D2") # agrégation critère de ward

plot(arbre, labels = FALSE, main = "Dendrogramme")

inertie <- sort(arbre$height, decreasing = TRUE)
plot(inertie[1:20], type = "s", xlab = "Nombre de classes", ylab = "Inertie") %>% 
  grid

# sauts d'inertie à 7 et 8 classes

typo_6 <- cutree(arbre, 6)
typo_8 <- cutree(arbre, 8)
typo_10 <- cutree(arbre, 10)

tabyl(typo_6)
tabyl(typo_8)
tabyl(typo_10)

europ24_sample <- europ24_sample  %>% 
  mutate(typo6 = as.factor(typo_6),
         typo8 = as.factor(typo_8),
         typo10 = as.factor(typo_10))

