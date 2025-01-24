#### APPARIEMENT DES DONNEES RECENSEMENT ET DES DONNEES ELECTORALES ----

names(europ24) # code_com
names(communes) # codgeo

match <- europ24 |> 
  right_join(communes, 
             by = c("code_com" = "codgeo"))

# on perd 300 communes

# 19 communes présentes dans les données du recensement mais pas dans les données europ24

nonmatch1 <- communes |> 
  anti_join(europ24, 
            by = c("codgeo" = "code_com"))

# 319 communes présentes dans les données europ24 et pas dans celles du recensement 2020

nonmatch2 <- europ24 |> 
  anti_join(communes, 
            by = c("code_com" = "codgeo" ))

# communes en ZZ : bureaux de vote de l'étranger
# communes en 98...: Nouvelle Calédonie
# communes en 97... : Mayotte
# trois communes métropolitaines : 
# 85212 - Sainte-Florence et 85165 - L'Oie : défusionnées au 1er janvier 2024
# 60694 - Les Hauts-Talican : fusion — au 1er janvier 2019 — des communes de Beaumont-les-Nonains, La Neuville-Garnier et Villotran. Beaumont-les-Nonains quitte les Hauts-Talican le 1er janvier 2024. 