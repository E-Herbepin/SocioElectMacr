# charger la géométrie avec sf
comsf <- st_read(dsn = "01_data//geoloc/geometry/COMMUNE.shp", 
                 stringsAsFactors = F)

# Simplifier pour que le code aille plus vite 
comsf<- st_simplify(comsf, dTolerance = 1000)


comsf <- comsf %>% rename(DepCom = INSEE_COM)%>% 
  select(-INSEE_CAN,-INSEE_ARR,-INSEE_DEP,-INSEE_REG, -POPULATION,-NOM, -NOM_M, -STATUT, -SIREN_EPCI)



# interroger le système de coordoonées 
st_crs(comsf)

# projeter le fond de carte dans le référentiel Lambert93 (adapté pour la France)
comsf <- st_transform(comsf, crs = 2154)

# visualiser la géométrie
#plot(comsf$geometry)

# joindre les données attributaires
comsf <- left_join(PresF, comsf, by = "DepCom")


  

#comsf<- comsf %>% filter (Nb_17_Votants >= 15)

comsf<-st_sf(comsf)


#Basique - vote macron en fonction des communes
#mapview(comsf, zcol = "pourc_macron_17",lwd = 0.1 )


# Calculer le percentile de chaque valeur (= si la valeur pour une commune est x, ça veut dire que la commune est dans le x-eme pourcentage de communes qui ont le plus voté macron)
ecdf_func <- ecdf(comsf$pourc_macron_17)
comsf$rang_macron_17 <- ecdf_func(comsf$pourc_macron_17) * 100
ecdf_func <- ecdf(comsf$pourc_macron_22)
comsf$rang_macron_22 <- ecdf_func(comsf$pourc_macron_22) * 100
ecdf_func <- ecdf(comsf$diff_pourc_macron)
comsf$rang_diff_macron <- ecdf_func(comsf$diff_pourc_macron) * 100

comsf_rur <- comsf %>% 
  filter(dens %in% c(5, 6,7))  


