#Création de la palette introduisant les zones non rurales
palette <- c(
  "3-3" = "#574249",
  "2-3" = "#627f8c",
  "1-3" = "#64acbe",
  "3-2" = "#985356",
  "2-2" = "#ad9ea5",
  "1-2" = "#b0d5df",
  "3-1" = "#c85a5a",
  "2-1" = "#e4acac",
  "1-1" = "#e8e8e8",
  "Zones non rurales" = "yellow"
)
# 2. Données pour la grille
legend_data <- expand.grid(
  x = 1:3,
  y = 1:3
) %>%
  mutate(
    bi_class = paste0(x, "-", y),
    fill = grpink_palette[bi_class]
  )
# 3. Ajouter une case pour les zones non rurales
extra_tile <- tibble(
  x = 1, y = 4.2,
  bi_class = "Zones non rurales", fill = "yellow"
)
legend_data_extended <- bind_rows(legend_data, extra_tile)
# Calculer le percentile de chaque valeur (= si la valeur pour une commune est x, ça veut dire que la commune est dans le x-eme pourcentage de communes qui ont le plus voté macron)
ecdf_func <- ecdf(comsf$pourc_macron_17)
comsf$rang_macron_17 <- ecdf_func(comsf$pourc_macron_17) * 100
ecdf_func <- ecdf(comsf$pourc_macron_22)
comsf$rang_macron_22 <- ecdf_func(comsf$pourc_macron_22) * 100
ecdf_func <- ecdf(comsf$diff_pourc_macron)
comsf$rang_diff_macron <- ecdf_func(comsf$diff_pourc_macron) * 100
comsf_rur <- comsf %>%
  filter(dens %in% c(5, 6,7))
comsf_nonrur <- comsf %>%
  filter(dens %in% c(1,2,3,4))
comsf$rur<-ifelse(comsf$dens %in% c(5,6,7), "Rural", NA)
comsf$nonrur<-ifelse(comsf$dens %in% c(1,2,3,4), "Non rural", NA)
#On filtre les géométries absentes (23 communes, dont une seule de plus de 1000 habitants)
comsf<- comsf%>% filter(st_geometry_type(comsf$geometry)!='GEOMETRYCOLLECTION')


#Carte bivariée
typo <- bi_class(comsf, x = pourc_macron_17 , y = pourc_melenchon_17 , style = "quantile", dim = 3)
typo$bi_class[typo$dens %in% c(1,2,3,4)] <- "Zones non rurales"
map <- ggplot() +
  geom_sf(data = typo, mapping = aes(fill = bi_class), color = "NA", size = 0.05, show.legend = FALSE) +
  scale_fill_manual(
    values = palette,  # Notre palette avec la case pour les zones non rurales
    name = "Classe bivariée"
  ) +
  bi_theme()
#Légende
legend_plot <- ggplot(legend_data_extended, aes(x = x, y = y)) +
  geom_tile(aes(fill = bi_class), color = "white") +
  geom_text(
    data = filter(legend_data_extended, bi_class == "Zones non rurales"),
    aes(label = "Zones non rurales"),
    hjust = 0, nudge_x = 0.6,
    size = 3
  ) +
  scale_fill_manual(values = c(grpink_palette, "Zones non rurales" = "yellow"), guide = "none") +
  coord_fixed() +
  labs(x = "Vote Macron 2017 →", y = "Vote Mélenchon 2017 → ") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 8),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.title.y = element_text(hjust = 0.1, vjust = 1.5)
  )
ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend_plot,0, 0.075, 0.32, 0.32)

