df_clusters <- data.frame(id = rownames(res_clust$data.clust),
                          Cluster = as.character(res_clust$data.clust$clust),
                          stringsAsFactors = FALSE)
PresFrural$id <- rownames(PresFrural)
if ("Cluster" %in% colnames(PresFrural)) PresFrural$Cluster <- NULL
PresFrural <- merge(PresFrural, df_clusters, by = "id", all.x = TRUE)
PresFrural$id <- NULL

# 1. Calculer la fréquence absolue
table_cluster <- table(PresFrural$Cluster)

# 2. Calculer la fréquence relative (en pourcentage)
pourcentage_cluster <- prop.table(table_cluster) * 100

# 3. Afficher les résultats avec arrondi
round(pourcentage_cluster, 2)