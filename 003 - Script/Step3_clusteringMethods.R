# CLUSTERING

  # LIMPIAR VALORES FALTANTES
  # En los algoritmos de clusteringbasado en distancias no es posible la clasificación de datos faltantes (NA), por lo que es necesario
  # realizar un proceso de limpieza de estos datos.
  videos_num_mm <- videos_num_mm[complete.cases(videos_num_mm),]
  videos_num_zs <- videos_num_zs[complete.cases(videos_num_zs),]

  # K?
  # Como primer acercamiento, se ejecuta el algoritmo de kmeans con un rango de valores de K, para poder definir el K que otorgue mejores
  # valores de distancias (inter y entre clusters)

  k_potencial_users_mm <- testingOptimalK(users_num_mm,40)
  plot(k_potencial_users_mm$K, k_potencial_users_mm$betweens_to_max, type="p", main = "Distancia InterClusters (Min-max)", xlab = "# Clusters (K)", ylab = "Distancia Inter", col="red")
  plot(k_potencial_users_mm$K, k_potencial_users_mm$withins_to_min, type="p", main = "Distancia IntraClusters (Min-max)", xlab = "# Clusters (K)", ylab = "Distancia Intra", col="blue")
  matplot(k_potencial_users_mm$K, cbind(k_potencial_users_mm$betweens_to_max, k_potencial_users_mm$withins_to_min),type = c("p","p"),col = c("red","blue"),main = "Users Intra/Inter-Clusters (Min-max)", xlab = "# Clusters (K)", ylab = "Distancias MM", xlim = c(0,60), pch=20)
  
  k_potencial_users_zs <- testingOptimalK(users_num_zs,40)
  plot(k_potencial_users_zs$K, k_potencial_users_zs$betweens_to_max, type="p", main = "Distancia InterClusters (Z-scores)", xlab = "# Clusters (K)", ylab = "Distancia Inter", col="red")
  plot(k_potencial_users_zs$K, k_potencial_users_zs$withins_to_min, type="p", main = "Distancia IntraClusters (Z-scores)", xlab = "# Clusters (K)", ylab = "Distancia Intra", col="blue")
  matplot(k_potencial_users_zs$K, cbind(k_potencial_users_zs$betweens_to_max, k_potencial_users_zs$withins_to_min),type = c("p","p"),col = c("red","blue"),main = "Users Intra/Inter-Clusters (Z-Scores)", xlab = "# Clusters (K)", ylab = "Distancias ZS", xlim = c(0,60), pch=20)
  
  k_potencial_videos_mm <- testingOptimalK(videos_num_mm,40)
  plot(k_potencial_videos_mm$K, k_potencial_videos_mm$betweens_to_max, type="p", main = "Distancia InterClusters (Min-max)", xlab = "# Clusters (K)", ylab = "Distancia Inter", col="red")
  plot(k_potencial_videos_mm$K, k_potencial_videos_mm$withins_to_min, type="p", main = "Distancia IntraClusters (Min-max)", xlab = "# Clusters (K)", ylab = "Distancia Intra", col="blue")
  matplot(k_potencial_videos_mm$K, cbind(k_potencial_videos_mm$betweens_to_max, k_potencial_videos_mm$withins_to_min),type = c("p","p"),col = c("red","blue"),main = "Videos Intra/Inter-Clusters (Min-max)", xlab = "# Clusters (K)", ylab = "Distancias MM", xlim = c(0,60), pch=20)

  k_potencial_videos_zs <- testingOptimalK(videos_num_zs,40)
  plot(k_potencial_videos_zs$K, k_potencial_videos_zs$betweens_to_max, type="p", main = "Distancia InterClusters (Z-scores)", xlab = "# Clusters (K)", ylab = "Distancia Inter", col="red")
  plot(k_potencial_videos_zs$K, k_potencial_videos_zs$withins_to_min, type="p", main = "Distancia IntraClusters (Z-scores)", xlab = "# Clusters (K)", ylab = "Distancia Intra", col="blue")
  matplot(k_potencial_videos_zs$K, cbind(k_potencial_videos_zs$betweens_to_max, k_potencial_videos_zs$withins_to_min),type = c("p","p"),col = c("red","blue"),main = "Videos Intra/Inter-Clusters (Z-Scores)", xlab = "# Clusters (K)", ylab = "Distancias ZS", xlim = c(0,60), pch=20)
  
  # Se desprende qque el número ideal de clusters se encuentra cerca de 40 para Usuarios 
  