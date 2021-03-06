# MINABLE VIEW: USERS
#
# Se preprocesan algunas variables para que sean útiles al estudio (por ejemplo, fecha de nacimiento -> edad)
#
  # Arreglo de variables para ser más "humanas"
    users$hora_afiliacion <- as.factor(users$hora_afiliacion)
    users$dia_afiliacion <- sapply(users$dia_afiliacion, getDayName)
    users$edad <- sapply(users$nacimiento, getEdad)
    users$tickets_canjeados <- sapply(users$tickets_canjeados, cleanTicketsCanjeados)
    users$shares_frequency <- mapply(setUserShareFrequency,users$difference_last_and_first_share,users$shares_totales) # Días
    users$quality <- mapply(getUserQuality,users$difference_last_and_first_share,users$shares_frequency) 
    users$quality_id <- mapply(getUserQualityId,users$difference_last_and_first_share,users$shares_frequency)
    users$categoria_dominante <- sapply(users$categoria_dominante, fixCategoria)
    users$fecha_afiliacion <- as.Date(users$fecha_afiliacion)
    users$activity_on_register <- sapply(users$fecha_afiliacion, getActivityOnRegister)
    
  # Renombrar variable
    names(users)[names(users)=="difference_last_and_first_share"] <- "total_activity"
    
  # Quitar variables innecesarias para el estudio
    users$stdv_share_difference <- NULL
    users$difference_last_raffle_first_share <- NULL
    
  # Limpiar variables con valores referenciales
     users$total_activity <- sapply(users$total_activity, function(x){ return(x+1)})
    
  # Discretización de variables numéricas: 
  # se desecha la discretización por intervalos iguales (width) debido a que se sabe de la presencia de outliers,
  # lo que genera intervalos con frecuencias inútiles (dejando +90% de los datos en un único rango), se utiliza la función "discretize" del paquete arules
  # DISCLAIMER: En algunos casos, se hace necesario "inflar" la cantidad de las categorías ya que hay demasiados valores en 0 y el algoritmo usará muy pocas categorías para los diferentes valores  
    users$rango_tickets_canjeados <- discretize(users$tickets_canjeados, method="cluster", categories=5) 
    users$rango_puntos_historicos <- discretize(users$puntos_historicos, method="cluster", categories=5)
    users$rango_puntos_gastados <- discretize(users$puntos_gastados, method="cluster", categories=5)
    users$rango_shares_totales <- discretize(users$shares_totales, method="cluster", categories=5)
    users$rango_concursos_participados <- discretize(users$concursos_participados, method="cluster", categories=5)
    users$rango_shares_frequecy <- discretize(users$shares_frequency, method="cluster", categories=5)
    users$rango_total_activity <- discretize(users$total_activity, method="cluster", categories=5)
    users$rango_activity_on_register <- discretize(users$total_activity, method="cluster", categories=5)
    
    
  # Normalización de variables numéricas:
  # Debido a que los intervalos númericos son muy iregulares entre si (puntos tiene orden de miles, tickets de decenas), se hace necesario
  # nomrbalizar las variables numéricas para poder realizar análisis de clustering 

    # Normalización por min-max
      max_puntos_historicos <- max(users$puntos_historicos)
      min_puntos_historicos <- min(users$puntos_historicos)
      users$normal_mm_puntos_historicos <- mapply(minmaxNormalization, users$puntos_historicos, min_puntos_historicos, max_puntos_historicos)
      
      max_puntos_gastados <- max(users$puntos_gastados)
      min_puntos_gastados <- min(users$puntos_gastados)
      users$normal_mm_puntos_gastados <- mapply(minmaxNormalization, users$puntos_gastados, min_puntos_gastados, max_puntos_gastados)
      
      max_shares_totales <- max(users$shares_totales)
      min_shares_totales <- min(users$shares_totales)
      users$normal_mm_shares_totales <- mapply(minmaxNormalization, users$shares_totales, min_shares_totales, max_shares_totales)
      
      max_tickets_canjeados <- max(users$tickets_canjeados)
      min_tickets_canjeados <- min(users$tickets_canjeados)
      users$normal_mm_tickets_canjeados <- mapply(minmaxNormalization, users$tickets_canjeados, min_tickets_canjeados, max_tickets_canjeados)
      
      max_concursos_participados <- max(users$concursos_participados)
      min_concursos_participados <- min(users$concursos_participados)
      users$normal_mm_concursos_participados <- mapply(minmaxNormalization, users$concursos_participados, min_concursos_participados, max_concursos_participados)
      
      max_total_activity <- max(users$total_activity)
      min_total_activity <- min(users$total_activity)
      users$normal_mm_total_activity <- mapply(minmaxNormalization, users$total_activity, min_total_activity, max_total_activity)
      
    # Normalización por z-scores
      mean_puntos_historicos <- mean(users$puntos_historicos)
      sd_puntos_historicos <- sd(users$puntos_historicos)
      users$normal_zs_puntos_historicos <- mapply(zscoreNormalization, users$puntos_historicos, sd_puntos_historicos, mean_puntos_historicos)
      
      mean_puntos_gastados <- mean(users$puntos_gastados)
      sd_puntos_gastados <- sd(users$puntos_gastados)
      users$normal_zs_puntos_gastados <- mapply(zscoreNormalization, users$puntos_gastados, sd_puntos_gastados, mean_puntos_gastados)
      
      mean_shares_totales <- mean(users$shares_totales)
      sd_shares_totales <- sd(users$shares_totales)
      users$normal_zs_shares_totales <- mapply(zscoreNormalization, users$shares_totales, sd_shares_totales, mean_shares_totales)
      
      mean_tickets_canjeados <- mean(users$tickets_canjeados)
      sd_tickets_canjeados <- sd(users$tickets_canjeados)
      users$normal_zs_tickets_canjeados <- mapply(zscoreNormalization, users$tickets_canjeados, sd_tickets_canjeados, mean_tickets_canjeados)
      
      mean_concursos_participados <- mean(users$concursos_participados)
      sd_concursos_participados <- sd(users$concursos_participados)
      users$normal_zs_concursos_participados <- mapply(zscoreNormalization, users$concursos_participados, sd_concursos_participados, mean_concursos_participados)
      
      mean_total_activity <- mean(users$total_activity)
      sd_total_activity <- sd(users$total_activity)
      users$normal_zs_total_activity <- mapply(zscoreNormalization, users$total_activity, sd_total_activity, mean_total_activity)
      
      
      
# MINABLE VIEW: VIDEOS
#
# Se preprocesan algunas variables para que sean útiles al estudio
#
  # Arreglo de variables para ser más "humanas"
    videos$release_difference <- sapply(videos$release_difference, releaseDifferenceToDays)
    videos$avg_ppv <- mapply(getAvgPpv, videos$total_views, videos$points_given)
    videos$is_depleted <- mapply(isDepleted, videos$saldo_actual, videos$points_given)
    videos$release_day <- sapply(videos$release_date, getReleaseDay)
    videos$release_date <- as.Date(videos$release_date)
    videos$release_date_youtube <- as.Date(videos$release_date_youtube)
    videos$release_date_minus_3 <- as.factor(videos$release_date - 3)
    videos$release_date_plus_3 <- as.factor(videos$release_date + 3)
    
  # Renombrar variable
    names(videos)[names(videos)=="X1_week_active_users_at_release"] <- "active_users"
    names(videos)[names(videos)=="X1_week_active_raffles"] <- "active_raffles"
    names(videos)[names(videos)=="X1_week_new_users_at_release"] <- "new_users"
    names(videos)[names(videos)=="total_users_at_release"] <- "total_users"
    
  # Quitar variables innecesarias para el estudio
    videos$X2_week_active_raffles <- NULL
    videos$active_raffles_at_release <- NULL
    videos$X2_week_new_users_at_release <- NULL
    videos$X2_week_active_users_at_release <- NULL
    videos$release_date_youtube <- NULL
    videos$male_shares <- NULL
    videos$female_shares <- NULL
    videos$url <- NULL
    
  # Discretización de variables numéricas: 
  # se desecha la discretización por intervalos iguales (width) debido a que se sabe de la presencia de outliers,
  # lo que genera intervalos con frecuencias inútiles (dejando +90% de los datos en un único rango), se utiliza la función "discretize" del paquete arules
  # DISCLAIMER: En algunos casos, se hace necesario "inflar" la cantidad de las categorías ya que hay demasiados valores en 0 y el algoritmo usará muy pocas categorías para los diferentes valores  
    videos$rango_duracion <- discretize(videos$duracion, method="cluster", categories=5) 
    videos$rango_release_difference <- discretize(videos$release_difference, method="cluster",categories=5)
    videos$rango_total_views <- discretize(videos$total_views, method="cluster",categories=5)
    videos$rango_points_given <- discretize(videos$points_given, method="cluster", categories=5) 
    videos$rango_shares_first_day <- discretize(videos$shares_first_day, method="cluster",categories=5)
    videos$rango_shares_first_week <- discretize(videos$shares_first_week, method="cluster",categories=5)
    videos$rango_shares_first_month <- discretize(videos$shares_first_month, method="cluster",categories=5)
    videos$rango_total_shares <- discretize(videos$total_shares, method="cluster",categories=5) 
    videos$rango_total_users <- discretize(videos$total_users, method="cluster",categories=5)
    videos$rango_active_users <- discretize(videos$active_users, method="cluster",categories=5) 
    videos$rango_new_users <- discretize(videos$new_users, method="cluster",categories=5)
    videos$rango_active_raffles <- discretize(videos$active_raffles, method="cluster",categories=5)
    videos$rango_avg_ppv <- discretize(videos$avg_ppv, method="cluster",categories=5)
    
  # Normalización de variables numéricas:
  # Debido a que los intervalos númericos son muy iregulares entre si (puntos tiene orden de miles, tickets de decenas), se hace necesario
  # nomrbalizar las variables numéricas para poder realizar análisis de clustering 
  
  # Normalización por min-max
    max_duracion <- max(videos$duracion)
    min_duracion <- min(videos$duracion)
    videos$normal_mm_duracion <- mapply(minmaxNormalization, videos$duracion, min_duracion, max_duracion)
    
    max_release_difference <- max_na(videos$release_difference)
    min_release_difference <- min_na(videos$release_difference)
    videos$normal_mm_release_difference <- mapply(minmaxNormalization, videos$release_difference, min_release_difference, max_release_difference)
    
    max_total_views <- max(videos$total_views)
    min_total_views <- min(videos$total_views)
    videos$normal_mm_total_views <- mapply(minmaxNormalization, videos$total_views, min_total_views, max_total_views)
    
    max_points_given <- max(videos$points_given)
    min_points_given <- min(videos$points_given)
    videos$normal_mm_points_given <- mapply(minmaxNormalization, videos$points_given, min_points_given, max_points_given)
    
    max_shares_first_day <- max_na(videos$shares_first_day)
    min_shares_first_day <- min_na(videos$shares_first_day)
    videos$normal_mm_shares_first_day <- mapply(minmaxNormalization, videos$shares_first_day, min_shares_first_day, max_shares_first_day)
    
    max_shares_first_week <- max(videos$shares_first_week)
    min_shares_first_week <- min(videos$shares_first_week)
    videos$normal_mm_shares_first_week <- mapply(minmaxNormalization, videos$shares_first_week, min_shares_first_week, max_shares_first_week)
    
    max_shares_first_month <- max(videos$shares_first_month)
    min_shares_first_month <- min(videos$shares_first_month)
    videos$normal_mm_shares_first_month <- mapply(minmaxNormalization, videos$shares_first_month, min_shares_first_month, max_shares_first_month)
    
    max_total_shares <- max(videos$total_shares)
    min_total_shares <- min(videos$total_shares)
    videos$normal_mm_total_shares <- mapply(minmaxNormalization, videos$total_shares, min_total_shares, max_total_shares)
    
    max_total_users <- max(videos$total_users)
    min_total_users <- min(videos$total_users)
    videos$normal_mm_total_users <- mapply(minmaxNormalization, videos$total_users, min_total_users, max_total_users)
    
    max_active_users <- max(videos$active_users)
    min_active_users <- min(videos$active_users)
    videos$normal_mm_active_users <- mapply(minmaxNormalization, videos$active_users, min_active_users, max_active_users)
    
    max_new_users <- max(videos$new_users)
    min_new_users <- min(videos$new_users)
    videos$normal_mm_new_users <- mapply(minmaxNormalization, videos$new_users, min_new_users, max_new_users)
    
    max_active_raffles <- max(videos$active_raffles)
    min_active_raffles <- min(videos$active_raffles)
    videos$normal_mm_active_raffles <- mapply(minmaxNormalization, videos$active_raffles, min_active_raffles, max_active_raffles)
    
    max_avg_ppv <- max_na(videos$avg_ppv)
    min_avg_ppv <- min_na(videos$avg_ppv)
    videos$normal_mm_avg_ppv <- mapply(minmaxNormalization, videos$avg_ppv, min_avg_ppv, max_avg_ppv)
    
  # Normalización por z-scores
    mean_duracion <- mean(videos$duracion)
    sd_duracion <- sd(videos$duracion)
    videos$normal_zs_duracion <- mapply(zscoreNormalization, videos$duracion, sd_duracion, mean_duracion)
    
    mean_release_difference <- mean_na(videos$release_difference)
    sd_release_difference <- sd(videos$release_difference, TRUE)
    videos$normal_zs_release_difference <- mapply(zscoreNormalization, videos$release_difference, sd_release_difference, mean_release_difference)
    
    mean_total_views <- mean(videos$total_views)
    sd_total_views <- sd(videos$total_views)
    videos$normal_zs_total_views <- mapply(zscoreNormalization, videos$total_views, sd_total_views, mean_total_views)
    
    mean_points_given <- mean(videos$points_given)
    sd_points_given <- sd(videos$points_given)
    videos$normal_zs_points_given <- mapply(zscoreNormalization, videos$points_given, sd_points_given, mean_points_given)
    
    mean_shares_first_day <- mean_na(videos$shares_first_day)
    sd_shares_first_day <- sd(videos$shares_first_day, TRUE)
    videos$normal_zs_shares_first_day <- mapply(zscoreNormalization, videos$shares_first_day, sd_shares_first_day, mean_shares_first_day)
    
    mean_shares_first_week <- mean(videos$shares_first_week)
    sd_shares_first_week <- sd(videos$shares_first_week)
    videos$normal_zs_shares_first_week <- mapply(zscoreNormalization, videos$shares_first_week, sd_shares_first_week, mean_shares_first_week)
    
    mean_shares_first_month <- mean(videos$shares_first_month)
    sd_shares_first_month <- sd(videos$shares_first_month)
    videos$normal_zs_shares_first_month <- mapply(zscoreNormalization, videos$shares_first_month, sd_shares_first_month, mean_shares_first_month)
    
    mean_total_shares <- mean(videos$total_shares)
    sd_total_shares <- sd(videos$total_shares)
    videos$normal_zs_total_shares <- mapply(zscoreNormalization, videos$total_shares, sd_total_shares, mean_total_shares)
    
    mean_total_users <- mean(videos$total_users)
    sd_total_users <- sd(videos$total_users)
    videos$normal_zs_total_users <- mapply(zscoreNormalization, videos$total_users, sd_total_users, mean_total_users)
    
    mean_active_users <- mean(videos$active_users)
    sd_active_users <- sd(videos$active_users)
    videos$normal_zs_active_users <- mapply(zscoreNormalization, videos$active_users, sd_active_users, mean_active_users)
    
    mean_new_users <- mean(videos$new_users)
    sd_new_users <- sd(videos$new_users)
    videos$normal_zs_new_users <- mapply(zscoreNormalization, videos$new_users, sd_new_users, mean_new_users)
    
    mean_active_raffles <- mean(videos$active_raffles)
    sd_active_raffles <- sd(videos$active_raffles)
    videos$normal_zs_active_raffles <- mapply(zscoreNormalization, videos$active_raffles, sd_active_raffles, mean_active_raffles)
    
    mean_avg_ppv <- mean_na(videos$avg_ppv)
    sd_avg_ppv <- sd(videos$avg_ppv, TRUE)
    videos$normal_zs_avg_ppv <- mapply(zscoreNormalization, videos$avg_ppv, sd_avg_ppv, mean_avg_ppv)
    
  # Teniendo la vista minable de videos y de usuarios, se crean copias para almacenar sólo datos numéricos y sólo datos discretos, para ser usadas en diferentes algoritmos
    # USUARIOS
      # Numéricos sin normalizar
        keep_users_num <- c(
          "puntos_historicos", 
          "puntos_gastados", 
          "shares_totales", 
          "concursos_participados", 
          "tickets_canjeados", 
          "total_activity"
        )
        users_num <- users[keep_users_num]
        
      # Numéricos normalizados con min-max
        keep_users_num_mm <- c(
          "normal_mm_puntos_historicos", 
          "normal_mm_puntos_gastados", 
          "normal_mm_shares_totales", 
          "normal_mm_tickets_canjeados", 
          "normal_mm_concursos_participados", 
          "normal_mm_total_activity"
        )
        users_num_mm <- users[keep_users_num_mm]
        
      # Numéricos normalizados con z-scores
        keep_users_num_zs <- c(
          "normal_zs_puntos_historicos", 
          "normal_zs_puntos_gastados", 
          "normal_zs_shares_totales", 
          "normal_zs_tickets_canjeados", 
          "normal_zs_concursos_participados", 
          "normal_zs_total_activity"
        )
        users_num_zs <- users[keep_users_num_zs]
        
      # Discretos
        keep_users_dis <- c(
          "categoria_dominante", 
          "uni", 
          "genero", 
          "hora_afiliacion", 
          "dia_afiliacion", 
          "edad", 
          "rango_tickets_canjeados", 
          "rango_puntos_historicos", 
          "rango_puntos_gastados", 
          "rango_shares_totales", 
          "rango_concursos_participados", 
          "rango_shares_frequecy", 
          "rango_total_activity"
        )
        users_dis <- users[keep_users_dis]
  
    # VIDEOS
      # Numéricos sin normalizar
        keep_videos_num <- c(
          "points_per_view", 
          "duracion", 
          "total_views", 
          "points_given", 
          "shares_first_day", 
          "shares_first_week", 
          "shares_first_month", 
          "total_shares", 
          "total_users", 
          "active_users", 
          "new_users", 
          "active_raffles",
          "avg_ppv"
        )
        videos_num <- videos[keep_videos_num]
        
      # Numéricos normalizados con min-max
        keep_videos_num_mm <- c(
          "normal_mm_duracion", 
          "normal_mm_release_difference", 
          "normal_mm_total_views", 
          "normal_mm_points_given", 
          "normal_mm_shares_first_day", 
          "normal_mm_shares_first_week", 
          "normal_mm_shares_first_month", 
          "normal_mm_total_shares", 
          "normal_mm_total_users", 
          "normal_mm_active_users", 
          "normal_mm_new_users", 
          "normal_mm_active_raffles", 
          "normal_mm_avg_ppv"
        )
        videos_num_mm <- videos[keep_videos_num_mm]
        
      # Numéricos normalizados con z-scores
        keep_videos_num_zs <- c(
          "normal_zs_duracion", 
          "normal_zs_release_difference", 
          "normal_zs_total_views", 
          "normal_zs_points_given", 
          "normal_zs_shares_first_day", 
          "normal_zs_shares_first_week", 
          "normal_zs_shares_first_month", 
          "normal_zs_total_shares", 
          "normal_zs_total_users", 
          "normal_zs_active_users", 
          "normal_zs_new_users", 
          "normal_zs_active_raffles", 
          "normal_zs_avg_ppv"
        )
        videos_num_zs <- videos[keep_videos_num_zs]
        
      # Discretos
        keep_videos_dis <- c(
          "category", 
          "is_depleted", 
          "release_day", 
          "rango_duracion", 
          "rango_release_difference", 
          "rango_total_views", 
          "rango_points_given", 
          #"rango_shares_first_day", 
          "rango_shares_first_week", 
          #"rango_shares_first_month", 
          "rango_total_shares", 
          "rango_total_users", 
          "rango_active_users", 
          "rango_new_users", 
          "rango_active_raffles", 
          "rango_avg_ppv"
        )
        videos_dis <- videos[keep_videos_dis]