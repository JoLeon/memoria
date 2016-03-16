# RELACIONES INTERESANTES

# submuestrar - sobremuestrar
# Utilizar herramientas de clasificacion
# Usar árboles y naive bayes? confusion matrix? prediciton (pasarle arbol) y performance

# OVERSAMPLING
#
# Videos:
#   Targets:
#     - Active users (+150) 
#     - Penetración (+70%)
#

#   Active users

    videos_base_active_users <- videos_processed[c("duracion", "shares_first_day", "new_users", "active_raffles", "release_difference_hours", "active_canjes", "active_users", "penetracion")]
    videos_base_active_users$success <- sapply(videos_base_active_users$active_users, function(actives){
      if(actives > 150){
        return(1)
      }
      return(0)
    })
    videos_base_active_users <- videos_base_active_users[c("duracion", "shares_first_day", "new_users", "active_raffles", "release_difference_hours", "active_canjes", "success")]
    
    active_users_success_group <- sqldf("SELECT * FROM videos_base_active_users WHERE success = 1")
    
    # Valores posibles para cada variable
    duracion <- sqldf("SELECT DISTINCT(duracion) FROM active_users_success_group")
    shares_first_day <- sqldf("SELECT DISTINCT(shares_first_day) FROM active_users_success_group")
    new_users <- sqldf("SELECT DISTINCT(new_users) FROM active_users_success_group")
    active_raffles <- sqldf("SELECT DISTINCT(active_raffles) FROM active_users_success_group")
    release_difference_hours <- sqldf("SELECT DISTINCT(release_difference_hours) FROM active_users_success_group")
    active_canjes <- sqldf("SELECT DISTINCT(active_canjes) FROM active_users_success_group")
    
    remaining_to_balance <- 609
    for (i in (1:remaining_to_balance)){
      print(paste("Iteraring ...", i,remaining_to_balance))
      random_row <- c(
        duracion$duracion[round(runif(1,1,length(duracion$duracion)))],
        shares_first_day$shares_first_day[round(runif(1,1,length(shares_first_day$shares_first_day)))],
        new_users$new_users[round(runif(1,1,length(new_users$new_users)))],
        active_raffles$active_raffles[round(runif(1,1,length(active_raffles$active_raffles)))],
        release_difference_hours$release_difference_hours[round(runif(1,1,length(release_difference_hours$release_difference_hours)))],
        active_canjes$active_canjes[round(runif(1,1,length(active_canjes$active_canjes)))],
        1
      )
      videos_base_active_users <- rbind(videos_base_active_users,random_row)
    }
    
# Penetracion
    
    videos_base_penetracion <- videos_processed[c("duracion", "shares_first_day", "new_users", "active_raffles", "release_difference_hours", "active_canjes", "active_users", "penetracion")]
    videos_base_penetracion$success <- sapply(videos_base_penetracion$penetracion, function(penetracion){
      if(penetracion > 0.7){
        return(1)
      }
      return(0)
    })
    videos_base_penetracion <- videos_base_penetracion[c("duracion", "shares_first_day", "new_users", "active_raffles", "release_difference_hours", "active_canjes", "active_users", "success")]
    
    active_users_success_group <- sqldf("SELECT * FROM videos_base_penetracion WHERE success = 1")
    
    # Valores posibles para cada variable
    duracion <- sqldf("SELECT DISTINCT(duracion) FROM active_users_success_group")
    shares_first_day <- sqldf("SELECT DISTINCT(shares_first_day) FROM active_users_success_group")
    new_users <- sqldf("SELECT DISTINCT(new_users) FROM active_users_success_group")
    active_raffles <- sqldf("SELECT DISTINCT(active_raffles) FROM active_users_success_group")
    release_difference_hours <- sqldf("SELECT DISTINCT(release_difference_hours) FROM active_users_success_group")
    active_canjes <- sqldf("SELECT DISTINCT(active_canjes) FROM active_users_success_group")
    active_users <- sqldf("SELECT DISTINCT(active_users) FROM active_users_success_group")
    
    remaining_to_balance <- 545
    for (i in (1:remaining_to_balance)){
      print(paste("Iteraring ...", i,remaining_to_balance))
      random_row <- c(
        duracion$duracion[round(runif(1,1,length(duracion$duracion)))],
        shares_first_day$shares_first_day[round(runif(1,1,length(shares_first_day$shares_first_day)))],
        new_users$new_users[round(runif(1,1,length(new_users$new_users)))],
        active_raffles$active_raffles[round(runif(1,1,length(active_raffles$active_raffles)))],
        release_difference_hours$release_difference_hours[round(runif(1,1,length(release_difference_hours$release_difference_hours)))],
        active_canjes$active_canjes[round(runif(1,1,length(active_canjes$active_canjes)))],
        active_users$active_users[round(runif(1,1,length(active_users$active_users)))],
        1
      )
      videos_base_penetracion <- rbind(videos_base_penetracion,random_row)
    }


#
# Users:
#   Targets:
#     - Quality
#     - Good/Bad quality
    
    users_processed$genero <- sapply(users_processed$genero, function(genero){
      if(is.na(genero)){
        if(runif(1,0,1) > 0.5){
          return(as.factor("M"))
        }
        return(as.factor("F"))
      }
      return(as.factor(genero))
    })
    users_quality <- users_processed[c(
      "puntos_historicos", "genero", "dia_afiliacion", "shares_totales", "recruitments", "concursos_participados", "premios_canjeados", 
      "edad", "quality", "sistema_registro", "densidad_videos", "calidad_videos", "densidad_concursos" 
    )]
    
    # "Daily, constant"                 4
    daily_constant_remaining  <- 1203
    daily_constant_success_group <- sqldf("SELECT * FROM users_quality WHERE quality = 'Daily, constant'")
    genero <- sqldf("SELECT DISTINCT(genero) FROM daily_constant_success_group")
    puntos_historicos <- sqldf("SELECT DISTINCT(puntos_historicos) FROM daily_constant_success_group")
    dia_afiliacion <- sqldf("SELECT DISTINCT(dia_afiliacion) FROM daily_constant_success_group")
    shares_totales <- sqldf("SELECT DISTINCT(shares_totales) FROM daily_constant_success_group")
    recruitments <- sqldf("SELECT DISTINCT(recruitments) FROM daily_constant_success_group")
    concursos_participados <- sqldf("SELECT DISTINCT(concursos_participados) FROM daily_constant_success_group")
    premios_canjeados <- sqldf("SELECT DISTINCT(premios_canjeados) FROM daily_constant_success_group")
    edad <- sqldf("SELECT DISTINCT(edad) FROM daily_constant_success_group")
    sistema_registro <- sqldf("SELECT DISTINCT(sistema_registro) FROM daily_constant_success_group")
    densidad_videos <- sqldf("SELECT DISTINCT(densidad_videos) FROM daily_constant_success_group")
    calidad_videos <- sqldf("SELECT DISTINCT(calidad_videos) FROM daily_constant_success_group")
    densidad_concursos <- sqldf("SELECT DISTINCT(densidad_concursos) FROM daily_constant_success_group")
    densidad_videos <- sqldf("SELECT DISTINCT(densidad_videos) FROM daily_constant_success_group")
 
    
    for (i in (1:daily_constant_remaining)){
      print(paste("Iteraring ...", i,daily_constant_remaining))
      random_row <- list(
        as.integer(puntos_historicos$puntos_historicos[round(runif(1,1,length(puntos_historicos$puntos_historicos)))]),
        as.factor(genero$genero[round(runif(1,1,length(genero$genero)))]),
        as.factor(dia_afiliacion$dia_afiliacion[round(runif(1,1,length(dia_afiliacion$dia_afiliacion)))]),
        as.integer(shares_totales$shares_totales[round(runif(1,1,length(shares_totales$shares_totales)))]),
        as.integer(recruitments$recruitments[round(runif(1,1,length(recruitments$recruitments)))]),
        as.integer(concursos_participados$concursos_participados[round(runif(1,1,length(concursos_participados$concursos_participados)))]),
        as.integer(premios_canjeados$premios_canjeados[round(runif(1,1,length(premios_canjeados$premios_canjeados)))]),
        as.integer(edad$edad[round(runif(1,1,length(edad$edad)))]),
        as.factor("Daily, constant"),
        as.factor(sistema_registro$sistema_registro[round(runif(1,1,length(sistema_registro$sistema_registro)))]),
        as.factor(densidad_videos$densidad_videos[round(runif(1,1,length(densidad_videos$densidad_videos)))]),
        as.factor(calidad_videos$calidad_videos[round(runif(1,1,length(calidad_videos$calidad_videos)))]),
        as.factor(densidad_concursos$densidad_concursos[round(runif(1,1,length(densidad_concursos$densidad_concursos)))])
      )
      users_quality <- rbind(users_quality,random_row)
    }
    
    # "Daily, for a month"              8
    daily_month_remaining     <- 1199
    daily_month_success_group <- sqldf("SELECT * FROM users_quality WHERE quality = 'Daily, for a month'")
    
    genero <- sqldf("SELECT DISTINCT(genero) FROM daily_month_success_group")
    puntos_historicos <- sqldf("SELECT DISTINCT(puntos_historicos) FROM daily_month_success_group")
    dia_afiliacion <- sqldf("SELECT DISTINCT(dia_afiliacion) FROM daily_month_success_group")
    shares_totales <- sqldf("SELECT DISTINCT(shares_totales) FROM daily_month_success_group")
    recruitments <- sqldf("SELECT DISTINCT(recruitments) FROM daily_month_success_group")
    concursos_participados <- sqldf("SELECT DISTINCT(concursos_participados) FROM daily_month_success_group")
    premios_canjeados <- sqldf("SELECT DISTINCT(premios_canjeados) FROM daily_month_success_group")
    edad <- sqldf("SELECT DISTINCT(edad) FROM daily_month_success_group")
    sistema_registro <- sqldf("SELECT DISTINCT(sistema_registro) FROM daily_month_success_group")
    densidad_videos <- sqldf("SELECT DISTINCT(densidad_videos) FROM daily_month_success_group")
    calidad_videos <- sqldf("SELECT DISTINCT(calidad_videos) FROM daily_month_success_group")
    densidad_concursos <- sqldf("SELECT DISTINCT(densidad_concursos) FROM daily_month_success_group")
    densidad_videos <- sqldf("SELECT DISTINCT(densidad_videos) FROM daily_month_success_group")
    
    
    for (i in (1:daily_month_remaining)){
      print(paste("Iteraring ...", i,daily_month_remaining))
      random_row <- list(
        as.integer(puntos_historicos$puntos_historicos[round(runif(1,1,length(puntos_historicos$puntos_historicos)))]),
        as.factor(genero$genero[round(runif(1,1,length(genero$genero)))]),
        as.factor(dia_afiliacion$dia_afiliacion[round(runif(1,1,length(dia_afiliacion$dia_afiliacion)))]),
        as.integer(shares_totales$shares_totales[round(runif(1,1,length(shares_totales$shares_totales)))]),
        as.integer(recruitments$recruitments[round(runif(1,1,length(recruitments$recruitments)))]),
        as.integer(concursos_participados$concursos_participados[round(runif(1,1,length(concursos_participados$concursos_participados)))]),
        as.integer(premios_canjeados$premios_canjeados[round(runif(1,1,length(premios_canjeados$premios_canjeados)))]),
        as.integer(edad$edad[round(runif(1,1,length(edad$edad)))]),
        as.factor("Daily, for a month"),
        as.factor(sistema_registro$sistema_registro[round(runif(1,1,length(sistema_registro$sistema_registro)))]),
        as.factor(densidad_videos$densidad_videos[round(runif(1,1,length(densidad_videos$densidad_videos)))]),
        as.factor(calidad_videos$calidad_videos[round(runif(1,1,length(calidad_videos$calidad_videos)))]),
        as.factor(densidad_concursos$densidad_concursos[round(runif(1,1,length(densidad_concursos$densidad_concursos)))])
      )
      users_quality <- rbind(users_quality,random_row)
    }
    
    
    # "Daily, for a week"               36
    daily_week_remaining      <- 1171
    daily_week_success_group <- sqldf("SELECT * FROM users_quality WHERE quality = 'Daily, for a week'")
    
    genero <- sqldf("SELECT DISTINCT(genero) FROM daily_week_success_group")
    puntos_historicos <- sqldf("SELECT DISTINCT(puntos_historicos) FROM daily_week_success_group")
    dia_afiliacion <- sqldf("SELECT DISTINCT(dia_afiliacion) FROM daily_week_success_group")
    shares_totales <- sqldf("SELECT DISTINCT(shares_totales) FROM daily_week_success_group")
    recruitments <- sqldf("SELECT DISTINCT(recruitments) FROM daily_week_success_group")
    concursos_participados <- sqldf("SELECT DISTINCT(concursos_participados) FROM daily_week_success_group")
    premios_canjeados <- sqldf("SELECT DISTINCT(premios_canjeados) FROM daily_week_success_group")
    edad <- sqldf("SELECT DISTINCT(edad) FROM daily_week_success_group")
    sistema_registro <- sqldf("SELECT DISTINCT(sistema_registro) FROM daily_week_success_group")
    densidad_videos <- sqldf("SELECT DISTINCT(densidad_videos) FROM daily_week_success_group")
    calidad_videos <- sqldf("SELECT DISTINCT(calidad_videos) FROM daily_week_success_group")
    densidad_concursos <- sqldf("SELECT DISTINCT(densidad_concursos) FROM daily_week_success_group")
    densidad_videos <- sqldf("SELECT DISTINCT(densidad_videos) FROM daily_week_success_group")
    
    
    for (i in (1:daily_week_remaining)){
      print(paste("Iteraring ...", i,daily_week_remaining))
      random_row <- list(
        as.integer(puntos_historicos$puntos_historicos[round(runif(1,1,length(puntos_historicos$puntos_historicos)))]),
        as.factor(genero$genero[round(runif(1,1,length(genero$genero)))]),
        as.factor(dia_afiliacion$dia_afiliacion[round(runif(1,1,length(dia_afiliacion$dia_afiliacion)))]),
        as.integer(shares_totales$shares_totales[round(runif(1,1,length(shares_totales$shares_totales)))]),
        as.integer(recruitments$recruitments[round(runif(1,1,length(recruitments$recruitments)))]),
        as.integer(concursos_participados$concursos_participados[round(runif(1,1,length(concursos_participados$concursos_participados)))]),
        as.integer(premios_canjeados$premios_canjeados[round(runif(1,1,length(premios_canjeados$premios_canjeados)))]),
        as.integer(edad$edad[round(runif(1,1,length(edad$edad)))]),
        as.factor("Daily, for a week"),
        as.factor(sistema_registro$sistema_registro[round(runif(1,1,length(sistema_registro$sistema_registro)))]),
        as.factor(densidad_videos$densidad_videos[round(runif(1,1,length(densidad_videos$densidad_videos)))]),
        as.factor(calidad_videos$calidad_videos[round(runif(1,1,length(calidad_videos$calidad_videos)))]),
        as.factor(densidad_concursos$densidad_concursos[round(runif(1,1,length(densidad_concursos$densidad_concursos)))])
      )
      users_quality <- rbind(users_quality,random_row)
    }
    
    
    # "Lost"                            347
    lost_remaining            <- 860
    lost_success_group <- sqldf("SELECT * FROM users_quality WHERE quality = 'Lost'")
    
    genero <- sqldf("SELECT DISTINCT(genero) FROM lost_success_group")
    puntos_historicos <- sqldf("SELECT DISTINCT(puntos_historicos) FROM lost_success_group")
    dia_afiliacion <- sqldf("SELECT DISTINCT(dia_afiliacion) FROM lost_success_group")
    shares_totales <- sqldf("SELECT DISTINCT(shares_totales) FROM lost_success_group")
    recruitments <- sqldf("SELECT DISTINCT(recruitments) FROM lost_success_group")
    concursos_participados <- sqldf("SELECT DISTINCT(concursos_participados) FROM lost_success_group")
    premios_canjeados <- sqldf("SELECT DISTINCT(premios_canjeados) FROM lost_success_group")
    edad <- sqldf("SELECT DISTINCT(edad) FROM lost_success_group")
    sistema_registro <- sqldf("SELECT DISTINCT(sistema_registro) FROM lost_success_group")
    densidad_videos <- sqldf("SELECT DISTINCT(densidad_videos) FROM lost_success_group")
    calidad_videos <- sqldf("SELECT DISTINCT(calidad_videos) FROM lost_success_group")
    densidad_concursos <- sqldf("SELECT DISTINCT(densidad_concursos) FROM lost_success_group")
    densidad_videos <- sqldf("SELECT DISTINCT(densidad_videos) FROM lost_success_group")
    
    
    for (i in (1:lost_remaining)){
      print(paste("Iteraring ...", i,lost_remaining))
      random_row <- list(
        as.integer(puntos_historicos$puntos_historicos[round(runif(1,1,length(puntos_historicos$puntos_historicos)))]),
        as.factor(genero$genero[round(runif(1,1,length(genero$genero)))]),
        as.factor(dia_afiliacion$dia_afiliacion[round(runif(1,1,length(dia_afiliacion$dia_afiliacion)))]),
        as.integer(shares_totales$shares_totales[round(runif(1,1,length(shares_totales$shares_totales)))]),
        as.integer(recruitments$recruitments[round(runif(1,1,length(recruitments$recruitments)))]),
        as.integer(concursos_participados$concursos_participados[round(runif(1,1,length(concursos_participados$concursos_participados)))]),
        as.integer(premios_canjeados$premios_canjeados[round(runif(1,1,length(premios_canjeados$premios_canjeados)))]),
        as.integer(edad$edad[round(runif(1,1,length(edad$edad)))]),
        as.factor("Lost"),
        as.factor(sistema_registro$sistema_registro[round(runif(1,1,length(sistema_registro$sistema_registro)))]),
        as.factor(densidad_videos$densidad_videos[round(runif(1,1,length(densidad_videos$densidad_videos)))]),
        as.factor(calidad_videos$calidad_videos[round(runif(1,1,length(calidad_videos$calidad_videos)))]),
        as.factor(densidad_concursos$densidad_concursos[round(runif(1,1,length(densidad_concursos$densidad_concursos)))])
      )
      users_quality <- rbind(users_quality,random_row)
    }
    
    
    # "Not captured"                    973
    not_captured_remaining    <- 234
    not_captured_success_group <- sqldf("SELECT * FROM users_quality WHERE quality = 'Not captured'")
    
    genero <- sqldf("SELECT DISTINCT(genero) FROM not_captured_success_group")
    puntos_historicos <- sqldf("SELECT DISTINCT(puntos_historicos) FROM not_captured_success_group")
    dia_afiliacion <- sqldf("SELECT DISTINCT(dia_afiliacion) FROM not_captured_success_group")
    shares_totales <- sqldf("SELECT DISTINCT(shares_totales) FROM not_captured_success_group")
    recruitments <- sqldf("SELECT DISTINCT(recruitments) FROM not_captured_success_group")
    concursos_participados <- sqldf("SELECT DISTINCT(concursos_participados) FROM not_captured_success_group")
    premios_canjeados <- sqldf("SELECT DISTINCT(premios_canjeados) FROM not_captured_success_group")
    edad <- sqldf("SELECT DISTINCT(edad) FROM not_captured_success_group")
    sistema_registro <- sqldf("SELECT DISTINCT(sistema_registro) FROM not_captured_success_group")
    densidad_videos <- sqldf("SELECT DISTINCT(densidad_videos) FROM not_captured_success_group")
    calidad_videos <- sqldf("SELECT DISTINCT(calidad_videos) FROM not_captured_success_group")
    densidad_concursos <- sqldf("SELECT DISTINCT(densidad_concursos) FROM not_captured_success_group")
    densidad_videos <- sqldf("SELECT DISTINCT(densidad_videos) FROM not_captured_success_group")
    
    
    for (i in (1:not_captured_remaining)){
      print(paste("Iteraring ...", i,not_captured_remaining))
      random_row <- list(
        as.integer(puntos_historicos$puntos_historicos[round(runif(1,1,length(puntos_historicos$puntos_historicos)))]),
        as.factor(genero$genero[round(runif(1,1,length(genero$genero)))]),
        as.factor(dia_afiliacion$dia_afiliacion[round(runif(1,1,length(dia_afiliacion$dia_afiliacion)))]),
        as.integer(shares_totales$shares_totales[round(runif(1,1,length(shares_totales$shares_totales)))]),
        as.integer(recruitments$recruitments[round(runif(1,1,length(recruitments$recruitments)))]),
        as.integer(concursos_participados$concursos_participados[round(runif(1,1,length(concursos_participados$concursos_participados)))]),
        as.integer(premios_canjeados$premios_canjeados[round(runif(1,1,length(premios_canjeados$premios_canjeados)))]),
        as.integer(edad$edad[round(runif(1,1,length(edad$edad)))]),
        as.factor("Not captured"),
        as.factor(sistema_registro$sistema_registro[round(runif(1,1,length(sistema_registro$sistema_registro)))]),
        as.factor(densidad_videos$densidad_videos[round(runif(1,1,length(densidad_videos$densidad_videos)))]),
        as.factor(calidad_videos$calidad_videos[round(runif(1,1,length(calidad_videos$calidad_videos)))]),
        as.factor(densidad_concursos$densidad_concursos[round(runif(1,1,length(densidad_concursos$densidad_concursos)))])
      )
      users_quality <- rbind(users_quality,random_row)
    }
    
    
    # "Weekly, constant"                60
    weekly_constant_remaining <- 1147
    weekly_constant_success_group <- sqldf("SELECT * FROM users_quality WHERE quality = 'Weekly, constant'")
    
    genero <- sqldf("SELECT DISTINCT(genero) FROM weekly_constant_success_group")
    puntos_historicos <- sqldf("SELECT DISTINCT(puntos_historicos) FROM weekly_constant_success_group")
    dia_afiliacion <- sqldf("SELECT DISTINCT(dia_afiliacion) FROM weekly_constant_success_group")
    shares_totales <- sqldf("SELECT DISTINCT(shares_totales) FROM weekly_constant_success_group")
    recruitments <- sqldf("SELECT DISTINCT(recruitments) FROM weekly_constant_success_group")
    concursos_participados <- sqldf("SELECT DISTINCT(concursos_participados) FROM weekly_constant_success_group")
    premios_canjeados <- sqldf("SELECT DISTINCT(premios_canjeados) FROM weekly_constant_success_group")
    edad <- sqldf("SELECT DISTINCT(edad) FROM weekly_constant_success_group")
    sistema_registro <- sqldf("SELECT DISTINCT(sistema_registro) FROM weekly_constant_success_group")
    densidad_videos <- sqldf("SELECT DISTINCT(densidad_videos) FROM weekly_constant_success_group")
    calidad_videos <- sqldf("SELECT DISTINCT(calidad_videos) FROM weekly_constant_success_group")
    densidad_concursos <- sqldf("SELECT DISTINCT(densidad_concursos) FROM weekly_constant_success_group")
    densidad_videos <- sqldf("SELECT DISTINCT(densidad_videos) FROM weekly_constant_success_group")
    
    
    for (i in (1:weekly_constant_remaining)){
      print(paste("Iteraring ...", i,weekly_constant_remaining))
      random_row <- list(
        as.integer(puntos_historicos$puntos_historicos[round(runif(1,1,length(puntos_historicos$puntos_historicos)))]),
        as.factor(genero$genero[round(runif(1,1,length(genero$genero)))]),
        as.factor(dia_afiliacion$dia_afiliacion[round(runif(1,1,length(dia_afiliacion$dia_afiliacion)))]),
        as.integer(shares_totales$shares_totales[round(runif(1,1,length(shares_totales$shares_totales)))]),
        as.integer(recruitments$recruitments[round(runif(1,1,length(recruitments$recruitments)))]),
        as.integer(concursos_participados$concursos_participados[round(runif(1,1,length(concursos_participados$concursos_participados)))]),
        as.integer(premios_canjeados$premios_canjeados[round(runif(1,1,length(premios_canjeados$premios_canjeados)))]),
        as.integer(edad$edad[round(runif(1,1,length(edad$edad)))]),
        as.factor("Weekly, constant"),
        as.factor(sistema_registro$sistema_registro[round(runif(1,1,length(sistema_registro$sistema_registro)))]),
        as.factor(densidad_videos$densidad_videos[round(runif(1,1,length(densidad_videos$densidad_videos)))]),
        as.factor(calidad_videos$calidad_videos[round(runif(1,1,length(calidad_videos$calidad_videos)))]),
        as.factor(densidad_concursos$densidad_concursos[round(runif(1,1,length(densidad_concursos$densidad_concursos)))])
      )
      users_quality <- rbind(users_quality,random_row)
    }
    
    
    # "Weekly, for a month"             34
    weekly_month_remaining    <- 1173
    weekly_month_success_group <- sqldf("SELECT * FROM users_quality WHERE quality = 'Weekly, for a month'")
    
    genero <- sqldf("SELECT DISTINCT(genero) FROM weekly_month_success_group")
    puntos_historicos <- sqldf("SELECT DISTINCT(puntos_historicos) FROM weekly_month_success_group")
    dia_afiliacion <- sqldf("SELECT DISTINCT(dia_afiliacion) FROM weekly_month_success_group")
    shares_totales <- sqldf("SELECT DISTINCT(shares_totales) FROM weekly_month_success_group")
    recruitments <- sqldf("SELECT DISTINCT(recruitments) FROM weekly_month_success_group")
    concursos_participados <- sqldf("SELECT DISTINCT(concursos_participados) FROM weekly_month_success_group")
    premios_canjeados <- sqldf("SELECT DISTINCT(premios_canjeados) FROM weekly_month_success_group")
    edad <- sqldf("SELECT DISTINCT(edad) FROM weekly_month_success_group")
    sistema_registro <- sqldf("SELECT DISTINCT(sistema_registro) FROM weekly_month_success_group")
    densidad_videos <- sqldf("SELECT DISTINCT(densidad_videos) FROM weekly_month_success_group")
    calidad_videos <- sqldf("SELECT DISTINCT(calidad_videos) FROM weekly_month_success_group")
    densidad_concursos <- sqldf("SELECT DISTINCT(densidad_concursos) FROM weekly_month_success_group")
    densidad_videos <- sqldf("SELECT DISTINCT(densidad_videos) FROM weekly_month_success_group")
    
    
    for (i in (1:weekly_month_remaining)){
      print(paste("Iteraring ...", i,weekly_month_remaining))
      random_row <- list(
        as.integer(puntos_historicos$puntos_historicos[round(runif(1,1,length(puntos_historicos$puntos_historicos)))]),
        as.factor(genero$genero[round(runif(1,1,length(genero$genero)))]),
        as.factor(dia_afiliacion$dia_afiliacion[round(runif(1,1,length(dia_afiliacion$dia_afiliacion)))]),
        as.integer(shares_totales$shares_totales[round(runif(1,1,length(shares_totales$shares_totales)))]),
        as.integer(recruitments$recruitments[round(runif(1,1,length(recruitments$recruitments)))]),
        as.integer(concursos_participados$concursos_participados[round(runif(1,1,length(concursos_participados$concursos_participados)))]),
        as.integer(premios_canjeados$premios_canjeados[round(runif(1,1,length(premios_canjeados$premios_canjeados)))]),
        as.integer(edad$edad[round(runif(1,1,length(edad$edad)))]),
        as.factor("Weekly, for a month"),
        as.factor(sistema_registro$sistema_registro[round(runif(1,1,length(sistema_registro$sistema_registro)))]),
        as.factor(densidad_videos$densidad_videos[round(runif(1,1,length(densidad_videos$densidad_videos)))]),
        as.factor(calidad_videos$calidad_videos[round(runif(1,1,length(calidad_videos$calidad_videos)))]),
        as.factor(densidad_concursos$densidad_concursos[round(runif(1,1,length(densidad_concursos$densidad_concursos)))])
      )
      users_quality <- rbind(users_quality,random_row)
    }
    