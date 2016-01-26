# Limpieza de variables que no se van a usar

videos_raw <- videos_raw[with(videos_raw, order(id)), ]
videos <- videos_raw
videos[1,]$total_users <- 40

keep <- c(
  "duracion",
  "release_difference", 
  "total_views",
  "shares_first_day", 
  "shares_first_month", 
  "shares_first_week", 
  "total_shares",
  "total_users_at_release", 
  "X1_week_active_users_at_release",
  "X1_week_new_users_at_release",
  "X1_week_active_raffles"
)

videos <- videos[keep]

# Renombrando variables

names(videos)[names(videos)=="X1_week_active_users_at_release"] <- "active_users"
names(videos)[names(videos)=="X1_week_active_raffles"] <- "active_raffles"
names(videos)[names(videos)=="X1_week_new_users_at_release"] <- "new_users"
names(videos)[names(videos)=="total_users_at_release"] <- "total_users"


# Humanizando algunas variables

videos$release_difference <- sapply(videos$release_difference, releaseDifferenceToDays)
videos$active_canjes <- sapply(videos$active_raffles, function(raf){
  if(raf == 0 ){
    return (round(runif(1,0,1)))
  }
  nominador <- (raf-runif(1,0,(raf-0.2)))
  if(nominador == 0){
    return (nominador)
  }
  canjes <- (nominador)/round(runif(1,1.5,2.5))
  if(canjes < 0){ canjes <- canjes*-1}
  return (round(canjes))
})
videos$duracion <- sapply(videos$duracion, function(d){
  if(d == 0){
    return (round(runif(1,25,180)))
  }
  return(d)
})
videos$active_users <- sapply(videos$active_users, function(a){
  if(a == 0){
    return (round(runif(1,10,15)))
  }
  if(a < 10){
    return (a+round(runif(1,10,12)))
  }
  if(a < 20){
    return (a+round(runif(1,3,7)))
  }
  if(a < 30){
    return (a+round(runif(1,2,5)))
  }
  return(a)
})
videos$penetracion <- mapply(function(total_shares, total_users){
  return (round(total_shares*100/total_users, 2))
}, videos$total_shares, videos$total_users)

# processing

total <- nrow(videos)
for (i in (1:total)){
  print(paste("Iteraring ...", i,total))
  duracion <- videos[i,]$duracion
  release_difference <- videos[i,]$release_difference
  shares_day <- videos[i,]$shares_first_day
  shares_week <- videos[i,]$shares_first_week
  shares_month <- videos[i,]$shares_first_month
  active_users <- videos[i,]$active_users
  active_raffles <- videos[i,]$active_raffles
  new_users <- videos[i,]$new_users
  total_users <- videos[i,]$total_users
  total_shares <- videos[i,]$total_shares
  
  # + raffles => + actives
  probability <- 0
  if(active_users > 10){ probability_raffles <- 0.001 }
  if(active_users > 20){ probability_raffles <- 0.005 }
  if(active_users > 30){ probability_raffles <- 0.015 }
  if(active_users > 40){ probability_raffles <- 0.05 }
  if(active_users > 50){ probability_raffles <- 0.10 }
  if(active_users > 60){ probability_raffles <- 0.15 }
  if(active_users > 70){ probability_raffles <- 0.67 }
  if(active_users > 80){ probability_raffles <- 0.80 }
  if(active_users > 90){ probability_raffles <- 0.90 }
  if(active_users > 100){ probability_raffles <- 0.97 }
  
  if(probability_raffles > runif(1,0,1)){
    videos[i,]$active_raffles <- round(runif(1,9,11))
  }
  else{
    if(probability_raffles > runif(1,0,1)){
      videos[i,]$active_raffles <- round(runif(1,6,8))
    }
    else{
      if(probability_raffles > runif(1,0,1)){
        videos[i,]$active_raffles <- round(runif(1,4,5))
      }
      else{
        videos[i,]$active_raffles <- round(runif(1,0,3))
      }
    }
  }
  
  # + canjes =/> + actives
  probability <- 0
  if(active_users > 10){ probability <- 0.01 }
  if(active_users > 20){ probability <- 0.03 }
  if(active_users > 30){ probability <- 0.08 }
  if(active_users > 40){ probability <- 0.20 }
  if(active_users > 50){ probability <- 0.32 }
  if(active_users > 60){ probability <- 0.50 }
  if(active_users > 70){ probability <- 0.72 }
  if(active_users > 80){ probability <- 0.85 }
  if(active_users > 90){ probability <- 0.92 }
  if(active_users > 100){ probability <- 0.98 }
  
  if(probability > runif(1,0,1)){
    videos[i,]$active_canjes <- round(runif(1,0,1))
  }
  else{
    if(probability > runif(1,0,1)){
      videos[i,]$active_canjes <- round(runif(1,0,2))
    }
    else{
      if(probability > runif(1,0,1)){
        videos[i,]$active_canjes <- round(runif(1,0,3))
      }
      else{
        videos[i,]$active_canjes <- round(runif(1,1,4))
      }
    }
  }
  
  # - duracion, - release_difference => + penetración
  probability_duracion <- 0
  if(duracion < 300){ probability_duracion <- 0.006 }
  if(duracion < 270){ probability_duracion <- 0.05 }
  if(duracion < 240){ probability_duracion <- 0.10 }
  if(duracion < 210){ probability_duracion <- 0.22 }
  if(duracion < 180){ probability_duracion <- 0.40 }
  if(duracion < 150){ probability_duracion <- 0.67 }
  if(duracion < 120){ probability_duracion <- 0.75 }
  if(duracion < 90){ probability_duracion <- 0.83 }
  if(duracion < 60){ probability_duracion <- 0.89 }
  if(duracion < 30){ probability_duracion <- 0.65 }
  
  probability_release_difference <- 0
  if(release_difference == 0){ probability_release_difference <- 0.93 }
  if(release_difference == 1){ probability_release_difference <- 0.87 }
  if(release_difference == 2){ probability_release_difference <- 0.80 }
  if(release_difference == 3){ probability_release_difference <- 0.67 }
  if(release_difference == 4){ probability_release_difference <- 0.40 }
  if(release_difference == 5){ probability_release_difference <- 0.22 }
  if(release_difference >= 7){ probability_release_difference <- 0.05 }
  
  if(probability_duracion > runif(1,0,1) && probability_release_difference > runif(1,0,1)){
    # Máxima penetracion, por lo tanto max(total_shares/active_users) tal que total_shares < total_users
    videos[i,]$penetracion <- round(runif(1,0.7,0.9),2)
  }
  else{
    if(probability_duracion > runif(1,0,1) && probability_release_difference > runif(1,0,1)){
      videos[i,]$penetracion <- round(runif(1,0.5,0.7),2)
    }
    else{
      if(probability_duracion > runif(1,0,1) && probability_release_difference > runif(1,0,1)){
        videos[i,]$penetracion <- round(runif(1,0.3,0.5),2)
      }
      else{
        videos[i,]$penetracion <- round(runif(1,0.01,0.3),2)
      }
    }
  }
  videos[i,]$total_shares <- round(active_users*videos[i,]$penetracion)
  if(videos[i,]$total_shares == 0){
    videos[i,]$total_shares <- round(runif(1,1,4))
    videos[i,]$penetracion <- videos[i,]$total_shares/videos[i,]$active_users
  }
}










# USERS


# Limpieza de variables que no se van a usar

users_raw <- users_raw[with(users_raw, order(id)), ]
users <- users_raw

keep <- c(
  "puntos_historicos",
  "fecha_afiliacion",
  "uni",
  "genero",
  "nacimiento",
  "hora_afiliacion",
  "dia_afiliacion",
  "puntos_gastados",
  "shares_totales",
  "categoria_dominante",
  "recruitments",
  "concursos_participados",
  "premios_canjeados",
  "tickets_canjeados",
  "difference_last_and_first_share",
  "difference_last_raffle_first_share"
)

users <- users[keep]

# Humanizando algunas variables

users$hora_afiliacion <- as.factor(users$hora_afiliacion)
users$dia_afiliacion <- sapply(users$dia_afiliacion, getDayName)
users$edad <- sapply(users$nacimiento, getEdad)
users$tickets_canjeados <- sapply(users$tickets_canjeados, cleanTicketsCanjeados)
users$shares_frequency <- mapply(setUserShareFrequency,users$difference_last_and_first_share,users$shares_totales) # Días
users$quality <- mapply(getUserQuality,users$difference_last_and_first_share,users$shares_frequency) 
users$quality_id <- mapply(getUserQualityId,users$difference_last_and_first_share,users$shares_frequency)
users$categoria_dominante <- sapply(users$categoria_dominante, fixCategoria)
users$fecha_afiliacion <- as.Date(users$fecha_afiliacion)

# processing

total <- nrow(users)
for (i in (1:total)){
  print(paste("Iteraring ...", i,total))
  edad <- users[i,]$edad
  if()
  release_difference <- videos[i,]$release_difference
  shares_day <- videos[i,]$shares_first_day
  shares_week <- videos[i,]$shares_first_week
  shares_month <- videos[i,]$shares_first_month
  active_users <- videos[i,]$active_users
  active_raffles <- videos[i,]$active_raffles
  new_users <- videos[i,]$new_users
  total_users <- videos[i,]$total_users
  total_shares <- videos[i,]$total_shares
  
  # + raffles => + actives
  probability <- 0
  if(active_users > 10){ probability_raffles <- 0.001 }
  if(active_users > 20){ probability_raffles <- 0.005 }
  if(active_users > 30){ probability_raffles <- 0.015 }
  if(active_users > 40){ probability_raffles <- 0.05 }
  if(active_users > 50){ probability_raffles <- 0.10 }
  if(active_users > 60){ probability_raffles <- 0.15 }
  if(active_users > 70){ probability_raffles <- 0.67 }
  if(active_users > 80){ probability_raffles <- 0.80 }
  if(active_users > 90){ probability_raffles <- 0.90 }
  if(active_users > 100){ probability_raffles <- 0.97 }
  
  if(probability_raffles > runif(1,0,1)){
    videos[i,]$active_raffles <- round(runif(1,9,11))
  }
  else{
    if(probability_raffles > runif(1,0,1)){
      videos[i,]$active_raffles <- round(runif(1,6,8))
    }
    else{
      if(probability_raffles > runif(1,0,1)){
        videos[i,]$active_raffles <- round(runif(1,4,5))
      }
      else{
        videos[i,]$active_raffles <- round(runif(1,0,3))
      }
    }
  }
  
  # + canjes =/> + actives
  probability <- 0
  if(active_users > 10){ probability <- 0.01 }
  if(active_users > 20){ probability <- 0.03 }
  if(active_users > 30){ probability <- 0.08 }
  if(active_users > 40){ probability <- 0.20 }
  if(active_users > 50){ probability <- 0.32 }
  if(active_users > 60){ probability <- 0.50 }
  if(active_users > 70){ probability <- 0.72 }
  if(active_users > 80){ probability <- 0.85 }
  if(active_users > 90){ probability <- 0.92 }
  if(active_users > 100){ probability <- 0.98 }
  
  if(probability > runif(1,0,1)){
    videos[i,]$active_canjes <- round(runif(1,0,1))
  }
  else{
    if(probability > runif(1,0,1)){
      videos[i,]$active_canjes <- round(runif(1,0,2))
    }
    else{
      if(probability > runif(1,0,1)){
        videos[i,]$active_canjes <- round(runif(1,0,3))
      }
      else{
        videos[i,]$active_canjes <- round(runif(1,1,4))
      }
    }
  }
  
  # - duracion, - release_difference => + penetración
  probability_duracion <- 0
  if(duracion < 300){ probability_duracion <- 0.006 }
  if(duracion < 270){ probability_duracion <- 0.05 }
  if(duracion < 240){ probability_duracion <- 0.10 }
  if(duracion < 210){ probability_duracion <- 0.22 }
  if(duracion < 180){ probability_duracion <- 0.40 }
  if(duracion < 150){ probability_duracion <- 0.67 }
  if(duracion < 120){ probability_duracion <- 0.75 }
  if(duracion < 90){ probability_duracion <- 0.83 }
  if(duracion < 60){ probability_duracion <- 0.89 }
  if(duracion < 30){ probability_duracion <- 0.65 }
  
  probability_release_difference <- 0
  if(release_difference == 0){ probability_release_difference <- 0.93 }
  if(release_difference == 1){ probability_release_difference <- 0.87 }
  if(release_difference == 2){ probability_release_difference <- 0.80 }
  if(release_difference == 3){ probability_release_difference <- 0.67 }
  if(release_difference == 4){ probability_release_difference <- 0.40 }
  if(release_difference == 5){ probability_release_difference <- 0.22 }
  if(release_difference >= 7){ probability_release_difference <- 0.05 }
  
  if(probability_duracion > runif(1,0,1) && probability_release_difference > runif(1,0,1)){
    # Máxima penetracion, por lo tanto max(total_shares/active_users) tal que total_shares < total_users
    videos[i,]$penetracion <- round(runif(1,0.7,0.9),2)
  }
  else{
    if(probability_duracion > runif(1,0,1) && probability_release_difference > runif(1,0,1)){
      videos[i,]$penetracion <- round(runif(1,0.5,0.7),2)
    }
    else{
      if(probability_duracion > runif(1,0,1) && probability_release_difference > runif(1,0,1)){
        videos[i,]$penetracion <- round(runif(1,0.3,0.5),2)
      }
      else{
        videos[i,]$penetracion <- round(runif(1,0.01,0.3),2)
      }
    }
  }
  videos[i,]$total_shares <- round(active_users*videos[i,]$penetracion)
  if(videos[i,]$total_shares == 0){
    videos[i,]$total_shares <- round(runif(1,1,4))
    videos[i,]$penetracion <- videos[i,]$total_shares/videos[i,]$active_users
  }
}