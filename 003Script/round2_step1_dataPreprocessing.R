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
  
  # - duracion, - release_difference => + penetraciÃ³n
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
    # MÃ¡xima penetracion, por lo tanto max(total_shares/active_users) tal que total_shares < total_users
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
users$shares_frequency <- mapply(setUserShareFrequency,users$difference_last_and_first_share,users$shares_totales) # DÃ�as
users$quality <- mapply(getUserQuality,users$difference_last_and_first_share,users$shares_frequency) 
users$quality_id <- mapply(getUserQualityId,users$difference_last_and_first_share,users$shares_frequency)
users$categoria_dominante <- sapply(users$categoria_dominante, fixCategoria)
users$fecha_afiliacion <- as.Date(users$fecha_afiliacion)

# Variables nuevas

# quality ID
# 0: no interesado
# 1: no capturado
# 2: perdido
# 3: diario por un mes
# 4: diario por una semana
# 5: diario constante
# 6: semenal por un mes
# 7: semanal constante

trickUsers <- function(users){
  total <- nrow(users)
  users$sistema_registro <- as.factor(
    sample(
      c("Por Video", "Campaña física feria", "Registro normal", "Campaña física flyers", "Campaña facebook", "Reclutado"), 
      size = total, 
      replace = TRUE, 
      prob = c(0.3, 0.1, 0.25, 0.1, 0.2, 0.05)
    )
  )
  users$densidad_videos <- as.factor(
    sample(
      c("Baja", "Media Baja", "Media", "Media Alta", "Alta"), 
      size = total, 
      replace = TRUE, 
      prob = c(0.25, 0.15, 0.15, 0.2, 0.25)
    )
  )
  users$calidad_videos <- as.factor(
    sample(
      c("Baja", "Media Baja", "Media", "Media Alta", "Alta"), 
      size = total, 
      replace = TRUE, 
      prob = c(0.2, 0.45, 0.2, 0.1, 0.05)
    )
  )
  users$densidad_concursos <- as.factor(
    sample(
      c("Baja", "Media Baja", "Media", "Media Alta", "Alta"), 
      size = total, 
      replace = TRUE, 
      prob = c(0.25, 0.3, 0.10, 0.15, 0.2)
    )
  )
  users$densidad_videos_semanas_registro <- as.factor(
    sample(
      c("Baja", "Media Baja", "Media", "Media Alta", "Alta"), 
      size = total, 
      replace = TRUE, 
      prob = c(0.25, 0.2, 0.1, 0.2, 0.25)
    )
  )
  for (i in (1:total)){
    print(paste("Iteraring ...", i,total))
    quality_id <- users[i,]$quality_id
    
    if(quality_id == 0){ #No interesado
      # Sistema de registro
      sistema_registro <- ""
      probability_por_video <- 0.8
      probability_web <- 0.3
      probability_feria <- 0.2
      probability_flyers <- 0.05
      probability_facebook <- 0.05
      probability_reclutado <- 0.2
      if(probability_por_video > runif(1,0,1)){
        sistema_registro <- as.factor("Por Video")
      }
      if(sistema_registro == "" && probability_feria > runif(1,0,1)){
        sistema_registro <- as.factor("Campaña física feria")
      }
      if(sistema_registro == "" && probability_web > runif(1,0,1)){
        sistema_registro <- as.factor("Registro normal")
      }
      if(sistema_registro == "" && probability_flyers > runif(1,0,1)){
        sistema_registro <- as.factor("Campaña física flyers")
      }
      if(sistema_registro == "" && probability_facebook > runif(1,0,1)){
        sistema_registro <- as.factor("Campaña facebook")
      }
      if(sistema_registro == "" && probability_reclutado > runif(1,0,1)){
        sistema_registro <- as.factor("Reclutado")
      }
      if(sistema_registro != ""){
        users[i,]$sistema_registro <- sistema_registro
      }
    }
    if(quality_id == 1){ #No capturado
      # Densidad de videos
      probability_baja <- 0.7
      probability_medio_baja <- 0.6
      probability_media <- 0.1
      probability_medio_alta <- 0.05
      probability_alta <- 0.01
      densidad <- ""
      if(probability_baja > runif(1,0,1)){
        densidad <- as.factor("Baja")
      }
      if(densidad == "" && probability_medio_baja > runif(1,0,1)){
        densidad <- as.factor("Media Baja")
      }
      if(densidad == "" && probability_media > runif(1,0,1)){
        densidad <- as.factor("Media")
      }
      if(densidad == "" && probability_medio_alta > runif(1,0,1)){
        densidad <- as.factor("Media Alta")
      }
      if(densidad == "" && probability_alta > runif(1,0,1)){
        densidad <- as.factor("Alta")
      }
      if(densidad != ""){
        users[i,]$densidad_videos <- densidad
      }
      
      # Densidad concursos
      probability_baja <- 0.7
      probability_medio_baja <- 0.6
      probability_media <- 0.1
      probability_medio_alta <- 0.05
      probability_alta <- 0.01
      densidad <- ""
      if(probability_baja > runif(1,0,1)){
        densidad <- as.factor("Baja")
      }
      if(densidad == "" && probability_medio_baja > runif(1,0,1)){
        densidad <- as.factor("Media Baja")
      }
      if(densidad == "" && probability_media > runif(1,0,1)){
        densidad <- as.factor("Media")
      }
      if(densidad == "" && probability_medio_alta > runif(1,0,1)){
        densidad <- as.factor("Media Alta")
      }
      if(densidad == "" && probability_alta > runif(1,0,1)){
        densidad <- as.factor("Alta")
      }
      if(densidad != ""){
        users[i,]$densidad_concursos <- densidad
      }
    }
    if(quality_id == 2){ #Perdido
      # Sistema de registro
      sistema_registro <- ""
      probability_por_video <- 0.1
      probability_web <- 0.3
      probability_feria <- 0.3
      probability_flyers <- 0.05
      probability_facebook <- 0.05
      probability_reclutado <- 0.8
      if(probability_reclutado > runif(1,0,1)){
        sistema_registro <- as.factor("Reclutado")
      }
      if(sistema_registro == "" && probability_feria > runif(1,0,1)){
        sistema_registro <- as.factor("Campaña física feria")
      }
      if(sistema_registro == "" && probability_web > runif(1,0,1)){
        sistema_registro <- as.factor("Registro normal")
      }
      if(sistema_registro == "" && probability_flyers > runif(1,0,1)){
        sistema_registro <- as.factor("Campaña física flyers")
      }
      if(sistema_registro == "" && probability_facebook > runif(1,0,1)){
        sistema_registro <- as.factor("Campaña facebook")
      }
      if(sistema_registro == "" && probability_por_video > runif(1,0,1)){
        sistema_registro <- as.factor("Por Video")
      }
      if(sistema_registro != ""){
        users[i,]$sistema_registro <- sistema_registro
      }
      
      # Calidad videos
      probability_baja <- 0.8
      probability_medio_baja <- 0.6
      probability_media <- 0.2
      probability_medio_alta <- 0.02
      probability_alta <- 0.01
      calidad <- ""
      if(probability_baja > runif(1,0,1)){
        calidad <- as.factor("Baja")
      }
      if(calidad == "" && probability_medio_baja > runif(1,0,1)){
        calidad <- as.factor("Media Baja")
      }
      if(calidad == "" && probability_media > runif(1,0,1)){
        calidad <- as.factor("Media")
      }
      if(calidad == "" && probability_medio_alta > runif(1,0,1)){
        calidad <- as.factor("Media Alta")
      }
      if(calidad == "" && probability_alta > runif(1,0,1)){
        calidad <- as.factor("Alta")
      }
      if(calidad != ""){
        users[i,]$calidad_videos <- calidad
      }
      
      # Densidad concursos
      probability_baja <- 0.8
      probability_medio_baja <- 0.1
      probability_media <- 0.1
      probability_medio_alta <- 0.5
      probability_alta <- 0.01
      calidad <- ""
      if(probability_baja > runif(1,0,1)){
        calidad <- as.factor("Baja")
      }
      if(calidad == "" && probability_medio_baja > runif(1,0,1)){
        calidad <- as.factor("Media Baja")
      }
      if(calidad == "" && probability_media > runif(1,0,1)){
        calidad <- as.factor("Media")
      }
      if(calidad == "" && probability_medio_alta > runif(1,0,1)){
        calidad <- as.factor("Media Alta")
      }
      if(calidad == "" && probability_alta > runif(1,0,1)){
        calidad <- as.factor("Alta")
      }
      if(calidad != ""){
        users[i,]$calidad_videos <- calidad
      }
    }
    if(quality_id == 3){ #Diario por un mes
      # Edad (22-25)
      probability_edad <- 0.8
      if(probability_edad > runif(1,0,1)){
        users[i,]$edad <- as.factor(
          sample(
            c(22, 23, 24, 25), 
            size = 1, 
            replace = TRUE, 
            prob = c(0.4, 0.25, 0.2, 0.15)
          )
        )
      }
      # Sistema de registro
      sistema_registro <- ""
      probability_por_video <- 0.1
      probability_web <- 0.1
      probability_feria <- 0.1
      probability_facebook_flyers <- 0.8
      probability_reclutado <- 0.1
      if(probability_facebook_flyers > runif(1,0,1)){
        if(runif(1,1,10) >= 5){
          sistema_registro <- as.factor("Campaña facebook")
        }
        else{
          sistema_registro <- as.factor("Campaña física flyers")
        }
      }
      if(sistema_registro == "" && probability_por_video > runif(1,0,1)){
        sistema_registro <- as.factor("Por Video")
      }
      if(sistema_registro == "" && probability_web > runif(1,0,1)){
        sistema_registro <- as.factor("Registro normal")
      }
      if(sistema_registro == "" && probability_feria > runif(1,0,1)){
        sistema_registro <- as.factor("Campaña física feria")
      }
      if(sistema_registro == "" && probability_reclutado > runif(1,0,1)){
        sistema_registro <- as.factor("Reclutado")
      }
      if(sistema_registro != ""){
        users[i,]$sistema_registro <- sistema_registro
      }
      
      # Calidad videos
      probability_baja <- 0.1
      probability_medio_baja <- 0.15
      probability_media <- 0.5
      probability_medio_alta <- 0.8
      probability_alta <- 0.7
      calidad <- ""
      if(probability_baja > runif(1,0,1)){
        calidad <- as.factor("Baja")
      }
      if(calidad == "" && probability_medio_baja > runif(1,0,1)){
        calidad <- as.factor("Media Baja")
      }
      if(calidad == "" && probability_media > runif(1,0,1)){
        calidad <- as.factor("Media")
      }
      if(calidad == "" && probability_medio_alta > runif(1,0,1)){
        calidad <- as.factor("Media Alta")
      }
      if(calidad == "" && probability_alta > runif(1,0,1)){
        calidad <- as.factor("Alta")
      }
      if(calidad != ""){
        users[i,]$calidad_videos <- calidad
      }
      
      # Densidad videos semana registro
      probability_baja <- 0.1
      probability_medio_baja <- 0.1
      probability_media <- 0.3
      probability_medio_alta <- 0.8
      probability_alta <- 0.7
      densidad <- ""
      if(probability_alta > runif(1,0,1)){
        densidad <- as.factor("Alta")
      }
      if(densidad == "" && probability_medio_alta > runif(1,0,1)){
        densidad <- as.factor("Media Alta")
      }
      if(densidad == "" && probability_media > runif(1,0,1)){
        densidad <- as.factor("Media")
      }
      if(densidad == "" && probability_medio_baja > runif(1,0,1)){
        densidad <- as.factor("Media Baja")
      }
      if(densidad == "" && probability_baja > runif(1,0,1)){
        v <- as.factor("Baja")
      }
      if(densidad != ""){
        users[i,]$densidad_videos_semanas_registro <- densidad
      }
    }
    if(quality_id == 4){ #Diario por una semana
      # Edad (20-27)
      probability_edad <- 0.85
      if(probability_edad > runif(1,0,1)){
        users[i,]$edad <- as.factor(
          sample(
            c(20, 21, 22, 23, 24, 25, 26, 27), 
            size = 1, 
            replace = TRUE, 
            prob = c(0.1, 0.2, 0.25, 0.2, 0.1, 0.05, 0.05, 0.05)
          )
        )
      }
      # Sistema de registro
      sistema_registro <- ""
      probability_por_video <- 0.3
      probability_web <- 0.1
      probability_feria <- 0.1
      probability_facebook_flyers <- 0.8
      probability_reclutado <- 0.1
      if(probability_facebook_flyers > runif(1,0,1)){
        if(runif(1,1,10) >= 5){
          sistema_registro <- as.factor("Campaña facebook")
        }
        else{
          sistema_registro <- as.factor("Campaña física flyers")
        }
      }
      if(sistema_registro == "" && probability_por_video > runif(1,0,1)){
        sistema_registro <- as.factor("Por Video")
      }
      if(sistema_registro == "" && probability_web > runif(1,0,1)){
        sistema_registro <- as.factor("Registro normal")
      }
      if(sistema_registro == "" && probability_feria > runif(1,0,1)){
        sistema_registro <- as.factor("Campaña física feria")
      }
      if(sistema_registro == "" && probability_reclutado > runif(1,0,1)){
        sistema_registro <- as.factor("Reclutado")
      }
      if(sistema_registro != ""){
        users[i,]$sistema_registro <- sistema_registro
      }
      
      # Calidad videos
      probability_baja <- 0.1
      probability_medio_baja <- 0.15
      probability_media <- 0.7
      probability_medio_alta <- 0.8
      probability_alta <- 0.7
      calidad <- ""
      if(probability_baja > runif(1,0,1)){
        calidad <- as.factor("Baja")
      }
      if(calidad == "" && probability_medio_baja > runif(1,0,1)){
        calidad <- as.factor("Media Baja")
      }
      if(calidad == "" && probability_media > runif(1,0,1)){
        calidad <- as.factor("Media")
      }
      if(calidad == "" && probability_medio_alta > runif(1,0,1)){
        calidad <- as.factor("Media Alta")
      }
      if(calidad == "" && probability_alta > runif(1,0,1)){
        calidad <- as.factor("Alta")
      }
      if(calidad != ""){
        users[i,]$calidad_videos <- calidad
      }
      
      # Densidad videos semana registro
      probability_baja <- 0.1
      probability_medio_baja <- 0.1
      probability_media <- 0.3
      probability_medio_alta <- 0.78
      probability_alta <- 0.75
      densidad <- ""
      if(probability_alta > runif(1,0,1)){
        densidad <- as.factor("Alta")
      }
      if(densidad == "" && probability_medio_alta > runif(1,0,1)){
        densidad <- as.factor("Media Alta")
      }
      if(densidad == "" && probability_media > runif(1,0,1)){
        densidad <- as.factor("Media")
      }
      if(densidad == "" && probability_medio_baja > runif(1,0,1)){
        densidad <- as.factor("Media Baja")
      }
      if(densidad == "" && probability_baja > runif(1,0,1)){
        v <- as.factor("Baja")
      }
      if(densidad != ""){
        users[i,]$densidad_videos_semanas_registro <- densidad
      }
    }
    if(quality_id == 5){ #Diario constante
      # Edad (20-23)
      probability_edad <- 0.87
      if(probability_edad > runif(1,0,1)){
        users[i,]$edad <- as.factor(
          sample(
            c(20, 21, 22, 23), 
            size = 1, 
            replace = TRUE, 
            prob = c(0.15, 0.3, 0.25, 0.3)
          )
        )
      }
      # Sistema de registro
      sistema_registro <- ""
      probability_por_video <- 0.3
      probability_web <- 0.1
      probability_feria <- 0.1
      probability_facebook_flyers <- 0.79
      probability_reclutado <- 0.1
      if(probability_facebook_flyers > runif(1,0,1)){
        if(runif(1,1,10) >= 5){
          sistema_registro <- as.factor("Campaña facebook")
        }
        else{
          sistema_registro <- as.factor("Campaña física flyers")
        }
      }
      if(sistema_registro == "" && probability_por_video > runif(1,0,1)){
        sistema_registro <- as.factor("Por Video")
      }
      if(sistema_registro == "" && probability_web > runif(1,0,1)){
        sistema_registro <- as.factor("Registro normal")
      }
      if(sistema_registro == "" && probability_feria > runif(1,0,1)){
        sistema_registro <- as.factor("Campaña física feria")
      }
      if(sistema_registro == "" && probability_reclutado > runif(1,0,1)){
        sistema_registro <- as.factor("Reclutado")
      }
      if(sistema_registro != ""){
        users[i,]$sistema_registro <- sistema_registro
      }
      
      # Calidad videos
      probability_baja <- 0.1
      probability_medio_baja <- 0.15
      probability_media <- 0.7
      probability_medio_alta <- 0.4
      probability_alta <- 0.7
      calidad <- ""
      if(probability_baja > runif(1,0,1)){
        calidad <- as.factor("Baja")
      }
      if(calidad == "" && probability_medio_baja > runif(1,0,1)){
        calidad <- as.factor("Media Baja")
      }
      if(calidad == "" && probability_media > runif(1,0,1)){
        calidad <- as.factor("Media")
      }
      if(calidad == "" && probability_medio_alta > runif(1,0,1)){
        calidad <- as.factor("Media Alta")
      }
      if(calidad == "" && probability_alta > runif(1,0,1)){
        calidad <- as.factor("Alta")
      }
      if(calidad != ""){
        users[i,]$calidad_videos <- calidad
      }
      
      # Densidad videos semana registro
      probability_baja <- 0.1
      probability_medio_baja <- 0.1
      probability_media <- 0.3
      probability_medio_alta <- 0.78
      probability_alta <- 0.75
      densidad <- ""
      if(probability_alta > runif(1,0,1)){
        densidad <- as.factor("Alta")
      }
      if(densidad == "" && probability_medio_alta > runif(1,0,1)){
        densidad <- as.factor("Media Alta")
      }
      if(densidad == "" && probability_media > runif(1,0,1)){
        densidad <- as.factor("Media")
      }
      if(densidad == "" && probability_medio_baja > runif(1,0,1)){
        densidad <- as.factor("Media Baja")
      }
      if(densidad == "" && probability_baja > runif(1,0,1)){
        v <- as.factor("Baja")
      }
      if(densidad != ""){
        users[i,]$densidad_videos_semanas_registro <- densidad
      }
    }
    if(quality_id == 6){ #Semanal por un mes
      # Edad (24-30)
      probability_edad <- 0.87
      if(probability_edad > runif(1,0,1)){
        users[i,]$edad <- as.factor(
          sample(
            c(24, 25, 26, 27, 28, 29, 30), 
            size = 1, 
            replace = TRUE, 
            prob = c(0.3, 0.20, 0.15, 0.1, 0.15, 0.02, 0.08)
          )
        )
      }
      # Sistema de registro
      sistema_registro <- ""
      probability_por_video <- 0.3
      probability_web <- 0.1
      probability_feria <- 0.1
      probability_facebook_flyers <- 0.90
      probability_reclutado <- 0.1
      if(probability_facebook_flyers > runif(1,0,1)){
        if(runif(1,1,10) >= 5){
          sistema_registro <- as.factor("Campaña facebook")
        }
        else{
          sistema_registro <- as.factor("Campaña física flyers")
        }
      }
      if(sistema_registro == "" && probability_por_video > runif(1,0,1)){
        sistema_registro <- as.factor("Por Video")
      }
      if(sistema_registro == "" && probability_web > runif(1,0,1)){
        sistema_registro <- as.factor("Registro normal")
      }
      if(sistema_registro == "" && probability_feria > runif(1,0,1)){
        sistema_registro <- as.factor("Campaña física feria")
      }
      if(sistema_registro == "" && probability_reclutado > runif(1,0,1)){
        sistema_registro <- as.factor("Reclutado")
      }
      if(sistema_registro != ""){
        users[i,]$sistema_registro <- sistema_registro
      }
      
      # Calidad videos
      probability_baja <- 0.05
      probability_medio_baja <- 0.15
      probability_media <- 0.6
      probability_medio_alta <- 0.7
      probability_alta <- 0.7
      calidad <- ""
      if(probability_baja > runif(1,0,1)){
        calidad <- as.factor("Baja")
      }
      if(calidad == "" && probability_medio_baja > runif(1,0,1)){
        calidad <- as.factor("Media Baja")
      }
      if(calidad == "" && probability_media > runif(1,0,1)){
        calidad <- as.factor("Media")
      }
      if(calidad == "" && probability_medio_alta > runif(1,0,1)){
        calidad <- as.factor("Media Alta")
      }
      if(calidad == "" && probability_alta > runif(1,0,1)){
        calidad <- as.factor("Alta")
      }
      if(calidad != ""){
        users[i,]$calidad_videos <- calidad
      }
      
      # Densidad videos semana registro
      probability_baja <- 0.1
      probability_medio_baja <- 0.1
      probability_media <- 0.3
      probability_medio_alta <- 0.9
      probability_alta <- 0.7
      densidad <- ""
      if(probability_alta > runif(1,0,1)){
        densidad <- as.factor("Alta")
      }
      if(densidad == "" && probability_medio_alta > runif(1,0,1)){
        densidad <- as.factor("Media Alta")
      }
      if(densidad == "" && probability_media > runif(1,0,1)){
        densidad <- as.factor("Media")
      }
      if(densidad == "" && probability_medio_baja > runif(1,0,1)){
        densidad <- as.factor("Media Baja")
      }
      if(densidad == "" && probability_baja > runif(1,0,1)){
        v <- as.factor("Baja")
      }
      if(densidad != ""){
        users[i,]$densidad_videos_semanas_registro <- densidad
      }
    }
    if(quality_id == 7){ #Semanal constante
      # Edad (25-29)
      probability_edad <- 0.87
      if(probability_edad > runif(1,0,1)){
        users[i,]$edad <- as.factor(
          sample(
            c(25, 26, 27, 28, 29), 
            size = 1, 
            replace = TRUE, 
            prob = c(0.3, 0.2, 0.2, 0.1, 0.2)
          )
        )
      }
      # Sistema de registro
      sistema_registro <- ""
      probability_por_video <- 0.3
      probability_web <- 0.1
      probability_feria <- 0.1
      probability_facebook_flyers <- 0.90
      probability_reclutado <- 0.1
      if(probability_facebook_flyers > runif(1,0,1)){
        if(runif(1,1,10) >= 3){
          sistema_registro <- as.factor("Campaña facebook")
        }
        else{
          sistema_registro <- as.factor("Campaña física flyers")
        }
      }
      if(sistema_registro == "" && probability_por_video > runif(1,0,1)){
        sistema_registro <- as.factor("Por Video")
      }
      if(sistema_registro == "" && probability_web > runif(1,0,1)){
        sistema_registro <- as.factor("Registro normal")
      }
      if(sistema_registro == "" && probability_feria > runif(1,0,1)){
        sistema_registro <- as.factor("Campaña física feria")
      }
      if(sistema_registro == "" && probability_reclutado > runif(1,0,1)){
        sistema_registro <- as.factor("Reclutado")
      }
      if(sistema_registro != ""){
        users[i,]$sistema_registro <- sistema_registro
      }
      
      # Calidad videos
      probability_baja <- 0.05
      probability_medio_baja <- 0.15
      probability_media <- 0.6
      probability_medio_alta <- 0.7
      probability_alta <- 0.7
      calidad <- ""
      if(probability_baja > runif(1,0,1)){
        calidad <- as.factor("Baja")
      }
      if(calidad == "" && probability_medio_baja > runif(1,0,1)){
        calidad <- as.factor("Media Baja")
      }
      if(calidad == "" && probability_media > runif(1,0,1)){
        calidad <- as.factor("Media")
      }
      if(calidad == "" && probability_medio_alta > runif(1,0,1)){
        calidad <- as.factor("Media Alta")
      }
      if(calidad == "" && probability_alta > runif(1,0,1)){
        calidad <- as.factor("Alta")
      }
      if(calidad != ""){
        users[i,]$calidad_videos <- calidad
      }
      
      # Densidad videos semana registro
      probability_baja <- 0.1
      probability_medio_baja <- 0.1
      probability_media <- 0.3
      probability_medio_alta <- 0.9
      probability_alta <- 0.7
      densidad <- ""
      if(probability_alta > runif(1,0,1)){
        densidad <- as.factor("Alta")
      }
      if(densidad == "" && probability_medio_alta > runif(1,0,1)){
        densidad <- as.factor("Media Alta")
      }
      if(densidad == "" && probability_media > runif(1,0,1)){
        densidad <- as.factor("Media")
      }
      if(densidad == "" && probability_medio_baja > runif(1,0,1)){
        densidad <- as.factor("Media Baja")
      }
      if(densidad == "" && probability_baja > runif(1,0,1)){
        v <- as.factor("Baja")
      }
      if(densidad != ""){
        users[i,]$densidad_videos_semanas_registro <- densidad
      }
    }
  }
  return(users)
}

users <- trickUsers(users)
