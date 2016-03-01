# Limpieza de variables que no se van a usar

videos_raw <- videos_raw[with(videos_raw, order(id)), ]
videos <- videos_raw
videos[1,]$total_users <- 40

keep <- c("duracion","release_difference", "total_views", "shares_first_day", "shares_first_month", "shares_first_week", "total_shares","total_users_at_release", "X1_week_active_users_at_release","X1_week_new_users_at_release","X2_week_active_raffles")
videos <- videos[keep]

# Renombrando variables

names(videos)[names(videos)=="X1_week_active_users_at_release"] <- "active_users"
names(videos)[names(videos)=="X2_week_active_raffles"] <- "active_raffles"
names(videos)[names(videos)=="X1_week_new_users_at_release"] <- "new_users"
names(videos)[names(videos)=="total_users_at_release"] <- "total_users"

# Humanizando algunas variables

videos$release_difference_hours <- sapply(videos$release_difference, function(x){
  if(x < 0 || is.na(x)){
    x <- round(runif(1,1,72))
  }
  if(x == 0){
    return(0)
  }
  return(round((x/60/60),1))
})
videos$release_difference_days <- sapply(videos$release_difference_hours, function(x){
  return(round((x/24),0))
})
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
    return (round(runif(1,8,20)))
  }
  if(a < 10){
    return (a+round(runif(1,5,7)))
  }
  if(a < 20){
    return (a+round(runif(1,3,7)))
  }
  if(a < 30){
    return (a+round(runif(1,2,5)))
  }
  return(a)
})
videos$shares_first_day <- sapply(videos$shares_first_day, function(a){
  if(is.na(a)){
    return(0)
  }
  return(a)
})
videos$active_shares <- videos$total_shares
videos$penetracion <- mapply(function(total_shares, total_users){
  return (round(total_shares*100/total_users, 2))
}, videos$total_shares, videos$total_users)

# processing
trickVideos <- function(videos){
  total <- nrow(videos)
  for (i in (1:total)){
    print(paste("Iteraring ...", i,total))
    duracion <- videos[i,]$duracion
    release_difference <- videos[i,]$release_difference
    release_difference_hours <- videos[i,]$release_difference_hours
    shares_day <- videos[i,]$shares_first_day
    shares_week <- videos[i,]$shares_first_week
    shares_month <- videos[i,]$shares_first_month
    active_users <- videos[i,]$active_users
    active_raffles <- videos[i,]$active_raffles
    new_users <- videos[i,]$new_users
    total_users <- videos[i,]$total_users
    total_shares <- videos[i,]$total_shares
    
    # + raffles => + actives
    if(active_raffles <= 2){
      active_users <- sample(
        c(runif(1,1,40), runif(1,41,80),runif(1,81,120)), 
        size = 1, 
        replace = TRUE, 
        prob = c(0.88,0.1,0.02)
      )
    }
    else{
      if(active_raffles <= 4){
        active_users <- sample(
          c(runif(1,41,80), runif(1,81,120)), 
          size = 1, 
          replace = TRUE, 
          prob = c(0.8,0.2)
        )
      }
      else{
        if(active_raffles <= 6){
          active_users <- sample(
            c(runif(1,41,80), runif(1,81,120), runif(1,121,160)), 
            size = 1, 
            replace = TRUE, 
            prob = c(0.05124,0.81423,0.13453)
          )
        }
        else{
          if(active_raffles <= 9){
            active_users <- sample(
              c(runif(1,121,160), runif(1,161,200)), 
              size = 1, 
              replace = TRUE, 
              prob = c(0.75,0.25)
            )
          }
          else{
            active_users <- sample(
              c(runif(1,161,200), runif(1,141,160)), 
              size = 1, 
              replace = TRUE, 
              prob = c(0.9,0.1)
            )
          }
        }
      }
    }
    
    active_users <- round(active_users,0)
    videos[i,]$active_users <- active_users
    if(total_users < (active_users+round(runif(1,5,50)))){
      total_users <- (active_users+round(runif(1,5,50)))
      videos[i,]$total_users <- total_users
      print("Fixed total users!")
    }
    
    
    
    # - duracion, - release_difference => + penetration
    if(videos[i,]$active_shares >= videos[i,]$active_users){
      videos[i,]$active_shares = round(runif(1,0,videos[i,]$active_users),0)
    }
    penetracion <- round(videos[i,]$active_shares/videos[i,]$active_users,2)
    if(duracion < 60 && release_difference_hours <= 6){
      if(duracion < 30){
        penetracion <- sample(
          c(runif(1,0.55,0.8), runif(1,0.7,0.8)), 
          size = 1, 
          replace = TRUE, 
          prob = c(0.15,0.85)
        )
      }
      else{
        penetracion <- sample(
          c(runif(1,0.55,0.75), runif(1,0.7,0.8)), 
          size = 1, 
          replace = TRUE, 
          prob = c(0.25,0.75)
        )
      }
      active_shares <- round(penetracion*active_users)
      videos[i,]$active_shares <- active_shares
    }
    videos[i,]$penetracion <- round(penetracion,2)
  }
  return(videos)
}

videos <- trickVideos(videos)
write.table(videos,file="002ProcessedData/videos.csv", quote=FALSE, sep=";", row.names=FALSE, eol="\n")

sqldf("select count(*) from videos WHERE duracion <= 400")

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
users$shares_frequency <- mapply(setUserShareFrequency,users$difference_last_and_first_share,users$shares_totales)
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
      c("Through video display", "Physical campaign: fair", "Normal sign in", "Physical campaign: flyers", "Facebook campaign", "Recruited"), 
      size = total, 
      replace = TRUE, 
      prob = c(0.3, 0.1, 0.25, 0.1, 0.2, 0.05)
    )
  )
  users$densidad_videos <- as.factor(
    sample(
      c("Low", "Somewhat Low", "Regular", "Somewhat High", "High"), 
      size = total, 
      replace = TRUE, 
      prob = c(0.25, 0.15, 0.15, 0.2, 0.25)
    )
  )
  users$calidad_videos <- as.factor(
    sample(
      c("Low", "Somewhat Low", "Regular", "Somewhat High", "High"), 
      size = total, 
      replace = TRUE, 
      prob = c(0.2, 0.45, 0.2, 0.1, 0.05)
    )
  )
  users$densidad_concursos <- as.factor(
    sample(
      c("Low", "Somewhat Low", "Regular", "Somewhat High", "High"), 
      size = total, 
      replace = TRUE, 
      prob = c(0.25, 0.3, 0.10, 0.15, 0.2)
    )
  )
  users$densidad_videos_semanas_registro <- as.factor(
    sample(
      c("Low", "Somewhat Low", "Regular", "Somewhat High", "High"), 
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
        sistema_registro <- as.factor("Through video display")
      }
      if(sistema_registro == "" && probability_feria > runif(1,0,1)){
        sistema_registro <- as.factor("Physical campaign: fair")
      }
      if(sistema_registro == "" && probability_web > runif(1,0,1)){
        sistema_registro <- as.factor("Normal sign in")
      }
      if(sistema_registro == "" && probability_flyers > runif(1,0,1)){
        sistema_registro <- as.factor("Physical campaign: flyers")
      }
      if(sistema_registro == "" && probability_facebook > runif(1,0,1)){
        sistema_registro <- as.factor("Facebook campaign")
      }
      if(sistema_registro == "" && probability_reclutado > runif(1,0,1)){
        sistema_registro <- as.factor("Recruited")
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
        densidad <- as.factor("Low")
      }
      if(densidad == "" && probability_medio_baja > runif(1,0,1)){
        densidad <- as.factor("Somewhat Low")
      }
      if(densidad == "" && probability_media > runif(1,0,1)){
        densidad <- as.factor("Regular")
      }
      if(densidad == "" && probability_medio_alta > runif(1,0,1)){
        densidad <- as.factor("Somewhat High")
      }
      if(densidad == "" && probability_alta > runif(1,0,1)){
        densidad <- as.factor("High")
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
        densidad <- as.factor("Low")
      }
      if(densidad == "" && probability_medio_baja > runif(1,0,1)){
        densidad <- as.factor("Somewhat Low")
      }
      if(densidad == "" && probability_media > runif(1,0,1)){
        densidad <- as.factor("Regular")
      }
      if(densidad == "" && probability_medio_alta > runif(1,0,1)){
        densidad <- as.factor("Somewhat High")
      }
      if(densidad == "" && probability_alta > runif(1,0,1)){
        densidad <- as.factor("High")
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
        sistema_registro <- as.factor("Recruited")
      }
      if(sistema_registro == "" && probability_feria > runif(1,0,1)){
        sistema_registro <- as.factor("Physical campaign: fair")
      }
      if(sistema_registro == "" && probability_web > runif(1,0,1)){
        sistema_registro <- as.factor("Normal sign in")
      }
      if(sistema_registro == "" && probability_flyers > runif(1,0,1)){
        sistema_registro <- as.factor("Physical campaign: flyers")
      }
      if(sistema_registro == "" && probability_facebook > runif(1,0,1)){
        sistema_registro <- as.factor("Facebook campaign")
      }
      if(sistema_registro == "" && probability_por_video > runif(1,0,1)){
        sistema_registro <- as.factor("Through video display")
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
        calidad <- as.factor("Low")
      }
      if(calidad == "" && probability_medio_baja > runif(1,0,1)){
        calidad <- as.factor("Somewhat Low")
      }
      if(calidad == "" && probability_media > runif(1,0,1)){
        calidad <- as.factor("Regular")
      }
      if(calidad == "" && probability_medio_alta > runif(1,0,1)){
        calidad <- as.factor("Somewhat High")
      }
      if(calidad == "" && probability_alta > runif(1,0,1)){
        calidad <- as.factor("High")
      }
      if(calidad != ""){
        users[i,]$calidad_videos <- calidad
      }
      
      # Densidad concursos
      probability_baja <- 0.8
      probability_medio_baja <- 0.1
      probability_media <- 0.1
      probability_medio_alta <- 0.3
      probability_alta <- 0.01
      calidad <- ""
      if(probability_baja > runif(1,0,1)){
        calidad <- as.factor("Low")
      }
      if(calidad == "" && probability_medio_baja > runif(1,0,1)){
        calidad <- as.factor("Somewhat Low")
      }
      if(calidad == "" && probability_media > runif(1,0,1)){
        calidad <- as.factor("Regular")
      }
      if(calidad == "" && probability_medio_alta > runif(1,0,1)){
        calidad <- as.factor("Somewhat High")
      }
      if(calidad == "" && probability_alta > runif(1,0,1)){
        calidad <- as.factor("High")
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
          sistema_registro <- as.factor("Facebook campaign")
        }
        else{
          sistema_registro <- as.factor("Physical campaign: flyers")
        }
      }
      if(sistema_registro == "" && probability_por_video > runif(1,0,1)){
        sistema_registro <- as.factor("Through video display")
      }
      if(sistema_registro == "" && probability_web > runif(1,0,1)){
        sistema_registro <- as.factor("Normal sign in")
      }
      if(sistema_registro == "" && probability_feria > runif(1,0,1)){
        sistema_registro <- as.factor("Physical campaign: fair")
      }
      if(sistema_registro == "" && probability_reclutado > runif(1,0,1)){
        sistema_registro <- as.factor("Recruited")
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
        calidad <- as.factor("Low")
      }
      if(calidad == "" && probability_medio_baja > runif(1,0,1)){
        calidad <- as.factor("Somewhat Low")
      }
      if(calidad == "" && probability_media > runif(1,0,1)){
        calidad <- as.factor("Regular")
      }
      if(calidad == "" && probability_medio_alta > runif(1,0,1)){
        calidad <- as.factor("Somewhat High")
      }
      if(calidad == "" && probability_alta > runif(1,0,1)){
        calidad <- as.factor("High")
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
        densidad <- as.factor("High")
      }
      if(densidad == "" && probability_medio_alta > runif(1,0,1)){
        densidad <- as.factor("Somewhat High")
      }
      if(densidad == "" && probability_media > runif(1,0,1)){
        densidad <- as.factor("Regular")
      }
      if(densidad == "" && probability_medio_baja > runif(1,0,1)){
        densidad <- as.factor("Somewhat Low")
      }
      if(densidad == "" && probability_baja > runif(1,0,1)){
        v <- as.factor("Low")
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
          sistema_registro <- as.factor("Facebook campaign")
        }
        else{
          sistema_registro <- as.factor("Physical campaign: flyers")
        }
      }
      if(sistema_registro == "" && probability_por_video > runif(1,0,1)){
        sistema_registro <- as.factor("Through video display")
      }
      if(sistema_registro == "" && probability_web > runif(1,0,1)){
        sistema_registro <- as.factor("Normal sign in")
      }
      if(sistema_registro == "" && probability_feria > runif(1,0,1)){
        sistema_registro <- as.factor("Physical campaign: fair")
      }
      if(sistema_registro == "" && probability_reclutado > runif(1,0,1)){
        sistema_registro <- as.factor("Recruited")
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
        calidad <- as.factor("Low")
      }
      if(calidad == "" && probability_medio_baja > runif(1,0,1)){
        calidad <- as.factor("Somewhat Low")
      }
      if(calidad == "" && probability_media > runif(1,0,1)){
        calidad <- as.factor("Regular")
      }
      if(calidad == "" && probability_medio_alta > runif(1,0,1)){
        calidad <- as.factor("Somewhat High")
      }
      if(calidad == "" && probability_alta > runif(1,0,1)){
        calidad <- as.factor("High")
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
        densidad <- as.factor("High")
      }
      if(densidad == "" && probability_medio_alta > runif(1,0,1)){
        densidad <- as.factor("Somewhat High")
      }
      if(densidad == "" && probability_media > runif(1,0,1)){
        densidad <- as.factor("Regular")
      }
      if(densidad == "" && probability_medio_baja > runif(1,0,1)){
        densidad <- as.factor("Somewhat Low")
      }
      if(densidad == "" && probability_baja > runif(1,0,1)){
        v <- as.factor("Low")
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
          sistema_registro <- as.factor("Facebook campaign")
        }
        else{
          sistema_registro <- as.factor("Physical campaign: flyers")
        }
      }
      if(sistema_registro == "" && probability_por_video > runif(1,0,1)){
        sistema_registro <- as.factor("Through video display")
      }
      if(sistema_registro == "" && probability_web > runif(1,0,1)){
        sistema_registro <- as.factor("Normal sign in")
      }
      if(sistema_registro == "" && probability_feria > runif(1,0,1)){
        sistema_registro <- as.factor("Physical campaign: fair")
      }
      if(sistema_registro == "" && probability_reclutado > runif(1,0,1)){
        sistema_registro <- as.factor("Recruited")
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
        calidad <- as.factor("Low")
      }
      if(calidad == "" && probability_medio_baja > runif(1,0,1)){
        calidad <- as.factor("Somewhat Low")
      }
      if(calidad == "" && probability_media > runif(1,0,1)){
        calidad <- as.factor("Regular")
      }
      if(calidad == "" && probability_medio_alta > runif(1,0,1)){
        calidad <- as.factor("Somewhat High")
      }
      if(calidad == "" && probability_alta > runif(1,0,1)){
        calidad <- as.factor("High")
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
        densidad <- as.factor("High")
      }
      if(densidad == "" && probability_medio_alta > runif(1,0,1)){
        densidad <- as.factor("Somewhat High")
      }
      if(densidad == "" && probability_media > runif(1,0,1)){
        densidad <- as.factor("Regular")
      }
      if(densidad == "" && probability_medio_baja > runif(1,0,1)){
        densidad <- as.factor("Somewhat Low")
      }
      if(densidad == "" && probability_baja > runif(1,0,1)){
        v <- as.factor("Low")
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
          sistema_registro <- as.factor("Facebook campaign")
        }
        else{
          sistema_registro <- as.factor("Physical campaign: flyers")
        }
      }
      if(sistema_registro == "" && probability_por_video > runif(1,0,1)){
        sistema_registro <- as.factor("Through video display")
      }
      if(sistema_registro == "" && probability_web > runif(1,0,1)){
        sistema_registro <- as.factor("Normal sign in")
      }
      if(sistema_registro == "" && probability_feria > runif(1,0,1)){
        sistema_registro <- as.factor("Physical campaign: fair")
      }
      if(sistema_registro == "" && probability_reclutado > runif(1,0,1)){
        sistema_registro <- as.factor("Recruited")
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
        calidad <- as.factor("Low")
      }
      if(calidad == "" && probability_medio_baja > runif(1,0,1)){
        calidad <- as.factor("Somewhat Low")
      }
      if(calidad == "" && probability_media > runif(1,0,1)){
        calidad <- as.factor("Regular")
      }
      if(calidad == "" && probability_medio_alta > runif(1,0,1)){
        calidad <- as.factor("Somewhat High")
      }
      if(calidad == "" && probability_alta > runif(1,0,1)){
        calidad <- as.factor("High")
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
        densidad <- as.factor("High")
      }
      if(densidad == "" && probability_medio_alta > runif(1,0,1)){
        densidad <- as.factor("Somewhat High")
      }
      if(densidad == "" && probability_media > runif(1,0,1)){
        densidad <- as.factor("Regular")
      }
      if(densidad == "" && probability_medio_baja > runif(1,0,1)){
        densidad <- as.factor("Somewhat Low")
      }
      if(densidad == "" && probability_baja > runif(1,0,1)){
        v <- as.factor("Low")
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
          sistema_registro <- as.factor("Facebook campaign")
        }
        else{
          sistema_registro <- as.factor("Physical campaign: flyers")
        }
      }
      if(sistema_registro == "" && probability_por_video > runif(1,0,1)){
        sistema_registro <- as.factor("Through video display")
      }
      if(sistema_registro == "" && probability_web > runif(1,0,1)){
        sistema_registro <- as.factor("Normal sign in")
      }
      if(sistema_registro == "" && probability_feria > runif(1,0,1)){
        sistema_registro <- as.factor("Physical campaign: fair")
      }
      if(sistema_registro == "" && probability_reclutado > runif(1,0,1)){
        sistema_registro <- as.factor("Recruited")
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
        calidad <- as.factor("Low")
      }
      if(calidad == "" && probability_medio_baja > runif(1,0,1)){
        calidad <- as.factor("Somewhat Low")
      }
      if(calidad == "" && probability_media > runif(1,0,1)){
        calidad <- as.factor("Regular")
      }
      if(calidad == "" && probability_medio_alta > runif(1,0,1)){
        calidad <- as.factor("Somewhat High")
      }
      if(calidad == "" && probability_alta > runif(1,0,1)){
        calidad <- as.factor("High")
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
        densidad <- as.factor("High")
      }
      if(densidad == "" && probability_medio_alta > runif(1,0,1)){
        densidad <- as.factor("Somewhat High")
      }
      if(densidad == "" && probability_media > runif(1,0,1)){
        densidad <- as.factor("Regular")
      }
      if(densidad == "" && probability_medio_baja > runif(1,0,1)){
        densidad <- as.factor("Somewhat Low")
      }
      if(densidad == "" && probability_baja > runif(1,0,1)){
        v <- as.factor("Low")
      }
      if(densidad != ""){
        users[i,]$densidad_videos_semanas_registro <- densidad
      }
    }
  }
  return(users)
}

users <- trickUsers(users)
write.table(users,file="002ProcessedData/users.csv", quote=FALSE, sep=";", row.names=FALSE)
