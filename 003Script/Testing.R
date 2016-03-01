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
  "X2_week_active_raffles"
)

videos <- videos[keep]

# Renombrando variables

names(videos)[names(videos)=="X1_week_active_users_at_release"] <- "active_users"
names(videos)[names(videos)=="X2_week_active_raffles"] <- "active_raffles"
names(videos)[names(videos)=="X1_week_new_users_at_release"] <- "new_users"
names(videos)[names(videos)=="total_users_at_release"] <- "total_users"


# Humanizando algunas variables

videos$release_difference <- sapply(videos$release_difference, function(x){
  if(x < 0 || is.na(x)){
    x <- round(runif(1,0,30))
  }
  if(x == 0){
    return(0)
  }
  return(round((x/60/60/24),0))
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
  }

videos <- trickVideos(videos)
write.table(videos,file="002ProcessedData/videos.csv", quote=FALSE, sep=";", row.names=FALSE, eol="\n")