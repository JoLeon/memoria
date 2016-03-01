# WIN

setwd("C:/Users/J/Documents/GitHub/memoria")
users_processed <- read.csv("002ProcessedData/users.csv", header = TRUE, sep=";")
videos_processed <- read.csv("002ProcessedData/videos.csv", header = TRUE, sep=";")

# Funciones

disRaffles <- function(raffles){
  if(raffles >= 10){
    return(as.factor("10 o más"))
  }
  if(raffles >= 7){
    return(as.factor("Entre 7 y 9"))
  }
  if(raffles >= 5){
    return(as.factor("Entre 5 y 6"))
  }
  if(raffles >= 3){
    return(as.factor("Entre 3 y 4"))
  }
  return(as.factor("2 o menos"))
}
disActiveUsers <- function(users){
  if(users > 160){ return(as.factor("Más de 160"))}
  if(users > 120){ return(as.factor("(120, 160]"))}
  if(users > 80){ return(as.factor("(80, 120]"))}
  if(users > 40){ return(as.factor("(40, 80]"))}
  return(as.factor("[0, 40]"))
}
disDuracion <- function(duracion){
  if(duracion <= 30){
    return(as.factor("30s o menos"))
  }
  if(duracion <= 60){
    return(as.factor("(30, 60]"))
  }
  if(duracion <= 120){
    return(as.factor("(60, 120]"))
  }
  if(duracion <= 180){
    return(as.factor("(120, 180]"))
  }
  if(duracion <= 240){
    return(as.factor("(180, 240]"))
  }
  if(duracion <= 300){
    return(as.factor("(240, 300]"))
  }
  if(duracion < 600){
    return(as.factor("(300, 600]"))
  }
  return(as.factor("600 o más"))
}
disRelease <- function(difference){
  if(difference <= 6){ return(as.factor("[0, 6]"))}
  if(difference <= 24){ return(as.factor("Entre 6 horas y 1 día"))}
  if(difference <= 72){ return(as.factor("Entre 1 y 3 días"))}
  if(difference <= 168){ return(as.factor("Entre 3 días y 1 semana"))}
  if(difference <= 372){ return(as.factor("Entre 1 y 2 semanas"))}
  return(as.factor("Más de 2 semanas"))
}
disPenetracion <- function(penetracion){
  if(penetracion > 0.9){ return(as.factor("Más del 90%"))}
  if(penetracion >= 0.8){ return(as.factor("Entre 80% y 90%"))}
  if(penetracion >= 0.7){ return(as.factor("Entre 70% y 80%"))}
  if(penetracion >= 0.6){ return(as.factor("Entre 60% y 70%"))}
  if(penetracion >= 0.5){ return(as.factor("Entre 50% y 60%"))}
  if(penetracion >= 0.4){ return(as.factor("Entre 40% y 50%"))}
  if(penetracion >= 0.3){ return(as.factor("Entre 30% y 40%"))}
  if(penetracion >= 0.2){ return(as.factor("Entre 20% y 30%"))}
  if(penetracion >= 0.1){ return(as.factor("Entre 10% y 20%"))}
  return(as.factor("Menos del 10%"))
}
videos_discrete <- videos
videos_discrete$active_users                  <- sapply(videos$active_users, disActiveUsers)
videos_discrete$active_raffles                <- sapply(videos$active_raffles, disRaffles)
videos_discrete$duracion                      <- sapply(videos$duracion, disDuracion)
videos_discrete$release_difference_hours      <- sapply(videos$release_difference_hours, disRelease)
videos_discrete$penetracion                   <- sapply(videos$penetracion, disPenetracion)

videos_test <- videos_discrete[c(
  "active_raffles",
  "active_users"
)]

videos_test <- videos_discrete[c(
  "duracion",
  "release_difference_hours",
  "penetracion"
)]

apriori_videos_appereance_list = list(lhs = c("active_raffles=10 o más", "active_raffles=Entre 7 y 9", "active_raffles=Entre 5 y 6", "active_raffles=Entre 3 y 4", "active_raffles=2 o menos"),default = "rhs")
res <- apriori(videos_test, parameter =list(support=0.04,confidence=0.4), appearance = apriori_videos_appereance_list)
inspect(res)

# JUNTAR PRIEMROS 2 RANGOS DE DURACION?

res <- apriori(videos_test, parameter =list(support=0.001,confidence=0.4))
inspect(res)
sqldf("SELECT * FROM videos_test WHERE active_raffles = '10 o más'")

sqldf("SELECT active_users FROM videos_processed WHERE active_raffles >= 9 AND active_users < 100")
