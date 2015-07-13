# Utilities
getDayName <- function(x){
  if(x==0){
    as.factor("Lunes")
  }
  else{
    if(x==1){
      as.factor("Martes")
    }
    else{
      if(x==2){
        as.factor("Miercoles")
      }
      else{
        if(x==3){
          as.factor("Jueves")
        }
        else{
          if(x==4){
            as.factor("Viernes")
          }
          else{
            if(x==5){
              as.factor("Sabado")
            }
            else{
              as.factor("Domingo")
            }
          }
        }
      }
    }
  }
}
setUserShareFrequency <- function(x,y){
  if(y==0){
    0
  }
  else{
    x/y
  }
}
getUserQuality <- function(x,y){
  if(x==-1){
    as.factor("No interesado/No comprendió")
  }
  else{
    if(x==0){
      as.factor("No capturado")
    }
    else{
      if(y<=1){
        if(x>=30 && x<=60){
          as.factor("Diario por un mes")
        }
        else{
          if(x>=7 && x<=29){
            as.factor("Diario por una semana")
          }
          else{
            if(x <=6){
              as.factor("Perdido")
            }
            else{
              as.factor("Diario constante")
            }
          }
        }
      }
      else{
        if(y<=7){
          if(x>=30 && x<=60){
            as.factor("Semenal por un mes")
          }
          else{
            if(x<=29){
              as.factor("Perdido")
            }
            else{
              as.factor("Semanal constante")
            }
          }
        }
        else{
          as.factor("Perdido")
        }
      }
    }
  }
}
getEdad <- function(fecha_nacimiento){
  if(!is.na(fecha_nacimiento)){
    nacimiento <- strptime(fecha_nacimiento, format = "%Y-%m-%d")
    hoy <- Sys.Date()
    edad <- difftime(hoy,nacimiento)
    
    as.factor(as.integer(as.numeric(edad, units="days")/365))
  }
  else{
    as.factor(NA)
  }
}
cleanTicketsCanjeados <- function(tickets_canjeados){
  if(is.na(tickets_canjeados)){
    0
  }
  else{
    as.numeric(tickets_canjeados)
  }
}
generalRanges <- function(variable, range, unit){
  if(is.na(variable) || variable == 0){
    return(NA)
  }
  else{
    rango = as.integer(variable/range)
    paste("Entre",((rango*range)+1),"y",((rango*range)+range),unit)
  }
}
generalRangesWithZero <- function(variable, range, unit){
  if(is.na(variable)){
    return(NA)
  }
  else{
    rango = as.integer(variable/range)
    paste("Entre",((rango*range)),"y",((rango*range)+(range-1)),unit)
  }
}
testingOptimalK <- function(data,interval){
  optimistic <- as.integer(sqrt(nrow(data)/2))
  kmin <- optimistic-interval
  kmax <- optimistic+interval
  if(kmin <= 0){
    kmin <- 2
  }
  result <- data.frame(list(K = 1, betweens_to_max = 2, withins_to_min = 3))
  print(paste("Iterando entre",kmin,"y",kmax,"..."))
  current_row <- 0
  for(k in kmin:kmax){
    print(paste("k:",k))
    current_row <- current_row + 1
    kbet <- c()
    kwit <- c()
    for(i in 1:100){
      kmeans_result <- kmeans(data,k)
      kbet <- append(kbet,kmeans_result$betweenss)
      kwit <- append(kwit,kmeans_result$tot.withinss)
    }
    result[current_row,] <- c(k, mean(kbet), mean(kwit))
  }
  return(result)
}

# Librerias
library(arules)
library(klaR)
library(cluster)
library(graphics)
library(fpc)
library(optpart)
library(outliers)

# Data

users <- read.csv("Data/users.csv", header = TRUE, sep=";")
videos <- read.csv("Data/videos.csv", header = TRUE, sep=";")


#
#   SACANDO OUTLIERS
#

users_clean <- users
str(users_clean)

# puntos_historicos
plot(users_clean$puntos_historicos)
plot(users_clean$puntos_historicos,ylim=c(0,80000))
users_clean <- subset(users_clean, puntos_historicos <= 80000)
plot(users_clean$puntos_historicos)

# puntos

plot(users_clean$puntos)
plot(users_clean$puntos,ylim=c(0,60000))
users_clean <- subset(users_clean, puntos <= 60000)
plot(users_clean$puntos)

# puntos_gastados

plot(users_clean$puntos_gastados)
plot(users_clean$puntos_gastados,ylim=c(0,60000))
users_clean <- subset(users_clean, puntos_gastados <= 60000)
plot(users_clean$puntos_gastados)

# shares_totales

plot(users_clean$shares_totales)
plot(users_clean$shares_totales,ylim=c(0,100))
users_clean <- subset(users_clean, shares_totales <= 100)
plot(users_clean$shares_totales)

# concursos_participados

plot(users_clean$concursos_participados)
plot(users_clean$concursos_participados,ylim=c(0,10))
users_clean <- subset(users_clean, concursos_participados <= 10)
plot(users_clean$concursos_participados)

# tickets_canjeados

plot(users_clean$tickets_canjeados)
plot(users_clean$tickets_canjeados,ylim=c(0,45))
users_clean$tickets_canjeados <- sapply(users_clean$tickets_canjeados, function(x){if(is.na(x)){ return(0)} else {return(x)} })
users_clean <- subset(users_clean, tickets_canjeados <= 45)
plot(users_clean$tickets_canjeados)


videos_clean <- videos
str(videos_clean)

# points_per_view

plot(videos_clean$points_per_view)
plot(videos_clean$points_per_view,ylim=c(0,110))
videos_clean <- subset(videos_clean, points_per_view <= 110)
plot(videos_clean$points_per_view)

# duracion

plot(videos_clean$duracion)
plot(videos_clean$duracion,ylim=c(0,600))
videos_clean <- subset(videos_clean, duracion <= 600)
plot(videos_clean$duracion)

# release_difference

plot(videos_clean$release_difference)
plot(videos_clean$release_difference,ylim=c(0,1000000))
videos_clean <- subset(videos_clean, release_difference <= 1000000)
videos_clean <- subset(videos_clean, release_difference >= 0)
plot(videos_clean$release_difference)

# total_views

plot(videos_clean$total_views)
plot(videos_clean$total_views,ylim=c(0,6500))
videos_clean <- subset(videos_clean, total_views <= 6500)
plot(videos_clean$total_views)

# points_given

plot(videos_clean$points_given)
plot(videos_clean$points_given,ylim=c(0,100000))
videos_clean <- subset(videos_clean, points_given <= 100000)
plot(videos_clean$points_given)

# shares_first_day

plot(videos_clean$shares_first_day)
plot(videos_clean$shares_first_day,ylim=c(0,15))
videos_clean <- subset(videos_clean, shares_first_day <= 15)
plot(videos_clean$shares_first_day)

# shares_first_week

plot(videos_clean$shares_first_week)
plot(videos_clean$shares_first_week,ylim=c(0,30))
videos_clean <- subset(videos_clean, shares_first_week <= 30)
plot(videos_clean$shares_first_week)

# total_shares

plot(videos_clean$total_shares)
plot(videos_clean$total_shares,ylim=c(0,80))
videos_clean <- subset(videos_clean, total_shares <= 80)
plot(videos_clean$total_shares)

#
#   APRIORI + ECLAT
#

# USERS

users_apriori <- users
users_apriori$hora_afiliacion <- as.factor(users_apriori$hora_afiliacion)
users_apriori$dia_afiliacion <- sapply(users_apriori$dia_afiliacion, getDayName)

users_apriori$edad <- sapply(users_apriori$nacimiento, getEdad)

users_apriori$tickets_canjeados <- sapply(users_apriori$tickets_canjeados, cleanTicketsCanjeados)

users_apriori$tickets_canjeados_rango <- mapply(generalRanges, users_apriori$tickets_canjeados, 5, "tickets")
users_apriori$tickets_canjeados_rango <- as.factor(users_apriori$tickets_canjeados_rango)

users_apriori$puntos_historicos_rango <- mapply(generalRanges, users_apriori$puntos_historicos, 500, "puntos")
users_apriori$puntos_historicos_rango <- as.factor(users_apriori$puntos_historicos_rango)

users_apriori$puntos_gastados_rango <- mapply(generalRanges, users_apriori$puntos_gastados, 500, "puntos")
users_apriori$puntos_gastados_rango <- as.factor(users_apriori$puntos_gastados_rango)

users_apriori$shares_totales_rango <- mapply(generalRanges, users_apriori$shares_totales, 5, "shares")
users_apriori$shares_totales_rango <- as.factor(users_apriori$shares_totales_rango)

users_apriori$concursos_participados_rango <- mapply(generalRanges, users_apriori$concursos_participados, 5, "concursos")
users_apriori$concursos_participados_rango <- as.factor(users_apriori$concursos_participados_rango)

users_apriori$shares_frequency <- mapply(setUserShareFrequency,users_apriori$difference_last_and_first_share,users_apriori$shares_totales)
users_apriori$quality <- mapply(getUserQuality,users_apriori$difference_last_and_first_share,users_apriori$shares_frequency)

keep <- c("uni", "genero", "hora_afiliacion", "dia_afiliacion", "categoria_dominante","edad", "quality", "tickets_canjeados_rango", "puntos_historicos_rango", "puntos_gastados_rango", "shares_totales_rango", "concursos_participados_rango")
users_apriori <- users_apriori[keep]

rules_users_apriori <- apriori(users_apriori, list(support=0.1))
inspect(rules_users_apriori)

rules_users_eclat <- eclat(users_apriori, list(support=0.2))
inspect(rules_users_eclat)

#VIDEOS

keep <- c("category","points_per_view","points_given","release_difference","duracion", "total_views","shares_first_day","shares_first_week","shares_first_month","total_shares","total_users_at_release","X1_week_active_users_at_release","X1_week_new_users_at_release","active_raffles_at_release")
videos_apriori <- videos[keep]

videos_apriori$duracion_rangos <- mapply(generalRanges, videos_apriori$duracion, 30, "segundos")
videos_apriori$duracion_rangos <- as.factor(videos_apriori$duracion_rangos)

videos_apriori$active_raffles_at_release_rangos <- mapply(generalRanges, videos_apriori$active_raffles_at_release, 3, "concursos")
videos_apriori$active_raffles_at_release_rangos <- as.factor(videos_apriori$active_raffles_at_release_rangos)

videos_apriori$new_users_rangos <- mapply(generalRanges, videos_apriori$X1_week_new_users_at_release , 15, "usuarios")
videos_apriori$new_users_rangos <- as.factor(videos_apriori$new_users_rangos)

videos_apriori$active_users_rangos <- mapply(generalRanges, videos_apriori$X1_week_active_users_at_release , 15, "usuarios")
videos_apriori$active_users_rangos <- as.factor(videos_apriori$active_users_rangos)

videos_apriori$total_users_rangos <- mapply(generalRanges, videos_apriori$total_users_at_release , 200, "usuarios")
videos_apriori$total_users_rangos <- as.factor(videos_apriori$total_users_rangos)

videos_apriori$total_shares_rangos <- mapply(generalRanges, videos_apriori$total_shares , 10, "shares")
videos_apriori$total_shares_rangos <- as.factor(videos_apriori$total_shares_rangos)

videos_apriori$shares_first_month_rango <- mapply(generalRanges, videos_apriori$shares_first_month, 5, "shares")
videos_apriori$shares_first_month_rango <- as.factor(videos_apriori$shares_first_month_rango)

videos_apriori$shares_first_week_rango <- mapply(generalRanges, videos_apriori$shares_first_week, 5, "shares")
videos_apriori$shares_first_week_rango <- as.factor(videos_apriori$shares_first_week_rango)

videos_apriori$shares_first_day_rango <- mapply(generalRanges, videos_apriori$shares_first_day, 5, "shares")
videos_apriori$shares_first_day_rango <- as.factor(videos_apriori$shares_first_day_rango)

videos_apriori$total_views_rango <- mapply(generalRanges, videos_apriori$total_views , 50, "vistas")
videos_apriori$total_views_rango <- as.factor(videos_apriori$total_views_rango)

videos_apriori$release_difference <- sapply(videos_apriori$release_difference, function(x){if(is.na(x) || x == 0){ return(NA)} else {return(round((x/60/60/24),0))}})
videos_apriori$release_difference_rango <- mapply(generalRangesWithZero, videos_apriori$release_difference , 3, "dias")
videos_apriori$release_difference_rango <- as.factor(videos_apriori$release_difference_rango)

videos_apriori$avg_ppv <- mapply(function(views, points){if(points == 0 || views == 0){ return(NA) } else { return(points/views) }}, videos_apriori$total_views, videos_apriori$points_given)
videos_apriori$avg_ppv_rangos <- mapply(generalRanges, videos_apriori$avg_ppv , 10, "puntos")
videos_apriori$avg_ppv_rangos <- as.factor(videos_apriori$avg_ppv_rangos)

keep <- c("active_users_rangos", "total_shares_rangos","total_views_rango","release_difference_rango","avg_ppv_rangos")
videos_apriori <- videos_apriori[keep]

rules_videos_apriori <- apriori(videos_apriori, list(support=0.2))
inspect(rules_videos_apriori)

rules_videos_eclat <- eclat(videos_apriori, list(support=0.4))
inspect(rules_videos_eclat)

#
#   K-MEANS + K-MODES
#

# USERS

keep <-c("puntos_historicos", "puntos", "puntos_gastados", "shares_totales", "concursos_participados","tickets_canjeados","difference_last_and_first_share")
users_kmeans <- users[keep]
rownames(users_kmeans) <- NULL
users_kmeans$tickets_canjeados <- sapply(users_kmeans$tickets_canjeados, function(x){if(is.na(x)){ return(0)} else {return(x)} })
users_kmeans <- users_kmeans[users_kmeans$difference_last_and_first_share != -1,]

users_kmeans_clean <- users_clean[keep]
rownames(users_kmeans_clean) <- NULL
users_kmeans_clean$tickets_canjeados <- sapply(users_kmeans_clean$tickets_canjeados, function(x){if(is.na(x)){ return(0)} else {return(x)} })
users_kmeans_clean <- users_kmeans_clean[users_kmeans_clean$difference_last_and_first_share != -1,]

# VIDEOS

keep <- c("points_per_view","duracion","total_views","total_shares","total_users_at_release","X1_week_active_users_at_release")
videos_kmeans <- videos[keep]
#videos_kmeans <- videos_kmeans[videos_kmeans$release_difference != -1, ] #SACAR RELEASE DIFFERENCE -1 (NA)
videos_kmeans <- videos_kmeans[complete.cases(videos_kmeans), ] # SACAR ROWS CONMISSING VALUES
names(videos_kmeans)[names(videos_kmeans)=="X1_week_active_users_at_release"] <- "active_users"
names(videos_kmeans)[names(videos_kmeans)=="total_users_at_release"] <- "total_users"

videos_kmeans_clean <- videos_clean[keep]
videos_kmeans_clean <- videos_kmeans_clean[complete.cases(videos_kmeans_clean), ] # SACAR ROWS CONMISSING VALUES
names(videos_kmeans_clean)[names(videos_kmeans_clean)=="X1_week_active_users_at_release"] <- "active_users"
names(videos_kmeans_clean)[names(videos_kmeans_clean)=="total_users_at_release"] <- "total_users"

#   SE BUSCA MAXIMIZAR DISTANCIA ENTRE CENTROS (BETWEENESS) Y MINIMIZAR DISTANCIA ENTRE CENTROS Y SUS DATOS (WITHINESS)
#   MAX(BETWEENSS) y MIN(WITHINSS)

optimos_kmeans_users <- testingOptimalK(users_kmeans,15)
optimos_kmeans_users_clean <- testingOptimalK(users_kmeans_clean,15)

# SE DESPRENDE QUE EL K OPTIMO PARA LOS DATOS ES K = 36
users_kmeans_result <- kmeans(users_kmeans, 36)
users_kmodes_result <- kmodes(users_kmeans, 36)

optimos_kmeans_videos <- testingOptimalK(videos_kmeans,15)

# SE DESPRENDE QUE EL K OPTIMO PARA LOS DATOS ES K = 32
videos_kmeans_result <- kmeans(videos_kmeans, 32)
videos_kmodes_result <- kmodes(videos_kmeans, 32)

#
#   DIST MATRIX
#

videos_dist = dist(videos_kmeans)
users_dist = dist(users_kmeans)

#
#   AGGLOMERATIVE NESTING
#

# USERS

users_agnes_result <- agnes(users, FALSE)
users_discrete_agnes_result <- agnes(users_apriori, FALSE)

# VIDEOS

videos_agnes_result <- agnes(videos, FALSE)
videos_discrete_agnes_result <- agnes(videos_apriori, FALSE)

#
#   DBSCAN
#

# USERS

users_dbscan_result <- dbscan(users_kmeans, eps=10, MinPts=10)
plot(users_dbscan_result, users_kmeans)

# VIDEOS

videos_dbscan_result <- dbscan(videos_kmeans, eps=20)
plot(videos_dbscan_result, videos_kmeans)

#
#   CLIQUIE
#

# USERS

#users_clique_result <- clique(users_dist,30)
