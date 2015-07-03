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

# Librerias
library(arules)
library(klaR)
library(cluster)

# Data

users <- read.csv("Data/users.csv", header = TRUE, sep=";")
videos <- read.csv("Data/videos.csv", header = TRUE, sep=";")

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

rules_users_apriori <- apriori(users_apriori)
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

rules_videos_apriori <- apriori(videos_apriori)
inspect(rules_videos_apriori)

rules_videos_eclat <- eclat(videos_apriori, list(support=0.4))
inspect(rules_videos_eclat)

#
#   K-MEANS + K-MODES
#

# USERS

keep <-c("puntos_historicos", "puntos", "puntos_gastados", "shares_totales", "shares_totales", "concursos_participados","tickets_canjeados","difference_last_and_first_share")
users_kmeans <- users[keep]
rownames(users_kmeans) <- NULL
users_kmeans$tickets_canjeados <- sapply(users_kmeans$tickets_canjeados, function(x){if(is.na(x)){ return(0)} else {return(x)} })
users_kmeans <- users_kmeans[users_kmeans$difference_last_and_first_share != -1,]

observations <- nrow(users_kmeans)
thumbs_clusters <- as.integer(sqrt(observations/2))

users_kmeans_result <- kmeans(users_kmeans, thumbs_clusters)
users_kmodes_result <- kmodes(users_kmeans, thumbs_clusters)

# VIDEOS

keep <- c("points_per_view","duracion","total_views","total_shares","total_users_at_release","X1_week_active_users_at_release")
videos_kmeans <- videos[keep]
#videos_kmeans <- videos_kmeans[videos_kmeans$release_difference != -1, ] #SACAR RELEASE DIFFERENCE -1 (NA)
videos_kmeans <- videos_kmeans[complete.cases(videos_kmeans), ] # SACAR ROWS CONMISSING VALUES
names(videos_kmeans)[names(videos_kmeans)=="X1_week_active_users_at_release"] <- "active_users"
names(videos_kmeans)[names(videos_kmeans)=="total_users_at_release"] <- "total_users"

observations <- nrow(videos_kmeans)
thumbs_clusters <- as.integer(sqrt(observations/2))

videos_kmeans_result <- kmeans(videos_kmeans, thumbs_clusters)
videos_kmodes_result <- kmodes(videos_kmeans, thumbs_clusters)

#
#   AGGLOMERATIVE NESTING
#

# USERS

users_agnes_result <- agnes(users, FALSE)

# VIDEOS

users_agnes_result <- agnes(videos, FALSE)

