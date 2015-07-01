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
# Libreria para reglas de asociación
library(arules)

users <- read.csv("Data/users.csv", header = TRUE, sep=";")

users$hora_afiliacion <- as.factor(users$hora_afiliacion)
users$dia_afiliacion <- sapply(users$dia_afiliacion, getDayName)

users$edad <- sapply(users$nacimiento, getEdad)

users$tickets_canjeados <- sapply(users$tickets_canjeados, cleanTicketsCanjeados)

users$tickets_canjeados_rango <- mapply(generalRanges, users$tickets_canjeados, 5, "tickets")
users$tickets_canjeados_rango <- as.factor(users$tickets_canjeados_rango)

users$puntos_historicos_rango <- mapply(generalRanges, users$puntos_historicos, 500, "puntos")
users$puntos_historicos_rango <- as.factor(users$puntos_historicos_rango)

users$puntos_gastados_rango <- mapply(generalRanges, users$puntos_gastados, 500, "puntos")
users$puntos_gastados_rango <- as.factor(users$puntos_gastados_rango)

users$shares_totales_rango <- mapply(generalRanges, users$shares_totales, 5, "shares")
users$shares_totales_rango <- as.factor(users$shares_totales_rango)

users$concursos_participados_rango <- mapply(generalRanges, users$concursos_participados, 5, "concursos")
users$concursos_participados_rango <- as.factor(users$concursos_participados_rango)

users$shares_frequency <- mapply(setUserShareFrequency,users$difference_last_and_first_share,users$shares_totales)
users$quality <- mapply(getUserQuality,users$difference_last_and_first_share,users$shares_frequency)

str(users)

keep <- c("uni", "genero", "hora_afiliacion", "dia_afiliacion", "categoria_dominante","edad", "quality", "tickets_canjeados_rango", "puntos_historicos_rango", "puntos_gastados_rango", "shares_totales_rango", "concursos_participados_rango")
users_factors <- users[keep]
rules <- apriori(users_factors)
inspect(rules)

videos <- read.csv("Data/videos.csv", header = TRUE, sep=";")
keep <- c("category","points_per_view","points_given","release_difference","duracion", "total_views","shares_first_day","shares_first_week","shares_first_month","total_shares","total_users_at_release","X1_week_active_users_at_release","X1_week_new_users_at_release","active_raffles_at_release")
videos_factors <- videos[keep]

videos_factors$duracion_rangos <- mapply(generalRanges, videos_factors$duracion, 30, "segundos")
videos_factors$duracion_rangos <- as.factor(videos_factors$duracion_rangos)

videos_factors$active_raffles_at_release_rangos <- mapply(generalRanges, videos_factors$active_raffles_at_release, 3, "concursos")
videos_factors$active_raffles_at_release_rangos <- as.factor(videos_factors$active_raffles_at_release_rangos)

videos_factors$new_users_rangos <- mapply(generalRanges, videos_factors$X1_week_new_users_at_release , 15, "usuarios")
videos_factors$new_users_rangos <- as.factor(videos_factors$new_users_rangos)

videos_factors$active_users_rangos <- mapply(generalRanges, videos_factors$X1_week_active_users_at_release , 15, "usuarios")
videos_factors$active_users_rangos <- as.factor(videos_factors$active_users_rangos)

videos_factors$total_users_rangos <- mapply(generalRanges, videos_factors$total_users_at_release , 200, "usuarios")
videos_factors$total_users_rangos <- as.factor(videos_factors$total_users_rangos)

videos_factors$total_shares_rangos <- mapply(generalRanges, videos_factors$total_shares , 10, "shares")
videos_factors$total_shares_rangos <- as.factor(videos_factors$total_shares_rangos)

videos_factors$shares_first_month_rango <- mapply(generalRanges, videos_factors$shares_first_month, 5, "shares")
videos_factors$shares_first_month_rango <- as.factor(videos_factors$shares_first_month_rango)

videos_factors$shares_first_week_rango <- mapply(generalRanges, videos_factors$shares_first_week, 5, "shares")
videos_factors$shares_first_week_rango <- as.factor(videos_factors$shares_first_week_rango)

videos_factors$shares_first_day_rango <- mapply(generalRanges, videos_factors$shares_first_day, 5, "shares")
videos_factors$shares_first_day_rango <- as.factor(videos_factors$shares_first_day_rango)

videos_factors$total_views_rango <- mapply(generalRanges, videos_factors$total_views , 500, "vistas")
videos_factors$total_views_rango <- as.factor(videos_factors$total_views_rango)

videos_factors$release_difference <- sapply(videos_factors$release_difference, function(x){if(is.na(x) || x == 0){ return(NA)} else {return(round((x/60/60/24),0))}})
videos_factors$release_difference_rango <- mapply(generalRangesWithZero, videos_factors$release_difference , 3, "dias")
videos_factors$release_difference_rango <- as.factor(videos_factors$release_difference_rango)

videos_factors$avg_ppv <- mapply(function(views, points){if(points == 0 || views == 0){ return(NA) } else { return(points/views) }}, videos_factors$total_views, videos_factors$points_given)
videos_factors$avg_ppv_rangos <- mapply(generalRanges, videos_factors$avg_ppv , 10, "puntos")
videos_factors$avg_ppv_rangos <- as.factor(videos_factors$avg_ppv_rangos)

keep = c("category", "active_raffles_at_release_rangos", "new_users_rangos", "active_users_rangos", "total_shares_rangos", "shares_first_day_rango","total_views_rango","release_difference_rango","avg_ppv_rangos")
videos_factors <- videos_factors[keep]

rules_videos <- apriori(videos_factors)
inspect(rules_videos)
