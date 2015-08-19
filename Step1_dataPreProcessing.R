# HELPERS

minmaxNormalization <- function(sample,min,max){
  return((sample-min)/(max-min))
}
zscoreNormalization <- function(sample,sd,mean){
  return((sample-mean)/(sd))
}
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
testingOptimalKVector <- function(data,interval){
  optimistic <- as.integer(sqrt(length(data)/2))
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
releaseDifferenceToDays <- function(x){
  if(is.na(x) || x == 0){ 
    return(NA)
  }
  else {
    return(round((x/60/60/24),0))
  }
}
getAvgPpv <- function(views, points){
  if(points == 0 || views == 0){ 
    return(NA)
  }
  else { 
    return(points/views) 
  }
}

# LIBRARIES

library(arules)
library(klaR)
library(cluster)
library(graphics)
library(fpc)
library(optpart)
library(Hmisc)

# LOADING THE DATA

users <- read.csv("Data/users.csv", header = TRUE, sep=";")
videos <- read.csv("Data/videos.csv", header = TRUE, sep=";")

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
    
  # Renombrar variable
    names(users)[names(users)=="difference_last_and_first_share"] <- "total_activity"
    
  # Quitar variables innecesarias para el estudio
    users$stdv_share_difference <- NULL
    users$difference_last_raffle_first_share <- NULL
    
  # Discretización de variables numéricas: 
  # se desecha la discretización por intervalos iguales (width) debido a que se sabe de la presencia de outliers,
  # lo que genera intervalos con frecuencias inútiles (dejando +90% de los datos en un único rango), se utiliza la función "discretize" del paquete arules
  # DISCLAIMER: En algunos casos, se hace necesario "inflar" la cantidad de las categorías ya que hay demasiados valores en 0 y el algoritmo usará muy pocas categorías para los diferentes valores  
    users$rango_tickets_canjeados <- discretize(users$tickets_canjeados, method="frequency", categories=50) 
    users$rango_puntos_historicos <- discretize(users$puntos_historicos, method="frequency", categories=30)
    users$rango_puntos_gastados <- discretize(users$puntos_gastados, method="frequency", categories=50)
    users$rango_shares_totales <- discretize(users$shares_totales, method="frequency", categories=20)
    users$rango_concursos_participados <- discretize(users$concursos_participados, method="frequency", categories=50)
    users$rango_shares_frequecy <- discretize(users$shares_frequency, method="frequency", categories=40)
    users$rango_total_activity <- discretize(users$total_activity, method="frequency", categories=45)
    
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
      
      
      
# MINABLE VIEW: USERS
#
# Se preprocesan algunas variables para que sean útiles al estudio
#
  # Arreglo de variables para ser más "humanas"
    videos$release_difference <- sapply(videos$release_difference, releaseDifferenceToDays)
    videos$avg_ppv <- mapply(getAvgPpv, videos$total_views, videos$points_given)
    
    videos$release_difference_rango <- mapply(generalRangesWithZero, videos_apriori$release_difference , 3, "dias")
    videos$release_difference_rango <- as.factor(videos_apriori$release_difference_rango)
    
    users$hora_afiliacion <- as.factor(users$hora_afiliacion)
    users$dia_afiliacion <- sapply(users$dia_afiliacion, getDayName)
    users$edad <- sapply(users$nacimiento, getEdad)
    users$tickets_canjeados <- sapply(users$tickets_canjeados, cleanTicketsCanjeados)
    users$shares_frequency <- mapply(setUserShareFrequency,users$difference_last_and_first_share,users$shares_totales) # Días
    
    # Renombrar variable
    names(users)[names(users)=="difference_last_and_first_share"] <- "total_activity"
    
    # Quitar variables innecesarias para el estudio
    users$stdv_share_difference <- NULL
    users$difference_last_raffle_first_share <- NULL
    
    # Discretización de variables numéricas: 
    # se desecha la discretización por intervalos iguales (width) debido a que se sabe de la presencia de outliers,
    # lo que genera intervalos con frecuencias inútiles (dejando +90% de los datos en un único rango), se utiliza la función "discretize" del paquete arules
    # DISCLAIMER: En algunos casos, se hace necesario "inflar" la cantidad de las categorías ya que hay demasiados valores en 0 y el algoritmo usará muy pocas categorías para los diferentes valores  
    users$rango_tickets_canjeados <- discretize(users$tickets_canjeados, method="frequency", categories=50) 
    users$rango_puntos_historicos <- discretize(users$puntos_historicos, method="frequency", categories=30)
    users$rango_puntos_gastados <- discretize(users$puntos_gastados, method="frequency", categories=50)
    users$rango_shares_totales <- discretize(users$shares_totales, method="frequency", categories=20)
    users$rango_concursos_participados <- discretize(users$concursos_participados, method="frequency", categories=50)
    users$rango_shares_frequecy <- discretize(users$shares_frequency, method="frequency", categories=40)
    users$rango_total_activity <- discretize(users$total_activity, method="frequency", categories=45)
    
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
