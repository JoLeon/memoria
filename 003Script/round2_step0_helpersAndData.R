# HELPERS

# WIN
 setwd("C:/Users/J/Documents/GitHub/memoria")

minmaxNormalization <- function(sample,min,max){
  if(is.na(sample)){
    return(NA)
  }
  else{
    return((sample-min)/(max-min))
  }
}
zscoreNormalization <- function(sample,sd,mean){
  if(is.na(sample)){
    return(NA)
  }
  else{
    return((sample-mean)/(sd))
  }
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
getUserQuality <- function(dif_last_first,share_frequency){
  if(dif_last_first==-1){
    as.factor("Not interested/Didn't get it")
  }
  else{
    if(dif_last_first==0){
      as.factor("Not captured")
    }
    else{
      if(share_frequency<=1){
        if(dif_last_first>=30 && dif_last_first<=60){
          as.factor("Daily, for a month")
        }
        else{
          if(dif_last_first>=7 && dif_last_first<=29){
            as.factor("Daily, for a week")
          }
          else{
            if(dif_last_first <=6){
              as.factor("Lost")
            }
            else{
              as.factor("Daily, constant")
            }
          }
        }
      }
      else{
        if(share_frequency<=7){
          if(dif_last_first>=30 && dif_last_first<=60){
            as.factor("Weekly, for a month")
          }
          else{
            if(dif_last_first<=29){
              as.factor("Lost")
            }
            else{
              as.factor("Weekly, constant")
            }
          }
        }
        else{
          as.factor("Lost")
        }
      }
    }
  }
}
getUserQualityId <- function(dif_last_first,share_frequency){
    # 0: no interesado
    # 1: no capturado
    # 2: perdido
    # 3: diario por un mes
    # 4: diario por una semana
    # 5: diario constante
    # 6: semenal por un mes
    # 7: semanal constante
  if(dif_last_first==-1){
    as.factor("0")
  }
  else{
    if(dif_last_first==0){
      as.factor("1")
    }
    else{
      if(share_frequency<=1){
        if(dif_last_first>=30 && dif_last_first<=60){
          as.factor("3")
        }
        else{
          if(dif_last_first>=7 && dif_last_first<=29){
            as.factor("4")
          }
          else{
            if(dif_last_first <=6){
              as.factor("2")
            }
            else{
              as.factor("5")
            }
          }
        }
      }
      else{
        if(share_frequency<=7){
          if(dif_last_first>=30 && dif_last_first<=60){
            as.factor("6")
          }
          else{
            if(dif_last_first<=29){
              as.factor("2")
            }
            else{
              as.factor("7")
            }
          }
        }
        else{
          as.factor("2")
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
    probability <- runif(1,0,1)
    if(probability > 0.2){
      return(as.factor(sample(c(20,21,22,23,24,25,26), size = 1, replace = TRUE, prob = c(0.05, 0.1, 0.3 ,0.15 ,0.20 ,0.15 ,0.05))))
    }
    if(probability > 0.15){
      return(as.factor(sample(c(17,18,19,27,28,29,30,31,32), size = 1, replace = TRUE, prob = c(0.055,0.055,0.1,0.15,0.15,0.15,0.1,0.1,0.1))))
    }
    if(probability > 0.10){
      return(as.factor(sample(c(13,14,15,16,33,34,35), size = 1, replace = TRUE, prob = c(0.01,0.09,0.15,0.25,0.25,0.15,0.1))))
    }
    return(as.factor(round(runif(1,36,47))))
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
      kmeans_result <- kmeans(data,k,iter.max = 40)
      kbet <- append(kbet,kmeans_result$betweenss)
      kwit <- append(kwit,kmeans_result$tot.withinss)
    }
    result[current_row,] <- c(k, mean(kbet), mean(kwit))
  }
  return(result)
}
testingOptimalKFixed <- function(data,kmin, kmax){
  #optimistic <- as.integer(sqrt(nrow(data)/2))
  #kmin <- optimistic-interval
  #kmax <- optimistic+interval
  #if(kmin <= 0){
  #kmin <- 2
  #}
  result <- data.frame(list(K = 1, betweens_to_max = 2, withins_to_min = 3))
  print(paste("Iterando entre",kmin,"y",kmax,"..."))
  current_row <- 0
  for(k in kmin:kmax){
    print(paste("k:",k))
    current_row <- current_row + 1
    kbet <- c()
    kwit <- c()
    for(i in 1:100){
      kmeans_result <- kmeans(data,k,iter.max = 40)
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
  if(x < 0 || is.na(x)){
    x <- round(runif(1,0,30))
  }
  if(x == 0){
    return(0)
  }
  return(round((x/60/60/24),0))
}
releaseDifferenceToHours <- function(x){
  if(x < 0 || is.na(x)){
    x <- round(runif(1,0,30))
  }
  if(x == 0){
    return(0)
  }
  return(round((x/60/60),0))
}
getAvgPpv <- function(views, points){
  if(points == 0 || views == 0){ 
    return(NA)
  }
  else { 
    return(points/views) 
  }
}
isDepleted <- function(saldo, points_given){
  if(saldo < 100 && points_given > 100){
    return(as.factor("1"))
  }
  else{
    return(as.factor("0"))
  }
}
getReleaseDay <- function(date){
  return(as.factor(weekdays(as.Date(date))))
}
max_na <- function(vector){
  maximo <- -Inf
  for(i in 1:length(vector)){
    if(!is.na(vector[i])){
      if(vector[i] > maximo){
        maximo <- vector[i]
      }
    }
  }
  return(maximo)
}
min_na <- function(vector){
  maximo <- Inf
  for(i in 1:length(vector)){
    if(!is.na(vector[i])){
      if(vector[i] < maximo){
        maximo <- vector[i]
      }
    }
  }
  return(maximo)
}
mean_na <- function(vector){
  registros <- 0
  total <- 0
  for(i in 1:length(vector)){
    if(!is.na(vector[i])){
      registros <- registros + 1
      total <- total + vector[i]
    }
  }
  return(total/registros)
}
fixCategoria <- function(cat){
  if(!is.na(cat) && cat == "M"){
    return(as.factor("MISC"))
  }
  else{
    return(as.factor(cat))
  }
}
getActivityOnRegister <- function(fecha_afiliacion_usuario){
  query_string = paste("SELECT sum(total_views)/count(*) FROM videos WHERE release_date_minus_3 < '",fecha_afiliacion_usuario,"' AND release_date_plus_3 > '",fecha_afiliacion_usuario,"'", sep="")
  total <- sqldf(query_string)
  return(as.numeric(total[1,1]))
}

# LIBRARIES

library(arules)
library(klaR)
library(cluster)
library(graphics)
library(fpc)
library(optpart)
library(Hmisc)
library(foreach)
library(sqldf)
library(e1071)
library(party)
library(rpart)
# MISSING IN UBUNTU!!!
library(partykit)
library(rpart.plot)
library(randomForest)

# DATA

#WIN
users_raw <- read.csv("002RawData/users.csv", header = TRUE, sep=";")
videos_raw <- read.csv("002RawData/videos.csv", header = TRUE, sep=";")
#shares_raw <- read.csv("../002Data/shares.csv", header = TRUE, sep=";")

#LINUX
#users_raw <- read.csv("/home/jleon/memoria/002RawData/users.csv", header = TRUE, sep=";")
#videos_raw <- read.csv("/home/jleon/memoria/002RawData/videos.csv", header = TRUE, sep=";")
#shares_raw <- read.csv("/home/jleon/memoria/002RawData/shares.csv", header = TRUE, sep=";")